.bs_sent <- function(x) {
    quanteda::tokens(x, what = "sentence")[[1]]
}

.bs_sent2 <- function(x) {
    ## to emulate tokenizers::token_sentences
    purrr::map(x, .bs_sent)
}

cal_avg_sentence_length <- function(input_text) {
    purrr::map(input_text, .bs_sent) %>%  purrr::map(tokenizers::tokenize_words) %>% purrr::map(~purrr::map_int(., length)) %>% purrr::map_dbl(mean)
}

## .parse_text_udpipe <- function(input_text, udpipe_model) {
##     as.data.frame(udpipe::udpipe_annotate(object = udpipe_model, x = input_text))
## }

.find_central_verb <- function(parse_tree_data) {
    parse_tree_data %>% dplyr::filter(stringr::str_detect(pos, "^VERB")) %>% dplyr::pull(token_id) -> res
    if (length(res) == 0) {
        parse_tree_data %>% dplyr::filter(stringr::str_detect(tag, "^VB")) %>% dplyr::pull(token_id) -> res
        if (length(res) == 0) {
            ##warning("No Verb. Getting Root instead.")
            res <- .find_root(parse_tree_data)
        }
    }
    return(min(res)[1])
}

.find_terminal <- function(parse_tree_data) {
    parse_tree_data %>% dplyr::filter(pos != "PUNCT")  %>% dplyr::summarise(max_id = max(token_id)) -> res
    if (nrow(res) == 0 | res$max_id[1] == -Inf) {
        return(NA)
    }
    res %>% dplyr::pull(max_id) %>% tail(n = 1) -> terminal
    return(terminal[1])
}

.find_root <- function(parse_tree_data) {
    parse_tree_data$token_id[parse_tree_data$dep_rel == "ROOT"] -> res
    return(res[1])
}

.safe_max <- function(x, replacement) {
    if (length(x) == 0) {
        return(replacement)
    }
    return(max(x))
}

.cal_max_dist_root_raw <- function(parse_tree_data) {
    igraph::graph_from_data_frame(parse_tree_data[parse_tree_data$head_token_id != 0, c('head_token_id', 'token_id')]) -> parse_tree
    root_node_id <- which(igraph::V(parse_tree)$name == .find_root(parse_tree_data))
    termin_node_id <- which(igraph::V(parse_tree)$name == .find_terminal(parse_tree_data))
    if (length(termin_node_id) == 0) {
        termin_node_id <- NA
    }
    central_node_id <- which(igraph::V(parse_tree)$name == .find_central_verb(parse_tree_data))
    central_node_paths <- igraph::all_simple_paths(parse_tree, from = central_node_id)
    if (length(central_node_paths) == 0) {
        ### return the theoretical maximum
        syntactic_dependency <- igraph::diameter(parse_tree)
    } else {
        central_node_paths %>% purrr::map_int(length) %>% .safe_max(replacement = igraph::diameter(parse_tree)) -> syntactic_dependency
    }
    if (is.na(termin_node_id) | root_node_id == termin_node_id) {
        syntactic_depth <- 0
    } else {
        igraph::all_simple_paths(parse_tree, from = root_node_id, to = termin_node_id) %>% purrr::map_int(length) %>% .safe_max(replacement = 0) -> syntactic_depth
    }
    return(tibble::tibble(syntactic_depth = syntactic_depth, syntactic_dependency = syntactic_dependency))
}

.cal_mean_dist_root_sentences <- function(parsed_content) {
    parsed_content %>% dplyr::group_by(sentence_id) %>% dplyr::group_nest() %>% dplyr::pull(data) %>% purrr::map_dfr(.cal_max_dist_root_raw)  %>% dplyr::summarise(syntactic_depth = mean(syntactic_depth, na.rm = TRUE), syntactic_dependency = mean(syntactic_dependency, na.rm = TRUE))
}

cal_mean_dist_root <- function(parsed_entire_content, parallel, .progress, plan = "multiprocess") {
    if (parallel) {
        future::plan(plan)
        furrr::future_map_dfr(parsed_entire_content, .cal_mean_dist_root_sentences, .progress = .progress)
    } else {
        purrr::map_dfr(parsed_entire_content, .cal_mean_dist_root_sentences)
    }
}

### steal from https://github.com/chainsawriot/quanteda/blob/yule_i/R/textstat_lexdiv.R
### hopefully the PR is accepted.

.cal_yule_i <- function(x) {
    ViN <- lapply(quanteda::docnames(x), function(y) {
        result <- as.data.frame(table(Matrix::colSums(x[y, ])), stringsAsFactors = FALSE)
        names(result) <- c("i", "ViN")
        result[["i"]] <- as.integer(result[["i"]])
        result[["n_tokens"]] <- quanteda::ntoken(x)[y]
        result[["n_types"]] <- quanteda::ntype(x)[y]
        subset(result, result$i > 0)
    })
    M_1 <- quanteda::ntype(x)
    M_2 <- vapply(ViN, function(y) sum(y$ViN * y$i^2), numeric(1))
    yule_i <- (M_1 ^ 2) / (M_2 - M_1)
    yule_i[yule_i == Inf] <- 0
    return(yule_i)
}

.cal_mtld_raw <- function(input_text, mtld_threshold = 0.72, mtld_min_ntoken = 10, backward = FALSE) {
    current_ntoken <- 0
    current_ttr <- 0
    types <- c()
    sent_brks <- quanteda::tokens(input_text, what = 'word')[[1]]
    if (backward) {
        sent_brks <- rev(sent_brks)
    }
    if (length(sent_brks) < mtld_min_ntoken) {
        return(NA)
    }
    current_nfactors <- 0
    for (word in sent_brks) {
        current_ntoken <- current_ntoken + 1
        types <- unique(c(types, word))
        current_ttr <- length(types) / current_ntoken 
        if (current_ttr < mtld_threshold) {
            if (current_ntoken >= mtld_min_ntoken) {
                current_nfactors <- current_nfactors + 1
            }
            current_ntoken <- 0
            types <- c()
            current_ttr <- 0
        }
    }
    remaining_traj <- (1 - current_ttr) / (1 - mtld_threshold)
    total_nfactors <- current_nfactors + remaining_traj
    length(sent_brks) / total_nfactors
}

.cal_mtld <- function(input_text, mtld_threshold = 0.72, mtld_min_ntoken = 10) { 
    mean(c(.cal_mtld_raw(input_text, mtld_threshold = mtld_threshold, mtld_min_ntoken = mtld_min_ntoken), .cal_mtld_raw(input_text, mtld_threshold = mtld_threshold, mtld_min_ntoken = mtld_min_ntoken, backward = TRUE)))
}

cal_mtld <- function(input_text, mtld_threshold = 0.72, mtld_min_ntoken = 10) {
    purrr::map_dbl(input_text, .cal_mtld, mtld_threshold = mtld_threshold, mtld_min_ntoken = mtld_min_ntoken)
}

#' Calculate the raw scores
#'
#' This function takes a vector of characters and calculate the raw scores required for the algorithm.
#' @importFrom magrittr %>%
#' @param input_text vector of characters.
#' @param mtld_threshold Threshold for the calculation of MTLD score.
#' @param mtld_min_ntoken Threhold for minimal length of a viable sentence.
#' @param parallel logical, whether or not to use parallel computation.
#' @param .progrss_bar logical, whether or not to display progress bar. Only possible when parallel = TRUE.
#' @param plan character, please refer to ?future::plan for other options.
#' @return tibble of raw scores
#' @examples
#' \dontrun{
#' require(spacyr)
#' data(fairy)
#' spacy_initialize(model = 'en')
#' fairy_rawscore <- calculate_textplex(fairy$text)
#' }
#' @export
calculate_textplex <- function(input_text, mtld_threshold = 0.72, mtld_min_ntoken = 10, parallel = TRUE, .progress = TRUE, plan = 'multiprocess') {
    parsed_entire_content <- spacyr::spacy_parse(input_text, pos = TRUE, dependency = TRUE, tag = TRUE, multi_thread = parallel) %>% dplyr::group_by(doc_id) %>% dplyr::group_nest() %>% dplyr::pull(data)
    tokenized_text <- quanteda::tokens(input_text)
    lexdiv_res <- quanteda::textstat_lexdiv(tokenized_text, measure = c("TTR"))
    ari_res <- quanteda::textstat_readability(quanteda::corpus(input_text), measure = "ARI")
    ## NB: the current implementation of quanteda::textstat_entropy does not adjust for ntokens as in T&B.
    input_dfm <- quanteda::dfm(tokenized_text)
    I <- .cal_yule_i(input_dfm)
    entropy <- quanteda::textstat_entropy(input_dfm) / log(quanteda::ntoken(input_dfm), 2) * 100
    lexdiv_res %>% dplyr::left_join(ari_res, by = 'document') %>% dplyr::mutate(I = I, avg_sl = cal_avg_sentence_length(input_text), entropy = entropy, mtld = cal_mtld(input_text, mtld_threshold = mtld_threshold, mtld_min_ntoken = mtld_min_ntoken)) %>% dplyr::bind_cols(cal_mean_dist_root(parsed_entire_content, parallel = parallel, .progress = .progress, plan = plan))
}

#' Fit a two-factor model as per Tolochko and Boomgaarden
#'
#' This function fits a two-factor model of text complexity as per Tolochko and Boomgaarden (2019).
#' @param raw_scores raw_scores extracted by calculate_textplex()
#' @param rotate which rotation to use. The original implementation uses "oblimin". Please consult ?psych::fa for other options.
#' @return a psych::principal fit.
#' @examples
#' 
#' @export
fit_two_factor_model <- function(raw_scores, rotate = "oblimin") {
    raw_scores %>% dplyr::select(TTR, I, entropy, mtld, avg_sl, ARI, syntactic_depth, syntactic_dependency) %>% psych::principal(nfactors = 2, rotate = rotate) -> fit
    return(fit)
}

#' Fifty H.C. Andersen’s fairy tales
#'
#' First fifty fairy tales from hcandersenr::hcandersen_en.
#'
#' @docType data
#'
#' @usage data(fairy)
#'
#' @examples
#' \dontrun{
#' data(fairy)
#' fairy_rawscore <- calculate_textplex(fairy$text)
#' }
"fairy"

#' Raw scores extracted from the fairy dataset
#'
#' Raw scores extracted from the fairy dataset using calculate_textplex
#'
#' @docType data
#'
#' @usage data(fairy)
#'
#' @examples
#' data(fairy_rawscore)
#' fit <- fit_two_factor_model(fairy_rawscore)
"fairy_rawscore"

#' Raw scores extracted from the state of the union addresses
#'
#' @docType data
#'
#' @usage data(sotu_rawscore)
#'
#' @examples
#' data(sotu_rawscore)
#' fit <- fit_two_factor_model(sotu_rawscore)
"sotu_rawscore"
