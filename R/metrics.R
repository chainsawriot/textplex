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

.parse_text_udpipe <- function(input_text, udpipe_model) {
    as.data.frame(udpipe::udpipe_annotate(object = udpipe_model, x = input_text))
}

.find_central_verb <- function(parse_tree_data) {
    res <- as.integer(parse_tree_data$token_id[stringr::str_detect(parse_tree_data$upos, "^VERB")])
    if (length(res) == 0) {
        res <- as.integer(parse_tree_data$token_id[stringr::str_detect(parse_tree_data$xpos, "^VB")])
        if (length(res) == 0) {
            ##warning("No Verb. Getting Root instead.")
            res <- .find_root(parse_tree_data)
        }
    }
    return(min(res)[1])
}

.find_terminal <- function(parse_tree_data) {
    parse_tree_data %>% dplyr::filter(upos != "PUNCT") %>% dplyr::mutate(token_id = as.numeric(token_id)) %>% dplyr::summarise(max_id = max(token_id)) -> res
    if (nrow(res) == 0) {
        return(NA)
    }
    res %>% dplyr::pull(max_id) %>% tail(n = 1) -> terminal
    return(terminal[1])
}

.find_root <- function(parse_tree_data) {
    as.integer(parse_tree_data$token_id[parse_tree_data$dep_rel == "root"]) -> res
    return(res[1])
}

.safe_max <- function(x, replacement) {
    if (length(x) == 0) {
        return(replacement)
    }
    return(max(x))
}

.cal_max_dist_root_raw <- function(input_text, udpipe_model) {
    ## Assuming the text is already tokenized into a sentence.
    if (stringr::str_detect(input_text, "^[[:blank:][:cntrl:]
[:punct:][:space:]]+$")) {
        return(tibble::tibble(syntactic_depth = NA, syntactic_dependency = NA))
    }
    parse_tree_data <- .parse_text_udpipe(input_text, udpipe_model)
    igraph::graph_from_data_frame(parse_tree_data[parse_tree_data$head_token_id != 0, c('head_token_id', 'token_id')]) -> parse_tree
    root_node_id <- which(igraph::V(parse_tree)$name == .find_root(parse_tree_data))
    termin_node_id <- which(igraph::V(parse_tree)$name == .find_terminal(parse_tree_data))
    central_node_id <- which(igraph::V(parse_tree)$name == .find_central_verb(parse_tree_data))
    central_node_paths <- igraph::all_simple_paths(parse_tree, from = central_node_id)
    if (length(central_node_paths) == 0) {
        ### return the theoretical maximum
        syntactic_dependency <- igraph::diameter(parse_tree)
    } else {
        central_node_paths %>% purrr::map_int(length) %>% .safe_max(replacement = igraph::diameter(parse_tree)) -> syntactic_dependency
    }
    if (root_node_id == termin_node_id | is.na(termin_node_id)) {
        syntactic_depth <- 0
    } else {
        igraph::all_simple_paths(parse_tree, from = root_node_id, to = termin_node_id) %>% purrr::map_int(length) %>% .safe_max(replacement = 0) -> syntactic_depth
    }
    return(tibble::tibble(syntactic_depth = syntactic_depth, syntactic_dependency = syntactic_dependency))
}

.cal_mean_dist_root_sentences <- function(sentences, udpipe_model) {
    res <- purrr::map_dfr(sentences, .cal_max_dist_root_raw, udpipe_model = udpipe_model)
    res %>% dplyr::summarise(syntactic_depth = mean(syntactic_depth, na.rm = TRUE), syntactic_dependency = mean(syntactic_dependency, na.rm = TRUE))
}

cal_mean_dist_root <- function(input_text, udpipe_model) {
    purrr::map_dfr(input_text, ~ purrr::map_dfr(.bs_sent2(.), .cal_mean_dist_root_sentences, udpipe_model = udpipe_model))
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
#' @param udpipe_model udpipe_model loaded with udpipe::udpipe_load_model().
#' @param mtld_threshold Threshold for the calculation of MTLD score.
#' @param mtld_min_ntoken Threhold for minimal length of a viable sentence.
#' @return tibble of raw scores
#' @examples
#' \dontrun{
#' require(udpipe)
#' data(fairy)
#' udpipe_model <- udpipe_load_model('english-ewt-ud-2.4-190531.udpipe')
#' fairy_rawscore <- calculate_textplex(fairy$text, udpipe_model)
#' }
#' @export
calculate_textplex <- function(input_text, udpipe_model, mtld_threshold = 0.72, mtld_min_ntoken = 10) {
    tokenized_text <- quanteda::tokens(input_text)
    lexdiv_res <- quanteda::textstat_lexdiv(tokenized_text, measure = c("TTR", "K"))
    ari_res <- quanteda::textstat_readability(quanteda::corpus(input_text), measure = "ARI")
    lexdiv_res %>% dplyr::left_join(ari_res, by = 'document') %>% dplyr::mutate(I = (1 / K) / 10000, avg_sl = cal_avg_sentence_length(input_text), entropy = quanteda::textstat_entropy(quanteda::dfm(tokenized_text), margin = "document"), mtld = cal_mtld(input_text, mtld_threshold = mtld_threshold, mtld_min_ntoken = mtld_min_ntoken)) %>% dplyr::bind_cols(cal_mean_dist_root(input_text, udpipe_model))
}

#' Fit a two-factor model as per Tolochko and Boomgaarden
#'
#' This function fits a two-factor model of text complexity as per Tolochko and Boomgaarden (2019).
#' @param raw_scores raw_scores extracted by calculate_textplex()
#' @return a psych::principal fit.
#' @examples
#' 
#' @export
fit_two_factor_model <- function(raw_scores) {
    raw_scores %>% select(-document, -K) %>% principal(nfactors=2, rotate="oblimin") -> fit
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
#' require(udpipe)
#' data(fairy)
#' udpipe_model <- udpipe_load_model('english-ewt-ud-2.4-190531.udpipe')
#' fairy_rawscore <- calculate_textplex(fairy$text, udpipe_model)
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
