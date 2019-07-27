
# textplex

<!-- badges: start -->

<!-- badges: end -->

The goal of textplex is to calculate text complexity using the algorithm
by Tolochko & Boomgaarden (2019). Further validation of the algorithm is
available in Tolochko, Song & Boomgaarden (2019).

**Caveats:**

1.  The current version uses udpipe to create dependency trees. Future
    version will provide an option to use
    [spacyr](https://github.com/quanteda/spacyr) (Benoit et al.) to
    maintain compatibility with the original implementation.
2.  Super slow, but premature optimization is the root of all evil.
3.  For algorithm by Benoit, Munger & Spirling (2019), use
    [sophistication](https://github.com/kbenoit/sophistication).

Reference:

1.  Tolochko, P., & Boomgaarden, H. G. (2019). Determining Political
    Text Complexity: Conceptualizations, Measurements, and Application.
    International Journal of Communication, 13, 21.
2.  Tolochko, P., Song, H., & Boomgaarden, H. (2019). “That Looks
    Hard\!”: Effects of Objective and Perceived Textual Complexity on
    Factual and Structural Political Knowledge. Political Communication,
    1-20.
3.  Benoit, K., Munger, K., & Spirling, A. (2019). Measuring and
    explaining political sophistication through textual complexity.
    American Journal of Political Science, 63(2), 491-508.

## Installation

You can install the experimental version of textplex from github:

``` r
## install.packages("devtools")
devtools::install_github("chainsawriot/textplex")
```

## Examples

Calculating raw scores for English content.

``` r
library(udpipe)
library(textplex)
## Download the english language model
## udpipe_download_model(language = "english")

## Load the english language model
udpipe_model <- udpipe_load_model("english-ewt-ud-2.4-190531.udpipe")

input_text <- c("A spectre is haunting Europe - the spectre of Communism. All the powers of old Europe have entered into a holy alliance to exorcise this spectre; Pope and Czar, Metternich and Guizot, French Radicals and German police-spies. Where is the party in opposition that has not been decried as communistic by its opponents in power? Where the Opposition that has not hurled back the branding reproach of Communism, against the more advanced opposition parties, as well as against its reactionary adversaries?", "The greatest improvement in the productive powers of labour, and the greater part of the skill, dexterity, and judgment with which it is anywhere directed, or applied, seem to have been the effects of the division of labour. The effects of the division of labour, in the general business of society, will be more easily understood by considering in what manner it operates in some particular manufactures.")

calculate_textplex(input_text, udpipe_model)
```

    ##   document       TTR        K      ARI            I avg_sl  entropy
    ## 1    text1 0.6750000 253.1250 12.53574 3.950617e-07  20.25 5.677584
    ## 2    text2 0.6567164 461.1272 18.51851 2.168599e-07  33.50 5.055991
    ##       mtld syntactic_depth syntactic_dependency
    ## 1 59.57308            3.75                  4.5
    ## 2 34.64706            4.50                  4.5

Aber sprichst du Deutsch?

``` r
## udpipe_download_model(language = "german")
de_udpipe_model <- udpipe_load_model("german-gsd-ud-2.4-190531.udpipe")

de_input_text <- c("Entschuldigung. Ich kann mit Ihnen auf Deutsch nicht sprechen, weil mein Deutsch sehr schlecht ist. Man sagt 'deutsche Sprache, schwere Sprache'. Ich glaube, dass ich nur Bahnhof verstehe.", "In mir drin ist alles rot, das Gegenteil von tot. Mein Herz es schlägt sich noch ganz gut. In mir drin ist alles rot und du bist ein Idiot, mein Freund. Du verschmähst mein süßes Blut.")

calculate_textplex(input_text, de_udpipe_model)
```

    ##   document       TTR        K      ARI            I avg_sl  entropy
    ## 1    text1 0.6750000 253.1250 12.53574 3.950617e-07  20.25 5.677584
    ## 2    text2 0.6567164 461.1272 18.51851 2.168599e-07  33.50 5.055991
    ##       mtld syntactic_depth syntactic_dependency
    ## 1 59.57308            1.75                  5.0
    ## 2 34.64706            4.00                  6.5

Fit the two-factor
model.

``` r
### it takes a long time. But you need to have a reasonable amount of data to fit the two-factor model.

library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(psych)

## library(hcandersenr)
## hcandersen_en %>% group_by(book) %>% summarise(text = paste(text, collapse = " ")) %>% head(50) -> fairy

data(fairy)

fairy_rawscore <- calculate_textplex(fairy$text, udpipe_model)

fit <- fit_two_factor_model(fairy_rawscore)
```

    ## Loading required namespace: GPArotation

``` r
fairy$syntactic_complexity <- fit$scores[,1]
fairy$semantic_complexity <- fit$scores[,2]

fairy %>% arrange(desc(syntactic_complexity)) %>% select(book, syntactic_complexity)
```

    ## # A tibble: 50 x 2
    ##    book                              syntactic_complexity
    ##    <chr>                                            <dbl>
    ##  1 Holger Danske                                    2.83 
    ##  2 At the uttermost parts of the sea                2.13 
    ##  3 The bell                                         1.59 
    ##  4 The angel                                        1.35 
    ##  5 A picture from the ramparts                      1.29 
    ##  6 A rose from Homer's grave                        1.22 
    ##  7 Grandmother                                      0.986
    ##  8 Ole the tower-keeper                             0.949
    ##  9 Soup from a sausage skewer                       0.895
    ## 10 A cheerful temper                                0.813
    ## # … with 40 more rows
