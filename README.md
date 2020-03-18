
# textplex

<!-- badges: start -->

<!-- badges: end -->

The goal of textplex is to calculate textual complexity using the
algorithm by Tolochko & Boomgaarden (2019). Further validation of the
algorithm is available in Tolochko, Song & Boomgaarden (2019).

**Caveats:**

1.  The current version uses
    [spacyr](https://github.com/quanteda/spacyr) (Benoit et al.) to
    maintain compatibility with the original implementation (unlike the
    initial implement, which used udpipe). Future versions will add back
    the option to use udpipe.
2.  Super slow (although has been improved), but premature optimization
    is the root of all evil. See [benchmark](#benchmark).
3.  For the algorithm by Benoit, Munger & Spirling (2019), use
    [sophistication](https://github.com/kbenoit/sophistication).

References:

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

Please also install and setup an spacyr environment, as per the
instructions from the [spacyr webpage](http://spacyr.quanteda.io/). In
essence, it means:

``` r
install.packages('spacyr')
library(spacyr)
spacy_insatll()
```

## Examples

Calculating raw scores for English content.

``` r
library(textplex)
library(spacyr)
## Load the english language model
spacy_initialize(model = 'en')
```

    ## Found 'spacy_condaenv'. spacyr will use this environment

    ## successfully initialized (spaCy Version: 2.1.6, language model: en)

    ## (python options: type = "condaenv", value = "spacy_condaenv")

``` r
input_text <- c("A spectre is haunting Europe - the spectre of Communism.
All the powers of old Europe have entered into a holy alliance to exorcise
 this spectre; Pope and Czar, Metternich and Guizot, French Radicals
and German police-spies. Where is the party in opposition that has not been
 decried as communistic by its opponents in power? Where the Opposition that
 has not hurled back the branding reproach of Communism, against the more
 advanced opposition parties, as well as against its reactionary adversaries?",
"The greatest improvement in the productive powers of labour, and the greater
 part of the skill, dexterity, and judgment with which it is anywhere
 directed, or applied, seem to have been the effects of the division of
 labour. The effects of the division of labour, in the general business of
 society, will be more easily understood by considering in what manner it
 operates in some particular manufactures.")

calculate_textplex(input_text)
```

    ##   document       TTR      ARI       I avg_sl  entropy     mtld
    ## 1    text1 0.6750000 12.53574 27.0000  20.25 87.45706 59.57308
    ## 2    text2 0.6567164 18.51851 11.8773  33.50 80.92269 34.64706
    ##   syntactic_depth syntactic_dependency
    ## 1            4.75                 5.25
    ## 2            7.00                 5.00

Aber sprichst du Deutsch?

``` r
spacy_finalize()

## spacy_download_langmodel("de")

spacy_initialize(model = 'de')
```

    ## Python space is already attached.  If you want to switch to a different Python, please restart R.

    ## successfully initialized (spaCy Version: 2.1.6, language model: de)

    ## (python options: type = "condaenv", value = "spacy_condaenv")

``` r
de_input_text <- c("Entschuldigung. Ich kann mit Ihnen auf Deutsch nicht
 sprechen, weil mein Deutsch sehr schlecht ist. Man sagt 'deutsche Sprache,
 schwere Sprache'. Ich glaube, dass ich nur Bahnhof verstehe.",
 "In mir drin ist alles rot, das Gegenteil von tot. Mein Herz es schlägt sich
 noch ganz gut. In mir drin ist alles rot und du bist ein Idiot, mein Freund.
 Du verschmähst mein süßes Blut.")

calculate_textplex(de_input_text)
```

    ## Warning in spacy_parse.character(input_text, pos = TRUE, dependency =
    ## TRUE, : lemmatization may not work properly in model 'de'

    ##   document       TTR      ARI        I avg_sl  entropy     mtld
    ## 1    text1 0.8571429 7.638571 41.14286      7 87.80296 42.59111
    ## 2    text2 0.7500000 1.779167 25.13793      9 87.30341 44.90182
    ##   syntactic_depth syntactic_dependency
    ## 1            1.50                  3.5
    ## 2            2.75                  4.0

Fit the two-factor model.

``` r
library(furrr)
```

    ## Loading required package: future

``` r
library(psych)
library(sotu)
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
spacy_finalize()

spacy_initialize(model = 'en')
```

    ## Python space is already attached.  If you want to switch to a different Python, please restart R.

    ## successfully initialized (spaCy Version: 2.1.6, language model: en)

    ## (python options: type = "condaenv", value = "spacy_condaenv")

``` r
### This is the preferred way to do parallelization. It doesn't use up all your RAM!

## sotu_rawscore <- furrr::future_map_dfr(sotu_text, calculate_textplex, .progress = TRUE)

data(sotu_rawscore)
fit <- fit_two_factor_model(sotu_rawscore)
```

    ## Loading required namespace: GPArotation

``` r
sotu_meta$syntactic_complexity <- fit$scores[,1]
sotu_meta$semantic_complexity <- fit$scores[,2]
sotu_meta$text <- sotu_text

sotu_meta %>% arrange(desc(syntactic_complexity)) %>% select(president, year, syntactic_complexity, text)
```

    ## # A tibble: 236 x 4
    ##    president      year syntactic_complex… text                             
    ##    <chr>         <int>              <dbl> <chr>                            
    ##  1 James Madison  1815               2.26 "\n\n Fellow-Citizens of the Sen…
    ##  2 James Madison  1816               1.90 "\n\n Fellow-Citizens of the Sen…
    ##  3 Martin Van B…  1840               1.81 "\n\n Fellow-Citizens of the Sen…
    ##  4 John Quincy …  1826               1.73 "\n\n Fellow Citizens of the Sen…
    ##  5 James Madison  1813               1.61 "\n\n Fellow-Citizens of the Sen…
    ##  6 Woodrow Wils…  1920               1.60 "\n\nGENTLEMEN OF THE CONGRESS:\…
    ##  7 John Tyler     1841               1.56 "\n\n To the Senate and House of…
    ##  8 John Tyler     1843               1.55 "\n\n To the Senate and House of…
    ##  9 Andrew Jacks…  1833               1.50 "\n\n Fellow Citizens of the Sen…
    ## 10 Andrew Jacks…  1831               1.50 "\n\n Fellow Citizens of the Sen…
    ## # … with 226 more rows

How to write a syntactically complex SOTU, President Madison?

``` r
sotu_meta %>% arrange(desc(syntactic_complexity)) %>%
    select(president, year, syntactic_complexity, text) %>%
    slice(1) %>% pull(text) %>% print
```

    ## [1] "\n\n Fellow-Citizens of the Senate and House of Representatives: \n\nI have the satisfaction on our present meeting of being able to communicate the successful termination of the war which had been commenced against the United States by the Regency of Algiers. The squadron in advance on that service, under Commodore Decatur, lost not a moment after its arrival in the Mediterranean in seeking the naval force of the enemy then cruising in that sea, and succeeded in capturing two of his ships, one of them the principal ship, commanded by the Algerine admiral. The high character of the American commander was brilliantly sustained on the occasion which brought his own ship into close action with that of his adversary, as was the accustomed gallantry of all the officers and men actually engaged. Having prepared the way by this demonstration of American skill and prowess, he hastened to the port of Algiers, where peace was promptly yielded to his victorious force.\n\nIn the terms stipulated the rights and honor of the United States were particularly consulted by a perpetual relinquishment on the part of the Dey of all pretensions to tribute from them. The impressions which have thus been made, strengthened as they will have been by subsequent transactions with the Regencies of Tunis and of Tripoli by the appearance of the larger force which followed under Commodore Bainbridge, the chief in command of the expedition, and by the judicious precautionary arrangements left by him in that quarter, afford a reasonable prospect of future security for the valuable portion of our commerce which passes within reach of the Barbary cruisers.\n\nIt is another source of satisfaction that the treaty of peace with Great Britain has been succeeded by a convention on the subject of commerce concluded by the plenipotentiaries of the two countries. In this result a disposition is manifested on the part of that nation corresponding with the disposition of the United States, which it may be hoped will be improved into liberal arrangements on other subjects on which the parties have mutual interests, or which might endanger their future harmony. Congress will decide on the expediency of promoting such a sequel by giving effect to the measure of confining the American navigation to American sea men - a measure which, at the same time that it might have that conciliatory tendency, would have the further advantage of increasing the independence of our navigation and the resources for our maritime defense.\n\nIn conformity with the articles in the treaty of Ghent relating to the Indians, as well as with a view to the tranquillity of our western and northwestern frontiers, measures were taken to establish an immediate peace with the several tribes who had been engaged in hostilities against the United States. Such of them as were invited to Detroit acceded readily to a renewal of the former treaties of friendship. Of the other tribes who were invited to a station on the Mississippi the greater number have also accepted the peace offered to them. The residue, consisting of the more distant tribes or parts of tribes, remain to be brought over by further explanations, or by such other means as may be adapted to the dispositions they may finally disclose.\n\nThe Indian tribes within and bordering on the southern frontier, whom a cruel war on their part had compelled us to chastise into peace, have latterly shown a restlessness which has called for preparatory measures for repressing it, and for protecting the commissioners engaged in carrying the terms of the peace into execution.\n\nThe execution of the act for fixing the military peace establishment has been attended with difficulties which even now can only be overcome by legislative aid. The selection of officers, the payment and discharge of the troops enlisted for the war, the payment of the retained troops and their reunion from detached and distant stations, the collection and security of the public property in the Quartermaster, Commissary, and Ordnance departments, and the constant medical assistance required in hospitals and garrisons rendered a complete execution of the act impracticable on the 1st of May, the period more immediately contemplated. As soon, however, as circumstances would permit, and as far as it has been practicable consistently with the public interests, the reduction of the Army has been accomplished; but the appropriations for its pay and for other branches of the military service having proved inadequate, the earliest attention to that subject will be necessary; and the expediency of continuing upon the peace establishment the staff officers who have hitherto been provisionally retained is also recommended to the consideration of Congress.\n\nIn the performance of the Executive duty upon this occasion there has not been wanting a just sensibility to the merits of the American Army during the late war; but the obvious policy and design in fixing an efficient military peace establishment did not afford an opportunity to distinguish the aged and infirm on account of their past services nor the wounded and disabled on account of their present sufferings.\n\nThe extent of the reduction, indeed, unavoidably involved the exclusion of many meritorious officers of every rank from the service of their country; and so equal as well as so numerous were the claims to attention that a decision by the standard of comparative merit could seldom be attained. Judged, however, in candor by a general standard of positive merit, the Army Register will, it is believed, do honor to the establishment, while the case of those officers whose names are not included in it devolves with the strongest interest upon the legislative authority for such provisions as shall be deemed the best calculated to give support and solace to the veteran and the invalid, to display the beneficence as well as the justice of the Government, and to inspire a martial zeal for the public service upon every future emergency.\n\nAlthough the embarrassments arising from the want of an uniform national currency have not been diminished since the adjournment of Congress, great satisfaction has been derived in contemplating the revival of the public credit and the efficiency of the public resources. The receipts into the Treasury from the various branches of revenue during the nine months ending on the 30th of September last have been estimated at $12.5M; the issues of Treasury notes of every denomination during the same period amounted to the sum of $14M, and there was also obtained upon loan during the same period a sum of $9M, of which the sum of $6M was subscribed in cash and the sum of $3M in Treasury notes.\n\nWith these means, added to the sum of $1.5M, being the balance of money in the Treasury on the 1st day of January, there has been paid between the 1st of January and the 1st of October on account of the appropriations of the preceding and of the present year (exclusively of the amount of the Treasury notes subscribed to the loan and of the amount redeemed in the payment of duties and taxes) the aggregate sum of $33.5M, leaving a balance then in the Treasury estimated at the sum of $3M. Independent, however of the arrearages due for military services and supplies, it is presumed that a further sum of $5M, including the interest on the public debt payable on the 1st of January next, will be demanded at the Treasury to complete the expenditures of the present year, and for which the existing ways and means will sufficiently provide.\n\nThe national debt, as it was ascertained on the 1st of October last, amounted in the whole to the sum of $120M, consisting of the unredeemed balance of the debt contracted before the late war ($39M), the amount of the funded debt contracted in consequence of the war ($64M), and the amount of the unfunded and floating debt, including the various issues of Treasury notes, $17M, which is in gradual course of payment.\n\nThere will probably be some addition to the public debt upon the liquidation of various claims which are depending, and a conciliatory disposition on the part of Congress may lead honorably and advantageously to an equitable arrangement of the militia expenses incurred by the several States without the previous sanction or authority of the Government of the United States; but when it is considered that the new as well as the old portion of the debt has been contracted in the assertion of the national rights and independence, and when it is recollected that the public expenditures, not being exclusively bestowed upon subjects of a transient nature, will long be visible in the number and equipments of the American Navy, in the military works for the defense of our harbors and our frontiers, and in the supplies of our arsenals and magazines the amount will bear a gratifying comparison with the objects which have been attained, as well as with the resources of the country.\n\nThe arrangements of the finances with a view to the receipts and expenditures of a permanent peace establishment will necessarily enter into the deliberations of Congress during the present session. It is true that the improved condition of the public revenue will not only afford the means of maintaining the faith of the Government with its creditors inviolate, and of prosecuting successfully the measures of the most liberal policy, but will also justify an immediate alleviation of the burdens imposed by the necessities of the war.\n\nIt is, however, essential to every modification of the finances that the benefits of an uniform national currency should be restored to the community. The absence of the precious metals will, it is believed, be a temporary evil, but until they can again be rendered the general medium of exchange it devolves on the wisdom of Congress to provide a substitute which shall equally engage the confidence and accommodate the wants of the citizens throughout the Union. If the operation of the State banks can not produce this result, the probable operation of a national bank will merit consideration; and if neither of these expedients be deemed effectual it may become necessary to ascertain the terms upon which the notes of the Government (no longer required as an instrument of credit) shall be issued upon motives of general policy as a common medium of circulation.\n\nNotwithstanding the security for future repose which the United States ought to find in their love of peace and their constant respect for the rights of other nations, the character of the times particularly inculcates the lesson that, whether to prevent or repel danger, we ought not to be unprepared for it. This consideration will sufficiently recommend to Congress a liberal provision for the immediate extension and gradual completion of the works of defense, both fixed and floating, on our maritime frontier, and an adequate provision for guarding our inland frontier against dangers to which certain portions of it may continue to be exposed.\n\nAs an improvement in our military establishment, it will deserve the consideration of Congress whether a corps of invalids might not be so organized and employed as at once to aid in the support of meritorious individuals excluded by age or infirmities from the existing establishment, and to procure to the public the benefit of their stationary services and of their exemplary discipline.\n\nI recommend also an enlargement of the Military Academy already established, and the establishment of others in other sections of the Union; and I can not press too much on the attention of Congress such a classification and organization of the militia as will most effectually render it the safeguard of a free state. If experience has shewn in the recent splendid achievements of militia the value of this resource for the public defense, it has shewn also the importance of that skill in the use of arms and that familiarity with the essential rules of discipline which can not be expected from the regulations now in force.\n\nWith this subject is intimately connected the necessity of accommodating the laws in every respect to the great object of enabling the political authority of the Union to employ promptly and effectually the physical power of the Union in the cases designated by the Constitution.\n\nThe signal services which have been rendered by our Navy and the capacities it has developed for successful cooperation in the national defense will give to that portion of the public force its full value in the eyes of Congress, at an epoch which calls for the constant vigilance of all governments. To preserve the ships now in a sound state, to complete those already contemplated, to provide amply the imperishable materials for prompt augmentations, and to improve the existing arrangements into more advantageous establishments for the construction, the repairs, and the security of vessels of war is dictated by the soundest policy.\n\nIn adjusting the duties on imports to the object of revenue the influence of the tariff on manufactures will necessarily present itself for consideration. However wise the theory may be which leaves to the sagacity and interest of individuals the application of their industry and resources, there are in this as in other cases exceptions to the general rule. Besides the condition which the theory itself implies of reciprocal adoption by other nations, experience teaches that so many circumstances must concur in introducing and maturing manufacturing establishments, especially of the more complicated kinds, that a country may remain long without them, although sufficiently advanced and in some respects even peculiarly fitted for carrying them on with success. Under circumstances giving a powerful impulse to manufacturing industry it has made among us a progress and exhibited an efficiency which justify the belief that with a protection not more than is due to the enterprising citizens whose interests are now at stake it will become at an early day not only safe against occasional competitions from abroad, but a source of domestic wealth and even of external commerce.\n\nIn selecting the branches more especially entitled to the public patronage a preference is obviously claimed by such as will relieve the United States from a dependence on foreign supplies, ever subject to casual failures, for articles necessary for the public defense or connected with the primary wants of individuals. It will be an additional recommendation of particular manufactures where the materials for them are extensively drawn from our agriculture, and consequently impart and insure to that great fund of national prosperity and independence an encouragement which can not fail to be rewarded.\n\nAmong the means of advancing the public interest the occasion is a proper one for recalling the attention of Congress to the great importance of establishing throughout our country the roads and canals which can best be executed under the national authority. No objects within the circle of political economy so richly repay the expense bestowed on them; there are none the utility of which is more universally ascertained and acknowledged; none that do more honor to the governments whose wise and enlarged patriotism duly appreciates them. Nor is there any country which presents a field where nature invites more the art of man to complete her own work for his accommodation and benefit.\n\nThese considerations are strengthened, moreover, by the political effect of these facilities for intercommunication in bringing and binding more closely together the various parts of our extended confederacy. Whilst the States individually, with a laudable enterprise and emulation, avail themselves of their local advantages by new roads, by navigable canals, and by improving the streams susceptible of navigation, the General Government is the more urged to similar undertakings, requiring a national jurisdiction and national means, by the prospect of thus systematically completing so inestimable a work; and it is a happy reflection that any defect of constitutional authority which may be encountered can be supplied in a mode which the Constitution itself has providently pointed out.\n\nThe present is a favorable season also for bringing again into view the establishment of a national seminary of learning within the District of Columbia, and with means drawn from the property therein, subject to the authority of the General Government. Such an institution claims the patronage of Congress as a monument of their solicitude for the advancement of knowledge, without which the blessings of liberty can not be fully enjoyed or long preserved; as a model instructive in the formation of other seminaries; as a nursery of enlightened preceptors, and as a central resort of youth and genius from every part of their country, diffusing on their return examples of those national feelings, those liberal sentiments, and those congenial manners which contribute cement to our Union and strength to the great political fabric of which that is the foundation.\n\nIn closing this communication I ought not to repress a sensibility, in which you will unite, to the happy lot of our country and to the goodness of a superintending Providence, to which we are indebted for it. Whilst other portions of mankind are laboring under the distresses of war or struggling with adversity in other forms, the United States are in the tranquil enjoyment of prosperous and honorable peace. In reviewing the scenes through which it has been attained we can rejoice in the proofs given that our political institutions, founded in human rights and framed for their preservation, are equal to the severest trials of war, as well adapted to the ordinary periods of repose.\n\nAs fruits of this experience and of the reputation acquired by the American arms on the land and on the water, the nation finds itself possessed of a growing respect abroad and of a just confidence in itself, which are among the best pledges for its peaceful career. Under other aspects of our country the strongest features of its flourishing condition are seen in a population rapidly increasing on a territory as productive as it is extensive; in a general industry and fertile ingenuity which find their ample rewards, and in an affluent revenue which admits a reduction of the public burdens without withdrawing the means of sustaining the public credit, of gradually discharging the public debt, of providing for the necessary defensive and precautionary establishments, and of patronizing in every authorized mode undertakings conducive to the aggregate wealth and individual comfort of our citizens.\n\nIt remains for the guardians of the public welfare to persevere in that justice and good will toward other nations which invite a return of these sentiments toward the United States; to cherish institutions which guarantee their safety and their liberties, civil and religious; and to combine with a liberal system of foreign commerce an improvement of the national advantages and a protection and extension of the independent resources of our highly favored and happy country.\n\nIn all measures having such objects my faithful cooperation will be afforded. JAMES MADISON\n"

# Benchmark

We use this code as the benchmark on a Early 2015 Macbook Air

``` r
start_time <- Sys.time()
sotu_rawscore <- furrr::future_map_dfr(sotu_text, calculate_textplex, .progress = TRUE)
end_time <- Sys.time()
print(end_time - start_time)
```

| version | time..min. |
| :------ | ---------: |
| 0.0.1   |      12.12 |
