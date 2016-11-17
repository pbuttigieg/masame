# The Multivariate AnalysiS Applications for Microbial Ecology (MASAME)

The apps in this directory will allow you to perform a set of statistical
analysis and screens typically suited to ecological data sets. These accompany an online guide to statistical analysis in microbial ecology, available here: http://mb3is.megx.net/gustame

Please see the following publication for more:
Buttigieg PL, Ramette A (2014) A Guide to Statistical Analysis in Microbial Ecology: a community-focused, living review of multivariate data analyses. FEMS Microbiol Ecol. 90: 543–550. 
http://dx.doi.org/10.1111/1574-6941.12437

If you have queries, wish to report errors, or have enhancement requests,
contact [Pier Luigi Buttigieg](http://orcid.org/0000-0002-4366-3088).


## Please fork and submit pull requests!

If you'd like to fork the code to improve or extend it, we're more than happy to integrate pull requests here and in [GUSTA ME](http://mb3is.megx.net/gustame). It would be even better if you have knowledge to share via [GUSTA ME](http://mb3is.megx.net/gustame) itself! Of course, all contributions will be micro-credited.

## Analysis Apps

### Running apps locally
You can run any of these apps on your own machine (assuming you've cloned the code locally and have all the R packages you need) by using the `runApp()` function from shiny. These are at the top of most scripts in this repo. For example, to run the ANOSIM app on your computer from the top level directory ("masame"), type the following in your R console:
```R
runApp('./ANOSIM', launch.browser = TRUE) 
```


### Running apps online

To launch an analysis app online, please click the link to the corresponding GUSTA ME page below and scroll to the bottom where the a launch link will be provided.

#### Data prep
- [screen your data set for multicollinear variables using correlation-based approaches](http://mb3is.megx.net/eatme/Diag_multicollinearity/)
- [screen your data set for univariate and multivariate normality and apply transformations](https://sites.google.com/site/mb3gustame/reference/transformations)
- [screen your data set for univariate and multivariate outliers](http://mb3is.megx.net/eatme/Diag_outliers/)

#### Ordination & clustering methods
- [Principal components analysis](https://sites.google.com/site/mb3gustame/indirect-gradient-analysis/pca)
- [(partial) redundancy analysis](https://sites.google.com/site/mb3gustame/constrained-analyses/rda)
- [distance-based redundancy analysis](https://sites.google.com/site/mb3gustame/constrained-analyses/rda/dbrda)
- [(partial) canonical correspondence analysis](https://sites.google.com/site/mb3gustame/constrained-analyses/cca)
- [Metric multidimensional scaling analysis or principal coordinates analysis](https://sites.google.com/site/mb3gustame/dissimilarity-based-methods/principal-coordinates-analysis)
- [Hierarchical cluster analysis](https://sites.google.com/site/mb3gustame/dissimilarity-based-methods/cluster-analysis/hierarchical-cluster-analysis)
- [Non-metric multidimensional scaling analysis](https://sites.google.com/site/mb3gustame/dissimilarity-based-methods/nmds)
- [Principal coordinates of neighbour matrices analysis](https://sites.google.com/site/mb3gustame/spatial-analysis/pcnm)

#### Hypothesis testing
- [Permutational multivariate analysis of variance Using distance matrices](https://sites.google.com/site/mb3gustame/hypothesis-tests/manova/npmanova)
- [Analysis of similarity](https://sites.google.com/site/mb3gustame/hypothesis-tests/anosim)


## Example data sets
Please find demo data sets in the "Example data sets" directory. These
are correctly formatted for upload to MASAME Apps under the default
upload settings. More information is available in the README file in 
the directory.


## Experimental updates
Some apps have been updated to support colour-coding, `vegan`'s ordination hulls, spiders, and ellipses, and more transformations.
If you'd like to try them out, they're in the [experimentalUpdates](https://github.com/pbuttigieg/masame/tree/master/experimentalUpdates) folder. You'll have to run them locally (see below for brief instructions), but it's quite straightforward to do so.


