

This folder contains data suitably formatted for upload to MASAME. All
data sets have headers and an initial column of row names.

Below, each file is briefly described. More complete descriptions for data sets
obtained from the R pacakge, vegan, can be found in the vegan package
description: http://cran.r-project.org/web/packages/vegan/vegan.pdf



##############################
#
# Response Data
#
##############################

=====================
dune.csv
=====================
A data set included in the vegan R package (dune). A table of 20 objects and
30 variables. Values represent the species abundance across sampling sites.


=====================
mite.csv
=====================
A data set included in the vegan R package (mite). A table of 70 objects and
35 variables. Values represent the species abundance across sampled soil cores.


=====================
varespec.csv
=====================
A data set included in the vegan R package (varespec) which accompanies the
varespec response data. A table of 24 objects and 44 response variables.
Values represent the coverage estmates of different species of vegetation.



##############################
#
# Explanatory data
#
##############################

=====================
duneEnv.csv
=====================
A data set included in the vegan R package (dune.env) which accompanies the
dune response data. A table of 20 objects and 5 explanatory variables. 
Variables are both numeric and factors.

=====================
miteEnv.csv
=====================
A data set included in the vegan R package (mite.env) which accompanies the 
mite response data. A table of 70 objects and 5 explanatory variables. 
Variables are both numeric and factors.


=====================
varechem.csv
=====================
A data set included in the vegan R package (varechem) which accompanies the 
varespec response data. A table of 24 objects and 14 explanatory variables.
Variables are numeric.


##############################
#
# Spatial data
#
##############################

=====================
mite_xy.csv
=====================
A data set included in the vegan R package (mite.xy). A table of 70 objects
and 2 variables. Values represent the x and y spatial coordinates of 
sampling sites.



##############################
#
# Grouping data
#
##############################

=====================
exampleGroups.csv
=====================
A simple CSV (derived from the "Shrub" variable from the mite.env data set)
which contains a column of row names and a column of groups coded as factors.
Numerically coded groups are also acceptable.



##############################
#
# Stratification data
#
##############################

NB: Any factor (whether numerically coded or not) can be used as a 
stratification variable.

=====================
exampleStrata.csv
=====================
A simple CSV which contains a column of row names and a column of numerically
coded strata.




##############################
#
# Weighting Data
#
##############################

=====================
mite_weights.csv
===================== 
A simple CSV which contains a column of row names and a column of weights
derived from the mite data set (mite). These weights are equivalent to each
row marginal sum divided by the total sum of the mite data set.