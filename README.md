# Shiny Linkage Analysis

A Shiny app to investigate Pearson's correlations in biometric data.

## Installing as a package (recommended)
This app, and it's dependencies, can be installed as a package using [remotes](https://github.com/r-lib/remotes) with the following command within a R session:
```
> remotes::install_github("ageller/ShinyLinkageAnalysis")
```

Once installed you can run the app with:
```
> library(ShinyLinkageAnalysis)
> ShinyLinkageAnalysis()
```

## Using the functions outside of the Shiny app

You can also use the same functions outside the Shiny app (for instance within RStudio):

```
# first, load the library.
> library(ShinyLinkageAnalysis)
```

```
# read in the data (in inputfile.csv) and format it correctly
> df <- ShinyLinkageAnalysis::readData("inputfile.csv") 
```

```
# get the Pearson's correlation statistics for dyad 1 using the "Negative" conversation 
# with a window of 15 seconds
> usedf <- ShinyLinkageAnalysis::runPearsonsCouple(df, 1, "Negative", 15) 
```

```
# generate a figure for this particular subset of the data 
> ShinyLinkageAnalysis::plotPearsonsCouple(usedf) 
```

```
# calculate the Pearson's statistics for every dyad and conversation in the data set 
# with a window of 15 seconds
> outdf <- ShinyLinkageAnalysis::runPearsonsAll(df, 15)
```

```
# write the output dataframe to a csv file named outputfile.csv
> ShinyLinkageAnalysis::exportToFile(outdf, "outputfile.csv") 
```

---

## Downloading from GitHub and running locally

If you prefer not to install this as a package, you can download these files (or clone this repo) from GitHub.  You will also need to install all the dependecies listed in Imports within the DESCRIPTION file and also devtools.  Then you can run this app in an R session within the root directory of this repo by typing the following commands:

```
> devtools::load_all()
> ShinyLinkageAnalysis()
```

If you want to use the functions outside the Shiny app :

```
# You will first need to load in the R script:
> source("R/analyzeLinkageData.R")
```

Then you can follow the examples from above, but removing `ShinyLinkageAnalysis::` .