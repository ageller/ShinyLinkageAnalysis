# Shiny Linkage Analysis
A Shiny app to investigate Pearson's correlations in biometric data.

This app can be run in a standard R session within the root directory of this repo by typing the following commands in a terminal:
```
$ R
> library(shiny)
> runApp()
```

You can also use the same functions outside the Shiny app (for instance within RStudio):

```
# First, load in the R script
> source("src/R/analyzeLinkageData.R")
```

```
# read in the data (in inputfile.csv) and format it correctly
> df <- readData("inputfile.csv") 
```

```
# get the Pearson's correlation statistics for dyad 1 using the "Negative" conversation 
# with a window of 15 seconds
> usedf <- runPearsonsCouple(df, 1, "Negative", 15) 
```

```
# generate a figure for this particular subset of the data 
> plotPearsonsCouple(usedf) 
```

```
# calculate the Pearson's statistics for every dyad and conversation in the data set 
# with a window of 15 seconds
> outdf <- runPearsonsAll(df, 15)
```

```
# write the output dataframe to a csv file named outputfile.csv
> exportToFile(outdf, "outputfile.csv") 
```