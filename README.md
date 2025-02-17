# Shiny Linkage Analysis

A Shiny app to investigate correlations in biometric data.

A live version is available [here](https://northwestern-linkage-analysis.shinyapps.io/LinkEx/).

## Installing as a package (recommended)
This app, and it's dependencies, can be installed as a package using [remotes](https://github.com/r-lib/remotes) with the following command within a R session:
```
remotes::install_github("rcds-dssv/ShinyLinkageAnalysis")
```

Once installed you can run the app with (within an R session):
```
library(shinyBS)
library(ShinyLinkageAnalysis)
ShinyLinkageAnalysis()
```

This should launch a web browser that contains the app.  

## Using the app

1. The first step is to select a data file, in .csv format, from your computer.  You can do this by clicking the "BROWSE" button.

2. Once that file is loaded you will see interactive tools to select the Dyad ID, Conversation, and Window for the correlation.  

3. After you make those selections click the "UPDATE PLOT" button.  This will produce a plot using the parameters you defined in the previous step.  The top two panels of the plot show the meanIBI measures for both individuals in the selected dyad.  The bottom two figures show the corresponding correlation coefficient and p-value (given the selected window).

4. You can adjust the parameters and remake the plot as many times as you want.

5. If you hover over the plot, you will see tools in the upper-right corner that enable you to zoom, pan, save the figure, etc..  Hovering over each tool will show you it's function.

6. If you would like to run the correlation analysis on all of the dyads and all conversations in the file, using the selected window, you can click the button "RUN ALL AND DOWNLOAD".  A progress indicator will appear in the bottom-right corner.  When the analysis is complete, a file will download.  This file should contain all the data from the input file but with two additional columns for the correlation coefficient and the p-value. 

## Using the functions outside of the Shiny app

You can also use the same functions outside the Shiny app (for instance within RStudio):

```
# first, load the library.
library(ShinyLinkageAnalysis)
```

```
# define the columns that will contain the data to be analyzed
columnNames = data.frame(list(
	individualID = "Ind_ID", 
	coupleID = "Couple_ID",
	task = "conversation", 
	independentVar = "intervalStartTime", 
	dependentVar = "meanIBI"
))
```

```
# read in the data (in inputfile.csv) and format it correctly
df <- ShinyLinkageAnalysis::readData("inputfile.csv") 
df <- ShinyLinkageAnalysis::processData(df, columnNames)
```

```
# get the Pearson's correlation statistics for dyad 1 using the "Negative" conversation 
# with a window of 15 seconds
usedf <- ShinyLinkageAnalysis::runCorrelationCouple(df, 1, "Negative", 15, columnNames) 
```

```
# generate a figure for this particular subset of the data 
# Note: there are may arguments that can be supplied to customize the plot.  
#    These are used within the Shiny app, but are also available here.  
#    If you would like to use them directly from the R command line and need assistance, 
#    please use the "issues" in GitHub to request additional documentation. 
ShinyLinkageAnalysis::plotCorrelationCouple(usedf, columnNames)  
```

```
# calculate the Pearson's statistics for every dyad and conversation in the data set 
# with a window of 15 seconds.
# Note: this will provide a reformatted file; if the user desires to keep the current formatting, 
#    they can supply the arg : format = "original".
outdf <- ShinyLinkageAnalysis::runCorrelationAll(df, 15, columnNames)
```

```
# write the output dataframe to a csv file named outputfile.csv
ShinyLinkageAnalysis::exportToFile(outdf, "outputfile.csv") 
```

---

## Downloading from GitHub and running locally

If you prefer not to install this as a package, you can download these files (or clone this repo) from GitHub.  You will also need to install all the dependecies listed in Imports within the DESCRIPTION file and also devtools.  Then you can run this app in an R session within the root directory of this repo by typing the following commands:

```
devtools::load_all()
ShinyLinkageAnalysis()
```

If you want to use the functions outside the Shiny app :

```
# You will first need to load in the R script:
source("R/analyzeLinkageData.R")
```

Then you can follow the examples from above, but removing `ShinyLinkageAnalysis::` .
