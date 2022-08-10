# Functions that can be used to analyze the linkage data from Claudia Haase's group
#
# general use :
#
# $ R
# > source("analyzeLinkageData.R")
# > df <- readData("inputfile.csv") # read in the data (in inputfile.csv) and format it correctly
# > usedf <- runPearsonsCouple(df, 1, "Negative", 15) # get the Pearson's correlation statistics for couple 1 using the "Negative" conversation with a window of 15 seconds
# > f <- plotPearsonsCouple(usedf) # generate a figure for this particular subset of the data
# > outdf <- runPearsonsAll(df, 15) # calculate the Pearson's statistics for every couple and conversation in the data set with window of 15 seconds
# > exportToFile(outdf, "outputfile.csv") # write the output dataframe to a csv file names outputfile.csv
#
# This script is also used within the Shiny app

readData <- function(filename){
	# read the data and format the columns for use with the other functions in this file
	df <- read.csv(filename)
	df$id = as.factor(df$id)
	df$Ind_ID = as.factor(df$Ind_ID)
	df$Couple_ID = as.factor(df$Couple_ID)
	df$conversation = as.factor(df$conversation)

	# remove any "couples" that only have a single Ind_ID value
	bad <- c()
	for (CoupleID in unique(df$Couple_ID)){
		usedf <- df[df$Couple_ID == CoupleID,]
		if (length(unique(usedf$Ind_ID)) != 2){
			bad <- append(bad, CoupleID)
		}
	}


	return(df[! df$Couple_ID %in% bad, ])
}

runPearsonsCouple <- function(df, coupleID, conversation, window, columnID = "meanIBI"){
	# select the dyad and the conversation
	usedf <- df[df$Couple_ID == coupleID & df$conversation == conversation,]

	# unique people
	Ind_IDs <- unique(usedf$Ind_ID)

	#the offset is half the windows
	offset <- as.integer((window - 1)/2)

	# create empty vectors to how the correlation coefficients and pvalues
	nMeasurements = nrow(usedf[usedf$Ind_ID == Ind_IDs[1],])
	pcor <- vector("list", nMeasurements)
	ppcor <- vector("list", nMeasurements)

	# start time
	currentTime = min(usedf$intervalStartTime)

	# loop through the data,  select the appropriate window (time +/- offset) around that time, 
	# and calculate the Pearson's correlation coefficient
	# I suppose for this data set, I could probably just go through by index rather than time, but that seems dangerous
	while(currentTime <= max(usedf$intervalStartTime)){
		
		# find the index in the array
		# this will return two values, one for the first person and one for the second person
		i <- which(usedf$intervalStartTime == currentTime)
		
		havePcor <- FALSE
		
		# if the time window is fully available in the data then try to calculate the Pearson's correlation
		if (currentTime - offset >= min(usedf$intervalStartTime) & currentTime + offset <= max(usedf$intervalStartTime)){
			
			# select the rows in the data frame within the time window for the first person
			rows0 <- usedf[usedf$Ind_ID == Ind_IDs[1] & 
						   usedf$intervalStartTime >= currentTime - offset & 
						   usedf$intervalStartTime <= currentTime + offset, ]
			
			# select the rows in the data frame within the time window for the second person
			rows1 <- usedf[usedf$Ind_ID == Ind_IDs[2] & 
						   usedf$intervalStartTime >= currentTime - offset & 
						   usedf$intervalStartTime <= currentTime + offset, ]
			
			# if each of these subsets of data are the same length, then calculate the Pearson's correlation
			if (nrow(rows0) == nrow(rows1) & sum(!is.na(rows0[[columnID]])) > 2 & sum(!is.na(rows1[[columnID]])) > 2){
				havePcor <- TRUE
				foo <- cor.test(rows0[[columnID]], rows1[[columnID]], method = "pearson")
				pcor[i] <- foo$estimate
				ppcor[i] <- foo$p.value
			}
		} 
			
		# if we failed to find the appropriate rows, then set the correlation values to NaN (could set to zero instead)
		if(!havePcor){
			pcor[i] <- NaN 
			ppcor[i] <- NaN 
		}
			

		# increment the time by one second
		currentTime <- currentTime + 1
	}

	usedf$pearson_correlation_coefficient <- as.double(pcor)
	usedf$pearson_correlation_pvalue <- as.double(ppcor)

	return(usedf)
}


plotPearsonsCouple <- function(usedf, columnID = "meanIBI"){
	# this assumes output from the runPearsonsCouple function

	# unique people
	Ind_IDs <- unique(usedf$Ind_ID)

	# create a new dataframe that I can be used with ggplot facets
	fee <- usedf[, c('intervalStartTime', columnID, 'Ind_ID')]
	foo <- select(usedf[usedf$Ind_ID == Ind_IDs[1],], intervalStartTime, pearson_correlation_coefficient)
	bar <- select(usedf[usedf$Ind_ID == Ind_IDs[1],], intervalStartTime, pearson_correlation_pvalue)
	fee$Ind_ID <- paste(fee$Ind_ID, columnID)
	foo$Ind_ID <- "Pearson coefficient"
	bar$Ind_ID <- "Pearson p-value"
	names(fee) <- c("Interval Start Time (s)", "value","group")
	names(foo) <- c("Interval Start Time (s)", "value","group")
	names(bar) <- c("Interval Start Time (s)", "value","group")
	plotData <- rbind(fee, foo, bar)

	# generate the plot
	f <- ggplot(data=plotData, aes(x=.data[["Interval Start Time (s)"]], y=value, group=group)) +
		geom_line() + geom_point() +
		facet_grid(rows = vars(group), scales = "free_y", switch = "y",
			labeller = as_labeller(c())
			) +
		ylab(NULL) + # remove the word "values"
		theme(strip.background = element_blank(), # remove the background
			strip.placement = "outside") # put labels to the left of the axis text

	return(f)
}

plotlyPearsonsCouple <- function(f){
	# convert the figure above into a plotly version for Shiny
	gp <- ggplotly(f, height = 800)

	# need to move the labels when using plotly (I suppose it doesn' use the theme values from ggplot)
	gp[["x"]][["layout"]][["annotations"]][[2]][["x"]] <- -0.06
	gp[["x"]][["layout"]][["annotations"]][[3]][["x"]] <- -0.06
	gp[["x"]][["layout"]][["annotations"]][[4]][["x"]] <- -0.06
	gp[["x"]][["layout"]][["annotations"]][[5]][["x"]] <- -0.06

	# rotate the labels to the proper orientation
	gp[["x"]][["layout"]][["annotations"]][[2]][["textangle"]] <- -90
	gp[["x"]][["layout"]][["annotations"]][[3]][["textangle"]] <- -90
	gp[["x"]][["layout"]][["annotations"]][[4]][["textangle"]] <- -90
	gp[["x"]][["layout"]][["annotations"]][[5]][["textangle"]] <- -90

	return(gp)
}

runPearsonsAll <- function(df, window, columnID = "meanIBI"){
	# generate Pearson's correlation statistics for all values in the input table
	# across all couples and all conversations

	# it will be important here to have a progress indicator

	# create an empty dataframe that will hold the final results
	outdf <- data.frame(matrix(ncol = ncol(df), nrow = 0))
	names(outdf) <- names(df)

	# unique dyads
	Couple_IDs <- unique(df$Couple_ID)
	conversations <- unique(df$conversation)

	# loop over the couples (dyads)
	if (interactive()){
		withProgress(message = 'Analyzing data', value = 0, {
			outdf <- runPearsonsAllLoop(df, outdf, Couple_IDs, conversations, window, columnID)
		})
	} else {
		outdf <- runPearsonsAllLoop(df, outdf, Couple_IDs, conversations, window, columnID)
	}

	return(outdf)

}
runPearsonsAllLoop <- function(df, outdf, Couple_IDs, conversations, window, columnID){
	# separating this out into another function so that I can have a version that I can control if I show a progress bar

	nTotal <- length(Couple_IDs)*length(conversations)
	for (CoupleID in Couple_IDs){
		# select the couple in order to get the unique conversations
		df0 <- df[df$Couple_ID == CoupleID, ]

		# unique conversations (redefine this incase there are different conversations for each dyad)
		conversations <- unique(df0$conversation)

		# loop over the conversations
		for (conv in conversations){

			# run the Pearson's correlation
			usedf <- runPearsonsCouple(df, CoupleID, conv, window, columnID)

			outdf <- rbind(outdf, usedf)
			if (interactive()) incProgress(1./nTotal, detail = paste(CoupleID, conv))
			if (! interactive()) print(paste(CoupleID, conv))

		}

	}

	return(outdf)

}

exportToFile <- function(df, filename){
	write.csv(df,filename, row.names = FALSE)
}