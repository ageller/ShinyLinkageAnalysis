# Functions that can be used to analyze the linkage data from Claudia Haase's group.
#
# General use:
#
# First, load in the R script
# > source("src/R/analyzeLinkageData.R")
#
# read in the data (in inputfile.csv) and format it correctly
# > df <- readData("inputfile.csv") 
#
# get the Pearson's correlation statistics for dyad 1 using the "Negative" conversation 
# with a window of 15 seconds
# > usedf <- runPearsonsCouple(df, 1, "Negative", 15) 
#
# generate a figure for this particular subset of the data 
# > plotPearsonsCouple(usedf) 
#
# calculate the Pearson's statistics for every dyad and conversation in the data set 
# with a window of 15 seconds
# Note: this will provide a reformatted file; if the user desires to keep the current formatting, 
#   they can supply the arg ```format = "original" ```.
# > outdf <- runPearsonsAll(df, 15)
#
# write the output dataframe to a csv file named outputfile.csv
# > exportToFile(outdf, "outputfile.csv") 
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

runPearsonsCouple <- function(df, coupleID, conversation, window, columnID = "meanIBI", format = "original"){
	# select the dyad and the conversation
	usedf <- df[df$Couple_ID == coupleID & df$conversation == conversation,]

	# unique people
	Ind_IDs <- unique(usedf$Ind_ID)

	# the offset is half the windows
	offset <- as.integer((window - 1)/2)

	# create empty vectors to store the correlation coefficients and pvalues
	nMeasurements = nrow(usedf[usedf$Ind_ID == Ind_IDs[1],])
	pcor <- vector("list", nMeasurements)
	ppcor <- vector("list", nMeasurements)

	# create a reformatted output file to pass as well
	usedf2 <- usedf[usedf$Ind_ID == Ind_IDs[1],][c('Couple_ID', 'conversation','intervalStartTime')]
	usedf2$Ind_ID_1 <- Ind_IDs[1]
	usedf2$Ind_ID_2 <- Ind_IDs[2]
	usedf2[[paste0(columnID,'_1')]] <- usedf[usedf$Ind_ID == Ind_IDs[1],][[columnID]]
	pcor2 <- vector("list", nMeasurements)
	ppcor2 <- vector("list", nMeasurements)
	column_2 <- vector("list", nMeasurements)

	# start time
	currentTime = min(usedf$intervalStartTime)

	# loop through the data,  select the appropriate window (time +/- offset) around that time, 
	# and calculate the Pearson's correlation coefficient
	# I suppose for this data set, I could probably just go through by index rather than time, but that seems dangerous
	while(currentTime <= max(usedf$intervalStartTime)){
		
		# find the index in the array
		# this will return two values, one for the first person and one for the second person
		i <- which(usedf$intervalStartTime == currentTime)

		# this only has the first person
		i2 <- which(usedf2$intervalStartTime == currentTime)

		column_2[i2] <- usedf[[columnID]][i[2]]

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
				pcor2[i2] <- foo$estimate
				ppcor2[i2] <- foo$p.value
			}
		} 
			
		# if we failed to find the appropriate rows, then set the correlation values to NaN (could set to zero instead)
		if(!havePcor){
			pcor[i] <- NaN 
			ppcor[i] <- NaN 
			pcor2[i2] <- NaN
			ppcor2[i2] <- NaN
		}
			

		# increment the time by one second
		currentTime <- currentTime + 1
	}

	usedf$pearson_correlation_pvalue <- as.double(ppcor)
	usedf$pearson_correlation_coefficient <- as.double(pcor)

	usedf2[[paste0(columnID,'_2')]] <- as.double(column_2)
	usedf2$pearson_correlation_pvalue <- as.double(ppcor2)
	usedf2$pearson_correlation_coefficient <- as.double(pcor2)

	ifelse(format == "new", return(usedf2), return(usedf))

}


plotPearsonsCouple <- function(usedf, columnID = "meanIBI", colors = c("orange", "blue", "black"), includeFacet = c(TRUE, TRUE, TRUE), addPlimit = TRUE, plimit = 0.05, plimit_color = 'red', plotPoints = c(TRUE, TRUE, TRUE), pointSize = 0.7, topHeightFac = 1){
	# this assumes output from the runPearsonsCouple function

	# unique people
	Ind_IDs <- unique(usedf$Ind_ID)

	# give names to the colors so that I can have a proper legend
	names(colors) <- c("Partner 1", "Partner 2", "Pearson")

	# create a new dataframe that can be used with ggplot facets
	fee <- usedf[, c("intervalStartTime", columnID, "Ind_ID")]
	fee$Ind_ID <- as.numeric(as.character(fee$Ind_ID))
	minID = min(as.numeric(as.character(Ind_IDs)))
	maxID = max(as.numeric(as.character(Ind_IDs)))
	fee$Ind_ID[fee$Ind_ID == minID] <- "Partner 1"
	fee$Ind_ID[fee$Ind_ID == maxID] <- "Partner 2"
	fee$group <- columnID
	foo <- select(usedf[usedf$Ind_ID == Ind_IDs[1],], intervalStartTime, pearson_correlation_coefficient)
	foo$Ind_ID <- "Pearson"
	foo$group <- "Pearson's coefficient"
	bar <- select(usedf[usedf$Ind_ID == Ind_IDs[1],], intervalStartTime, pearson_correlation_pvalue)
	bar$Ind_ID <- "Pearson"
	bar$group <- "Pearson's p-value"

	names(fee) <- c("Time (s)", "value", "ID", "group")
	names(foo) <- c("Time (s)", "value", "ID", "group")
	names(bar) <- c("Time (s)", "value", "ID", "group")
	plotData <- rbind(fee, foo, bar)

	groups <- unique(plotData$group)

	# include only the desired facets
	plotData <- plotData %>% filter(group %in% groups[includeFacet])

	# generate the plot
	f <- ggplot(data=plotData, aes(x=.data[["Time (s)"]], y=value, group=group, color=ID)) +
		geom_line(aes(group=ID)) + 
		scale_color_manual(values=colors, name = "", breaks = c('Partner 1', 'Partner 2')) + 
		facet_grid(rows = vars(group), scales = "free_y", switch = "y",
			labeller = as_labeller(c())
			) +
		ylab(NULL) + # remove the word "values"
		theme(strip.background = element_blank(), # remove the background
			legend.position = c(1.07, 0.98),
			strip.placement = "outside",  # put labels to the left of the axis text
			plot.margin = margin(
				t = 10,  # Top margin
				r = 70,  # Right margin
				b = 10,  # Bottom margin
				l = 40)  # Left margin
			)

	# if the user wants to add points, include only in the desired facets
	if (any(plotPoints)) f <- f + geom_point(data = plotData %>% filter(group %in% groups[plotPoints]),
			aes(x=.data[["Time (s)"]], y=value, group=group, color=ID), size=pointSize)

	# add a horizontal line to the p-value plot (beneath the other lines)
	if (includeFacet[3] && addPlimit){
		fline <- geom_hline(data = plotData %>% filter(group == "Pearson's p-value"),
			aes(yintercept = plimit), color = plimit_color, linetype = 1)
		f$layers <- c(fline, f$layers)
	}

	# only include the legend if the first plot is there
	if (!includeFacet[1]) f <- f + theme(legend.position = "none")

	# adjust the top panel height
	# NOTE: this does NOT work when converting to ggplotly (instead use the topHeightFac arg in plotlyPerasonsCouple)
	if (includeFacet[1] && topHeightFac != 1){
		gt = ggplot_gtable(ggplot_build(f))
		gt$heights[7] = topHeightFac*gt$heights[7]
		f <- as.ggplot(gt)
	}

	return(f)
}

plotlyPearsonsCouple <- function(f, topHeightFac = 1){
	# convert the figure above into a plotly version for Shiny
	gp <- ggplotly(f, height = 800)

	# move the labels to the left when using plotly (I suppose it doesn' use the theme values from ggplot)
	# also rotate the labels to the proper orientation
	gp$x$layout$annotations[[2]]$x <- -0.08
	gp$x$layout$annotations[[2]]$textangle <- -90
	if (length(gp$x$layout$annotations) >= 3){
		gp$x$layout$annotations[[3]]$x <- -0.08
		gp$x$layout$annotations[[3]]$textangle <- -90
	}
	if (length(gp$x$layout$annotations) >= 4){
		gp$x$layout$annotations[[4]]$x <- -0.08
		gp$x$layout$annotations[[4]]$textangle <- -90
	}

	# remove the "-1" from the legend for Pearson(again!)
	for (i in 1:length(gp$x$data)){
		if (gp$x$data[[i]]$name == "Pearson") gp$x$data[[i]]$showlegend <- FALSE  
	}

	# adjust the sizing
	if (topHeightFac != 1 && !is.null(gp$x$layout$yaxis2)){
		spacing <- as.numeric(gp$x$layout$yaxis$domain[[1]] - gp$x$layout$yaxis2$domain[[2]])
		size <- as.numeric(1. - gp$x$layout$yaxis$domain[[1]])
		if (!is.null(gp$x$layout$yaxis3)){
			gp$x$layout$yaxis$domain <- c(2.*size/topHeightFac + 2*spacing, 1)
			gp$x$layout$yaxis2$domain <- c(size/topHeightFac + spacing,  2.*size/topHeightFac + spacing)
			gp$x$layout$yaxis3$domain <- c(0, size/topHeightFac)
		} else {
			gp$x$layout$yaxis$domain <- c(size/topHeightFac + spacing,  1)
			gp$x$layout$yaxis2$domain <- c(0, size/topHeightFac)
		}
	}

	return(gp)
}

runPearsonsAll <- function(df, window, columnID = "meanIBI", format = "new"){
	# generate Pearson's correlation statistics for all values in the input table
	# across all couples and all conversations

	# it will be important here to have a progress indicator

	# create an empty dataframe that will hold the final results
	if (format == "new"){
		outdf <- setNames(
			data.frame(matrix(ncol = 11, nrow = 0)), 
			c('Couple_ID', 'Ind_ID_1', 'Ind_ID_2', 'conversation','intervalStartTime', 
			  paste0(columnID,'_1'), paste0(columnID,'_2'), 'pearson_correlation_coefficient',
			  'pearson_correlation_pvalue')
		)
	} else {
		outdf <- data.frame(matrix(ncol = ncol(df), nrow = 0))
		names(outdf) <- names(df)
	}

	# unique dyads
	Couple_IDs <- unique(df$Couple_ID)
	conversations <- unique(df$conversation)

	# loop over the couples (dyads)
	if (interactive()){
		withProgress(message = 'Analyzing data', value = 0, {
			outdf <- runPearsonsAllLoop(df, outdf, Couple_IDs, conversations, window, columnID, format = format)
		})
	} else {
		outdf <- runPearsonsAllLoop(df, outdf, Couple_IDs, conversations, window, columnID, format = format)
	}

	return(outdf)

}
runPearsonsAllLoop <- function(df, outdf, Couple_IDs, conversations, window, columnID, format){
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
			usedf <- runPearsonsCouple(df, CoupleID, conv, window, columnID, format=format)

			outdf <- rbind(outdf, usedf)
			if (interactive()) incProgress(1./nTotal, detail = paste(CoupleID, conv))
			if (! interactive()) print(paste(CoupleID, conv))

		}

	}

	# add on the "in-phase" and "anti-phase" columns
	in_phase <- outdf$pearson_correlation_coefficient
	in_phase[in_phase < 0] <- 0 
	anti_phase <- outdf$pearson_correlation_coefficient
	anti_phase[anti_phase > 0] <- 0 
	outdf$pearson_correlation_coefficient_in_phase <- in_phase
	outdf$pearson_correlation_coefficient_anti_phase <- anti_phase
	
	return(outdf)

}

exportToFile <- function(df, filename){
	write.csv(df,filename, row.names = FALSE)
}


###########################
# previous plotting code, with 4 panels
###########################

plotPearsonsCoupleOrg <- function(usedf, columnID = "meanIBI"){
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
		geom_line() + geom_point(size=0.7) +
		facet_grid(rows = vars(group), scales = "free_y", switch = "y",
			labeller = as_labeller(c())
			) +
		ylab(NULL) + # remove the word "values"
		theme(strip.background = element_blank(), # remove the background
			strip.placement = "outside") # put labels to the left of the axis text

	return(f)
}

plotlyPearsonsCoupleOrg <- function(f){
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