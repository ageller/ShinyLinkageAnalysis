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


plotPearsonsCouple <- function(usedf, columnID = "meanIBI", colors = c("Partner 1" = "#F8766D", "Partner 2" = "#00BFC4", "Pearson" = "black", "plimit" = "#7CAE00", "prects" = "#C77CFF"), includeFacet = c(TRUE, TRUE, TRUE), addPlimit = TRUE, plimit = 0.05, prectsAlpha = 0.3, plotPoints = c(TRUE, TRUE, TRUE), pointSize = 0.7, topHeightFac = 1, showPrects = FALSE){
	# this assumes output from the runPearsonsCouple function

	# unique people
	Ind_IDs <- unique(usedf$Ind_ID)


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
	f <- ggplot(data = plotData, aes(x = .data[["Time (s)"]], y = value, group = group, color = ID)) +
		geom_line() + 
		scale_color_manual(values = colors, name = "", breaks = c('Partner 1', 'Partner 2')) + 
		facet_grid(rows = vars(group), scales = "free_y", switch = "y",
			labeller = as_labeller(c())
		) +
		scale_y_continuous(expand = c(0.01, 0.01)) + 
		scale_x_continuous(expand = c(0.01, 0.01)) + 
		ylab(NULL) + # remove the word "values"
		theme_bw() + # this looks great in ggplot, but when I resize in plotly the outside boxes don't change
		theme(strip.background = element_blank(), # remove the background for the strip labels
			legend.position = c(1.07, 0.98),
			#panel.background = element_blank(), # remove the gray background
			#panel.border = element_rect(color = "black", fill = NA, size = 1), # add a full border (doesn't work when resized in plotly!)
			#axis.line = element_line(color = "black"), # add lines for the bottom and left side (won't add lines for all axis in facets)
			strip.placement = "outside",  # put labels to the left of the axis text
			plot.margin = margin(
				t = 10,  # Top margin
				r = 80,  # Right margin
				b = 10,  # Bottom margin
				l = 40)  # Left margin
		) 

	# if the user wants to add points, include only in the desired facets
	if (any(plotPoints)) f <- f + geom_point(data = plotData %>% filter(group %in% groups[plotPoints]),
			aes(x = .data[["Time (s)"]], y = value, group = group, color = ID), size = pointSize)

	# add a horizontal line to the p-value plot (beneath the other lines)
	if (includeFacet[3] && addPlimit){
		fline <- geom_hline(data = plotData %>% filter(group == "Pearson's p-value"),
			aes(yintercept = plimit), color = colors['plimit'], linetype = 1)
		f$layers <- c(fline, f$layers)
	}

	# only include the legend if the first plot is there
	if (!includeFacet[1]) f <- f + theme(legend.position = "none")

	# adjust the top panel height
	# NOTE: this does NOT work when converting to ggplotly (instead use the topHeightFac arg in plotlyPearsonsCouple)
	if (includeFacet[1] && topHeightFac != 1){
		gt = ggplot_gtable(ggplot_build(f))
		gt$heights[7] = topHeightFac*gt$heights[7]
		f <- as.ggplot(gt)
	}

	# add rects for the significant regions 
	# NOTE: ggplot's annotate function is perfect, but it won't work with ggplotly!  The main issue is that ggplotly won't allow Inf for the 
	#   ymax and ymin.  If I change these y limits to non-Inf values, then I need to have diff limits for each facet, which annotate can't do.
	#   So, I will create a bunch of rects for each facet...
	if (showPrects){
		significantT <- usedf$intervalStartTime[usedf$pearson_correlation_pvalue < plimit & !is.na(usedf$pearson_correlation_pvalue)]

		# limit these to start and end values for drawing rects
		# https://stackoverflow.com/questions/26603858/r-how-to-find-non-sequential-elements-in-an-array
		rectStarts <- significantT[c(TRUE, diff(significantT) != 1)]
		rectEnds <- significantT[c(diff(significantT) != 1, TRUE)]

		# use ggplot's annotate feature.  (Does not work well with ggplotly, see above)
		# for (i in 1:length(rectStarts)){
		# 	annot <- annotate( "rect", xmin = rectStarts[i], xmax = rectEnds[i], ymin = -Inf, ymax = Inf, fill = colors['prects'], color = colors['prects'], alpha = prectsAlpha)
		# }

		# create rects 
		for (i in 1:length(groups[includeFacet])){
			# draw the rectangles (below the other layers)
			ylims = layer_scales(f, i = i)$y$get_limits()
			rectData <- data.frame("xmin" = rectStarts, "xmax" = rectEnds, "group" = groups[includeFacet][i], "ymin" = ylims[1], "ymax" = ylims[2])

		    annot <- geom_rect(data = rectData, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), alpha = prectsAlpha, fill = colors['prects'], inherit.aes = FALSE)
			f$layers <- c(annot, f$layers)
		}
	}

	return(f)
}

plotlyPearsonsCouple <- function(f, columnID = "meanIBI", topHeightFac = 1, height = 800,  columnYlimit = c(NA, NA)){
	# convert the figure above into a plotly version for Shiny
	gp <- ggplotly(f, height = height)

	# move the labels to the left when using plotly (I suppose it doesn' use the theme values from ggplot)
	# also rotate the labels to the proper orientation
	gp$x$layout$annotations[[2]]$x <- -0.09
	gp$x$layout$annotations[[2]]$textangle <- -90
	if (length(gp$x$layout$annotations) >= 3){
		gp$x$layout$annotations[[3]]$x <- -0.09
		gp$x$layout$annotations[[3]]$textangle <- -90
	}
	if (length(gp$x$layout$annotations) >= 4){
		gp$x$layout$annotations[[4]]$x <- -0.09
		gp$x$layout$annotations[[4]]$textangle <- -90
	}

	# set consistent font sizes and colors for annotations
	for (i in 2:length(gp$x$layout$annotations)){
		gp$x$layout$annotations[[i]]$font$size <- gp$x$layout$annotations[[1]]$font$size
		gp$x$layout$annotations[[i]]$font$color <- gp$x$layout$annotations[[1]]$font$color
	}

	# remove the "-1" from the legend for Pearson(again!)
	for (i in 1:length(gp$x$data)){
		if (gp$x$data[[i]]$name == "Pearson") gp$x$data[[i]]$showlegend <- FALSE  
	}

	# adjust the size of each panel Pearson's plots are smaller in the y dimension
	if (topHeightFac != 1 && !is.null(gp$x$layout$yaxis2)){
		spacing <- as.numeric(gp$x$layout$yaxis$domain[[1]] - gp$x$layout$yaxis2$domain[[2]])
		size <- as.numeric(1. - gp$x$layout$yaxis$domain[[1]])
		if (!is.null(gp$x$layout$yaxis3)){
			# data
			gp$x$layout$yaxis$domain <- c(2.*size/topHeightFac + 2*spacing, 1)
			gp$x$layout$yaxis2$domain <- c(size/topHeightFac + spacing,  2.*size/topHeightFac + spacing)
			gp$x$layout$yaxis3$domain <- c(0, size/topHeightFac)

			# border from theme_bw (apparently there are two rects for each axis)
			gp$x$layout$shapes[[1]]$y0 <- 2.*size/topHeightFac + 2*spacing
			gp$x$layout$shapes[[1]]$y1 <- 1.
			gp$x$layout$shapes[[2]]$y0 <- gp$x$layout$shapes[[1]]$y0 
			gp$x$layout$shapes[[2]]$y1 <- gp$x$layout$shapes[[1]]$y1

			gp$x$layout$shapes[[3]]$y0 <- size/topHeightFac + spacing
			gp$x$layout$shapes[[3]]$y1 <- 2.*size/topHeightFac + spacing
			gp$x$layout$shapes[[4]]$y0 <- gp$x$layout$shapes[[3]]$y0
			gp$x$layout$shapes[[4]]$y1 <- gp$x$layout$shapes[[3]]$y1

			gp$x$layout$shapes[[5]]$y0 <- 0
			gp$x$layout$shapes[[5]]$y1 <- size/topHeightFac
			gp$x$layout$shapes[[6]]$y0 <- gp$x$layout$shapes[[5]]$y0
			gp$x$layout$shapes[[6]]$y1 <- gp$x$layout$shapes[[5]]$y1
		} else {
			# data
			gp$x$layout$yaxis$domain <- c(size/topHeightFac + spacing,  1)
			gp$x$layout$yaxis2$domain <- c(0, size/topHeightFac)

			#border from theme_bw (apparently there are two rects for each axis)
			gp$x$layout$shapes[[1]]$y0 <- size/topHeightFac + spacing
			gp$x$layout$shapes[[1]]$y1 <- 1.
			gp$x$layout$shapes[[2]]$y0 <- gp$x$layout$shapes[[1]]$y0 
			gp$x$layout$shapes[[2]]$y1 <- gp$x$layout$shapes[[1]]$y1

			gp$x$layout$shapes[[3]]$y0 <- size/topHeightFac
			gp$x$layout$shapes[[3]]$y1 <- 0.
			gp$x$layout$shapes[[4]]$y0 <- gp$x$layout$shapes[[3]]$y0
			gp$x$layout$shapes[[4]]$y1 <- gp$x$layout$shapes[[3]]$y1
		}

		# move the annotations to the correct y positions
		gp$x$layout$annotations[[2]]$y <- (gp$x$layout$yaxis$domain[[2]] - gp$x$layout$yaxis$domain[[1]])/2. + gp$x$layout$yaxis$domain[[1]]
		gp$x$layout$annotations[[3]]$y <- (gp$x$layout$yaxis2$domain[[2]] - gp$x$layout$yaxis2$domain[[1]])/2. + gp$x$layout$yaxis2$domain[[1]]
		if (!is.null(gp$x$layout$yaxis3)) gp$x$layout$annotations[[4]]$y <- (gp$x$layout$yaxis3$domain[[2]] - gp$x$layout$yaxis3$domain[[1]])/2. + gp$x$layout$yaxis3$domain[[1]]

	}


	# adjust the y axis limits for the top panel (if it is not a Pearson's plot)
	if (!is.null(gp$x$layout$yaxis) && gp$x$layout$annotations[[2]]$text == columnID){
		if (!is.na(columnYlimit[1])) gp$x$layout$yaxis$range[1] = columnYlimit[1]
		if (!is.na(columnYlimit[2])) gp$x$layout$yaxis$range[2] = columnYlimit[2]
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

	# add on the absolute value, "in-phase" and "anti-phase" columns
	absolute_value <- abs(outdf$pearson_correlation_coefficient)
	in_phase <- outdf$pearson_correlation_coefficient
	in_phase[in_phase < 0] <- 0 
	anti_phase <- outdf$pearson_correlation_coefficient
	anti_phase[anti_phase > 0] <- 0 
	outdf$pearson_correlation_coefficient_absolute_value <- absolute_value
	outdf$pearson_correlation_coefficient_in_phase <- in_phase
	outdf$pearson_correlation_coefficient_anti_phase <- abs(anti_phase)
	
	return(outdf)

}

renameColumns <- function(df){
	names(df)[names(df) == 'pearson_correlation_pvalue'] <- 'overall_linkage_pearson_correlation_pvalue'
	names(df)[names(df) == 'pearson_correlation_coefficient'] <- 'overall_linkage_pearson_correlation'
	names(df)[names(df) == 'pearson_correlation_coefficient_absolute_value'] <- 'absolute_linkage_pearson_correlation'
	names(df)[names(df) == 'pearson_correlation_coefficient_in_phase'] <- 'inphase_linkage_pearson_correlation'
	names(df)[names(df) == 'pearson_correlation_coefficient_anti_phase'] <- 'antiphase_linkage_pearson_correlation'

	return(df)
}

exportToFile <- function(df, filename){
	# rename columns
	outdf <- renameColumns(df)

	# write to file
	write.csv(outdf,filename, row.names = FALSE)
}


###########################
# previous plotting code, with 4 panels (not used in Shiny app)
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