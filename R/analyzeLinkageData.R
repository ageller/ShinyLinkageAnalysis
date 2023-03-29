# Functions that can be used to analyze the linkage data from Claudia Haase's group.
#
# General use:
#
# First, load in the R script
# > source("src/R/analyzeLinkageData.R")
#
# define the column names for the analysis 
# > columnNames = data.frame(list(individualID = "Ind_ID", 
# 							  coupleID = "Couple_ID",
# 							  task = "conversation", 
# 							  independentVar = "intervalStartTime", 
# 							  dependentVar = "meanIBI"))
#
# read in the data (in inputfile.csv) and format it correctly
# > df <- processData(readData("inputfile.csv") )
#
# get the Pearson's correlation statistics for dyad 1 using the "Negative" task 
# with a window of 15 seconds
# > usedf <- runCorrelationCouple(df, 1, "Negative", 15, columnNames) 
#
# generate a figure for this particular subset of the data 
# > plotCorrelationCouple(usedf, columnNames) 
#
# calculate the Pearson's statistics for every dyad and task in the data set 
# with a window of 15 seconds
# Note: this will provide a reformatted file; if the user desires to keep the current formatting, 
#   they can supply the arg ```format = "original" ```.
# > outdf <- runCorrelationAll(df, 15, columnNames)
#
# write the output dataframe to a csv file named outputfile.csv
# > exportToFile(outdf, "outputfile.csv") 
#
# This script is also used within the Shiny app



readData <- function(filename){
	# read the data 
	df <- read.csv(filename)

	return(df)
}

processData <- function(df,  columnNames = data.frame(list(individualID = "Ind_ID", coupleID = "Couple_ID", task = "conversation", independentVar = "intervalStartTime", dependentVar = "meanIBI")) ) {

	# subset
	# df <- df[c(columnNames$individualID, columnNames$coupleID, columnNames$task, columnNames$independentVar, columnNames$dependentVar)]

	# format the columns for use with the other functions in this file
	df[columnNames$individualID] = as.factor(df[[columnNames$individualID]]) 
	df[columnNames$coupleID] = as.factor(df[[columnNames$coupleID]])
	df[columnNames$task] = as.factor(df[[columnNames$task]])

	# remove any "couples" that only have a single individualID value
	bad <- c()
	for (CoupleID in unique(df[[columnNames$coupleID]])) {
		usedf <- df[columnNames$coupleID == CoupleID,]
		if (length(unique(df[[columnNames$individualID]])) != 2) {
			bad <- append(bad, CoupleID)
		}
	}

	return(df[! df[columnNames$coupleID] %in% bad, ])

}

runCorrelationCouple <- function(df, coupleID, task, window, columnNames = data.frame(list(individualID = "Ind_ID", coupleID = "Couple_ID", task = "conversation", independentVar = "intervalStartTime", dependentVar = "meanIBI")), format = "original", correlationMethod = "pearson"){

	# select all the couple information to get the individual IDs so that they stay in the same order regardless of conversation
	# for instance if only one member of the couple has data on a given conversation, there will be a nan which possibly will reorder 
	# the individual IDs for that one conversation in the output file.  This should avoid that potential issue.
	foo <- df[df[columnNames$coupleID] == coupleID,][[columnNames$individualID]]
	
	# unique people
	Ind_IDs <- unique(na.omit(foo))

	# select the dyad and the task
	usedf <- df[df[columnNames$coupleID] == coupleID & df[columnNames$task] == task,]

	# below I want to use one individual ID to select values for columns (which are repeated in the input data set for both individual IDs)
	useIndex <- 1
	nMeasurements = nrow(usedf[usedf[[columnNames$individualID]] == Ind_IDs[1],])
	if (nMeasurements == 0) useIndex <- 2

	# the offset is half the window
	offset <- as.integer((window - 1)/2)

	# create empty vectors to store the correlation coefficients and pvalues
	nMeasurements <- nrow(usedf[usedf[[columnNames$individualID]] == Ind_IDs[useIndex],])
	pcor <- vector("list", nMeasurements)
	ppcor <- vector("list", nMeasurements)

	# create a reformatted output file to pass as well
	usedf2 <- usedf[usedf[[columnNames$individualID]] == Ind_IDs[useIndex],][c(columnNames$coupleID, columnNames$task, columnNames$independentVar)]
	usedf2[[paste0(columnNames$individualID,'_1')]] <- Ind_IDs[1]
	usedf2[[paste0(columnNames$individualID,'_2')]] <- Ind_IDs[2]
	usedf2[[paste0(columnNames$dependentVar,'_1')]] <- usedf[usedf[[columnNames$individualID]] == Ind_IDs[useIndex],][[columnNames$dependentVar]]
	# reorder columns
	usedf2 <- usedf2[, c(columnNames$coupleID, paste0(columnNames$individualID,'_1'), paste0(columnNames$individualID,'_2'), columnNames$task, columnNames$independentVar, paste0(columnNames$dependentVar,'_1'))]

	# empty vectors to stor the correlation data
	pcor2 <- vector("list", nMeasurements)
	ppcor2 <- vector("list", nMeasurements)
	column_2 <- vector("list", nMeasurements)

	# start value for the independent variable
	currentXvalue = min(usedf[columnNames$independentVar])

	# loop through the data,  select the appropriate window (value +/- offset) around that value, 
	# and calculate the correlation coefficient
	# I suppose for this data set, I could probably just go through by index rather than value, but that seems dangerous
	while(currentXvalue <= max(usedf[columnNames$independentVar])){
		
		# find the index in the array
		# this will return two values, one for the first person and one for the second person
		i <- which(usedf[[columnNames$independentVar]] == currentXvalue)

		# this only has the first person
		i2 <- which(usedf2[[columnNames$independentVar]] == currentXvalue)

		column_2[i2] <- usedf[[columnNames$dependentVar]][i[2]]

		havePcor <- FALSE
		
		# if the value window is fully available in the data then try to calculate the correlation
		if (currentXvalue - offset >= min(usedf[columnNames$independentVar]) & currentXvalue + offset <= max(usedf[columnNames$independentVar])){
			
			# select the rows in the data frame within the value window for the first person
			rows0 <- usedf[usedf[[columnNames$individualID]] == Ind_IDs[1] & 
						   usedf[[columnNames$independentVar]] >= currentXvalue - offset & 
						   usedf[[columnNames$independentVar]] <= currentXvalue + offset, ]
			
			# select the rows in the data frame within the value window for the second person
			rows1 <- usedf[usedf[[columnNames$individualID]] == Ind_IDs[2] & 
						   usedf[[columnNames$independentVar]] >= currentXvalue - offset & 
						   usedf[[columnNames$independentVar]] <= currentXvalue + offset, ]
			

			# if each of these subsets of data are the same length, then calculate the correlation
			if (nrow(rows0) == nrow(rows1)){
				if (sum(!is.na(rows0[[columnNames$dependentVar]])) > 2 & sum(!is.na(rows1[[columnNames$dependentVar]])) > 2){
					havePcor <- TRUE
					foo <- cor.test(rows0[[columnNames$dependentVar]], rows1[[columnNames$dependentVar]], method = correlationMethod, exact = FALSE)
					pcor[i] <- foo$estimate
					ppcor[i] <- foo$p.value
					pcor2[i2] <- foo$estimate
					ppcor2[i2] <- foo$p.value
				}
			}
		} 
			
		# if we failed to find the appropriate rows, then set the correlation values to NaN (could set to zero instead)
		if(!havePcor){
			pcor[i] <- NaN 
			ppcor[i] <- NaN 
			pcor2[i2] <- NaN
			ppcor2[i2] <- NaN
		}
			

		# increment the value by one 
		currentXvalue <- currentXvalue + 1
	}

	usedf$correlation_pvalue <- as.double(ppcor)
	usedf$correlation_coefficient <- as.double(pcor)

	usedf2[[paste0(columnNames$dependentVar,'_2')]] <- as.double(column_2)
	usedf2$correlation_pvalue <- as.double(ppcor2)
	usedf2$correlation_coefficient <- as.double(pcor2)

	ifelse(format == "new", return(usedf2), return(usedf))

}


plotCorrelationCouple <- function(usedf, columnNames = data.frame(list(individualID = "Ind_ID", coupleID = "Couple_ID", task = "conversation", independentVar = "intervalStartTime", dependentVar = "meanIBI")), colors = c("Partner 1" = "#F8766D", "Partner 2" = "#00BFC4", "Correlation" = "black", "plimit" = "#7CAE00", "prects" = "#C77CFF"), includeFacet = c(TRUE, TRUE, TRUE), addPlimit = TRUE, plimit = 0.05, prectsAlpha = 0.3, plotPoints = c(TRUE, TRUE, TRUE), pointSize = 0.7, topHeightFac = 1, showPrects = FALSE, forPlotly = FALSE, dependentYrange = c(NA, NA), correlationMethod = "pearson", xAxisLabel = 'Time (s)'){
	# this assumes output from the runCorrelationCouple function

	# unique people
	Ind_IDs <- unique(usedf[[columnNames$individualID]])

	# create a new dataframe that can be used with ggplot facets
	fee <- usedf[, c(columnNames$independentVar, columnNames$dependentVar, columnNames$individualID)]
	fee[columnNames$individualID] <- as.numeric(as.character(fee[[columnNames$individualID]]))
	minID = min(as.numeric(as.character(Ind_IDs)))
	maxID = max(as.numeric(as.character(Ind_IDs)))
	fee[columnNames$individualID][fee[columnNames$individualID] == minID] <- "Partner 1"
	fee[columnNames$individualID][fee[columnNames$individualID] == maxID] <- "Partner 2"
	fee$group <- columnNames$dependentVar
	foo <- select(usedf[usedf[[columnNames$individualID]] == Ind_IDs[1],], columnNames$independentVar, correlation_coefficient)
	foo[columnNames$individualID] <- "Correlation"
	foo$group <- "Correlation coefficient" # added Z at the start because my previous code relies on the correlation plots being sorted at the bottom of the list (removed later)
	bar <- select(usedf[usedf[[columnNames$individualID]] == Ind_IDs[1],], columnNames$independentVar, correlation_pvalue)
	bar[columnNames$individualID] <- "Correlation"
	bar$group <- "Correlation p-value" # added Z at the start because my previous code relies on the correlation plots being sorted at the bottom of the list (Removed later)

	names(fee) <- c(xAxisLabel, "value", "ID", "group")
	names(foo) <- c(xAxisLabel, "value", "ID", "group")
	names(bar) <- c(xAxisLabel, "value", "ID", "group")
	plotData <- rbind(fee, foo, bar)

	groups <- unique(plotData$group)

	plotData$group_f <- factor(plotData$group, levels = groups)

	# include only the desired facets
	plotData <- plotData %>% filter(group %in% groups[includeFacet])

	# generate the plot
	f <- ggplot(data = plotData, aes(x = .data[[xAxisLabel]], y = value, group = group, color = ID)) +
		scale_color_manual(values = colors, name = "", breaks = c('Partner 1', 'Partner 2')) + 
		facet_grid(rows = vars(group_f), scales = "free_y", switch = "y",
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

	# if this is for plotly, then don't include the extra aes for the line because it shows up as a duplicate in the tooltip!
	ifelse(forPlotly, f <- f + geom_line(), f <- f + geom_line(aes(group = ID)) )

	# adjust the y axis limits for each panel
	# this user can specify for the top panel showing dependent vs. independent measurements 
	panel_ranges <- vector(mode = "list", length = 0)
	if (includeFacet[1]) {
		initialRange <- layer_scales(f, i = 1)$y$get_limits()
		if (is.na(dependentYrange[1])) dependentYrange[1] <- initialRange[1]
		if (is.na(dependentYrange[2])) dependentYrange[2] <- initialRange[2]

		panel_ranges <- append(panel_ranges, list(list(y = dependentYrange)))
	}
	if (includeFacet[2]) panel_ranges <- append(panel_ranges, list(list(y = c(-1.02, 1.02))))
	if (includeFacet[3]) panel_ranges <- append(panel_ranges, list(list(y = c(-0.01, 1.01))))
	f <- f + coord_panel_ranges(panel_ranges = panel_ranges)
	

	# if the user wants to add points, include only in the desired facets
	if (any(plotPoints)) f <- f + geom_point(data = plotData %>% filter(group %in% groups[plotPoints]),
			aes(x = .data[[xAxisLabel]], y = value, group = group, color = ID), size = pointSize)

	# add a horizontal line to the p-value plot (beneath the other lines)
	if (includeFacet[3] & addPlimit){
		fline <- geom_hline(data = plotData %>% filter(group == "Correlation p-value"),
			aes(yintercept = plimit), color = colors['plimit'], linetype = 1)
		f$layers <- c(fline, f$layers)
	}

	# only include the legend if the first plot is there
	if (!includeFacet[1]) f <- f + theme(legend.position = "none")

	# adjust the top panel height
	# NOTE: this does NOT work when converting to ggplotly (instead use the topHeightFac arg in plotlyCorrelationCouple)
	if (includeFacet[1] & topHeightFac != 1){
		gt = ggplot_gtable(ggplot_build(f))
		gt$heights[7] = topHeightFac*gt$heights[7]
		f <- as.ggplot(gt)
	}

	# add rects for the significant regions 
	# NOTE: ggplot's annotate function is perfect, but it won't work with ggplotly!  The main issue is that ggplotly won't allow Inf for the 
	#   ymax and ymin.  If I change these y limits to non-Inf values, then I need to have diff limits for each facet, which annotate can't do.
	#   So, I will create a bunch of rects for each facet...
	if (showPrects){
		significantT <- usedf[[columnNames$independentVar]][usedf$correlation_pvalue < plimit & !is.na(usedf$correlation_pvalue)]

		# limit these to start and end values for drawing rects
		# https://stackoverflow.com/questions/26603858/r-how-to-find-non-sequential-elements-in-an-array
		rectStarts <- significantT[c(TRUE, diff(significantT) != 1)]
		rectEnds <- significantT[c(diff(significantT) != 1, TRUE)]

		# use ggplot's annotate feature.  (Does not work well with ggplotly, see above)
		# for (i in 1:length(rectStarts)){
		# 	annot <- annotate( "rect", xmin = rectStarts[i], xmax = rectEnds[i], ymin = -Inf, ymax = Inf, fill = colors['prects'], color = colors['prects'], alpha = prectsAlpha)
		# }

		# create rects 
		for (i in 1:length(includeFacet)){
			if (includeFacet[i]){
				# draw the rectangles (below the other layers)
				if (i == 1) ylims <- dependentYrange
				if (i == 2) ylims <- c(-1.02, 1.02)
				if (i == 3) ylims <- c(-0.01, 1.01)

				rectData <- data.frame("xmin" = rectStarts, "xmax" = rectEnds, "group" = groups[i], "ymin" = ylims[1], "ymax" = ylims[2])

				annot <- geom_rect(data = rectData, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), alpha = prectsAlpha, fill = colors['prects'], inherit.aes = FALSE)
				f$layers <- c(annot, f$layers)
			}
		}
	}

	return(f)
}

plotlyCorrelationCouple <- function(f, columnNames = data.frame(list(individualID = "Ind_ID", coupleID = "Couple_ID", task = "conversation", independentVar = "intervalStartTime", dependentVar = "meanIBI")), topHeightFac = 1, height = 800, correlationMethod = "pearson"){
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

	# remove the "-1" from the legend for Correlation(again!)
	for (i in 1:length(gp$x$data)){
		if (gp$x$data[[i]]$name == "Correlation") gp$x$data[[i]]$showlegend <- FALSE  
	}

	# adjust the size of each panel Correlation plots are smaller in the y dimension
	if (topHeightFac != 1 & !is.null(gp$x$layout$yaxis2)){
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

	return(gp)
}

runCorrelationAll <- function(df, window, columnNames = data.frame(list(individualID = "Ind_ID", coupleID = "Couple_ID", task = "conversation", independentVar = "intervalStartTime", dependentVar = "meanIBI")), format = "new", correlationMethod = "pearson"){
	# generate correlation statistics for all values in the input table
	# across all couples and all tasks

	# it will be important here to have a progress indicator

	# create an empty dataframe that will hold the final results
	if (format == "new"){
		outdf <- setNames(
			data.frame(matrix(ncol = 11, nrow = 0)), 
			c('Couple_ID', 'Ind_ID_1', 'Ind_ID_2', columnNames$task, columnNames$independentVar, 
			  paste0(columnNames$dependentVar,'_1'), paste0(columnNames$dependentVar,'_2'), 'correlation_coefficient',
			  'correlation_pvalue')
		)
	} else {
		outdf <- data.frame(matrix(ncol = ncol(df), nrow = 0))
		names(outdf) <- names(df)
	}

	# unique dyads
	Couple_IDs <- unique(df[[columnNames$coupleID]])
	tasks <- unique(df[[columnNames$task]])

	# loop over the couples (dyads)
	if (interactive()){
		withProgress(message = 'Analyzing data', value = 0, {
			outdf <- runCorrelationAllLoop(df, outdf, Couple_IDs, tasks, window, format, columnNames, correlationMethod)
		})
	} else {
		outdf <- runCorrelationAllLoop(df, outdf, Couple_IDs, tasks, window, format, columnNames, correlationMethod)
	}

	return(outdf)

}
runCorrelationAllLoop <- function(df, outdf, Couple_IDs, tasks, window, format, columnNames = data.frame(list(individualID = "Ind_ID", coupleID = "Couple_ID", task = "conversation", independentVar = "intervalStartTime", dependentVar = "meanIBI")), correlationMethod = "pearson") {
	# separating this out into another function so that I can have a version that I can control if I show a progress bar

	nTotal <- length(Couple_IDs)*length(tasks)
	for (CoupleID in Couple_IDs){
		# select the couple in order to get the unique tasks
		df0 <- df[df[columnNames$coupleID] == CoupleID, ]

		# unique tasks (redefine this incase there are different tasks for each dyad)
		tasks <- unique(df0[[columnNames$task]])

		# loop over the tasks
		for (conv in tasks){

			# run the correlation
			usedf <- runCorrelationCouple(df, CoupleID, conv, window, columnNames, format = format, correlationMethod = correlationMethod)

			outdf <- rbind(outdf, usedf)
			if (interactive()) incProgress(1./nTotal, detail = paste(CoupleID, conv))
			if (! interactive()) print(paste(CoupleID, conv))

		}

	}

	# add on the absolute value, "in-phase" and "anti-phase" columns
	absolute_value <- abs(outdf$correlation_coefficient)
	in_phase <- outdf$correlation_coefficient
	in_phase[in_phase < 0] <- 0 
	anti_phase <- outdf$correlation_coefficient
	anti_phase[anti_phase > 0] <- 0 
	outdf$correlation_coefficient_absolute_value <- absolute_value
	outdf$correlation_coefficient_in_phase <- in_phase
	outdf$correlation_coefficient_anti_phase <- abs(anti_phase)
	
	return(outdf)

}

renameColumns <- function(df, correlationMethod = "pearson"){
	names(df)[names(df) == 'correlation_pvalue'] <- paste0('overall_linkage_',correlationMethod,'_correlation_pvalue')
	names(df)[names(df) == 'correlation_coefficient'] <- paste0('overall_linkage_',correlationMethod,'_correlation')
	names(df)[names(df) == 'correlation_coefficient_absolute_value'] <- paste0('absolute_linkage_',correlationMethod,'_correlation')
	names(df)[names(df) == 'correlation_coefficient_in_phase'] <- paste0('inphase_linkage_',correlationMethod,'_correlation')
	names(df)[names(df) == 'correlation_coefficient_anti_phase'] <- paste0('antiphase_linkage_',correlationMethod,'_correlation')

	return(df)
}

exportToFile <- function(df, filename, correlationMethod = "pearson"){
	# rename columns
	outdf <- renameColumns(df, correlationMethod)
	print(outdf)

	# write to file
	write.csv(outdf, filename, row.names = FALSE)
}