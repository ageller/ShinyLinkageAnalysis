readData <- function(filename){
	# read the data and format the columns for use with the other functions in this file
	df <- read.csv(filename)
	df$id = as.factor(df$id)
	df$Ind_ID = as.factor(df$Ind_ID)
	df$Couple_ID = as.factor(df$Couple_ID)
	df$conversation = as.factor(df$conversation)
	
	return(df)
}

runPearsonsCouple <- function(df, coupleID, conversation, window){
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
						   usedf$intervalStartTime > currentTime - offset & 
						   usedf$intervalStartTime < currentTime + offset, ]
			
			# select the rows in the data frame within the time window for the second person
			rows1 <- usedf[usedf$Ind_ID == Ind_IDs[2] & 
						   usedf$intervalStartTime > currentTime - offset & 
						   usedf$intervalStartTime < currentTime + offset, ]
			
			# if each of these subsets of data are the same length, then calculate the Pearson's correlation
			if (nrow(rows0) == nrow(rows1) & sum(!is.na(rows0$meanIBI)) > 2 & sum(!is.na(rows1$meanIBI)) > 2){
				havePcor <- TRUE
				foo <- cor.test(rows0$meanIBI, rows1$meanIBI, method = "pearson")
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


plotPearsonsCouple <- function(usedf){
	# this assumes output from the runPearsonsCouple function

	# unique people
	Ind_IDs <- unique(usedf$Ind_ID)

	# create a new dataframe that I can be used with ggplot facets
	fee <- select(usedf, intervalStartTime, meanIBI, Ind_ID)
	foo <- select(usedf[usedf$Ind_ID == Ind_IDs[1],], intervalStartTime, pearson_correlation_coefficient)
	bar <- select(usedf[usedf$Ind_ID == Ind_IDs[1],], intervalStartTime, pearson_correlation_pvalue)
	foo$Ind_ID <- "Pearson coefficient"
	bar$Ind_ID <- "Pearson p-value"
	names(fee) <- c("intervalStartTime", "value","group")
	names(foo) <- c("intervalStartTime", "value","group")
	names(bar) <- c("intervalStartTime", "value","group")
	plotData <- rbind(fee, foo, bar)

	# get the conversation for the title (there should be only one)
	conversation <- unique(usedf$conversation)

	# generate the plot
	f <- ggplot(data=plotData, aes(x=intervalStartTime, y=value, group=group)) +
		geom_line() + geom_point() +
		labs(title=paste(conversation)) +
		facet_grid(rows = vars(group), scales = "free_y", switch = "y",
			labeller = as_labeller(c())
			) +
		ylab(NULL) + # remove the word "values"
		theme(strip.background = element_blank(), # remove the background
			strip.placement = "outside") # put labels to the left of the axis text

	return(f)
}

runPearsonsAll <- function(df, window){
	# generate Pearson's correlation statistics for all values in the input table
	# across all couples and all conversations

	# it will be important here to have a progress indicator

	# create an empty dataframe that will hold the final results
	outdf <- data.frame(matrix(ncol = ncol(df), nrow = 0))
	names(outdf) <- names(df)

	# unique dyads
	Couple_IDs <- unique(df$Couple_ID)

	# loop over the couples (dyads)
	for (CoupleID in Couple_IDs){
		# select the couple in order to get the unique conversations
		df0 <- df[df$Couple_ID == CoupleID, ]

		# unique conversations
		conversations <- unique(df0$conversation)

		# loop over the conversations
		for (conv in conversations){
			print(paste(CoupleID, conv))

			# run the Pearson's correlation
			usedf <- runPearsonsCouple(df, CoupleID, conv, window)

			outdf <- rbind(outdf, usedf)

		}

	}

	return(outdf)

}

exportToFile <- function(df, filename){
	write.csv(df,filename, row.names = FALSE)
}