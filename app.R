library(shiny)
#library(shinyjs)
library(shinythemes)

library(dplyr)
library(ggplot2)
library(gridExtra)

library(plotly)

# the script that has all functions needed for analyzing and plotting the linkage data
source("src/R/analyzeLinkageData.R")

# read in the data
#### I should include this as a text entry box in the ui
df <- readData("src/data/EC_Physio_continuous_LONG_winsorized.csv")

# unique dyads
Couple_IDs <- unique(df$Couple_ID)

# unique conversations
conversations <- unique(df$conversation)


# Define UI
ui <- fluidPage(

	# set the colors using the shinytheme library : https://rstudio.github.io/shinythemes/
	theme = shinytheme("paper"),

	# App title
	headerPanel("Linkage Data Explorer"),

	# ui
	sidebarPanel(
		h5("1. Select the subset of the data you want to analyze."),
		selectInput(
			"coupleID", "Couple ID:",
			Couple_IDs,
			selected = "1"
		),
		selectInput(
			"conversation", "Conversation:",
			conversations,
			selected = "Negative"
		),

		h5("1. Select the time window for the Pearson's correlation."),
		sliderInput("window", "Window (s) :", 0, 600, 15, step = 1),

		h5("3. Click the button below to update the plot."),
        actionButton("updatePlot", "Update Plot"),


	),

	# plots
	mainPanel(
		plotlyOutput("PearsonsPlot"),
		height = "1000px",
	)
)

# Define server logic
server <- function(input, output) {

	# when button is clicked, select the data and update plots object
	observe({
		input$updatePlot
		isolate({

			# Run the correlation on the desired data
			usedf <- runPearsonsCouple(df, input$coupleID, input$conversation, input$window)

			# Generate the plot
			f <- plotPearsonsCouple(usedf)

			# Render the plot using plotly (for interactivity)
			#### Can the height be dynamic based on the page size (e.g., using css)?
			#https://stackoverflow.com/questions/61122868/long-facet-wrap-labels-in-ggplotly-plotly-overlap-facets-strip-background
			output$PearsonsPlot <- renderPlotly(
				plotlyPearsonsCouple(f)
			)
		})
	})

}

# create the app
shinyApp(ui, server)