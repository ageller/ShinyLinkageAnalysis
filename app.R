library(shiny)
library(shinyjs)
library(shinythemes)

library(dplyr)
library(ggplot2)
library(gridExtra)

library(plotly)

# the script that has all functions needed for analyzing and plotting the linkage data
source("src/R/analyzeLinkageData.R")

# global variabe to hold the data (will be defined when file is loaded)
df <- NULL

# Define UI
ui <- fluidPage(

	# set the colors using the shinytheme library : https://rstudio.github.io/shinythemes/
	theme = shinytheme("paper"),

	useShinyjs(),

	# change a few styles
	tags$head(
		tags$style(HTML("
			.input-group .form-control {
				padding-left:10px;
			}
			#file1_progress{
				height:20px;
			}
		"))
	),

	# App title
	headerPanel("Linkage Data Explorer"),

	# ui
	sidebarPanel(
		h5("1. Select your data file (.csv format)."),
		fileInput("file1", "",
			accept = c(
			"text/csv",
			"text/comma-separated-values,text/plain",
			".csv")
		),
		tags$hr(),
		uiOutput("uiStep2"),
		uiOutput("uiStep3"),

	),

	# plots
	mainPanel(
		plotlyOutput("PearsonsPlot"),
		height = "1000px",
		id = "mainPanel"
	)

)

# Define server logic
server <- function(input, output) {
	# increase the maximum file size
	options(shiny.maxRequestSize = 30*1024^2) 



	observe({
		input$file1
		isolate({
			hide("uiStep2")
			hide("uiStep3")
			hide("mainPanel")

			# read in the file
			file <- input$file1
			ext <- tools::file_ext(file$datapath)

			req(file)
			validate(need(ext == "csv", "Please upload a .csv file"))

			df <<- readData(file$datapath)

			# unique dyads
			Couple_IDs <- unique(df$Couple_ID)

			# unique conversations
			conversations <- unique(df$conversation)


			output$uiStep2 <- renderUI(
				div(id = "uiStep2",
					hr(style = "border-top: 1px solid #000000;"),

					h5("2. Select the subset of the data you want to analyze."),
					selectInput(
						"coupleID", "Dyad ID:",
						Couple_IDs,
						selected = "1"
					),
					selectInput(
						"conversation", "Conversation:",
						conversations,
						selected = conversations[1]
					),

					h5("3. Select the time window for the Pearson's correlation."),
					sliderInput("window", "Window (seconds) :", 0, 600, 15, step = 1),

					h5("4. Click the button below to update the plot."),
					actionButton("updatePlot", "Update Plot"),
				)
			)

			output$uiStep3 <- renderUI(
				div(id = "uiStep3", style="padding-top:30px",
					hr(style = "border-top: 1px solid #000000;"),

					h5("5. Click the button below to analyze all dyads and conversations and download the data as a .csv file."),
					downloadButton("runAll", "Run All and Download"),
				)
			)

			show("uiStep2")
			show("uiStep3")

		})
	})

	# when button is clicked, select the data and update plots object
	observeEvent(input$updatePlot, {
		
		#hide("mainPanel")

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

		show("mainPanel")


	})

	output$runAll <- downloadHandler(
		filename = function() {
			paste0('linkageData-', Sys.Date(), '.csv')
		},
		content = function(con) {
			outdf <- runPearsonsAll(df, input$window)
			write.csv(outdf, con, row.names = FALSE)
		}
	)



}

# create the app
shinyApp(ui, server)


