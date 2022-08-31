library(shiny)
library(shinyjs)
library(shinythemes)

library(dplyr)
library(ggplot2)
library(grid)

library(plotly)

# the script that has all functions needed for analyzing and plotting the linkage data
# This is not necessary when run as a package
# source("R/analyzeLinkageData.R")

ShinyLinkageAnalysis <- function(){
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

		# App title and credit
		headerPanel("Linkage Data Explorer"),
		div(style="font-size:14px; padding-left:18px; margin-bottom:20px",
			"Credit: Aaron M. Geller - Northwestern University IT Research Computing Services (RCS)"),

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
	server <- function(input, output, session) {
		# increase the maximum file size
		options(shiny.maxRequestSize = 30*1024^2) 


		# after the file is loaded, show the rest of the UI
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
						fluidRow(
							column(10, sliderInput("windowSliderValue", "Window (seconds) :", 0, 600, 15, step = 1)),
							column(2, textInput("windowTextValue", "", value = 15)),
						),

						h5("4. Click the button below to update the plot."),
						actionButton("updatePlot", "Update Plot"),
					)
				)

				output$uiStep3 <- renderUI(
					div(id = "uiStep3", style="padding-top:30px",
						hr(style = "border-top: 1px solid #000000;"),

						h5("5. Click the button below to analyze all dyads and conversations at the window specified above and download the data as a .csv file."),
						downloadButton("runAll", "Run All and Download"),
					)
				)

				show("uiStep2")
				show("uiStep3")

			})
		})

		# when the update plot button is clicked, select the data and update plots object
		observeEvent(input$updatePlot, {
			
			#hide("mainPanel")

			# Run the correlation on the desired data
			usedf <- runPearsonsCouple(df, input$coupleID, input$conversation, as.numeric(input$windowTextValue))

			# Generate the plot
			f <- plotPearsonsCouple(usedf)

			# Render the plot using plotly (for interactivity)
			output$PearsonsPlot <- renderPlotly(
				plotlyPearsonsCouple(f, topHeightFac = 1.5)
			)

			show("mainPanel")


		})

		# when the run all button is clicked, analyze the full file and download the results
		output$runAll <- downloadHandler(
			filename = function() {
				paste0('linkageData-', Sys.Date(), '.csv')
			},
			content = function(con) {
				outdf <- runPearsonsAll(df, as.numeric(input$windowTextValue))
				write.csv(outdf, con, row.names = FALSE)
			}
		)


		# handlers to connect the slider and text entry for the window
		# https://stackoverflow.com/questions/47822736/in-sync-sliderinput-and-textinput
		observeEvent(input$windowTextValue,{
			if(as.numeric(input$windowTextValue) != input$windowSliderValue & input$windowTextValue != "" &  input$windowSliderValue != ""){
				updateSliderInput(session = session, inputId = "windowSliderValue", value = input$windowTextValue) 
			} else {
				if (input$windowTextValue == "") updateSliderInput(session = session, inputId = "windowSliderValue", value = 1) 
			}
		})
		observeEvent(input$windowSliderValue,{
			if(as.numeric(input$windowTextValue) != input$windowSliderValue & input$windowTextValue != "" &  input$windowSliderValue != ""){
				updateTextInput(session = session, inputId = "windowTextValue", value = input$windowSliderValue) 
			}
		})		

	}

	# create the app
	shinyApp(ui, server)
}

