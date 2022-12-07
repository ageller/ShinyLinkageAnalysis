library(shiny)
library(shinyjs)
library(shinythemes)
library(shinyBS)
library(shinyalert)

library(dplyr)
library(ggplot2)
library(grid)
library(colourpicker)

library(plotly)

# the script that has all functions needed for analyzing and plotting the linkage data
# This is not necessary when run as a package
# source("R/analyzeLinkageData.R")

ShinyLinkageAnalysis <- function(){
	# global variable to hold the data (will be defined when file is loaded)
	df <- NULL

	# global variable to hold the column names for use in the analysis (will be defined after the file is loaded)
	columnNames <<- data.frame(list(
		individualID = NULL, 
		coupleID = NULL, 
		task = NULL,
		independentVar = NULL,
		dependentVar = NULL
	))
				

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
			style = "max-height: 80vh; overflow-y: auto;",
			h5("1. Select your data."),
			h6("Load your data file (.csv format)."),
			fileInput("file1", "",
				accept = c(
				"text/csv",
				"text/comma-separated-values,text/plain",
				".csv")
			),
			uiOutput("uiStep1b"),
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

		# Show the IRB agreement
		shinyalert(
			title = "<p style = 'font-size:3vh; font-weight:bold'>Welcome to the <i>Linkage Data Explorer</i>.  Before proceeding, please read and agree to the following privacy statement.</p>
			<p style = 'font-size:3vh; margin-top:2vh'>I agree to not upload any identifiable information, code, or key per HIPAA and PII standard and to abide by any additional regulations of my host country and institution.</p>
<p style = 'font-size:2vh; font-style: italic; margin-top:2vh'>Identifiable means that the identity of an individual is or may be ascertained by the researcher (e.g., name, social security number). Code means an individual’s identifiable information has been replaced by a code (e.g., numbers, letters). Key means that there is a key to link the identifiable information and code of an individual.</p>", 
			 html = TRUE, showConfirmButton = TRUE, size = "l", confirmButtonText = "I Agree", closeOnEsc = FALSE, closeOnClickOutside = FALSE, immediate = TRUE
		)

		# after the file is loaded, select the columns
		observe({
			input$file1
			isolate({
				hide("uiStep1b")
				hide("uiStep2")
				hide("uiStep3")
				hide("mainPanel")

				# read in the file
				file <- input$file1
				ext <- tools::file_ext(file$datapath)

				req(file)
				validate(need(ext == "csv", "Please upload a .csv file"))

				df <<- readData(file$datapath)

				output$uiStep1b <- renderUI(
					div(id = "uiStep1b",
						h6("Identify the columns to use in this analysis.  (Place your mouse over a dropdown for more information.)"),

						selectInput("individualIDColumn", "Individual ID column name:", colnames(df) ),
						selectInput("coupleIDColumn", "Dyad ID column name:", colnames(df) ),
						selectInput("taskColumn", "Task column name :", colnames(df) ),
						selectInput("independentVarColumn", "Independent variable column name:", colnames(df) ),
						selectInput("dependentVarColumn", "Dependent variable column name:", colnames(df) ),
						bsTooltip(id = "individualIDColumn", title = "Select the column that contains unique values for each individual."),
						bsTooltip(id = "coupleIDColumn", title = "Select the column that contains unique values for each couple.  There should be two individual IDs for each couple ID."),
						bsTooltip(id = "taskColumn", title = "Select the column that contains the task names during which measurements were made of each couple.   This column can contain multiple different tasks. (For instance, this could contain \"control\" and \"experiment\" tasks.)"),
						bsTooltip(id = "independentVarColumn", title = "Select the column that contains the variable that should be plotted on the \"x\" axis (e.g., time)."),
						bsTooltip(id = "dependentVarColumn", title = "Select the column that contains the variable that should be plotted on the \"y\" axis (e.g., heart rate)."),


						h5("Click the button below to prepare the data."),
						actionButton("dataProcessed", "Prepare the data"),
					)
				)

				show("uiStep1b")

			})
		})


		# after the data is processed, show the rest of the UI
		observeEvent(input$dataProcessed, {
			isolate({
				hide("uiStep2")
				hide("uiStep3")
				hide("mainPanel")

				# process the data
				columnNames <<- data.frame(list(
					individualID = input$individualIDColumn, 
					coupleID = input$coupleIDColumn, 
					task = input$taskColumn,
					independentVar = input$independentVarColumn,
					dependentVar = input$dependentVarColumn
				))
				
				df <<- processData(df, columnNames)

				# unique dyads
				Couple_IDs <- unique(df[columnNames$coupleID])

				# unique tasks
				tasks <- unique(df[columnNames$task])


				output$uiStep2 <- renderUI(
					div(id = "uiStep2",
						hr(style = "border-top: 1px solid #000000;"),

						h5("2. Select the subset of the data you want to analyze."),
						fluidRow(
							column(6, selectInput("coupleID", "Dyad ID:", Couple_IDs, selected = "1")),
							column(6, selectInput("task", "Task:", tasks, selected = tasks[1])),
						),

						h5("3. Select the time window for the Pearson's correlation."),
						fluidRow(
							column(10, sliderInput("windowSliderValue", "Window (seconds) :", 0, 600, 15, step = 1)),
							column(2, textInput("windowTextValue", "", value = 15)),
						),

						h5("4. Customize the plot."),
						bsCollapse(id = "collapsableCustomize",
							bsCollapsePanel("Click here to show/hide options", "", style = "default",
								div(
									checkboxInput("showMeasurement", paste("Show", columnNames$dependentVar, "panel"), value = TRUE),
									checkboxInput("showPC", "Show Pearson's coefficient panel", value = TRUE),
									checkboxInput("showPP", "Show Pearson's p-value panel", value = TRUE),
									checkboxInput("showMeasurementPoints", paste("Include points in", columnNames$dependentVar, "panel"), value = FALSE),
									checkboxInput("showPCPoints", "Include points in Pearson's coefficient panel", value = TRUE),
									checkboxInput("showPPPoints", "Include points in Pearson's p-value panel", value = TRUE),
									checkboxInput("showPlimit", "Show Pearson's p-value limit line", value = TRUE),
									checkboxInput("showPrects", "Show rectangle for significant correlation regions", value = FALSE),
									colourInput("partner1Color1","Color for Partner 1:", value = "#F8766D"),
									colourInput("partner2Color2","Color for Partner 2:", value = "#00BFC4"),
									colourInput("pearsonColor","Color for Pearson's plots:", value = "black"),
									colourInput("plimitColor","Color for p-value limit line:", value = "#7CAE00"),
									colourInput("prectsColor","Color for regions of significant correlation", value = "#C77CFF"),
									textInput("prectsAlpha", "Opacity for regions of significant correlation:", value = 0.3),
									textInput("plimitVal", "Pearson's p-value significance limit:", value = 0.05),
									textInput("maxYlimit", paste("Maximum value for the y axix on the", columnNames$dependentVar, "panel (leave blank for autoscaling)"), value = NA),
									textInput("minYlimit", paste("Minimum value for the y axix on the", columnNames$dependentVar, "panel (leave blank for autoscaling)"), value = NA)
								)
							)
						),

						h5("5. Click the button below to update the plot."),
						actionButton("updatePlot", "Update Plot"),
					)
				)

				output$uiStep3 <- renderUI(
					div(id = "uiStep3", style="padding-top:30px",
						hr(style = "border-top: 1px solid #000000;"),

						h5("6. Click the button below to analyze all dyads and tasks at the window specified above and download the data as a .csv file."),
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
			usedf <- runPearsonsCouple(df, input$coupleID, input$task, as.numeric(input$windowTextValue), columnNames)

			# Generate the plot

			f <- plotPearsonsCouple(usedf, columnNames, includeFacet = c(input$showMeasurement, input$showPC, input$showPP), addPlimit = input$showPlimit, plimit = as.numeric(input$plimitVal), plotPoints = c(input$showMeasurementPoints, input$showPCPoints, input$showPPPoints), colors = c("Partner 1" = input$partner1Color1, "Partner 2" = input$partner2Color2, "Pearson" = input$pearsonColor, "plimit" = input$plimitColor, "prects" = input$prectsColor), showPrects = input$showPrects, prectsAlpha = as.numeric(input$prectsAlpha), forPlotly = TRUE, dependentYrange = c(as.numeric(input$minYlimit), as.numeric(input$maxYlimit)))

			height <- sum(c(460, 180, 180)*c(input$showMeasurement, input$showPC, input$showPP))
			height <- max(height, 400)
			topHeightFac <- 1.0
			if (input$showMeasurement) topHeightFac <- 1.5

			# Render the plot using plotly (for interactivity)
			output$PearsonsPlot <- renderPlotly(
				plotlyPearsonsCouple(f, columnNames, topHeightFac = topHeightFac, height = height )
			)

			show("mainPanel")


		})

		# when the run all button is clicked, analyze the full file and download the results
		output$runAll <- downloadHandler(
			filename = function() {
				paste0('linkageData-', Sys.Date(), '.csv')
			},
			content = function(fname) {
				outdf <- runPearsonsAll(df, as.numeric(input$windowTextValue), columnNames)
				write.csv(renameColumns(outdf), fname, row.names = FALSE)
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

