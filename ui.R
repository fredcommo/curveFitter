########################################
#
# nplr App deployed on Synapse
#
########################################
# Deploy on shiny.io
# Sys.setlocale(locale="en_US.UTF-8")
# deployApp(account="fredcommo")
# old: ${iframe?site=https%3A%2F%2Ffcommo%2Dshinyapps%2Eshinyapps%2Eio%2FnplrApp%2F&height=2000}
# new: ${iframe?site=https%3A%2F%2Ffredcommo%2Eshinyapps%2Eio%2FnplrApp%2F&height=2000}
# https://fredcommo.shinyapps.io/nplrApp/

shinyUI(
	fluidPage(

########################################
# TITLE

		headerPanel(
			div(class="col-sm-12 lato", id="pageTitle",
				tags$span('curve',tags$strong('Fitter')),
				tags$span(class="small", ' a free Drug',tags$strong('Response'),'Analysis App.')
				),
			"Curve Fitter"
			),

########################################
# SIDEBAR PANEL
  		sidebarPanel(
  			
			########################################
			# IMPORT GOOGLE FONTS

			tags$link(
				rel = "stylesheet", 
			    href="https://fonts.googleapis.com/css?family=Lato:400,100,100italic,300,300italic,400italic,700,700italic,900,900italic",
				type="text/css"
				),

			tags$link(
				rel = "stylesheet", 
			    href="https://fonts.googleapis.com/css?family=Calligraffitti",
				type="text/css"
				),

			########################################
			# LOAD CSS FILE
    		includeCSS("nplr.css"),
   
			########################################
			# LINK TO NPLR
   			h5(
				em(
					a("...using R package 'nplr'",
						href="http://cran.r-project.org/web/packages/nplr/index.html",
						target="_blank")
				),
			 	align="right"
			),

			########################################
			# CHOOSE FILE
			withTags(
				div(class="col-sm-12 section-title",
						h3("Choose a file", em(id="supportedFiles", "(.csv, .tsv, .txt)"))
					)
				),
		    withTags(
		    	div(
		    		class="row",
			    		div(class="col-xs-12 btn-input",
				            div(class="col-xs-6 checkboxText", "File with headers"),
				            div(class="col-xs-6", checkboxInput('header', 'yes', TRUE))
				        	),
			    		div(class="col-xs-12", id="inputFile",
				    		fileInput('file1', '',
				    			accept=c('text/csv',
				    				'text/tab-separated-values,text/comma-separated-values,text/plain',
				    				'.csv', '.txt', '.tsv'
				    				)
				    			)
				    	)
		            )
		    ),

			########################################
			# TRANSFORM
			withTags(div(class="col-sm-12 section-title", h3("Tranform"))),
		    withTags(
		    	div(class="row",
			    	div(class="col-xs-12 btn-input",
				            div(class="col-xs-6 checkboxText", "Convert Log10[conc.]"),
				            div(class="col-xs-6", checkboxInput('toLog', 'yes', TRUE))
			            )
		    	)
		    ),
		    withTags(
		    	div(class="row",
		    		div(class="col-xs-12 btn-input",
			            div(class="col-xs-6 checkboxText", "Compute props"),
			            div(class="col-xs-6", checkboxInput('props', 'yes', FALSE))
		            )
		    	)
		    ),

			########################################
			# ANALYSE
			withTags(div(class="col-sm-12 section-title", h3("Analyse"))),
		    withTags(
		    	div(class="col-sm-12 btn-input",
					    h4("Number of parameters"),
					    radioButtons('npar', '', c("best"='all', "2"='2', "3"='3', "4"='4', "5"='5'), "all")
				    )
		    ),


			########################################
			# Settings
			withTags(div(class="col-sm-12 section-title", h3("Visualize"))),

			# HIGHLIGHT
		    uiOutput('modelNames'),

			# SAVE OPTIONS
		    withTags(
		    	div(class='col-sm-12',

				    div(class = "row",
					    	div(class="col-xs-12 radioText", "Show values"),
							div(class="col-xs-12 btn-input",
								div(class="col-xs-4", checkboxInput(inputId = "points", label = "Points", value=FALSE)),
								div(class="col-xs-4", checkboxInput(inputId = "Means", label = "Means", value=TRUE)),
								div(class="col-xs-4", checkboxInput(inputId = "SDerr", label = "SDerr", value=TRUE))
								)
				    	),

				    # Show x-axis as Log
			    	div(class="row",
			    		div(class="col-xs-12 btn-setting",
				            div(class="col-xs-6 checkboxText", "Conc. in Log10"),
				            div(class="col-xs-6", checkboxInput('showAsLog', 'yes', TRUE))
			            )
			    	),

				    # Show legend
			    	div(class="row",
			    		div(class="col-xs-12 btn-setting",
				            div(class="col-xs-6 checkboxText", "Show legend"),
				            div(class="col-xs-6", checkboxInput('showLegend', 'yes', TRUE))
			            )
			    	),

			        # Name axes
			        div(class="row",
					        div(class='col-xs-6', textInput("xlabel", 'x-axis name', "Log10(Conc.)")),
					        div(class='col-xs-6', textInput("ylabel", 'y-axis name', 'Response (Vs. control)'))
			        	)
			    )
		    ),

			########################################
			# DOWNLOAD BUTTONS
			withTags(div(class="col-sm-12 section-title", h3("Save"))),
		    withTags(
		    	div(class="row",
		    		div(class="col-sm-12", id="saving-subtitle", h4("Filename (without extension)")),
		    		div(class="col-sm-12", id="fileName", textInput("fname", '', 'myResults')),
			    	div(class='col-sm-12',
			            	div(class='col-sm-6 save-btn', downloadButton("downloadPLot", class="btn-lg btn-success", "Save Plot")),
			            	div(class='col-sm-6 save-btn', downloadButton("downloadData", class="btn-lg btn-success", "Save Results"))
			            )
		    		)
		    )
		),
		# end sidebarPanel

########################################
# OUTPUTS PANEL
		mainPanel(
			tabsetPanel(

				tabPanel("Curve",

					# Plot setting Sliders
					withTags(
						div(class="row sliders-panel",

							# Points size slider
						    div(class="col-xs-4",
								div(class="col-xs-12 radioText", "Points size"),
						    	div(class="col-xs-12 slider-input",
						    		sliderInput("pSize", label="", min=0, max=1, value=.3, step=.01))
						    	),

						    # Line width slider
						    div(class="col-xs-4",
								div(class="col-xs-12 radioText", "Lines width"),
						    	div(class="col-xs-12 slider-input",
						    		sliderInput("lWidth", label="", min=0, max=1, value=.5, step=.01))
						    	),

						    # Legend size slider
						    div(class="col-xs-4",
								div(class="col-xs-12 radioText", "Legend size"),
						    	div(class="col-xs-12 slider-input",
						    		sliderInput("legendSize", label="", min=0, max=1, value=.5, step=.01))
						    	)

							)
						), # withTags

					# Plot output
					h6(verbatimTextOutput("checkFile"),
                        style="visibility: collapse; height: 0px;"),

                    conditionalPanel(
                        condition = "output.checkFile == '0'",
                        	div(class="col-sm-12",
	                        	uiOutput('message'),
	                        	imageOutput("welcomeImage")
                        	)
                        ),

                    conditionalPanel(
                        "output.checkFile == '1'",
                        plotOutput("plot", width = "100%", height = "100%")
                        )

					# withTags(
					# 	div(class='row',
					# 		uiOutput('message'),
					# 		div(class = "col-sm-12", plotOutput('plot'))
					# 		)
					# 	)

					), # tabpanel Curve

				tabPanel("Summary",
					withTags(					
						div(class = "col-sm-12",
								h3(id="model-summary", "Model(s) summary", align="center"),
								tableOutput('summary')
								)
						)
					) # tabpanel summary

		    ) #tabsetPanel
		), #main panel
########################################
# Footer
	withTags(
			div(class="row",
				div(class="col-sm-12 footer",

						span(class="calligraffitti", "DesignedBy"),
						span(class="lato", "FredCommo"),
						span(HTML("&copy;2016. ")),
						span(id="email",
							a("frederic.commo@gustaveroussy.fr",
			    				href="mailto:frederic.commo@gustaveroussy.fr?Subject=nplrApp",
			    				target="_top")
							)

					)
				)
			)
	)
)
