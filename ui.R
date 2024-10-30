# Define UI for application that draws a histogram
ui <- dashboardPage(
  # Page header
  dashboardHeader(
    title = "Nitrate Leaching Potential v1.0",
    titleWidth = 450),
  
  # sidebarMenu--------------------
  dashboardSidebar(
    sidebarMenu(
      menuItem("Predict nitrate leaching", tabName = "nitrate"),
      menuItem("Variables", tabName = "assumptions"),
      menuItem("References", tabName = "resources")
    )
  ),
  
  dashboardBody(
    shinyDashboardThemes(
      theme = "poor_mans_flatly"
    ),
    tabItems(
      # nitrate model-------------------
      tabItem(tabName = "nitrate",
              fluidRow(
                column(1,
                       h2(img(src = "grasslandColorCenter.png", height = 60))),
                
                column(10, align = "center",
                       h2("Nitrate Leaching Potential")),
                
                column(1,
                       h2(img(src = "uw-crest.png", height = 60)))),
              hr(style = "margin-top:0px"),
              p("To determine nitrate leaching potential, select your soil type and then define your cropping system. 
                You can hit 'Predict nitrate leaching' as many times as you like under different soils and cropping systems
                to compare."),
              p("The 'Variables' page lists the inputs and outputs for the nitrate leaching model for the current scenario."),
              hr(style = "margin-top:0px"),
              fixedRow(
                column(12,
                       h3("Select Your Soil Series"),
                       helpText(
                         a(
                           "Click here to find your soil (NRCS Web Soil Survey)",
                           href = "https://websoilsurvey.sc.egov.usda.gov/App/WebSoilSurvey.aspx",
                           target = "_blank")))),
              # soil selection begins
              # row of selectInputs for selecting soils
              fixedRow(
                # Input 1: dropdown for county selections
                column(4,
                       selectInput(
                         inputId = "county",
                         label = h4("County"),
                         choices = counties)),
                #Input 2: dropdown for component name selections. list is created in server.
                column(4, uiOutput("compname")),
                # Input 3: dropdown for soil symbol. list is created in server.
                column(4, uiOutput("musym"))),
              
              # crop management definition begins
              fixedRow(
                column(12,
                       h3(("Define your cropping System")))
              ),
              
              # row of input including crop rotation, cover crop and tillage (density for dry lot and pasture)
              fixedRow(
                ## COL 1 ##
                # crop rotation selection
                column(4,
                       radioButtons(
                         inputId = "crop",
                         label = h4("Crop Rotation"),
                         choices = crops)),
                
                ## COL 2 ##
                # cover crop, or density, depending on crop rotation
                column(4,
                       uiOutput("cover"),
                       uiOutput("legume")),
                
                ## COL 3 ##
                # tillage selection or density (if rotational pasture)
                column(4,
                       uiOutput("rotation"),
                       uiOutput("density"))
              ),
              
              
              # fertilizer (turned off for dry lot)
              fixedRow(
                column(6, align = "center",
                       uiOutput("fert")),
                
                column(6, align = "center",
                       uiOutput("manure"))
              ),
              
              # enter previous predictions
              fixedRow(
                conditionalPanel(
                  #input$crop == "Continuous corn grain"
                  condition = "input.crop != 'Dry lot'",
                  column(12,
                         h3(("Yield")))
                )
              ),
              br(),
              fluidRow(
                column(12,
                       uiOutput("yield_cc")),
                column(6,
                       uiOutput("yield_cg1")),
                column(6, 
                       uiOutput("yield_cg2")),
                column(4, 
                       uiOutput("yield_cso1")),
                column(4, 
                       uiOutput("yield_cso2")),
                column(4, 
                       uiOutput("yield_cso3")),
                column(3, 
                       uiOutput("yield_dr1")),
                column(3, 
                       uiOutput("yield_dr2")),
                column(3, 
                       uiOutput("yield_dr3")),
                column(3, 
                       uiOutput("yield_dr4")),
                column(3, 
                       uiOutput("yield_pt"))),
              fixedRow(
                column(12,
                       h3("Estimated erosion"))
              ),
              tags$div(
                "Estimate erosion or enter predicted erosion from ",
                tags$a(href="https://data-viz.it.wisc.edu/Grassland/soilPredictions/", 
                       "SnapPlus shiny app.")),
              column(6,
                     numericInput(inputId = "erosion",
                                  label = "Predicted erosion (tons/acre)",
                                  value = 2)),
              fluidRow(
                column(12, align = "center",
                       actionBttn(
                         inputId = "runModels",
                         label = "Predict nitrate leaching",
                         style = "simple",
                         color = "success"))
              ),
              br(),
              hr(style = "margin-top:0px"),
              uiOutput("plotData")
              # fluidRow(
              #   column(4,
              #          plotlyOutput("Nloss")),
              #   column(4,
              #          plotlyOutput("NlossH2O")),
              #   column(4,
              #          plotlyOutput("Nleach"))
              # ),
              # fluidRow(
              #   column(12,
              #          gt_output("scenarios"))
              # ),
              # fluidRow(
              #   column(6,
              #          actionButton("reset", "Clear scenarios")))),
      ),
      # variables------------------------------
      tabItem(tabName = "assumptions",
              p("Below are the inputs and outputs for the most recent scenario."),
              fluidRow(
                column(12,
                       gt_output("Ninputs"))
                ),
              br(),
              fluidRow(
                column(12,
                       gt_output("Noutputs"))
                ),
              br(),
              hr(style = "margin-top:0px"),
              fluidRow(
                column(12,
                       gt_output("leachYear"))
              ),
              br(),
              hr(style = "margin-top:0px"),
              fluidRow(
                column(6, 
                       textOutput("manureP"))
              ),
              fluidRow(
                column(6, 
                       textOutput("om"))
              )),
      # references----------------
      tabItem(tabName = "resources",
              fluidRow(
                column(1,
                       h2(img(src = "grasslandColorCenter.png", height = 60))),
                
                column(10, align = "center",
                       h3("Works Cited")),
                
                column(1,
                       h2(img(src = "uw-crest.png", height = 60)))),
              hr(style = "margin-top:0px"),
              p("Carrie Laboski and John Peters, Nutrient application guidelines for field, 
                vegetable, and fruit crops in Wisconsin. UW Extension A2809."),
              p("Meisinger, J. J., & Randall, G. W. (1991). Estimating nitrogen budgets 
                for soil-crop systems. In R. F. Follett, D. R. Keeney, & R. M. Cruse 
                (Eds.), Managing nitrogen for groundwater quality and farm profitability
                (pp. 85–124)"),
              p("Masarik, K. 2021. Soil nitrogen balance calculator: version 1.0. University of WI-Madison, Extension."),
              p("USDA Natural Resource Conservation Service (NRCS). Feb 2016. Wisconsin Conservation
                Planning Technical Note 1: Nutrient Management (590)."),
              hr(style = "margin-top:0px"),
              fluidRow(
                column(12,
                       tags$div(
                         "This app is part of the ",
                         tags$a(href="https://grasslandag.org/", 
                                "Grassland 2.0 project."), "It was designed to compare nitrate leaching potential 
                         under different crop managements located in Wisconsin, USA.")
                       )
                )
              )
      )
    )
  )
