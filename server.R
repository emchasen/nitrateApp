
server <- function(input, output) {
  # soil selection---------------  
  ## soil comp by county update-------------
  observeEvent(input$county, { 
    soils_by_county <- soils %>% filter(County == input$county)
    comps <- sort(unique(soils_by_county$compnam)) # here is the reactive value (avail_comps = reactive())
    
    output$compname <- renderUI({
      selectInput(
        inputId = "compname",
        label = h4("Soil Name"),
        choices = comps) # comps would be a reactive value
    })
  })
  
  ## musym by soil comp update-------------------
  observeEvent(input$compname, { # might need req
    soils_by_comp <-
      soils %>% filter(County == input$county, compnam == input$compname)
    musyms <- sort(unique(soils_by_comp$MUSYM))
    
    output$musym <- renderUI({
      selectInput(
        inputId = "musym",
        label = h4("Soil Symbol"),
        choices = musyms)
    })
  })
  
  # crop selection------------------
  observeEvent(input$crop, {
    ## unique options by crop---------------
    options <- filter(Ninputs, crop == input$crop)
    
    covers <- unique(options$cover)
    rotations <- unique(options$rotation)
    densities <- unique(options$density)
    legumes <- unique(options$legume)
    
    # cover crop
    output$cover <- renderUI({
      selectInput(
        inputId = "cover",
        label = h4("Cover Crop System"),
        choices = covers,
        selected = "None")
    })
    
    # rotation and density
    output$legume <- renderUI({
      selectInput(
        inputId = "legume",
        label = h4("Legume interseed"),
        choices = legumes
      )
    })
    
    # rotation and density
    output$rotation <- renderUI({
      radioButtons(
        inputId = "rotation",
        label = h4("Rotational Management"),
        choices = rotations
      )
    })
    
    # rotation and density
    output$density <- renderUI({
      radioButtons(
        inputId = "density",
        label = h4("Animal density"),
        choices = densities
      )
    })
    
    ## hide elements when appropriate--------------------
    if (anyNA(covers)) output$cover <- NULL
    if (anyNA(rotations)) output$rotation <- NULL
    if (anyNA(densities)) output$density <- NULL
    if (anyNA(legumes)) output$legume <- NULL
    
    ## fertilizer----------------------
    output$fert <- renderUI({
      
      req(input$crop != "Dry lot")
      
      sliderInput(
        inputId = "fertilizerN",
        label = h4("Fertilizer N: Percent of crop N recommendation met with synthetic fertilizer"),
        min = 0, max = 150, value = 50, step = 25)
    })
    
    output$manure <- renderUI({
      
      req(input$crop != "Dry lot")
      
      sliderInput(
        inputId = "manureN",
        label = h4("Manure N: Percent of crop N recommendation or allowable N met with manure"),
        min = 0, max = 150, value = 50, step = 25)
    })
    
    ## yields--------------------
    # this would be one renderUI for the whole row (make list based on which crop rotation)
    ### cc yield-------------------
    output$yield_cc <- renderUI({
      
      req(input$crop == "Continuous corn grain")
      
      numericInput(
        inputId = "yield_cc",
        label = "Estimated corn grain yield (bu/acre)",
        value = 180)
    })
    
    ### cg yield-------------------
    output$yield_cg1 <- renderUI({
      
      req(input$crop == "Corn grain to soybeans")
      
      numericInput(
        inputId = "yield_cg1",
        label = "Estimated corn grain yield (bu/acre)",
        value = 180)
    })
    
    output$yield_cg2 <- renderUI({
      
      req(input$crop == "Corn grain to soybeans")
      
      numericInput(
        inputId = "yield_cg2",
        label = "Estimated soybean yield (bu/acre)",
        value = 55)
    })
    
    ### cso yield------------------
    output$yield_cso1 <- renderUI({
      
      req(input$crop == "Corn silage to soybeans to oats")
      
      numericInput(
        inputId = "yield_cso1",
        label = "Estimated corn silage yield (tons/acre)",
        value = 21.5)
    })
    
    output$yield_cso2 <- renderUI({
      
      req(input$crop == "Corn silage to soybeans to oats")
      
      numericInput(
        inputId = "yield_cso2",
        label = "Estimated soybean yield (bushels/acre)",
        value = 55)
    })
    
    output$yield_cso3 <- renderUI({
      
      req(input$crop == "Corn silage to soybeans to oats")
      
      numericInput(
        inputId = "yield_cso3",
        label = "Estimated oat yield (bushels/acre)",
        value = 62)
    })
    
    ### dr yield---------------------------
    output$yield_dr1 <- renderUI({
      
      req(input$crop == "Corn silage to corn grain to alfalfa (3x)")
      
      numericInput(
        inputId = "yield_dr1",
        label = "Estimated corn grain (bushels/acre)",
        value = 180)
    })
    
    output$yield_dr2 <- renderUI({
      
      req(input$crop == "Corn silage to corn grain to alfalfa (3x)")
      
      numericInput(
        inputId = "yield_dr2",
        label = "Estimated corn silage (tons/acre)",
        value = 21.5)
    })
    
    output$yield_dr3 <- renderUI({
      
      req(input$crop == "Corn silage to corn grain to alfalfa (3x)")
      
      numericInput(
        inputId = "yield_dr3",
        label = "Estimated alfalfa seeding year yield (tons/acre)",
        value = 1.5)
    })
    
    output$yield_dr4 <- renderUI({
      
      req(input$crop == "Corn silage to corn grain to alfalfa (3x)")
      
      numericInput(
        inputId = "yield_dr4",
        label = "Estimated alfalfa yield (tons/acre)",
        value = 4)
    })
    
    ### pt yield--------------------------
    output$yield_pt <- renderUI({
      
      req(input$crop == "Pasture")
      
      numericInput(
        inputId = "yield_pt",
        label = "Estimated pasture yield (tons/acre)",
        value = 3.5)
    })
    
  })
  
  ## stocking density--------------------
  observeEvent(list(input$crop, input$rotation), {
    
    options <- filter(Ninputs, crop == input$crop)
    
    if (!anyNA(options$rotation)) {
      req(input$rotation)
      options <- filter(options, rotation == input$rotation)
    }
    
    densities <- unique(options$density)
    
    if (anyNA(densities)) {
      output$density <- NULL
    } else {
      output$density <- renderUI({
        radioButtons(
          inputId = "density",
          label = h4("Herd density"),
          choices = densities
        )
      })
    }
  })
  
  # reactive values and dfs------------------------
  scenario <- reactiveValues()
  scenario$df <- data.frame()
  
  leachPotential <- reactiveValues()
  leachPotential$df <- data.frame(scenario = numeric(0), rotLeachN = numeric(0))
  rotTotalLossN <- reactiveValues()
  rotTotalLossN$df <- data.frame(scenario = numeric(0), rotLossN = numeric(0))
  H2OlossN <- reactiveValues()
  H2OlossN$df <- data.frame(scenario = numeric(0), rotNlossH2O = numeric(0))
  vals <- reactiveValues(count = 0)
  
  # run models--------------------
  observeEvent(input$runModels, {
    # soil properties
    soil <- soils %>%
      filter(County == input$county,
             compnam == input$compname,
             MUSYM == input$musym)
    
    # process user inputs
    #crop <- input$crop
    options <- filter(Ninputs, crop == input$crop)
    ## initialize variable to keep track of leachable N for each year of rotation
    rotYrs = unique(options$rotYrs)
    rotationCrops = sort(unique(options$cropYear))
    if(input$crop != "Dry lot") {
      fert <- input$fertilizerN
      manure <- input$manureN
    } else {
      fert = 0
      manure = 0
    }
    
    cropSystem = c() # paste the values from within the loop
    Ninputs_m = matrix(ncol = 9, nrow = rotYrs)
    #Ninputs_m = matrix(nrow = rotYrs, ncol = 9) # if we want dmYield
    Noutputs_m = matrix(ncol = 8, nrow = rotYrs)
    totalNloss = c()
    NlossH2O = c()
    leachN = c()
    manurePpercent = c()
    ## begin for loop-------------------------
    for (i in 1:rotYrs) {
      #print(i)
      # filter to correct row 
      cropYearNinputs <- filter(options, cropYear == rotationCrops[i])
      if (!anyNA(cropYearNinputs$cover)) {
        cropYearNinputs <- filter(cropYearNinputs, cover == input$cover)
        #cropYearNinputs <- filter(cropYearNinputs, cover == "None")
      }
      if (!anyNA(cropYearNinputs$rotation)) {
        cropYearNinputs <- filter(cropYearNinputs, rotation == input$rotation)
      }
      if (!anyNA(cropYearNinputs$density)) {
        cropYearNinputs <- filter(cropYearNinputs, density == input$density)
      }
      if (!anyNA(cropYearNinputs$legume)) {
        cropYearNinputs <- filter(cropYearNinputs, legume == input$legume)
      }
      ### define rasterLookup--------------
      rasterLookup <- unique(cropYearNinputs$rasterLookup)
      om <- unique(soil$om)
      rasterVal = c()
      if(rasterLookup == 'nResponse') {
        rasterVal = soil$nResponse
      } else {
        if(om < 2) {
          rasterVal = '<2'
        } else if(between(om, 2, 9.9)) {
          rasterVal = '2-9.9'
        } else if(between(om, 10, 20)) {
          rasterVal = '10-20' 
        } else if(om > 20) {
          rasterVal = '>20'
        }
      }
      cropYearNinputs <- filter(cropYearNinputs, rasterVals %in% rasterVal)
      #print(cropYearNinputs)
      
      ### model inputs-------------------
      fertNrec = cropYearNinputs$FertN
      manureNallow = cropYearNinputs$ManureN
      NfixPct = cropYearNinputs$NfixPct
      NH3loss = cropYearNinputs$NH3loss
      grazedManureN = cropYearNinputs$grazedManureN
      Nharv_content = cropYearNinputs$Nharv_content
      
      denitr <- filter(denit, DrainClass_char %in% soil$drangcl)
      denitr <- filter(denitr, omLow <= om & om <= omHigh)
      denitLoss <- denitr$Denitr
      
      ### yield conversions to dm tons------------------
      yield = c()
      if(input$crop == "Continuous corn grain") {
        yield = input$yield_cc * 56/2000 * (1-0.155) # corn grain conversion bu/ac to dm ton/acre
      } else if(input$crop == "Corn grain to soybeans" & i == 1) {
        yield = input$yield_cg1 * 56/2000 * (1-0.155) # corn grain conversion bu/ac to dm ton/acre
      } else if(input$crop == "Corn grain to soybeans" & i == 2) {
        yield = input$yield_cg2 * 60/2000 * 0.792 * 0.9008 # soybean conversion bu/ac to dm ton/acre
      } else if(input$crop == "Corn silage to soybeans to oats" & i == 1) {
        yield = input$yield_cso1 * (1-0.65) # dm tons of corn silage
      } else if(input$crop == "Corn silage to soybeans to oats" & i == 2) {
        yield = input$yield_cso2 * 60/2000 * 0.792 * 0.9008 # soybean conversion bu/ac to dm ton/acre
      } else if(input$crop == "Corn silage to soybeans to oats" & i == 3) {
        yield = input$yield_cso3 * 32/2000 * (1-0.14) # oat conversion bu/ac to dm ton/acre
      } else if(input$crop == "Corn silage to corn grain to alfalfa (3x)" & i == 1) {
        yield = input$yield_dr1 * 56/2000 * (1-0.155) # corn grain conversion bu/ac to dm ton/acre
      } else if(input$crop == "Corn silage to corn grain to alfalfa (3x)" & i == 2) {
        yield = input$yield_dr2 * (1-0.65) # dm tons of corn silage 
      } else if(input$crop == "Corn silage to corn grain to alfalfa (3x)" & i == 3) {
        yield = input$yield_dr3 *  (1-0.13) # alfalfa dm tons
      } else if(input$crop == "Corn silage to corn grain to alfalfa (3x)" & i == 4) {
        yield = input$yield_dr4 * (1-0.13) # alfalfa dm tons
      } else if(input$crop == "Corn silage to corn grain to alfalfa (3x)" & i == 5) {
        yield = input$yield_dr4 * (1-0.13) # alfalfa dm tons
      } else if(input$crop == "Pasture") {
        yield = input$yield_pt # pasture dm tons
      } else if(input$crop == "Dry lot") {
        yield = 0
      }
      
      ### inputs and outputs-------------------
      fertN = fert/100 * fertNrec  ## actual fertilizer N applied in lb/ac
      manureN = manure/100 * manureNallow  ## actual manure N applied in lb/ac
      #manureN = manure/100 * fertNrec  ## actual manure N applied in lb/ac
      precip = 32  ## varies by region, inches/year (ridgeValley = 43, driftless = 44, ne = 35, cb = 38)
      precN = 0.5 * precip * 0.226  ## precipitation N inputs in lb/ac
      dryN = precN  ## assume dry deposition is equal to precipitation, lb/ac
      harvN = yield * 2000 * Nharv_content  ## harvested N output, lb/ac (crop yield in tons dm, convert to lbs dm) # dry lot yield = 0
      fixN = harvN * (NfixPct / 100) + 3  ## N fixation input, lb/ac
      NH3_N = fertN * NH3loss / 100  ## ammonia loss output, lb/ac
      denitN = fertN * denitLoss / 100  ## denitrification loss,
      erosN = input$erosion * om * 2  ## note that OM is in units of % ## erosion from models = tons/acre
      #erosN = 2 * om * 2
      inputsN = fertN + manureN + precN + dryN + fixN + grazedManureN
      gasN = 0.01 * inputsN  ## misc gases are estimated as 1% of inputs
      NH3senN = 8  ## ammonia loss at senescence
      runoffN = 0
      outputsN = harvN + NH3_N + denitN + erosN + gasN + NH3senN + runoffN
      
      ### fill matrices------------------
      Ninputs_m[i, 1] = i
      Ninputs_m[i, 2] = fertNrec
      Ninputs_m[i, 3] = fertN 
      Ninputs_m[i, 4] = manureNallow
      Ninputs_m[i, 5] = manureN
      Ninputs_m[i, 6] = precN
      Ninputs_m[i, 7] = dryN
      Ninputs_m[i, 8] = fixN
      Ninputs_m[i, 9] = grazedManureN
      
      Noutputs_m[i, 1] = i
      Noutputs_m[i, 2] = harvN
      Noutputs_m[i, 3] = NH3_N 
      Noutputs_m[i, 4] = denitN
      Noutputs_m[i, 5] = erosN 
      Noutputs_m[i, 6] = gasN 
      Noutputs_m[i, 7] = NH3senN 
      Noutputs_m[i, 8] = runoffN
      
      
      # nitrate leaching for a single year----------------
      leachN[i] = if_else(inputsN - outputsN < 0, 0, inputsN - outputsN)
      totalNloss[i] = leachN[i] + (outputsN - harvN)
      NlossH2O[i] = leachN[i] + (erosN + runoffN)
      print(leachN[i])
      print(totalNloss[i])
      print(NlossH2O[i])
      nLosses <- data.frame(totalNloss = totalNloss, NlossH2O = NlossH2O, leachN = leachN)
      print(nLosses)
      
      # manureP from manureN--------------------- 
      availableManureN = manureN 
      appliedManureN = availableManureN/0.4
      manureP = appliedManureN/3
      Pneeds = cropYearNinputs$Pneeds
      # what percent of Pneeds have we met with appliedManureN
      manurePpercent[i] = 100*(manureP/Pneeds)
    }
    
    
    # leaching by year------------------------
    # display leaching from each year
    output$leachYear <- render_gt({
      
      ##TODO may have to fix this table
      nLosses %>%
        gt(rownames_to_stub = TRUE) %>%
        tab_header(title = "Nitrate leaching by year",
                   subtitle = "N lbs") %>%
        fmt_number(columns = where(is.numeric),
                   decimals = 1) %>%
        cols_label(totalNloss = "Total N loss",
                   NlossH2O = "N loss to H2O",
                   leachN = "Nitrate leaching")
    })
    
    # manureP%------------------------
    output$manureP <- renderText({
      paste("Average manure P percent:", round(mean(manurePpercent),0))
    })
    
    # soil om----------------------------
    output$om <- renderText({
      paste("Soil om percent:", om)
    })
    
    # input table----------------------
    inputNames = c('Year', 'Recommended N', 'Fertilizer N', 'Allowed manure N', 'Manure N', 'Precipitation', 'Dry deposition', 'Fixation', 'Grazing manure')
    output$Ninputs <- render_gt(
      
      Ninputs_m %>%
        as_tibble(.name_repair = ~inputNames) %>%
        gt(rowname_col = "Year") %>%
        fmt_number(columns = 'Recommended N':'Grazing manure',
                   decimals = 1) %>%
        tab_header(title = "Nitrogen inputs",
                   subtitle = "N lbs/acre/year")  %>%
        tab_stubhead(label = "Year") %>%
        tab_style(
          style = list(
            cell_text(weight = "bold")
          ),
          locations = cells_column_labels()
        )
      
    )
    
    # output table--------------------
    outputNames = c('Year', 'Harvested N', 'Ammonia loss', 'Denitrification', 'Erosion', 'Misc. gases', 'Ammonia at senescence', 'Runoff')
    output$Noutputs <- render_gt(
      
      Noutputs_m %>%
        as_tibble(.name_repair = ~outputNames) %>%
        gt(rowname_col = "Year") %>%
        fmt_number(columns = 'Harvested N':'Runoff',
                   decimals = 1) %>%
        tab_header(title = "Nitrogen outputs",
                   subtitle = "N lbs/acre/year") %>%
        tab_stubhead(label = "Year") %>%
        tab_style(
          style = list(
            cell_text(weight = "bold")
          ),
          locations = cells_column_labels()
        )
    )
    
    # crop system name--------------------
    if (is.na(cropYearNinputs$cover)){
      cover <- NULL
    } else {
      ifelse(cropYearNinputs$cover == "None", cover <- "No cover", cover <- cropYearNinputs$cover)} 
    if (is.na(cropYearNinputs$rotation)){ 
      rotation <- NULL
    } else {
      rotation <- cropYearNinputs$rotation} 
    if (is.na(cropYearNinputs$density)) {
      density <- NULL
    } else {
      density <- cropYearNinputs$density} 
    if (is.na(cropYearNinputs$legume)) {
      legume <- NULL
    } else {
      ifelse(cropYearNinputs$legume == "Yes", legume <- "with legume", legume <- "no legume")}
    
    if(input$crop != "Dry lot") {
      cropSystem = str_trim(paste(cropYearNinputs$crop, cover, rotation, density, legume, 
                                  "fertN%:", fert, "manureN%:", manure))
    }  else {
      cropSystem = str_trim(paste(cropYearNinputs$crop, density, "density"))
    }
    
    
    
    # number of scenarios
    print('rotation values')
    vals$count <- vals$count + 1
    rotLeachN <- round(mean(leachN),2)
    print(rotLeachN)
    newLeachN <- c(scenario = vals$count, rotLeachN = rotLeachN)
    rotLossN = round(mean(totalNloss), 2)
    print(rotLossN)
    newTotalNloss <- c(scenario = vals$count, rotLossN = rotLossN)
    rotNlossH2O = round(mean(NlossH2O), 2)
    print(rotNlossH2O)
    newNlossH2O <- c(scenario = vals$count, rotNlossH2O = rotNlossH2O)
    
    # scenario dataframe
    newScenario <- c(scenario = vals$count, 'Soil series' = unique(soil$compnam), 'Map Symbol' = unique(soil$MUSYM),
                     'Crop system' = cropSystem, 'Total N loss' = rotLossN, 'N loss to water' = rotNlossH2O, 'N leach potential' = rotLeachN)
    
    scenario$df <- bind_rows(scenario$df, newScenario)
    
    # scenario dataframe---------------
    output$scenarios <- render_gt({
      scenario$df %>%
        as_tibble() %>%
        gt()
    })
    
    rotTotalLossN$df <- bind_rows(rotTotalLossN$df, newTotalNloss)
    
    # nitrogen loss plot--------------------------
    output$Nloss <- renderPlotly({
      
      validate(
        need(is.data.frame(rotTotalLossN$df), "add scenarios")
      )
      
      y <- list(
        title = " ",
        #range = c(0, max(pred_table$df$Erosion)),
        side = "top"
      )
      x <- list(
        title = "",
        zeroline = FALSE,
        showline = FALSE,
        showticklabels = FALSE,
        showgrid = FALSE
      )
      
      plot_ly(rotTotalLossN$df,
              y = ~rotLossN, x = ~scenario,
              marker = list(color = 'rgba(50, 171, 96, 0.7)'),
              type = "bar",
              hovertext = ~rotLossN,
              hoverinfo = "text") %>%
        layout(title = "PREDICTED <br> TOTAL NITRATE LOSS <br> (lbs/acre)",
               xaxis = x, yaxis = y, barmode = 'group',
               margin = list(t=100))
      
    })
    
    # nitrate water loss plot----------------
    H2OlossN$df <- bind_rows(H2OlossN$df, newNlossH2O)
    
    # nitrogen leaching plot
    output$NlossH2O <- renderPlotly({
      
      validate(
        need(is.data.frame(H2OlossN$df), "add scenarios")
      )
      
      y <- list(
        title = " ",
        #range = c(0, max(pred_table$df$Erosion)),
        side = "top"
      )
      x <- list(
        title = "",
        zeroline = FALSE,
        showline = FALSE,
        showticklabels = FALSE,
        showgrid = FALSE
      )
      
      plot_ly(H2OlossN$df,
              y = ~rotNlossH2O, x = ~scenario,
              marker = list(color = 'rgba(50, 171, 96, 0.7)'),
              type = "bar",
              hovertext = ~rotNlossH2O,
              hoverinfo = "text") %>%
        layout(title = "PREDICTED <br> NITRATE LOSS IN H2O <br> (lbs/acre)",
               xaxis = x, yaxis = y, barmode = 'group',
               margin = list(t=100))
      
    })
    
    # nitrate leaching plot------------
    
    leachPotential$df <- bind_rows(leachPotential$df, newLeachN)
    
    # nitrogen leaching plot
    output$Nleach <- renderPlotly({
      
      validate(
        need(is.data.frame(leachPotential$df), "add scenarios")
      )
      
      y <- list(
        title = " ",
        #range = c(0, max(pred_table$df$Erosion)),
        side = "top"
      )
      x <- list(
        title = "",
        zeroline = FALSE,
        showline = FALSE,
        showticklabels = FALSE,
        showgrid = FALSE
      )
      
      plot_ly(leachPotential$df,
              y = ~rotLeachN, x = ~scenario,
              marker = list(color = 'rgba(50, 171, 96, 0.7)'),
              type = "bar",
              hovertext = ~rotLeachN,
              hoverinfo = "text") %>%
        layout(title = "PREDICTED <br> NITRATE LEACHING <br> (lbs/acre)",
               xaxis = x, yaxis = y, barmode = 'group',
               margin = list(t=100))
      
    })
    
  })
  
  observeEvent(input$reset, {
    # reset the preds
    scenario$df <- NULL
    leachPotential$df <- NULL
    output$manureP <- NULL
    output$leachYear <- NULL
  })
  
}