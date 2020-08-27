

###########################################################
# AqMaD RShiny interface                                  #
#                                                         #
# Auteurs: Willem Stolte                                  #
#                                                         #
#                                                         #
# Datum : 2020-07-22                                      #
# Bedrijf: Deltares                                       #
# Licentie: GNU General Public License                    #
#                                                         #           
# Contact : Willem Stolte                                 #
# Email : willem.stolte@deltares.nl                       #
#                                                         #
########################################################### 



##=PREPARATION========

##==legend=====

## COMMENTS
## ##      = comment
## #       = outcommented code
## #!#     = should be checked if location of script changes 
## #?#     = to do
## ##==    = block separator

## OBJECTS
## fnXXX   = filename
## dirXXX  = directory (path) / foldername
## oXXX    = loaded object
## dfXXX   = dataframe      
## dtXXX   = datatable 

##==install packages and open libraries ====

getPackage <- function(pkg){
    if(!require(pkg, character.only = TRUE)){
        install.packages(pkg, dependencies = TRUE)
        library(pkg, character.only = TRUE)
    }
    return(TRUE)
}


getPackage("shiny")         
getPackage("shinydashboard")
getPackage("leaflet")
getPackage("lubridate")
getPackage("shinycssloaders")
getPackage("V8")
# library(htmlwidgets)
# getPackage("mapview")
# library(plotly)
# library(shinyjs)



##==settings=======

options(shiny.display.mode="showcase")
options(scipen = 999)  ## no scientific numbering
options(shiny.reactlog = T)
# options(shiny.maxRequestSize = 30*1024^2) ## maximum upload size is now 30Mb, standard for R shiny is 5Mb. 
## rshiny shinyapps.io max limit size = 32MB. 

# jsResetCode <- "shinyjs.reset = function() {history.go(0)}" # Define the js method that resets the page



##== load functions ====

source('functionality.r')



##==set paths ========

#!# directories

#!# files



##== set variables =======

# variables = reactiveValues(soortgroep = NULL, soortgroepnaam = NULL, watertype = NULL)



##==load files=========

## no files to load yet



##== set functions=====

## no functions yet



##==USER INTERFACE==================

header  <- dashboardHeader(title = "iDelftWQ")

sidebar <- dashboardSidebar(
    sidebarMenu(id = "side_tabs",
                menuItem(text = "Start", tabName = "start", icon = icon("home")),
                menuItem(text = "Station map", tabName = "stationMap", icon = icon("map")),
                menuItem(text = "Time series", tabName = "timeSeries", icon = icon("list-alt", lib = "glyphicon")),                # menuItem(text = "Z-waardes bekijken", tabName = "z_waardes", icon = icon("cogs")),
                menuItem(text = "Target diagrams", tabName = "targetDiagrams", icon = icon("upload")),
                menuItem(text = "Ecoplots", tabName = "ecoPlots", icon = icon("upload")),
                uiOutput("substanceUI"), 
                uiOutput("locationUI"),
                uiOutput("layerUI")
                # menuItem(text = "Achtergrondinformatie", tabName = "info", icon = icon("info"))
    ))
## rows have a grid width of 12, so a box with width = 4, takes up one third of the space
## tops will be lined out, bottoms not
## heights are in pixels.. 

body    <- dashboardBody(
    
    tabItems(
        
        ##===startpagina==============
        tabItem(tabName = "start",
                fluidRow(
                    box(title = "Welkom to hisView",
                        solidHeader = T,
                        status = "success",
                        collapsible = F,
                        p("hisView is meant to view and compare water quality model output with other model output and data"),
                        img(src='deltares_logo.png'),
                        width = 12
                    ),
                    box(
                        textInput(
                            inputId = "con1", 
                            label = "path to output 1", 
                            width = 1000, 
                            value = "p:/11204882-002-interreg-wadden-sea/simulations/A07_waq_normal_e3_2006_new_obs/DFM_OUTPUT_DCSM-FM_0_5nm_waq/DCSM-FM_0_5nm_waq_0000_his.nc"
                        )
                    ),
                    box(
                        textInput(
                            inputId = "con2", 
                            label = "path to output 2", 
                            width = 1000, 
                            value = "another path"
                        )
                    )
                )
        ),
        
        ##========= map page ============================
        tabItem(tabName = "stationMap",
                fluidRow(
                    leafletOutput("map1", height = "90vh"),
                    width = 12#,
                )
        ),
        
        
        ##========= time series page ============================
        tabItem(tabName = "timeSeries",
                fluidRow(
                    box(title = "Time Series", 
                        # textOutput("testContent1"),
                        # tableOutput("testContent3"),
                        plotOutput("timePlot1"),
                        width = 12,
                        solidHeader = T, 
                        # background = "green",
                        status = "success")
                )),
        
        
        ##== Target Diagrams ==============
        
        tabItem(tabName = "targetDiagrams", 
                fluidRow(
                    box(title = "Future target diagram plots",
                        solidHeader = T,
                        status = "success",
                        collapsible = T,
                        collapsed = F,
                        width = 12
                    ),
                    
                )),
        
        
        ##== Limitation ecoplots ==============
        
        tabItem(tabName = "ecoPlots", 
                fluidRow(
                    box(title = "Future ecoplots",
                        solidHeader = T,
                        status = "success",
                        collapsible = T,
                        collapsed = F,
                        width = 12
                    ),
                    
                )),
        
        
        ##====achtergrondinformatie pagina ====
        tabItem(tabName = "info",
                fluidRow(
                    box(title = "Download achtergrondinformatie",
                        solidHeader = T,
                        status = "success",
                        collapsible = T,
                        collapsed = F,
                        width = 12,
                        # h3("Handleiding AqMaD"),
                        # downloadButton("handleiding_aqmad", "Download handleiding voor AqMaD"),
                        p("De R-code voor deze tool is op aanvraag beschikbaar bij Willem.Stolte (at Deltares.nl)")
                    )))))


ui <- dashboardPage(skin = "green", header, sidebar, body,  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"), tags$style(".leaflet-top {z-index:999!important;}"))) ## tags$head etc is noodzakelijk om de zoombuttons van leaflet achter de dropdown box te krijgen. 
# ui <- dashboardPage(header, sidebar, body,  tags$head(tags$style(".leaflet-top {z-index:999!important;}"))) ## tags$head etc is noodzakelijk om de zoombuttons van leaflet achter de dropdown box te krijgen. 


##==SET SERVER=========


server <- function(input, output, session) {
    
    
    ##========= reset shiny app ==================
    # observeEvent(input$reset_button, {js$reset()}) 
    
    
    ###== First connection ==================================
    
    # opens connection to first model output file
    nc1 <- reactive({ncdf4::nc_open(input$con1)})
    
    # prints model output path
    output$testContent1 <- renderText({
        input$con1
    })
    
    # Makes list with all variables in model output file and their dimensions
    allVars1 <- reactive({
        map(nc1()$var, list("size"))
    })
    
    # Makes dataframe with all variables and their dimension number
    # 1 = either timestep or location var
    # 2 = surface specific in something/m2
    # 3 = volume specific such as substances in g/m3
    varDims1 <- reactive({
        map(allVars1(), function(x) length(x)) %>%
            unlist(use.names = T) %>%
            as.data.frame() %>%
            rownames_to_column() %>%
            rename(variable = rowname, dims = ".")
    })
    
    subVars1 <- reactive({
        varDims1() %>% 
            filter(dims == 3) %>%
            filter(!grepl("water_quality_output", variable))
    })
    
    # Makes dataframe with all locations coordinates, names and id
    locVars1 <- reactive({
        colbind_loc_vars(nc1())
    })
    
    layers1 <- reactive({
        seq(1, nc1()$dim$laydim$len)
    })
    
    
    output$timePlot1 <- renderPlot({
        dff1 <- nc_his2df(nc = nc1(), 
                          vars = input$subs1, 
                          station_id = input$locs1, 
                          layer = input$layer1) %>% 
            mutate(plot = "left")
        # dff2 <- nc_his2df(nc2(), input$subs1, input$locs1, input$layer1) %>% mutate(plot = "right")
        dff <- dff1 #%>% bind_rows(dff2)
        
        ggplot(dff, aes(datetime, value)) + 
            geom_path(aes(color = plot)) + 
            facet_grid(variable ~ location, scales = "free")
    })
    
    output$map1 <- renderLeaflet({
        
        selectedLocs <- locVars1() %>%
            filter(station_id %in% input$locs1) %>%
            sf::st_as_sf(coords = c("station_x_coordinate", "station_y_coordinate"), crs = 4326)
        
        otherLocs <- locVars1() %>%
            filter(!station_id %in% input$locs1) %>%
            sf::st_as_sf(coords = c("station_x_coordinate", "station_y_coordinate"), crs = 4326)
        
        leaflet() %>%
            addTiles() %>%
            addCircleMarkers(data = otherLocs, 
                             # lng = ~station_x_coordinate,
                             # lat = ~station_y_coordinate,
                             label = ~station_id,
                             radius = 4, stroke = F, fillOpacity = 1, labelOptions = labelOptions(noHide = T, textOnly = T, opacity = 0.5)
            ) %>%
            addCircleMarkers(data = selectedLocs,
                             # lng = ~station_x_coordinate,
                             # lat = ~station_y_coordinate,
                             label = ~station_id,
                             radius = 10, stroke = F, fillColor = "red", fillOpacity = 1, labelOptions = labelOptions(noHide = T, textOnly = F)
            )
    })    
    
    ###== Second connection ==================================
    
    # opens connection to first model output file
    nc2 <- reactive({ncdf4::nc_open(input$con2)})
    
    # prints model output path
    output$testContent2 <- renderText({
        input$con2
    })
    
    # Makes list with all variables in model output file and their dimensions
    allVars2 <- reactive({
        map(nc2()$var, list("size"))
    })
    
    # Makes dataframe with all variables and their dimension number
    # 1 = either timestep or location var
    # 2 = surface specific in something/m2
    # 3 = volume specific such as substances in g/m3
    varDims2 <- reactive({
        map(allVars2(), function(x) length(x)) %>%
            unlist(use.names = T) %>%
            as.data.frame() %>%
            rownames_to_column() %>%
            rename(variable = rowname, dims = ".")
    })
    
    subVars2 <- reactive({
        varDims2() %>% 
            filter(dims == 3) %>%
            filter(!grepl("water_quality_output", variable))
    })
    
    # Makes dataframe with all locations coordinates, names and id
    locVars2 <- reactive({
        colbind_loc_vars(nc2())
    })
    
    layers2 <- reactive({
        seq(1, nc2()$dim$laydim$len)
    })
    
    
    # output$timePlot2 <- renderPlot({
    #     dff <- nc_his2df(nc2(), input$subs2, input$locs2, input$layer2)
    #     ggplot(dff, aes(datetime, value)) + 
    #         geom_path(aes()) + 
    #         facet_grid(variable ~ location, scales = "free")
    # })
    
    
    ##==UI OPBOUW=========================================
    
    output$substanceUI <- renderUI({
        tagList(
            selectInput("subs1", "substances", subVars1()$variable, selected = "NO3", multiple = T)
        )
    })
    
    output$locationUI <- renderUI({
        tagList(
            selectInput("locs1", "locations", locVars1()$station_id, selected = "NOORDWK20", multiple = T)
        )
    })
    
    output$layerUI <- renderUI({
        tagList(
            selectInput("layer1", "layers", layers1())
        )
    })
    
    
    
    
    
    ##==voorbeeld==============
    # output$huidigAbiotiekLocatie <- renderUI({
    #     req(huidigAbiotiekGroep())
    #     choice_of_locaties <- unique(na.omit(huidigAbiotiekGroep()$locname))
    #     selectInput("huidigAbiotiekLocatie_ui", "Locatie:", choice_of_locaties)
    # })
    # 
    
}

shinyApp(ui, server)
