# Kiri Daust
# August 2021

library(shiny)
library(htmltools)
library(leaflet)
library(leaflet.extras)
library(data.table)
library(leafgl)
library(shinyjs)
library(RPostgreSQL)
library(sf)

source("leaflet_tiles.R")

mbtk="pk.eyJ1Ijoid2htYWNrZW4iLCJhIjoiY2twaDVkNXU5MmJieTJybGE3cWRtY3Q4aCJ9.ISBkzSHFfrr78AVP2y2FeQ"
mblbsty = "whmacken/ckph5q6d21q1318nz4shnyp20"
mbsty="whmacken/ckph5e7y01fhr17qk5nhnpo10"

drv <- dbDriver("PostgreSQL")
sapply(dbListConnections(drv), dbDisconnect)
con <- dbConnect(drv, user = "postgres", password = "postgres", host = "138.197.168.220", 
                 port = 5432, dbname = "bec_master")

envNames <- dbGetQuery(con,"select column_name from information_schema.columns where table_name = 'env'")[,1]
# Define UI for application that draws a histogram

ui <- fluidPage(
  titlePanel("Download BGC Plot Data", windowTitle = "BGC Plots"),
  tags$head(tags$style("#tableModal .modal-dialog {width: fit-content !important;}")),
  fluidRow(
    useShinyjs(),
    column(3,
           h2("Instructions"),
           p("Draw a rectangle on the map to select desired plots. Then, selected desired columns from the 
             environment table and click Run Query"),
           actionButton("showplots","Show Plots"),
           radioButtons("selectlayer", "Choose a Layer to Select", choices = c("Districts","BGCs")),
           selectInput("envHeaders","Select Environment Fields:", choices = envNames, multiple = T,
                       selected = c("plotnumber","projectid","date","zone","subzone",
                                    "siteseries","moistureregime","nutrientregime",
                                    "elevation","slopegradient","aspect")),
           sliderInput("vegcov","Select Min % Cover:", min = 0, max = 5, value = 0.5,step = 0.1),
           actionButton("runquery","Run Query!"),
           br(),
           hidden(
             div(id = "data",
                 wellPanel(
                   fluidRow(
                     column(6,
                            actionButton("showenv","View Environment Table"),
                            downloadButton("dldenv","Download Environment")
                     ),
                     column(6,
                            actionButton("showveg","View Veg Table"),
                            downloadButton("dldveg","Download Veg")
                     )
                   )
                 )
                 
              )
           )
           ),
    column(9,
           textOutput("selections"),
           leafglOutput("map",height = "85vh"),
           actionButton("clearhl","Clear Selection")
           )
  )
  
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  global_plotnum <- reactiveValues(plotno = NULL)
  global_select <- reactiveValues(BGC = character(), District = character())
  
  output$map <- renderLeaflet({
    leaflet() %>%
      setView(lng = -122.77222, lat = 51.2665, zoom = 6) %>%
      leaflet::addTiles(
        urlTemplate = paste0("https://api.mapbox.com/styles/v1/", mbsty, "/tiles/{z}/{x}/{y}?access_token=", mbtk),
        attribution = '&#169; <a href="https://www.mapbox.com/feedback/">Mapbox</a>',
        group = "Hillshade",
        options = leaflet::pathOptions(pane = "mapPane")) %>%
      leaflet::addProviderTiles(leaflet::providers$Esri.WorldImagery, group = "Satellite",
                                options = leaflet::pathOptions(pane = "mapPane")) %>%
      leaflet::addTiles(
        urlTemplate = paste0("https://api.mapbox.com/styles/v1/", mblbsty, "/tiles/{z}/{x}/{y}?access_token=", mbtk),
        attribution = '&#169; <a href="https://www.mapbox.com/feedback/">Mapbox</a>',
        group = "Cities",
        options = leaflet::pathOptions(pane = "overlayPane")) %>%
      addBGCTiles() %>%
      addDrawToolbar(
        targetGroup='draw',
        editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions())) %>%
      leaflet::addLayersControl(
        baseGroups = c("Hillshade","Satellite"),
        overlayGroups = c("BGCs","Districts","Cities","draw"),
        position = "topright")
  })
  
  observeEvent(input$dist_click,{
    global_select$District <- append(global_select$District,input$dist_click)
  })
  
  observeEvent(input$bgc_click,{
    global_select$BGC <- append(global_select$BGC,input$bgc_click)
  })
  
  observeEvent(input$showplots,{
    withProgress(message = "Working...", {
      dat <- st_read(con, query = "select plotnumber,geom from env")
    })
    
    leafletProxy("map") %>%
      addGlPoints(data = dat, layerId = "plots", popup = ~ plotnumber)
  })
  
  observeEvent(input$selectlayer,{
    if(input$selectlayer == "Districts"){
      session$sendCustomMessage("selectDist","puppy")
    }else{
      session$sendCustomMessage("selectBGC","puppy")
    }
  })
  
  observeEvent(input$clearhl,{
    session$sendCustomMessage("clearDist","puppy")
    session$sendCustomMessage("clearBGC","puppy")
    global_select$BGC <- character()
    global_select$District <- character()
  })
  
  output$selections <- renderText({
    c(global_select$BGC,global_select$District)
  })
  
  observeEvent(input$runquery,{
    plotnums <- global_plotnum$plotno
    withProgress(message = "Getting Data...", {
      veg <- RPostgreSQL::dbGetQuery(con,paste0("select * from veg where plotno IN ('",paste(plotnums,collapse = "','"),
                                                "') AND cover > ",input$vegcov))
      esql <- paste0("select ",paste(input$envHeaders,collapse = ","),
                     " from env where plotnumber IN ('",paste(plotnums,collapse = "','"),"')")
      env <- RPostgreSQL::dbGetQuery(con,statement = esql)
    })

    output$env_tab <- renderTable({
      env
    })
    output$dldenv <- downloadHandler(
      filename = "EnvironmentTable.csv",
      content = function(file){
        fwrite(env,file)
      }
    )
    output$veg_tab <- renderTable({
      veg
    })
    output$dldveg <- downloadHandler(
      filename = "VegTable.csv",
      content = function(file){
        fwrite(veg,file)
      }
    )
    shinyjs::show("data")
  })
  
  observeEvent(input$showenv,{
    showModal(div(id = "tableModal",
                  modalDialog(h2("Query Results!"),
                              tableOutput("env_tab"),
                              easyClose = TRUE
                  )
    ))
  })
  
  observeEvent(input$showveg,{
    showModal(div(id = "tableModal",
                  modalDialog(h2("Query Results!"),
                              tableOutput("veg_tab"),
                              easyClose = TRUE
                  )
    ))
  })
  
  observeEvent(input$map_draw_new_feature,{
    feature <- input$map_draw_new_feature
    temp <- feature$geometry$coordinates[[1]]
    coordMat <- matrix(unlist(temp),ncol = 2,byrow = T)
    feat <- st_as_sf(as.data.frame(coordMat),coords = c(1,2),crs = 4326)
    feat <- st_cast(st_combine(feat),"POLYGON")
    withProgress({
      sql <- paste0(
        "with poly as (
        SELECT ST_PolygonFromText('", st_as_text(feat), "',4326) geom
      ) 
      
      select plotnumber
      from env 
      join poly
      on ST_Within(env.geom,poly.geom)
      "
      )
      dat <- dbGetQuery(con,sql)[,1]
      global_plotnum$plotno <- dat
    })
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
