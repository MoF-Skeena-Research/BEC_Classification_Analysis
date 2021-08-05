

library(shiny)
library(htmltools)
library(leaflet)
library(leaflet.extras)
library(data.table)
library(leafgl)

subzones_colours_ref <- fread("WNA_v12_HexCols.csv")
setnames(subzones_colours_ref,c("BGC","Col"))
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
    column(3,
           h2("Instructions"),
           p("Draw a rectangle on the map to select desired plots. Then, selected desired columns from the 
             environment table and click Run Query"),
           actionButton("showplots","Show Plots"),
           selectInput("envHeaders","Select Environment Fields:", choices = envNames, multiple = T,
                       selected = c("plotnumber","projectid","date","zone","subzone",
                                    "siteseries","moistureregime","nutrientregime",
                                    "elevation","slopegradient","aspect")),
           actionButton("runquery","Run Query!")
           ),
    column(9,
           
           leafglOutput("map",height = "85vh")
           )
  )
  
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  global_plotnum <- reactiveValues(plotno = NULL)
  
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
  
  observeEvent(input$showplots,{
    withProgress(message = "Fetching Data", {
      dat <- st_read(con, query = "select plotnumber,geom from env")
    })
    
    leafletProxy("map") %>%
      addGlPoints(data = dat, layerId = "plots", popup = ~ plotnumber)
  })
  
  observeEvent(input$runquery,{
    plotnums <- global_plotnum$plotno
    veg <- dbGetQuery(con,paste0("select * from veg where plotno IN ('",paste(plotnums,collapse = "','"),"')"))
    esql <- paste0("select ",paste(input$envHeaders,collapse = ","),
                   " from env where plotnumber IN ('",paste(plotnums,collapse = "','"),"')")
    env <- dbGetQuery(con,esql)
    output$env_tab <- renderTable({
      veg
    })
    showModal(div(id = "tableModal",
                  modalDialog(h2("Query Results!"),
                              tableOutput("env_tab")
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
