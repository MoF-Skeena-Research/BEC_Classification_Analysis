library(analogsea)
Sys.setenv(DO_PAT="eae4166ed2fac0e3c41660fe26a009bb0176ab8bceeaf753faf5189f58a06520")

temp <- analogsea::droplets()
server <- temp$`shiny-server`
setwd("./shinyapp")
analogsea::droplet_ssh(server,"rm -R /srv/shiny-server/becplots")
analogsea::droplet_ssh(server, "mkdir /srv/shiny-server/becplots")
analogsea::droplet_upload(server, "./app.R", "/srv/shiny-server/becplots/app.R")
analogsea::droplet_upload(server, "./leaflet_tiles.R", "/srv/shiny-server/becplots/leaflet_tiles.R")
analogsea::droplet_upload(server, "./htmlwidgets", "/srv/shiny-server/becplots")
##analogsea::droplet_upload(server, "./WNA_v12_HexCols.csv", "/srv/shiny-server/becplots/WNA_v12_HexCols.csv")
analogsea::droplet_ssh(server, "chown -R shiny:shiny /srv/shiny-server")
analogsea::droplet_ssh(server, "systemctl restart shiny-server")
