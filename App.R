library(osmdata)
library(ggplot2)
library(dplyr)
library(here)
library(RColorBrewer)
library(scales)
library(shiny)
library(bslib)
library(shinyjs)
library(sf)
require(DT)
# library(plotly)

# By locaion
university <- "UW-Madison"
uwm_bb <- getbb(university)
uwm_bb.width <- abs(uwm_bb["x", "max"] - uwm_bb["x", "min"])
uwm_bb.height <- abs(uwm_bb["y", "max"] - uwm_bb["y", "min"])
# Bounding Box
uwm_bb_orig_x_min = -89.437
uwm_bb_orig_x_max = -89.393
uwm_bb_orig_y_min = 43.067
uwm_bb_orig_y_max = 43.082

# minlat <- 43.0664600
# minlon <- -89.4271200 
# maxlat <- 43.0802700 
# maxlon <- -89.3950600

colors_osm <- 
  list(water = "#B3D2DE",
       vegetation = "#79A45C",
       parking = "#AAAAAA",
       main_road = "#F8C394", 
       other_roads = "#000000",
       cycle_path = "#296218",
       rail_road = "#333333") # remeber to dash style it


# Extract data from OSM databse if XML file doesn't exist, or from XML file if it does
osm_data_dir <- file.path("Data", "Map_Data")


osmdata_sf_wrapper <- function(bbox, file_path, values, key = NULL) {
  if(is.null(key)) {
    output.q <- opq(bbox = bbox) %>%
      add_osm_features(features = values)    
  } else {
    output.q <- opq(bbox = bbox) %>%
      add_osm_feature(key = key, value = values) 
  }
  if (file.exists(file_path)) {
    print(sprintf("Extracting map data from local file : %s", file_path))
    output.sf <- osmdata_sf(q = output.q, doc = file_path)
  } else {
    print("Extracting map data from online OSM database")
    output.sf <- osmdata_sf(q = output.q)
    output.q %>% 
      osmdata_xml(file = file_path)
  }
  return(output.sf)
}

print("loading data for Lake Mendota (Request 1/6)")
lake_mendota.sf <- osmdata_sf_wrapper(bbox = uwm_bb, 
                                      file_path = file.path(osm_data_dir, 'lake_mendota.osm'),
                                      key = "name",
                                      values = "Lake Mendota")
print("loading data for Major Roads (Request 2/6)")
uw_major_roads.sf <- osmdata_sf_wrapper(bbox = uwm_bb, 
                                        file_path = file.path(osm_data_dir, 'uw_major_roads.osm'),
                                        key = "highway",
                                        values = c("motorway", "primary", "secondary"))
print("loading data for Minor Roads (Request 3/6)")
uw_minor_roads.sf <- osmdata_sf_wrapper(bbox = uwm_bb, 
                                        file_path = file.path(osm_data_dir, 'uw_minor_roads.osm'),
                                        key = "highway",
                                        values = c("tertiary", "residential"))
print("loading data for Railways (Request 4/6)")
uw_railways.sf <- osmdata_sf_wrapper(bbox = uwm_bb, 
                                     file_path = file.path(osm_data_dir, 'uw_railways.osm'),
                                     key = "railway", 
                                     values = "rail")
print("loading data for Cycle Paths (Request 5/6)")
uw_cyclepaths.sf <- osmdata_sf_wrapper(bbox = uwm_bb, 
                                       file_path = file.path(osm_data_dir, 'uw_cyclepaths.osm'),
                                       key = "highway", 
                                       values = "cycleway")
print("loading data for Buildings (Request 6/6)")
uw_buildings.sf <- osmdata_sf_wrapper(bbox = uwm_bb, 
                                      file_path = file.path(osm_data_dir, 'uw_buildings.osm'),
                                      values = c(sprintf("\"operator\" = \"%s\"", university),
                                                 "\"building\" = \"university\"", 
                                                 "\"name\" = \"University Club\"",
                                                 "\"name\" = \"American Family Children's Hospital\""))

  
print("Combine Lake Mendota Files")
lake_mendota_for_plot.sf <- 
  dplyr::bind_rows(
    lake_mendota.sf$osm_polygons %>% 
      select(name, geometry),
    lake_mendota.sf$osm_multipolygons %>% 
      select(name, geometry)
  ) 

print("Combine Building Files")
uw_buildings_for_plot.sf <- 
  dplyr::bind_rows(
    uw_buildings.sf$osm_polygons %>% 
      select(name, amenity, building, geometry),
    uw_buildings.sf$osm_multipolygons %>% 
      select(name, amenity, building, geometry)
  ) %>% 
  filter(!(name %in% c("Lakeshore Nature Preserve"))) %>% 
  mutate(amenity = ifelse(is.na(amenity), "none", amenity)) %>% 
  filter(amenity != "parking" & !is.na(building))
# uw_buildings_polygons_for_plot.sf <- 
#   uw_buildings.sf$osm_polygons %>% 
#   select(name, amenity, building, geometry) %>%
#   filter(!(name %in% c("Lakeshore Nature Preserve"))) %>% 
#   mutate(amenity = ifelse(is.na(amenity), "none", amenity)) %>% 
#   filter(amenity != "parking" & !is.na(building))
# 
# 
# uw_buildings_for_plot.sf <- 
#   uw_buildings.sf$osm_multipolygons %>% 
#   select(name, amenity, building, geometry) %>%
#   filter(!(name %in% c("Lakeshore Nature Preserve"))) %>% 
#   mutate(amenity = ifelse(is.na(amenity), "none", amenity)) %>% 
#   filter(amenity != "parking" & !is.na(building))


# Load post-doc data
print("Get Number of Post-Docs")
link_to_n_postdocs_file <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vSwQbIxNFp7AtBDyuWhEykEmZEk7yu8cc5xKEz9YYaN3_flPyAspDVSLqQWgTVbp-fn8DpVB-0zyiq0/pub?gid=1211731426&single=true&output=csv"
# n_postdocs.df <- read.csv(file.path(here(), 'Data', 'Post-Doc_Counts', 'Buildings_AllPostdocFeb10_2025.csv')) %>%
n_postdocs.df <- read.csv(link_to_n_postdocs_file) %>% 
  rename(n_postdocs = count) %>% 
  group_by(name_osm) %>% 
  dplyr::summarise(n_postdocs = sum(n_postdocs)) %>% 
  filter(name_osm != "")
uw_buildings_for_plot.sf <- 
  merge(uw_buildings_for_plot.sf,
        n_postdocs.df, 
        by.x = "name", 
        by.y = "name_osm", 
        all.x = TRUE)


# Create the plot object, using the osm_lines element of tucson_major
print("Plot Buildings")
uwmadison.plot <- ggplot() +
  geom_sf(data = uw_buildings_for_plot.sf,
          mapping = aes(fill = n_postdocs),
          inherit.aes = FALSE,
          color = "black",
          size = 0.2) + 
  geom_sf_text(data = uw_buildings_for_plot.sf,
               aes(label = as.character(n_postdocs)), 
               size = 4, fontface = "bold",
               check_overlap = FALSE)

print("Plot Roads")
uwmadison_with_roads.plot <- uwmadison.plot + 
  geom_sf(data = uw_major_roads.sf$osm_lines,
          inherit.aes = FALSE,
          color = colors_osm$main_road,  # medium gray
          linewidth = 0.9) + 
  geom_sf(data = uw_minor_roads.sf$osm_lines,
          inherit.aes = FALSE,
          color = colors_osm$other_roads,  # medium gray
          linewidth = 0.5) + 
  geom_sf(data = uw_cyclepaths.sf$osm_lines,
          inherit.aes = FALSE,
          color = colors_osm$cycle_path,  # medium gray
          linewidth = 0.8) + 
  geom_sf(data = uw_railways.sf$osm_lines,
          inherit.aes = FALSE,
          color = colors_osm$rail_road,  # medium gray
          linetype = "dashed",
          linewidth = 0.5)

print("Plot Lake")
uwmadison_with_lake.plot <- uwmadison_with_roads.plot +
  geom_sf(data = lake_mendota_for_plot.sf,
          inherit.aes = FALSE,
          fill = colors_osm$water, 
          size = 0.2) + 
  geom_sf_text(data = lake_mendota_for_plot.sf,
               aes(label = name), 
               size = 2, 
               check_overlap = TRUE)

myColors <- colorRampPalette(brewer.pal(11, "Spectral"))(13) 
print("Setup - Done")


# plot.height <- 7 
# ggsave(filename = file.path(here(), "Post_Doc_Campus_Map_Simple_2025.png"),
#        plot = map_to_save.plot, width = 1.05*plot.height * uwm_bb.width/uwm_bb.height,
#        height = plot.height, units = "in")



ui <- page_sidebar(
  useShinyjs(),
  tags$script(HTML("
    $(document).on('wheel', '#plot-container', function(e) {
      e.preventDefault();
      var direction = (e.originalEvent.deltaY < 0) ? 'in' : 'out';
      Shiny.setInputValue('zoom_direction', {
        direction: direction,
        x: e.offsetX,
        y: e.offsetY,
        nonce: Math.random() // ensure value changes every time
      }, {priority: 'event'});
    });
  ")),
  sidebar = sidebar(
    width = 400,
    # North boundary
    numericInput(
      inputId = "y_north",
      value = uwm_bb_orig_y_max,
      label = "North", 
      min = uwm_bb_orig_y_min, 
      max = uwm_bb_orig_y_max, 
      step=0.001
    ),
    fluidRow(
      # West limit
      column(6, numericInput(
        inputId = "x_west", 
        value = uwm_bb_orig_x_min, 
        label = "West", 
        min = uwm_bb_orig_x_min, 
        max = uwm_bb_orig_x_max, 
        step=0.001)),
      # East limit
      column(6, numericInput(
        inputId = "x_east", 
        value = uwm_bb_orig_x_max,
        label = "East", 
        min = uwm_bb_orig_x_min, 
        max = uwm_bb_orig_x_max, 
        step=0.001))
    ),
    # South limit
    numericInput(
      inputId = "y_south",
      value = uwm_bb_orig_y_min,
      label = "South", 
      min = uwm_bb_orig_y_min, 
      max = uwm_bb_orig_y_max, 
      step=0.001
    ),
    actionButton(
      inputId = "reset_window", 
      label = 'Reset Window'
    ),
    textInput(
      inputId = "plot_title",
      label = "Plot title:",
      value = "Map of UW-Madison Postdoc Locations (February 2025)",
      placeholder = "Enter title here"
    ),
    hr(style = "border-top: 1px solid #000000;"),
    # fluidRow(
    #   column(8, textInput(
    #     inputId = "file_name_plot", 
    #     value = "Map of Post-Docs on Campus", 
    #     label = "File name")),
    #   column(4, numericInput(
    #     inputId = 'plot_height',
    #     value = 7,
    #     label = "Picture Height (in)"))
    # ), 
    textInput(
        inputId = "file_name_plot", 
        value = "UW-Madison_Postdocs_Census_Data_Feb_2025.png", 
        label = "File name"),
    numericInput(
        inputId = 'plot_height',
        value = 7,
        label = "Picture Height (in)"), 
    downloadButton(
      'download_ggplot',
      'Download Map'),
    hr(style = "border-top: 1px solid #000000;"),
    textInput(
      inputId = "n_postdoc_csv_file", 
      value = link_to_n_postdocs_file, 
      placeholder = "Link to post-doc census CSV file",
      label = NULL),
    actionButton(
      inputId = "update_map", 
      label = 'Update Map')
    # fluidRow(
    #   column(9, textInput(
    #     inputId = "n_postdoc_csv_file", 
    #     value = link_to_n_postdocs_file, 
    #     placeholder = "Link to post-doc census CSV file",
    #     label = NULL)),
    #   column(3, actionButton(
    #     inputId = "update_map", 
    #     label = 'Update Map'))
    #     )
  ),
  navset_card_underline(
    nav_panel("Instructions", 
              card(card_header("Instructions"),
                   htmlOutput(outputId = "text_1"), 
                   min_height = "150px")),
    nav_panel("Map", 
      card(card_header("Post Doc Campus Map"),
             # plotlyOutput("map_ggplot"),
             div(id = "plot-container",        
                 plotOutput(outputId = "map_ggplot",
                            brush = brushOpts(id = "map_zoom_brush"),
                            dblclick = "map_dblclick", 
                            hover = hoverOpts("map_hover", delay = 0, delayType = "throttle"))),
             min_height = "500px"), 
      card(card_header("Post-Doc Census Data"),
           DT::DTOutput(outputId = "n_postdocs.df"))),
    nav_panel("Dataset Fullscreen", 
              card(card_header("Post-Doc Census Data"),
                   DT::DTOutput(outputId = "n_postdocs.df.zoom")),
              card(downloadButton(outputId = "download_census_data", 
                                  label = "Download Census Data (csv)")))
  )
)

# Define server

server <- function(input, output, session) {
  # sciexpo_study.react <- reactive({
  #   sciexpo_study.df %>%
  #     filter(dv == input$y) %>% 
  #     slice_head(n = input$sample_size)
  # }
  # ) 
  # 
  # sciexpo_study_summary.react <- reactive({
  #   cols2group <- c(input$x, input$color)
  #   coucou <- sciexpo_study.react() 
  #   coucou <- coucou %>%
  #     group_by(across(all_of(cols2group))) %>% 
  #     dplyr::summarise(dv_mean = mean(value), 
  #                      dv_sd = ifelse(is.na(sd(value)), 0, sd(value)),
  #                      eb_min = dv_mean-dv_sd,
  #                      eb_max = dv_mean+dv_sd)
  #   return(coucou)
  # }
  # ) 
  # 
  
  plot_map <- function() {
    map_to_save.plot <- uwmadison_with_lake.plot + 
      coord_sf(xlim = c(input$x_west, input$x_east),
               ylim = c(input$y_south, input$y_north),
               expand = FALSE) +
      theme_void() +
      theme(plot.background = element_rect(fill = "#FFFFFF")) +
      scale_fill_distiller(palette = "RdYlBu", direction = -1) + 
      ggtitle(input$plot_title)
    
    return(map_to_save.plot)    
  }
  
  output$map_ggplot <- renderPlot({
    # print(sciexpo_study.react())
    # print(sciexpo_study_summary.react())
    plot_map()
  })
    
  observeEvent(input$zoom_direction, {
    req(input$map_hover)
    
    # Zoom factor
    mouse <- input$map_hover
    zoom <- input$zoom_direction
    factor <- if (zoom$direction == "in") 1.2 else 0.8
    
    # # Compute new ranges
    # x_mid <- mean(c(input$x_west, input$x_east))
    # y_mid <- mean(c(input$y_south, input$y_north))
    
    # Zoom toward mouse location
    x_focus <- mouse$x
    y_focus <- mouse$y
    
    # x_range <- diff(c(input$x_west, input$x_east)) * factor / 2
    # y_range <- diff(c(input$y_south, input$y_north)) * factor / 2
    
    update_window_limits(y_north = min(y_focus - (y_focus - input$y_north) * factor,
                                       uwm_bb_orig_y_max), 
                         x_west = max(x_focus - (x_focus - input$x_west) * factor, 
                                      uwm_bb_orig_x_min), 
                         x_east = min(x_focus - (x_focus - input$x_east) * factor,
                                      uwm_bb_orig_x_max), 
                         y_south = max(y_focus - (y_focus - input$y_south) * factor,
                                       uwm_bb_orig_y_min))
  })
  
  
  update_window_limits <- function(y_north, x_west, x_east, y_south) {
    updateNumericInput(session = session, inputId = "y_north", 
                       value = y_north)
    updateNumericInput(session = session, inputId = "x_west", 
                       value = x_west)
    updateNumericInput(session = session, inputId = "x_east", 
                       value = x_east)
    updateNumericInput(session = session, inputId = "y_south", 
                       value = y_south)
  }
  
  observeEvent(input$reset_window, {
    update_window_limits(y_north = uwm_bb_orig_y_max, 
                         x_west = uwm_bb_orig_x_min, 
                         x_east = uwm_bb_orig_x_max, 
                         y_south = uwm_bb_orig_y_min)
  })
  
  observeEvent(input$map_zoom_brush, {
    brush <- input$map_zoom_brush
    update_window_limits(y_north = brush$ymax, 
                         x_west = brush$xmin, 
                         x_east = brush$xmax, 
                         y_south = brush$ymin)
    session$resetBrush("map_zoom_brush")
  })
  
  observeEvent(input$map_dblclick, {
    update_window_limits(y_north = uwm_bb_orig_y_max, 
                         x_west = uwm_bb_orig_x_min, 
                         x_east = uwm_bb_orig_x_max, 
                         y_south = uwm_bb_orig_y_min)
  })
  
  output$n_postdocs.df <- renderDataTable({
    n_postdocs.df
  })
  
  output$n_postdocs.df.zoom <- renderDataTable({
    n_postdocs.df
  })
  
  output$download_ggplot <- downloadHandler(
    filename = function(){sprintf(input$file_name_plot)},
    content = function(file){
      ggsave(file, 
             plot = last_plot(), 
             width = 1.05*input$plot_height * uwm_bb.width/uwm_bb.height, # Need to update that 
             height = input$plot_height, 
             dpi = 300)
  })
  
  output$download_census_data <- downloadHandler(
    filename = function(){sprintf("%s.png", input$file_name_plot)},
    content = function(file){
      write.csv(n_postdocs.df, file)
  })
  
  output$text_1 <- renderUI({
    HTML(paste0(
      "Use side panel to set the title of the map, download the current view of the plot (enter the file name you want), adjust the height of the picture if the resoluaiton is too low<br>",
      "To adjust the view of the map, you can either manually enter valyes for the limits of the map in the sidebar, or ",
      "drag on an area of the map to zoom on it, or ",
      "zoom to Scroll (if zoom stops working, move mouse away from plot and back over it).", 
      "Note that you can't zoom out more than the default values."))
  })
}

# Create a Shiny app object

shinyApp(ui = ui, server = server)


