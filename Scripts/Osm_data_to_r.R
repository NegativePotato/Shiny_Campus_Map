library(osmdata)
library(ggplot2)
library(dplyr)
library(here)
library(RColorBrewer)
library(scales)
# library(sp)

# By locaion
university <- "UW-Madison"
uwm_bb <- getbb(university)
uwm_bb.width <- abs(uwm_bb["x", "max"] - uwm_bb["x", "min"])
uwm_bb.height <- abs(uwm_bb["y", "max"] - uwm_bb["y", "min"])
# Bounding Box
minlat <- 43.0664600
minlon <- -89.4271200 
maxlat <- 43.0802700 
maxlon <- -89.3950600

colors_osm <- 
  list(water = "#B3D2DE",
       vegetation = "#79A45C",
       parking = "#AAAAAA",
       main_road = "#F8C394", 
       other_roads = "#000000",
       cycle_path = "#296218",
       rail_road = "#333333") # remeber to dash style it

# osm_data_file_path <- file.path(here(), "Data", "Map_Data", 'uw_campus.osm')
# if(!file.exists(osm_data_file_path)) {
#   all_data.osm <- uwm_bb %>%
#     opq() %>%
#     osmdata_xml(file = osm_data_file_path)
# }


# Filter by "operator" -> "UW-Madison"
# selects the "ways" 
# lake_mendota.q <- opq(bbox = uwm_bb) %>%
#   add_osm_feature(key = "name", 
#                   value = "Lake Mendota")
# lake_mendota.sf <- osmdata_sf(q = lake_mendota.q, 
#                               doc = osm_data_file_path)


# lakeshore_nature_preserve_for_plot.sf <- uwm_bb %>%
#   opq() %>%
#   add_osm_feature(key = "name", 
#                   value = "Lakeshore Nature Preserve") %>%
#   osmdata_sf() %>% 
#   .$osm_polygons


# MAKE A FUNCTION FOR THIS !!! 

# Code to automaticaly load OSM data from OSM database
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

# DO THIS
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

lake_mendota_for_plot.sf <- 
  dplyr::bind_rows(
    lake_mendota.sf$osm_polygons %>% 
      select(name, geometry),
    lake_mendota.sf$osm_multipolygons %>% 
      select(name, geometry)
  ) 

uw_buildings_for_plot.sf <- 
  dplyr::bind_rows(
    uw_buildings.sf$osm_polygons %>% 
      select(name, geometry, amenity, building),
    uw_buildings.sf$osm_multipolygons %>% 
      select(name, geometry, amenity, building)
  ) %>% 
  filter(!(name %in% c("Lakeshore Nature Preserve"))) %>% 
  mutate(amenity = ifelse(is.na(amenity), "none", amenity)) %>% 
  filter(amenity != "parking" & !is.na(building))

# Load post-doc data
print("Get Number of Post-Docs")
n_postdocs.df <- read.csv(file.path(here(), 'Data', 'Post-Doc_Counts', 'Buildings_AllPostdocFeb10_2025.csv')) %>% 
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
          mapping = aes(fill = n_postdocs, 
                        geometry = geometry),
          inherit.aes = FALSE,
          color = "black",
          size = 0.2) + 
  geom_sf_text(data = uw_buildings_for_plot.sf,
               aes(label = as.character(n_postdocs), 
                   geometry = geometry), 
               size = 2, 
               check_overlap = TRUE)

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
map_to_save.plot <- uwmadison_with_lake.plot + 
  coord_sf(xlim = c(-89.437, -89.393),
           ylim = c(43.082, 43.067),
           expand = FALSE) +
  theme_void() +
  theme(plot.background = element_rect(fill = "#FFFFFF")) +
  scale_fill_distiller(palette = "RdYlBu", direction = -1)

plot.height <- 7 
ggsave(filename = file.path(here(), "Post_Doc_Campus_Map_Simple_2025.png"), 
      plot = map_to_save.plot, width = 1.05*plot.height * uwm_bb.width/uwm_bb.height, 
      height = plot.height, units = "in")







# uwmadison_with_lake_and_preserve.plot <- uwmadison_with_lake.plot +
#   geom_sf(data = lakeshore_nature_preserve_for_plot.sf,
#           inherit.aes = FALSE,
#           fill = colors_osm$vegetation, 
#           size = 0.2)
# 
# uwmadison_with_lake_and_preserve.plot + 
#   coord_sf(xlim = c(-89.437, -89.393),
#            ylim = c(43.082, 43.067),
#            expand = FALSE) +
#   theme_void()



















if (FALSE) {
  # Save the building names of the dataset (to be matched to the building names 
  # in the dataset from the UW Post-Doctoral Offices)
  building_names.df <- 
    bind_rows(
      uw_buildings.sf$osm_polygons,
      uw_buildings.sf$osm_multipolygons
    ) %>% 
    as.data.frame() %>%
    select(osm_id, name, starts_with("addr"), building) 
  write.csv(x = building_names.df,
            file = file.path(here(), "Data", "Map_Data", "building_names.csv"))
  
  
  building_data.df <- 
    uw_buildings_for_plot.sf %>% 
    as.data.frame() %>% 
    select(!geometry)
  write.csv(x = building_data.df,
            file = file.path(here(), "Data", "Map_Data", "building_data.csv"))
}



