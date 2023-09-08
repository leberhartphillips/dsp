# install.packages("remotes")
# remotes::install_github("mpio-be/dbo")
# install.packages("ggplot2")
# install.packages("mapview")
# install.packages("duckdb")
# install.packages("giscoR")
# install.packages("rnaturalearthdata")
# install.packages("elevatr")
# install.packages("terra")
# install.packages("tidyterra")
require(terra)
require(tidyterra)
require(viridis)
require(dbo)
require(ggplot2)
require(mapview)
require(sf)
require(duckdb)
require(tidyverse)
require(ggmap)
require(rnaturalearth)
require(giscoR)
require(elevatr)
library(ggnewscale)
library(ggblend)

# register_google(key = "")
con <- dbo::dbcon(db = "AVES_ranges")

TTDO <- dbq(con, 'SELECT scinam, SHAPE
                FROM AVES_ranges.ranges_v2
                    WHERE scinam = "oreopholus ruficollis" ',
            geom = "SHAPE"
)

bbox <- sf::st_bbox(DSPL)

#### pretty basemap ----
# set projection
lcc_params <- sf::st_crs("+proj=lcc +lat_1=-28.0 +lat_2=-20.0 +lat_0=-24.7859 +lon_0=-65.4117 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")

DSPL <- 
  dbo::dbq(con, 'SELECT scinam, SHAPE
          FROM AVES_ranges.ranges_v2
              WHERE scinam = "phegornis mitchellii" ',
      geom = "SHAPE")
sf::st_crs(DSPL) <- 4326

# extract the region of interest (i.e., a bounded region of South America)
south_america_crop <- 
  giscoR::gisco_get_coastallines() %>% 
  sf::st_crop(c(xmin = -82, ymin = -45, xmax = -50, ymax = -5)) %>% 
  sf::st_transform(lcc_params)

# extract the DEM of the region of interest and project
south_america_dem <- elevatr::get_elev_raster(locations = south_america_crop, z = 6, clip = "locations")
# writeRaster(south_america_dem, "data/spatial/DSP_DEM_zoom_6.tif", filetype = "GTiff", overwrite = TRUE)
# south_america_dem <- rast("data/spatial/DSP_DEM_zoom_6.tif")
south_america_dem_ras <- rast(south_america_dem)
south_america_dem_proj <- terra::project(south_america_dem_ras, crs(south_america_crop))
# south_america_dem_proj <- projectRaster(south_america_dem, crs = crs(south_america_crop))

# read the water bodies shapefile, project, and filter based on type (rivers and lakes)
south_america_rivers <-
  sf::st_read("data/spatial/South_America_Hydrography/South_America_Hydrography.shp") %>%
  sf::st_transform(lcc_params)
  # filter(type %in% c("river", "lake"))

# Load the world polygons dataset
world_polygons <-
  rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") %>% 
  sf::st_transform(lcc_params)

# transform DSP range
DSPL <- 
  DSPL %>% 
  sf::st_transform(lcc_params)

ggplot() +
  geom_sf(data = world_polygons, alpha = 0.5, color = "gray40") +
  # geom_sf(data = graticules, color = "gray90") +
  # geom_sf(data = south_america_water, color = "#698ecf") +
  geom_sf(data = DSPL, alpha = 0.5) +
  # guides(fill = guide_colorsteps(barwidth = 20,
  #                                barheight = .5,
  #                                title.position = "right")) +
  labs(fill = "m", x = "Longitude", y = "Latitude") +
  coord_sf(xlim = c(bbox["xmin"] - 1750000, bbox["xmax"] + 1050000),
           ylim = c(bbox["ymin"] - 1900000, bbox["ymax"] + 2000000),
           crs = lcc_params) +
  luke_theme +
  theme(legend.position = "none")

# convert the raster into a data.frame of xyz
mdtdf <- as.data.frame(south_america_dem_proj, xy = TRUE)
names(mdtdf)[3] <- "alt"

# create a hillshade of the region of interest
# estimate the slope
sl <- terrain(south_america_dem_proj, "slope", unit = "radians")

# estimate the aspect or orientation
asp <- terrain(south_america_dem_proj, "aspect", unit = "radians")

# pass multiple directions to shade()
hillmulti <- purrr::map(c(270, 15, 60, 330), function(dir){ 
  terra::shade(sl, asp, 
               angle = 45, 
               direction = dir,
               normalize = TRUE)}
)

# create a multidimensional raster and reduce it by summing up
hillmulti <- terra::rast(hillmulti) %>% sum()

# convert the hillshade to xyz
hillmultidf <- as.data.frame(hillmulti, xy = TRUE)

# map
m <- ggplot() +
  list(
    geom_raster(data = hillmultidf,
                aes(x, y, fill = sum),
                show.legend = FALSE,
                alpha = .5),
    scale_fill_distiller(palette = "Greys"),
    new_scale_fill(),
    geom_raster(data = mdtdf,
                aes(x, y, fill = alt),
                alpha = .5),
    scale_fill_hypso_tint_c(palette = "usgs-gswa2"
                            # breaks = c(180, 250, 500, 1000,
                            #            1500,  2000, 2500,
                            #            3000, 3500, 4000)
    )
  ) %>% blend("multiply") +
  geom_sf(data = south_america_rivers, color = "#698ecf") +
  geom_sf(data = DSPL, alpha = 0.5) +
  # geom_path(data = BADO_curve_plot_df,
  #           aes(x = st_coordinates(geometry)[, 1], y = st_coordinates(geometry)[, 2], group = code),
  #           color = "grey40", alpha = 1,
  #           arrow = arrow(angle = 25, ends = "last", type = "closed", length = unit(0.1, "inches"))) +
  geom_sf(data = south_america_crop, alpha = 0, color = "gray40") +
  guides(fill = guide_colorsteps(barwidth = 20,
                                 barheight = .5,
                                 title.position = "right")) +
  labs(fill = "m", x = "Longitude", y = "Latitude") +
  coord_sf(xlim = c(bbox["xmin"] - 1750000, bbox["xmax"] + 1050000),
           ylim = c(bbox["ymin"] - 1900000, bbox["ymax"] + 1900000),
           crs = lcc_params) +
  luke_theme +
  theme(legend.position = "none")

m

ggsave("figs/dsp_hillshade_test.png", m, 
       width = 10, 
       height = 8, 
       unit = "in",
       device = png, 
       type = "cairo",
       bg = "white")

# Define the latitude at which you want to cut the MULTIPOLYGON object
cut_latitude_north <- -18.92499
cut_latitude_south <- -27.85638

DSPL_n <- 
  st_crop(DSPL, xmin = -77.30139, xmax = -64.90839, 
          ymin = cut_latitude_north, ymax = -9.993591) %>% 
  st_transform(lcc_params)
DSPL_c <- 
  st_crop(DSPL, xmin = -77.30139, xmax = -64.90839, 
          ymin = cut_latitude_north, ymax = cut_latitude_south) %>% 
  st_transform(lcc_params)
DSPL_s <- 
  st_crop(DSPL, xmin = -77.30139, xmax = -64.90839, 
          ymin = -36.78778, ymax = cut_latitude_south) %>% 
  st_transform(lcc_params)


# Create a horizontal line representing the cut latitude
cut_line_north <- st_linestring(matrix(c(-77.30139, -64.90839, cut_latitude_north, cut_latitude_north), ncol = 2))
cut_line_south <- st_linestring(matrix(c(-77.30139, -64.90839, cut_latitude_south, cut_latitude_south), ncol = 2))

cut_line_north <- st_sfc(cut_line_north, crs = 4326)
cut_line_south <- st_sfc(cut_line_south, crs = 4326)

crs_parameters <- list(
  proj = "lcc",
  lat_0 = -24.7859,    # Central latitude of Salta, Argentina
  lon_0 = -65.4117,    # Central longitude of Salta, Argentina
  lat_1 = -26.0,       # First standard parallel
  lat_2 = -23.0,       # Second standard parallel
  ellps = "GRS80"      # Ellipsoid (You can adjust this if needed)
)

# Create the LCC CRS
lcc_crs <- st_crs(crs_parameters)

# Display the CRS
lcc_crs

lcc_params <- st_crs("+proj=lcc +lat_1=-26.0 +lat_2=-23.0 +lat_0=-24.7859 +lon_0=-65.4117 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")

# Load the graticules dataset
graticules <- 
  st_graticule(ndiscr = 100) %>% 
  st_transform(lcc_params) %>%
  st_geometry

# Load the world polygons dataset
world_polygons <-
  ne_countries(scale = "medium", returnclass = "sf") %>% 
  st_transform(lcc_params)

bbox <- st_bbox(DSPL)

ggplot() +
  geom_sf(data = graticules, color = "gray90") +
  geom_sf(data = world_polygons, 
          fill = "gray90", color = "gray40") +
  geom_sf(data = DSPL_n, fill = "yellow") +
  geom_sf(data = DSPL_c, fill = "blue") +
  geom_sf(data = DSPL_s, fill = "red") +
  coord_sf(xlim = c(bbox["xmin"] - 1800000, bbox["xmax"] + 1500000),
           ylim = c(bbox["ymin"] - 3600000, bbox["ymax"] + 2000000),
           crs = lcc_params) +
  labs(x = "Longitude", y = "Latitude")
  # geom_sf(data = BADO_curve_plot_df,
  #         aes(color = code), alpha = 0.5)

ggplot() +
  # geom_sf(data = cut_line_north) +
  # geom_sf(data = cut_line_south) +
  
  geom_sf(data = DSPL_n, fill = "yellow") +
  geom_sf(data = DSPL_c, fill = "yellow") +
  geom_sf(data = DSPL_s, fill = "yellow")# +
  
  # geom_sf(data = result)

# Cut the MULTIPOLYGON object at the specified latitude
result <- st_intersection(DSPL, cut_line_north)

# Create a horizontal line representing the cut latitude
cut_line <- st_linestring(matrix(c(-77.30139, cut_latitude,-64.90839, cut_latitude), ncol = 2))

# Cut the MULTIPOLYGON object at the specified latitude
result <- st_intersection(multipolygon, cut_line)

DSPL_ <- sf::as_Spatial(st_geometry(DSPL), IDs = as.character(1:nrow(DSPL)))
spd <- sp::SpatialPolygonsDataFrame(DSPL_)

(-36.78778 - -9.993591)/3

-36.78778+8.931396
-9.993591+-8.931396


df <- DSPL_
df$geometry <- NULL

df <- as.data.frame(df)
  as.data.frame()
  sp::SpatialPolygonsDataFrame(spd, data = df)

df <- nc
df$geometry <- NULL
df <- as.data.frame(df)

closeCon(con)

ggplot() +
  geom_sf(data = DSPL) #+
  geom_sf(data = TTDO)
  
as.data.frame(DSPL)

mapview(DSPL)

#### extract all e-bird obs on DSP up until 2020 ----
# con <- dbConnect(duckdb::duckdb())
# 
# x = dbGetQuery(
#   con,
#   "SELECT * FROM read_csv_auto('/ds/raw_data_kemp/COMPARATIVE/EBIRD/ebd_relDec-2020.txt', normalize_names=True)
#     WHERE scientific_name = 'Phegornis mitchellii';"
# )
# dbDisconnect(con)
# saveRDS(x, file = "DSP/data/dsp_ebird_upto_2020.rds")
y = readRDS(file = "DSP/data/dsp_ebird_upto_2020.rds")
y_ <- 
  y %>% 
  select(global_unique_identifier, observation_date, latitude, longitude) %>% 
  mutate(observation_date_j = as.numeric(format(observation_date, "%j"))) %>% 
  mutate(observation_date_c = as.Date(observation_date_j, origin = "2008-01-01")) %>% 
  mutate(region = ifelse(latitude < -27.85638, "south", 
                         ifelse(latitude > -18.92499, "north", "central"))) %>% 
  mutate(region = factor(region, levels = c("north", "central", "south")))

DSPL <- dbq(con, 'SELECT scinam, SHAPE
                FROM AVES_ranges.ranges_v2
                    WHERE scinam = "phegornis mitchellii" ',
            geom = "SHAPE"
)

max(y_$observation_date)

cut(x, quantile(x, c(0, 0.33, 0.66, 1)), include.lowest=TRUE)

ggplot() +
  geom_histogram(data = y_, aes(observation_date_c, fill = region), binwidth = 7) +
  # geom_density(data = y_, aes(observation_date_c), adjust = 1/2) +
  scale_x_date(date_labels = "%B", expand = c(0.01, 0.01), date_breaks = "1 months") +
  facet_grid(region ~ .) +
  ylab("number of ebird checklists detecting Phegornis (prior to 1 January 2021)") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

ggplot() +
  stat_density2d(data = y_, aes(x = longitude, y = latitude, fill = ..density..), geom = 'tile', contour = F)

map_sf <- get_map(c(left = -83, bottom = -57, right = -47, top = 0), 
                  zoom = 4, 
                  maptype = 'satellite')
ggmap(map_sf) +
  # geom_sf(data = DSPL) +
  stat_density2d(data = y_, aes(x = longitude, y = latitude, fill = ..density..), geom = 'tile', contour = F, alpha = 0.5) +
  scale_fill_viridis(option = 'inferno') +
  labs(fill = str_c('Number of', '\ne-bird observations')) +
  theme(text = element_text(color = "#444444"),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank()) +
  guides(fill = guide_legend(override.aes= list(alpha = 1)))
