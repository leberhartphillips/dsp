# install.packages("devtools")
# devtools::install_github("ropensci/rnaturalearthhires")
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
library(patchwork)

# register_google(key = "")
con <- dbo::dbcon(db = "AVES_ranges")

#### pretty basemap ----
# set projection
lcc_params <- sf::st_crs("+proj=lcc +lat_1=-28.0 +lat_2=-20.0 +lat_0=-24.7859 +lon_0=-65.4117 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")

# extact the DSP range shapefile from the database
DSPL <- 
  dbo::dbq(con, 'SELECT scinam, season, SHAPE
          FROM AVES_ranges.ranges_v2
              WHERE scinam = "phegornis mitchellii" ',
      geom = "SHAPE")

TTDO <- 
  dbo::dbq(con, 'SELECT scinam, season, SHAPE
          FROM AVES_ranges.ranges_v2
              WHERE scinam = "oreopholus ruficollis" ',
           geom = "SHAPE")

# set the coordinate system
sf::st_crs(DSPL) <- 4326

# determine the bounding box
bbox <- sf::st_bbox(DSPL)

# extract the coastlines of the region of interest (i.e., a bounded region of South America)
south_america_crop <- 
  giscoR::gisco_get_coastallines() %>% 
  sf::st_crop(c(xmin = -82, ymin = -45, xmax = -50, ymax = -4)) %>% 
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
  rnaturalearth::ne_coastline(scale = "large", returnclass = "sf") %>% 
  sf::st_crop(c(xmin = -82, ymin = -45, xmax = -50, ymax = -4)) %>% 
  sf::st_transform(lcc_params)

# transform DSP range
DSPL_ <- 
  DSPL %>% 
  sf::st_transform(lcc_params)

# Define the latitude at which you want to cut the MULTIPOLYGON object
cut_latitude_north <- -18.92499
cut_latitude_south <- -27.85638

DSPL_n <- 
  st_crop(DSPL, xmin = -77.30139, xmax = -64.90839, 
          ymin = cut_latitude_north, ymax = -9.993591) %>% 
  sf::st_transform(lcc_params)

DSPL_c <- 
  st_crop(DSPL, xmin = -77.30139, xmax = -64.90839, 
          ymin = cut_latitude_north, ymax = cut_latitude_south) %>% 
  st_transform(lcc_params)

DSPL_s <- 
  st_crop(DSPL, xmin = -77.30139, xmax = -64.90839, 
          ymin = -36.78778, ymax = cut_latitude_south) %>% 
  st_transform(lcc_params)

# convert the raster into a data.frame of xyz
south_america_dem_proj_df <- as.data.frame(south_america_dem_proj, xy = TRUE)
names(south_america_dem_proj_df)[3] <- "alt"

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

# extract all e-bird obs on DSP up until July 2023
DSPL_ebird_ <- 
  fread("data/ebird/ebd_diaplo1_unv_smp_relJul-2023/ebd_diaplo1_unv_smp_relJul-2023.txt") %>% 
  rename(observation_date = `OBSERVATION DATE`,
         global_unique_identifier = `GLOBAL UNIQUE IDENTIFIER`, 
         observation_date = `OBSERVATION DATE`, 
         latitude = LATITUDE, 
         longitude = LONGITUDE) %>% 
  select(global_unique_identifier, observation_date, latitude, longitude) %>% 
  mutate(observation_date_j = as.numeric(format(observation_date, "%j"))) %>% 
  mutate(observation_date_c = as.Date(observation_date_j, origin = "2008-01-01")) %>% 
  mutate(region = ifelse(latitude < -27.85638, "south", 
                         ifelse(latitude > -18.92499, "north", "central"))) %>% 
  mutate(region = factor(region, levels = c("north", "central", "south")),
         month = month(observation_date_c))

# transform the ebird data
DSPL_ebird_t <- 
  DSPL_ebird_ %>% 
  st_as_sf(coords = c("longitude","latitude")) %>% 
  st_set_crs(4326) %>% 
  sf::st_transform(lcc_params) #%>% 
  # mutate(long = unlist(map(.$geometry,1)),
  #        lat = unlist(map(.$geometry,2))) %>% 
  # as.data.frame() %>% 
  # select(-geometry)

DSPL_ebird_t_ <- 
  DSPL_ebird_ %>% 
  st_as_sf(coords = c("longitude","latitude")) %>% 
  st_set_crs(4326) %>% 
  sf::st_transform(lcc_params) %>%
  mutate(long = unlist(map(.$geometry,1)),
         lat = unlist(map(.$geometry,2))) %>%
  as.data.frame() %>%
  select(-geometry)

# study sites
DSPL_sites <- 
  data.frame(site = c("el_yeso", "vilama"),
             long = c(-69.91477, -66.69594),
             lat = c(-33.62214, -22.50621)) %>% 
  st_as_sf(coords = c("long", "lat")) %>% 
  st_set_crs(4326) %>% 
  sf::st_transform(lcc_params) %>% 
  st_coordinates() %>% 
  st_linestring()

# map
DSPL_map <- 
  ggplot() +
  list(
    geom_raster(data = hillmultidf,
                aes(x, y, fill = sum),
                show.legend = FALSE,
                alpha = .5),
    scale_fill_distiller(palette = "Greys"),
    new_scale_fill(),
    geom_raster(data = south_america_dem_proj_df,
                aes(x, y, fill = alt),
                alpha = .5),
    scale_fill_hypso_tint_c(palette = "usgs-gswa2"
                            # breaks = c(180, 250, 500, 1000,
                            #            1500,  2000, 2500,
                            #            3000, 3500, 4000)
    )
  ) %>% blend("multiply") +
  geom_sf(data = south_america_rivers, color = "#698ecf", alpha = 0.5) +
  # geom_sf(data = DSPL, alpha = 0.5) +
  geom_sf(data = DSPL_n, fill = "#1b9e77", alpha = 0.3) +
  geom_sf(data = DSPL_c, fill = "#d95f02", alpha = 0.3) +
  geom_sf(data = DSPL_s, fill = "#7570b3", alpha = 0.3) +
  geom_sf(data = DSPL_ebird_t, color = "black", alpha = 0.1, pch = 16) + 
  geom_sf(data = DSPL_sites, size = 5, color = "white", pch = 0, fill = NA, stroke = 1.5) +
  # geom_path(data = BADO_curve_plot_df,
  #           aes(x = st_coordinates(geometry)[, 1], y = st_coordinates(geometry)[, 2], group = code),
  #           color = "grey40", alpha = 1,
  #           arrow = arrow(angle = 25, ends = "last", type = "closed", length = unit(0.1, "inches"))) +
  geom_sf(data = world_polygons, alpha = 0.5, color = "gray40") +
  guides(fill = guide_colorsteps(barwidth = 20,
                                 barheight = .5,
                                 title.position = "right")) +
  labs(fill = "m", x = "Longitude", y = "Latitude") +
  coord_sf(xlim = c(bbox["xmin"] - 1750000, bbox["xmax"] + 1050000),
           ylim = c(bbox["ymin"] - 1400000, bbox["ymax"] + 1900000),
           crs = lcc_params) +
  luke_theme +
  theme(legend.position = "none",
        panel.background = element_rect(fill = 'transparent'),
        plot.background = element_rect(fill = 'transparent', color = NA))

DSPL_ebird_hist <- 
  ggplot() +
  geom_histogram(data = DSPL_ebird_, aes(observation_date_c, fill = region), binwidth = 7) +
  # geom_density(data = y_, aes(observation_date_c), adjust = 1/2) +
  scale_x_date(date_labels = "%B", expand = c(0.01, 0.01), date_breaks = "1 months") +
  facet_grid(region ~ .) +
  ylab("number of ebird checklists detecting Phegornis\n(prior to 1 July 2023)") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  scale_fill_manual(values = c("#1b9e77", "#d95f02", "#7570b3")) +
  luke_theme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        axis.title.x = element_blank(),
        legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        strip.background = element_blank(),
        strip.text = element_text(size = 12))

DSPL_map_hist <- 
  DSPL_map | DSPL_ebird_hist +
  theme(plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent"))
DSPL_map_hist

# save map to disk
ggsave("figs/dsp_ebird_map_hist.png", DSPL_map_hist, 
       width = 12, 
       height = 8, 
       unit = "in",
       device = png, 
       # type = "cairo",
       bg = "transparent")

DSPL <- dbq(con, 'SELECT scinam, SHAPE
                FROM AVES_ranges.ranges_v2
                    WHERE scinam = "phegornis mitchellii" ',
            geom = "SHAPE"
)

max(y_$observation_date)

cut(x, quantile(x, c(0, 0.33, 0.66, 1)), include.lowest=TRUE)



ggplot() +
  geom_sf(data = south_america_rivers, color = "#698ecf", alpha = 0.5) +
  # geom_sf(data = DSPL, alpha = 0.5) +
  geom_sf(data = DSPL_n, fill = "#1b9e77", alpha = 0.3) +
  geom_sf(data = DSPL_c, fill = "#d95f02", alpha = 0.3) +
  geom_sf(data = DSPL_s, fill = "#7570b3", alpha = 0.3) +
  geom_sf(data = DSPL_ebird_t, fill = "black", alpha = 0.1) +
  geom_sf(data = world_polygons, alpha = 0.5, color = "gray40") +
  guides(fill = guide_colorsteps(barwidth = 20,
                                 barheight = .5,
                                 title.position = "right")) +
  labs(fill = "m", x = "Longitude", y = "Latitude") +
  coord_sf(xlim = c(bbox["xmin"] - 1750000, bbox["xmax"] + 1050000),
           ylim = c(bbox["ymin"] - 1900000, bbox["ymax"] + 1900000),
           crs = lcc_params) +
  luke_theme +
  theme(legend.position = "none")

map_sf <- get_map(c(left = -83, bottom = -57, right = -47, top = 0), 
                  zoom = 4, 
                  maptype = 'satellite')
ggmap(map_sf) +
  # geom_sf(data = DSPL) +
  stat_density2d(data = DSPL_ebird_, aes(x = longitude, y = latitude, fill = ..density..), geom = 'tile', contour = F, alpha = 0.5) +
  scale_fill_viridis(option = 'inferno') +
  labs(fill = str_c('Number of', '\ne-bird observations')) +
  theme(text = element_text(color = "#444444"),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank()) +
  guides(fill = guide_legend(override.aes= list(alpha = 1)))

#### junkyard ----
## extract e-bird data from Mihai's database
# con <- dbConnect(duckdb::duckdb())
# 
# x = dbGetQuery(
#   con,
#   "SELECT * FROM read_csv_auto('/ds/raw_data_kemp/COMPARATIVE/EBIRD/ebd_relDec-2020.txt', normalize_names=True)
#     WHERE scientific_name = 'Phegornis mitchellii';"
# )
# dbDisconnect(con)
# saveRDS(x, file = "DSP/data/dsp_ebird_upto_2020.rds")
