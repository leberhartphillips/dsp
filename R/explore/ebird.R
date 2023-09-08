# install.packages("remotes")
# remotes::install_github("mpio-be/dbo")
# install.packages("ggplot2")
# install.packages("mapview")
# install.packages("duckdb")
install.packages("rnaturalearthdata")
require(viridis)
require(dbo)
require(ggplot2)
require(mapview)
require(sf)
require(duckdb)
require(tidyverse)
require(ggmap)
require(rnaturalearth)
# register_google(key = "")
con <- dbcon(db = "AVES_ranges")

DSPL <- dbq(con, 'SELECT scinam, SHAPE
                FROM AVES_ranges.ranges_v2
                    WHERE scinam = "phegornis mitchellii" ',
            geom = "SHAPE"
)

TTDO <- dbq(con, 'SELECT scinam, SHAPE
                FROM AVES_ranges.ranges_v2
                    WHERE scinam = "oreopholus ruficollis" ',
            geom = "SHAPE"
)

st_crs(DSPL) <- 4326

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
