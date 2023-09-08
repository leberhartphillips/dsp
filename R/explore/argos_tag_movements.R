#### libraries ----
# remotes::install_github("mpio-be/dbo")
library(dbo)
library(mapview)
library(tidyverse)
library(sf)
library(googlesheets4)
library(lubridate)
library(stringr)
library(rnaturalearth)
library(geosphere)
library(RColorBrewer)
library(elevatr)
library(giscoR)
library(tidyterra)
library(ggnewscale)
library(ggblend)

# # Define the NZTM projection for New Zealand
# lcc_params <- st_crs("+proj=lcc +lat_1=-36 +lat_2=-36 +lat_0=-36 +lon_0=162 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")
# nz_bound <- gisco_get_countries(country = "New Zealand", resolution = "03")
# au_bound <- gisco_get_countries(country = "Australia", resolution = "03")
# nz_au_bound <- st_union(nz_bound, au_bound)
# nz_au_bound_crop <- 
#   st_crop(nz_au_bound, c(xmin = 140, ymin = -46, xmax = 180, ymax = -30)) %>% 
#   st_transform(lcc_params)
# # import the lakes boundaries
# nz_lakes <- 
#   st_read("data/spatial/NZ_map/lds-nz-lake-polygons-topo-150k-SHP/nz-lake-polygons-topo-150k.shp") %>% 
#   st_transform(lcc_params)
#   
# nz_rivers <- 
#   st_read("data/spatial/NZ_map/lds-nz-river-polygons-topo-150k-SHP/nz-river-polygons-topo-150k.shp") %>% 
#   st_transform(lcc_params)
# 
# mapview(nz_au_bound_crop)
# 
# nz_au_dem <- elevatr::get_elev_raster(locations = nz_au_bound_crop, z = 5, clip = "locations")
# 
# nz_au_dem_proj <- terra::project(mdt, crs(nz_au_bound_crop))
# 
# elevation_data <- 
#   elevatr::get_elev_raster(locations = nz_au_bound_crop, z = 5, clip = "locations")
# elevation_data <- projectRaster(elevation_data, crs = crs(nz_au_bound_crop))
# elevation_data <- as.data.frame(nz_au_dem_proj, xy = TRUE)
# colnames(elevation_data)[3] <- "elevation"
# # remove rows of data frame with one or more NA's,using complete.cases
# elevation_data <- elevation_data[complete.cases(elevation_data), ]
# 
# ggplot() +
#   geom_raster(data = elevation_data, aes(x = x, y = y, fill = elevation)) +
#   geom_sf(data = nz_au_bound_crop, color = "white", fill = NA) +
#   geom_sf(data = graticules, color = "gray90") +
#   # geom_sf(data = world_polygons, 
#   #         fill = "gray90", color = "gray40") +
#   geom_sf(data = BADO_curve_plot_df,
#           aes(color = code), alpha = 0.5) +
#   # geom_sf(data = nz_lakes, color = "blue", fill = NA) +
#   # geom_sf(data = nz_rivers, color = "blue", fill = NA) +
#   # coord_sf() +
#   scale_fill_viridis_c() +
#   geom_path(data = BADO_curve_plot_df,
#             aes(x = st_coordinates(geometry)[, 1], y = st_coordinates(geometry)[, 2], group = code),
#             color = "grey40", alpha = 0.5) +
#   geom_sf(data = world_polygons, alpha = 0, color = "gray40") +
#   coord_sf(xlim = c(bbox["xmin"] - 1750000, bbox["xmax"] + 1500000),
#            ylim = c(bbox["ymin"] - 1250000, bbox["ymax"] + 200000),
#            crs = lcc_params) +
#   labs(x = "Longitude", y = "Latitude") +
#   luke_theme +
#   theme(legend.position = "none")

# define projection
lcc_params <- st_crs("+proj=lcc +lat_1=-36 +lat_2=-36 +lat_0=-36 +lon_0=162 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")

# get political boundaries of NZ and AU, bind together, crop to region of interest, and project
nz_bound <- gisco_get_countries(country = "New Zealand", resolution = "03")
au_bound <- gisco_get_countries(country = "Australia", resolution = "03")
nz_au_bound <- st_union(nz_bound, au_bound)
nz_au_bound_crop <- 
  st_crop(nz_au_bound, c(xmin = 140, ymin = -46, xmax = 180, ymax = -30)) %>% 
  st_transform(lcc_params)

au_water <-
  st_read("data/spatial/NZ_map/AUS_wat/AUS_water_areas_dcw.shp") %>%
  # st_transform(lcc_params) %>% 
  filter(HYC_DESCRI == "Perennial/Permanent")

nz_water <-
  st_read("data/spatial/NZ_map/NZL_wat/NZL_water_areas_dcw.shp") %>%
  # st_transform(lcc_params) %>% 
  filter(HYC_DESCRI == "Perennial/Permanent")

nz_au_water <- 
  rbind(nz_water, au_water) %>% 
  st_transform(lcc_params)

# nz_au_water_crop <- 
#   st_crop(nz_au_water, c(xmin = 140, ymin = -46, xmax = 180, ymax = -30)) %>% 
#   st_transform(lcc_params)

# as.data.frame(nz_rivers)
# 
# mapview(nz_au_water)

# extract the DEM of the region of interest and project
nz_au_dem <- elevatr::get_elev_raster(locations = nz_au_bound_crop, z = 6, clip = "locations")
writeRaster(nz_au_dem, "data/spatial/NZ_map/DEM_zoom_6.tif")
nz_au_dem_proj <- terra::project(mdt, crs(nz_au_bound_crop))
mdt <- nz_au_dem_proj

# convert the raster into a data.frame of xyz
mdtdf <- as.data.frame(mdt, xy = TRUE)
names(mdtdf)[3] <- "alt"

# create a hillshade of the region of interest
# estimate the slope
sl <- terrain(mdt, "slope", unit = "radians")

# estimate the aspect or orientation
asp <- terrain(mdt, "aspect", unit = "radians")

# pass multiple directions to shade()
hillmulti <- map(c(270, 15, 60, 330), function(dir){ 
  terra::shade(sl, asp, 
               angle = 45, 
               direction = dir,
               normalize= TRUE)}
)

# create a multidimensional raster and reduce it by summing up
hillmulti <- terra::rast(hillmulti) %>% sum()

# convert the hillshade to xyz
hillmultidf <- as.data.frame(hillmulti, xy = TRUE)

# map
m <- ggplot() +
  geom_sf(data = graticules, color = "gray90") +
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
  geom_sf(data = nz_au_water, color = NA, fill = "#698ecf") +
  # geom_sf(data = BADO_curve_plot_df,
  #         aes(color = code), alpha = 0.5) +
  geom_path(data = BADO_curve_plot_df,
            aes(x = st_coordinates(geometry)[, 1], y = st_coordinates(geometry)[, 2], group = code),
            color = "grey40", alpha = 1,
            arrow = arrow(angle = 25, ends = "last", type = "closed", length = unit(0.1, "inches"))) +
  # geom_sf(data = world_polygons, alpha = 0, color = "gray40") +
  guides(fill = guide_colorsteps(barwidth = 20,
                                 barheight = .5,
                                 title.position = "right")) +
  labs(fill = "m", x = "Longitude", y = "Latitude") +
  coord_sf(xlim = c(bbox["xmin"] - 1750000, bbox["xmax"] + 1500000),
           ylim = c(bbox["ymin"] - 1250000, bbox["ymax"] + 200000),
           crs = lcc_params) +
  luke_theme +
  theme(legend.position = "none")

ggsave("tabs_figs/for_katie/mdt_hillshade_blend.png", m, 
       width = 10, 
       height = 8, 
       unit = "in",
       device = png, 
       type = "cairo",
       bg = "white")

#### Plotting misc ----
luke_theme <- 
  theme_bw() +
  theme(
    text = element_text(family = "Franklin Gothic Book"),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 12),
    axis.title.x = element_text(size = 12),
    axis.text.x  = element_text(size = 10), 
    axis.title.y = element_text(size = 12),
    axis.text.y = element_text(size = 10),
    strip.text = element_text(size = 12),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks = element_line(linewidth = 0.5, colour = "grey40"),
    panel.border = element_rect(linetype = "solid", colour = "grey"),
    legend.position = "right"
  )

#### basemap data ----
# Define the NZTM projection for New Zealand
lcc_params <- st_crs("+proj=lcc +lat_1=-36 +lat_2=-36 +lat_0=-36 +lon_0=162 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")

# Load the graticules dataset
graticules <- 
  st_graticule(ndiscr = 100) %>% 
  st_transform(lcc_params) %>%
  st_geometry

# Load the world polygons dataset
world_polygons <-
  ne_countries(scale = "medium", returnclass = "sf") %>% 
  st_transform(lcc_params)

#### connect to databases ----
# scidb
con <- dbo::dbcon(server = "scidb_replica")

# BADO google sheets
BADO_caps_raw <- 
  read_sheet("https://docs.google.com/spreadsheets/d/1Tp26Z23HSXXZSoGXD4dbP3xukhrY1kWhzQrbtRHt4EY/edit#gid=1382609695", 
             sheet = "Captures", col_types = "c") 

BADO_resights_raw <- 
  read_sheet("https://docs.google.com/spreadsheets/d/1Tp26Z23HSXXZSoGXD4dbP3xukhrY1kWhzQrbtRHt4EY/edit#gid=1382609695", 
             sheet = "Resights", col_types = "c")

#### custom functions ----
sfc_as_cols <- function(x, geometry, names = c("x","y")) {
  if (missing(geometry)) {
    geometry <- sf::st_geometry(x)
  } else {
    geometry <- rlang::eval_tidy(enquo(geometry), x)
  }
  stopifnot(inherits(x,"sf") && inherits(geometry,"sfc_POINT"))
  ret <- sf::st_coordinates(geometry)
  ret <- tibble::as_tibble(ret)
  stopifnot(length(names) == ncol(ret))
  x <- x[ , !names(x) %in% names]
  ret <- setNames(ret,names)
  dplyr::bind_cols(x,ret)
}

# function that does the opposite of "%in%"
`%!in%` = Negate(`%in%`)

#### wrangle Argos and banding data

# classify PTT color combos
PTTs <- 
  c("XX.RB|MX.RL", "XX.RR|MX.RG", "XX.RR|MX.GO", "XX.RR|MX.OW", "XX.RB|MX.WO",
    "XX.RB|MX.WR", "XX.RB|MX.OG", "XX.RB|MX.GB", "XX.RW|MX.LG", "XX.RB|MX.OY",
    "MX.YY|XX.BO", "MX.YY|XX.BY", "MX.YY|XX.BW", "MX.YY|XX.BR", "MX.YW|XX.GW", 
    "MX.YW|XX.BY", "MX.YW|XX.RG")

# wrangle coordinate system of cap and resight data
BADO_resights <-
  BADO_resights_raw %>% 
  filter(utm != "WGS87") %>% 
  filter(!is.na(easting)) %>% 
  st_as_sf(., coords = c("easting", "northing"), crs = 2193) %>% 
  st_transform(., crs = 4326) %>% 
  sfc_as_cols(., names = c("longitude", "latitude")) %>% 
  st_drop_geometry() %>%
  bind_rows(BADO_resights_raw %>% filter(utm == "WGS87") %>% 
              rename(latitude = northing, longitude = easting) %>% 
              mutate(longitude = as.numeric(longitude),
                     latitude = as.numeric(latitude))) %>% 
  filter(sex != "J") %>% 
  filter(site %!in% c("KT", "KK")) %>%
  mutate(date = paste(year, 
                      ifelse(nchar(date) == 3, 
                             str_sub(date, start = 2, end = 3), 
                             str_sub(date, start = 3, end = 4)), 
                      ifelse(nchar(date) == 3, 
                             str_sub(date, start = 1, end = 1), 
                             str_sub(date, start = 1, end = 2)), 
                      sep = "-") %>% as.Date(., format = "%Y-%m-%d")) %>% 
  left_join(., BADO_caps_raw %>% filter(sex != "J") %>% filter(!is.na(easting)) %>% select(code, tag),
            by = "code") %>%
  mutate(source = "resight") %>% 
  st_as_sf(., 
           coords = c("longitude", "latitude"),
           crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0") %>% 
  mutate(status = ifelse(code %in% PTTs, "tag", "no-tag")) %>%
  mutate(tag = ifelse(tag == "66891" & year == "2021", "66891_1",
                      ifelse(tag == "66891" & year == "2022", "66891_2", tag))) %>% 
  rename(tagID = tag) %>% 
  select(source, code, sex, date, tagID, status)

BADO_caps <-
  BADO_caps_raw %>% 
  filter(utm != "WGS87") %>% 
  mutate(easting = as.numeric(easting),
         northing = as.numeric(northing)) %>% 
  filter(!is.na(easting)) %>% 
  st_as_sf(., coords = c("easting", "northing"), crs = 2193) %>% 
  st_transform(., crs = 4326) %>% 
  sfc_as_cols(., names = c("longitude", "latitude")) %>% 
  st_drop_geometry() %>% 
  bind_rows(., BADO_caps_raw %>% filter(utm == "WGS87") %>% 
              rename(longitude = easting, latitude = northing) %>% 
              mutate(longitude = as.numeric(longitude), latitude = as.numeric(latitude))) %>% 
  group_by(ring) %>% 
  slice(1) %>% 
  ungroup() %>% 
  mutate(tag = ifelse(code == "XX.RR|MX.RG" & year == "2021", "66891_1",
                      ifelse(tag == "66891" & year == "2022", "66891_2", 
                             ifelse(code == "XX.RW|MX.LG", "234673", tag))),
         date = ifelse(code == "XX.RW|MX.LG", "2610",
                       ifelse(code == "XX.RR|MX.RG", "1410", date)),
         year = ifelse(code == "XX.RW|MX.LG", "2022",
                       ifelse(code == "XX.RR|MX.RG", "2021", year))) %>% 
  mutate(date = paste(year, 
                      ifelse(nchar(date) == 3, 
                             str_sub(date, start = 2, end = 3), 
                             str_sub(date, start = 3, end = 4)), 
                      ifelse(nchar(date) == 3, 
                             str_sub(date, start = 1, end = 1), 
                             str_sub(date, start = 1, end = 2)), 
                      sep = "-") %>% as.Date(., format = "%Y-%m-%d")) %>% 
  rename(tagID = tag) %>% 
  mutate(source = "capture") %>% 
  select(source, code, tagID, sex, date, longitude, latitude) %>% 
  st_as_sf(., 
           coords = c("longitude", "latitude"),
           crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0") %>% 
  mutate(status = ifelse(code %in% PTTs, "tag", "no-tag")) %>% 
  filter(code %in% BADO_resights$code | code %in% PTTs)

# # extract deployment capture event for each PTT-tagged bird
# PTT_caps <-
#   BADO_caps %>%
#   filter(code %in% PTTs) %>%
#   filter(!is.na(tag) & tag != 0) %>%
#   group_by(ring) %>%
#   slice(1) %>%
#   ungroup() %>%
#   mutate(date2 = paste(year,
#                        ifelse(nchar(date) == 3,
#                               str_sub(date, start = 2, end = 3),
#                               str_sub(date, start = 3, end = 4)),
#                        ifelse(nchar(date) == 3,
#                               str_sub(date, start = 1, end = 1),
#                               str_sub(date, start = 1, end = 2)),
#                        sep = "-") %>% as.Date(., format = "%Y-%m-%d")) %>%
#   mutate(tag = ifelse(tag == "66891" & year == "2021", "66891_1",
#                       ifelse(tag == "66891" & year == "2022", "66891_2", tag))) %>%
#   rename(tagID = tag)

#### wrangle BADO Argos data ----
BADO_PTTs <- 
  dbq(q = "SELECT * FROM ARGOS.2022_BADO") %>% 
  bind_rows(dbq(q = "SELECT * FROM ARGOS.2020_BADO")) %>% 
  filter(longitude > 100 & latitude < 0) %>% 
  mutate(temp = (0.474 * S1) - 35.662,
         volt = (0.0079 * S2) + 2.4129) %>% 
  select(tagID, locationDate, locationClass, latitude, longitude, temp, volt) %>% 
  distinct() %>% 
  mutate(source = "tag") %>% 
  st_as_sf(., 
           coords = c("longitude", "latitude"),
           crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0") %>%
  # sfc_as_cols(., names = c("lon", "lat")) %>% 
  filter(locationClass %in% c(1, 2, 3)) %>% 
  filter(year(locationDate) > 2020) %>%
  mutate(tagID = ifelse(tagID == "66891" & locationDate < as.Date("2022-10-28", format = "%Y-%m-%d"), "66891_1",
                        ifelse(tagID == "66891" & locationDate >= as.Date("2022-10-28", format = "%Y-%m-%d"), "66891_2", tagID))) %>%  
  left_join(., BADO_caps %>% st_drop_geometry() %>% select(code, tagID, date, sex), by = "tagID") %>% #filter(tagID == "234673")
  rename(cap_date = date,) %>% 
  filter(locationDate >= cap_date) %>% 
  mutate(date = as.Date(locationDate),
         status = "tag") %>% 
  select(-cap_date, -locationClass, -locationDate, -temp, -volt)

BADO_PTTs %>% 
  pull(tagID) %>% 
  unique()

# check the data
mapview(BADO_PTTs, zcol = "tagID")

max_dist_location <- 
  BADO_caps %>% 
  sfc_as_cols(., names = c("cap_longitude", "cap_latitude")) %>%
  st_drop_geometry() %>%
  rename(cap_date = date) %>% 
  select(code, cap_date, cap_longitude, cap_latitude) %>% #filter(code %in% c("XX.RR|MX.RG", "XX.RW|MX.LG"))
  left_join(bind_rows(BADO_PTTs, BADO_resights), ., by = "code") %>% 
  sfc_as_cols(., names = c("loc_longitude", "loc_latitude")) %>%
  st_drop_geometry() %>%
  mutate(dist_from_deploy = distHaversine(p1 = matrix(c(loc_longitude, loc_latitude), ncol = 2),
                                          p2 = matrix(c(cap_longitude, cap_latitude), ncol = 2))/1000) %>% 
  group_by(code) %>% 
  mutate(max_dist = max(dist_from_deploy)) %>% 
  filter(max_dist == dist_from_deploy) %>% 
  distinct() %>% 
  arrange(desc(max_dist))

BADO_curve_plot_df <- 
  max_dist_location %>% 
  filter(max_dist > 100) %>% 
  select(code, date, status, cap_longitude, cap_latitude) %>% 
  rename(longitude = cap_longitude,
         latitude = cap_latitude) %>% 
  mutate(data_type = "capture") %>% 
  bind_rows(., max_dist_location %>% 
              select(code, date, status, loc_longitude, loc_latitude) %>% 
              rename(longitude = loc_longitude,
                     latitude = loc_latitude) %>% 
              mutate(data_type = "movement")) %>% 
  st_as_sf(., 
           coords = c("longitude", "latitude"),
           crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

#### map of b-dot movements ----
bbox <- st_bbox(BADO_curve_plot_df)

max_dist_location %>% 
  filter(max_dist > 51) %>% 
  group_by(status) %>% 
  summarise(mean_distance = mean(max_dist))

map_tagging <-
  ggplot() +
  geom_sf(data = graticules, color = "gray90") +
  geom_sf(data = world_polygons, 
          fill = "gray90", color = "gray40") +
  geom_sf(data = BADO_curve_plot_df,
          aes(color = code), alpha = 0.5) +
  # geom_path(data = as.data.frame(arc5),
  #           aes(x=longitude, y=latitude, group = NULL), size = 4, color = "black") +
  # geom_curve(data = max_dist_location, 
  #            aes(x = cap_longitude, xend = loc_longitude, 
  #                y = cap_latitude, yend = loc_latitude, 
  #                group = code), 
  #            lineend = "round", curvature = -0.5) +
  geom_path(data = BADO_curve_plot_df,
            aes(x = st_coordinates(geometry)[, 1], y = st_coordinates(geometry)[, 2], group = code),
            color = "grey40", alpha = 0.5) +
  geom_sf(data = world_polygons, alpha = 0, color = "gray40") +
  coord_sf(xlim = c(bbox["xmin"] - 1750000, bbox["xmax"] + 1500000),
           ylim = c(bbox["ymin"] - 1250000, bbox["ymax"] + 200000),
           crs = lcc_params) +
  labs(x = "Longitude", y = "Latitude") +
  luke_theme +
  theme(legend.position = "none") +
  facet_grid(status ~ .)

ggsave(map_tagging,
       filename = "tabs_figs/for_katie/bdot_movements_map.png",
       width = 7,
       height = 7, units = "in",
       dpi = 300)

library(tidyverse)
library(lubridate)
library(hms)

# PTT IDs:
# 234672,234673,66894,234658,234750,66969,66891,66967,234751,66955,234674,234659,234668,66903

argos_data <-
  read.csv("/Users/luketheduke2/Downloads/ArgosData_2022_11_11_12_14_02.csv", stringsAsFactors = FALSE) %>% 
  
  mutate(Start.date.time_utc = ymd_hms(Start.date.time, tz = 'UTC'),
         Middle.date.time_utc = ymd_hms(Middle.date.time, tz = 'UTC'),
         End.date.time_utc = ymd_hms(End.date.time, tz = 'UTC'),
         Duration = as.hms(Duration),
         Start.azimuth = as.numeric(Start.azimuth),
         Middle.azimuth = as.numeric(Middle.azimuth),
         End.azimuth = as.numeric(End.azimuth)) %>% 
  
  mutate(Start.date.time_NZ = with_tz(Start.date.time_utc, 'Pacific/Auckland'),
         Middle.date.time_NZ = with_tz(Middle.date.time_utc, 'Pacific/Auckland'),
         End.date.time_NZ = with_tz(End.date.time_utc, 'Pacific/Auckland')) %>%
  
  mutate(Start.time_NZ = as.hms(Start.date.time_NZ, tz = 'Pacific/Auckland'),
         Middle.time_NZ = as.hms(Middle.date.time_NZ, tz = 'Pacific/Auckland'),
         End.time_NZ = as.hms(End.date.time_NZ, tz = 'Pacific/Auckland')) %>% 
  
  # mutate(session = ifelse(Start.time_NZ >= hms::as.hms('07:00:00', tz = 'Pacific/Auckland') & 
  #                           End.time_NZ <= hms::as.hms('08:00:00', tz = 'Pacific/Auckland'), "morning",
  #                  ifelse(Start.time_NZ >= hms::as.hms('09:30:00', tz = 'Pacific/Auckland') & 
  #                           End.time_NZ <= hms::as.hms('12:30:00', tz = 'Pacific/Auckland'), "mid-day",
  #                  ifelse(Start.time_NZ >= hms::as.hms('14:00:00', tz = 'Pacific/Auckland') & 
  #                           End.time_NZ <= hms::as.hms('18:30:00', tz = 'Pacific/Auckland'), "afternoon",
  #                  ifelse(Start.time_NZ >= hms::as.hms('20:30:00', tz = 'Pacific/Auckland') & 
  #                           End.time_NZ <= hms::as.hms('21:30:00', tz = 'Pacific/Auckland'), "evening", "out")))),
  # 
  #        date = date(Start.date.time_NZ)) %>% 

mutate(session = ifelse(Middle.time_NZ >= hms::as.hms('07:00:00', tz = 'Pacific/Auckland') & 
                          Middle.time_NZ <= hms::as.hms('08:00:00', tz = 'Pacific/Auckland'), "morning",
                        ifelse(Middle.time_NZ >= hms::as.hms('09:30:00', tz = 'Pacific/Auckland') & 
                                 Middle.time_NZ <= hms::as.hms('12:30:00', tz = 'Pacific/Auckland'), "mid-day",
                               ifelse(Middle.time_NZ >= hms::as.hms('14:00:00', tz = 'Pacific/Auckland') & 
                                        Middle.time_NZ <= hms::as.hms('18:30:00', tz = 'Pacific/Auckland'), "afternoon",
                                      ifelse(Middle.time_NZ >= hms::as.hms('20:30:00', tz = 'Pacific/Auckland') & 
                                               Middle.time_NZ <= hms::as.hms('21:30:00', tz = 'Pacific/Auckland'), "evening", "out")))),
       
       date = date(Start.date.time_NZ)) %>% 
  
  mutate(Duration = hour(Duration)*60 + minute(Duration) + second(Duration)/60) %>% 
  
  filter((Middle.azimuth >= 230 | Middle.azimuth <= 40)) %>% 
  filter(date == "2020-09-22") %>% 
  select(date, Start.time_NZ, End.time_NZ, Duration, Middle.azimuth)
# (Start.azimuth >= 230 | Start.azimuth <= 40) | (End.azimuth >= 230 | End.azimuth <= 40)) %>%

# group_by(date, session) %>% 
# 
# summarise(total_time = sum(Duration)) %>% 

filter(session != "out" & session != "evening") %>% 
  
  arrange(session, desc(total_time)) %>% 
  View()

#Create function to draw Brezier curve
bezier.curve <- function(p1, p2, p3) {
  n <- seq(0,1,length.out=50)
  bx <- (1-n)^2 * p1[[1]] +
    (1-n) * n * 2 * p3[[1]] +
    n^2 * p2[[1]]
  by <- (1-n)^2 * p1[[2]] +
    (1-n) * n * 2 * p3[[2]] +
    n^2 * p2[[2]]
  data.frame(lon=bx, lat=by)
}

bezier.arc <- function(p1, p2) {
  intercept.long <- (p1[[1]] + p2[[1]]) / 2
  intercept.lat  <- 85
  p3 <- c(intercept.long, intercept.lat)
  bezier.curve(p1, p2, p3)
}

arc3 <- bezier.arc(p1 = c(max_dist_location %>% filter(tagID == "234659") %>% pull(cap_longitude),
                          max_dist_location %>% filter(tagID == "234659") %>% pull(cap_latitude)), 
                   p2 = c(max_dist_location %>% filter(tagID == "234659") %>% pull(loc_longitude),
                          max_dist_location %>% filter(tagID == "234659") %>% pull(loc_latitude)))

bezier.uv.arc <- function(p1, p2) {
  # Get unit vector from P1 to P2
  u <- p2 - p1
  u <- u / sqrt(sum(u*u))
  d <- sqrt(sum((p1-p2)^2))
  # Calculate third point for spline
  m <- d / 2
  h <- floor(d * .2)
  # Create new points in rotated space 
  pp1 <- c(0,0)
  pp2 <- c(d,0)
  pp3 <- c(m, h)
  mx <- as.matrix(bezier.curve(pp1, pp2, pp3))
  # Now translate back to original coordinate space
  theta <- acos(sum(u * c(1,0))) * sign(u[2])
  ct <- cos(theta)
  st <- sin(theta)
  tr <- matrix(c(ct,  -1 * st, st, ct),ncol=2)
  tt <- matrix(rep(p1,nrow(mx)),ncol=2,byrow=TRUE)
  points <- tt + (mx %*% tr)
  tmp.df <- data.frame(points)
  colnames(tmp.df) <- c("lon","lat")
  tmp.df
}

arc4 <- bezier.uv.arc(p1 = c(max_dist_location %>% filter(tagID == "234659") %>% pull(cap_longitude),
                             max_dist_location %>% filter(tagID == "234659") %>% pull(cap_latitude)), 
                      p2 = c(max_dist_location %>% filter(tagID == "234659") %>% pull(loc_longitude),
                             max_dist_location %>% filter(tagID == "234659") %>% pull(loc_latitude)))

bezier.uv.merc.arc <- function(p1, p2) {
  pp1 <- p1
  pp2 <- p2
  pp1[2] <- asinh(tan(p1[2]/180 * pi))/pi * 180
  pp2[2] <- asinh(tan(p2[2]/180 * pi))/pi * 180
  
  arc <- bezier.uv.arc(pp1,pp2)
  arc$lat <-  atan(sinh(arc$lat/180 * pi))/pi * 180
  arc
}
max_dist_location
arc5 <- bezier.uv.merc.arc(p1 = c(max_dist_location %>% filter(tagID == "234659") %>% pull(cap_longitude),
                                  max_dist_location %>% filter(tagID == "234659") %>% pull(cap_latitude)), 
                           p2 = c(max_dist_location %>% filter(tagID == "234659") %>% pull(loc_longitude),
                                  max_dist_location %>% filter(tagID == "234659") %>% pull(loc_latitude))) %>% 
  rename(longitude = lon,
         latitude = lat)
