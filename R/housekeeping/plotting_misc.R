library(extrafont)

# define the plotting theme to be used in subsequent ggplots
luke_theme <- 
  theme_bw() +
  theme(
    text = element_text(family = "Franklin Gothic Book"),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    axis.title.x = element_text(size = 10),
    axis.text.x  = element_text(size = 8), 
    axis.title.y = element_text(size = 10),
    axis.text.y = element_text(size = 8),
    # panel.grid.major = element_blank(),
    # panel.grid.minor = element_blank(),
    axis.ticks = element_line(size = 0.25, lineend = "round", colour = "grey60"),
    axis.ticks.length = unit(0.1, "cm"),
    panel.border = element_rect(linetype = "solid", colour = "grey")
  )

# Find fonts from computer that you want. Use regular expressions to do this
# For example, load all fonts that are 'candara' or 'Candara'
extrafont::font_import(pattern = "[F/f]ranklin", prompt = FALSE, paths = "/Library/Fonts/Microsoft")

# check which fonts were loaded
extrafont::fonts()
extrafont::fonttable()
extrafont::loadfonts() # load these into R

#### load image of Banded Dotterel ----
img <- readPNG("/Users/Luke/Documents/Academic_Projects/New_Zealand/media/BD_flying_cut-out.png")
g <- rasterGrob(img, interpolate = TRUE)