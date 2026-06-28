"C:\Users\JNamita\Downloads\courageous_camel_2024_draft_2025-03-20.1.RData"


s3_file_path <- "s3://digital-atlas/era/data/courageous_camel_2024_draft_2025-03-20.1.RData"
install.packages("aws.s3")  # For working with S3
install.packages("arrow")   # If the file is in Parquet format
library(aws.s3)
library(arrow)

#########################################################################################
#Skinny cow download

if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman")
}
pacman::p_load(
  sf, knitr, rnaturalearth, rnaturalearthdata, 
  ggplot2, viridis, shiny, dplyr, treemap, treemapify, plotly, data.table,
  s3fs, arrow, devtools, gh, htmlwidgets,remotes,gridExtra, DT
)

if(!require(eragri)){
  remotes::install_github(repo="https://github.com/ERAgriculture/eragri")
  library(eragri)
}

if(!require(eragri)){
  remotes::install_github(repo="https://github.com/ERAgriculture/eragri")
  library(eragri)
}
# Set a directory for downloaded data
dl_dir <- "downloaded_data"

# Create the directory if it doesn't already exist
if(!dir.exists(dl_dir)){
  dir.create(dl_dir, showWarnings = FALSE)
}

# Create an S3 filesystem handle
s3 <- s3fs::S3FileSystem$new(anonymous = TRUE)
era_s3 <- "s3://digital-atlas/era"

# List files in the s3 bucket
all_files <- s3$dir_ls(file.path(era_s3, "data"))

# Filter for the "skinny_cow_2022" RData file, selecting the most recent version
target_file <- tail(
  grep(".RData", grep("skinny_cow_2022", all_files, value = TRUE), value = TRUE),
  1
)

# Define a local path for saving the downloaded file
save_path <- file.path(getwd(), dl_dir, basename(target_file))

# Download the file if it does not exist already
if (!file.exists(save_path)) {
  s3$file_download(target_file, save_path, overwrite = TRUE)
}

# Load the livestock data using the miceadds package
livestock_data <- miceadds::load.Rdata2(
  file = basename(save_path),
  path = dirname(save_path)
)

sites_SC<- livestock_data$Site.Out
animals_SC<- livestock_data$Prod.Out

###########################################################################################
#Courageos camel download

s3_file_path <- "s3://digital-atlas/era/data/courageous_camel_2024_draft_2025-03-20.1.RData"

# Download the file locally
temp_file <- tempfile(fileext = ".RData")
save_object(s3_file_path, file = temp_file)

# Load the .RData file
load(temp_file)

# Check what was loaded
ls()

sites_CC <- data[[2]]
codes_CC <- data[[1]]
animals_CC<- data[[6]]
animals_CC <- animals_CC %>%
  rename(P.Product = V.Product)


# Install necessary packages (if not already installed)
install.packages(c("ggplot2", "sf", "rnaturalearth", "rnaturalearthdata", "ggthemes", "ggdensity", "viridis"))

######################################################################################
#Merge

# Define the columns you want to retain
common_cols <- c("B.Code", "Site.ID", "Site.LatD", "Site.LonD", "Country")

# Combine the datasets
combined_sites <- bind_rows(
  select(sites_CC, all_of(common_cols)),
  select(sites_SC, all_of(common_cols))
)

common_cols_prod <- c("B.Code","P.Product")
combined_prods <- bind_rows(
  select(animals_CC, all_of(common_cols_prod)),
  select(animals_SC, all_of(common_cols_prod))
)


######################################################################################
#Plots
# Load libraries
library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggthemes)
library(ggdensity)
library(viridis)
library(dplyr)

# Ensure coordinates are numeric
combined_sites <- combined_sites %>%
  mutate(
    Site.LatD = as.numeric(Site.LatD),
    Site.LonD = as.numeric(Site.LonD)
  ) %>%
  filter(!is.na(Site.LatD) & !is.na(Site.LonD))

# Count the number of papers per country
paper_counts <- combined_sites %>%
  group_by(Country) %>%
  summarise(N_Papers = n(), .groups = "drop")

# Load only African countries
world <- ne_countries(scale = "medium", continent = "Africa", returnclass = "sf")

# Fix Tanzania name if needed
world <- world %>%
  mutate(admin = if_else(admin == "United Republic of Tanzania", "Tanzania", admin))


# Ensure CRS is consistent
world <- st_transform(world, crs = 4326)

# Convert combined_sites to spatial data
sites_sf <- st_as_sf(combined_sites, coords = c("Site.LonD", "Site.LatD"), crs = 4326, remove = FALSE)

# Join paper counts to map
map_data <- world %>%
  dplyr::select(admin, geometry) %>%
  rename(Country = admin) %>%
  left_join(paper_counts, by = "Country")

# Plot the map
map<- ggplot() +
  geom_sf(data = map_data, aes(fill = N_Papers), color = "white") +
  geom_point(data = sites_sf, aes(x = Site.LonD, y = Site.LatD), 
             shape = 21, color = "black", fill = "white", size = 2, alpha = 0.5) +
  scale_fill_viridis_c(
    option = "mako",
    direction = -1,
    na.value = "gray95"
  ) +
  labs(fill = "Livestock Papers") +
  theme_minimal() +
  theme(
    legend.position = "bottom",          # ⬅ Move to bottom
    legend.direction = "horizontal",     # ⬅ Make it horizontal
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank()
  ) +
  guides(fill = guide_colorbar(
    barwidth = 10, barheight = 0.5, title.position = "top", title.hjust = 0.5
  )) +
  coord_sf(xlim = c(-20, 55), ylim = c(-35, 38), expand = FALSE)

# Display the map
map

################################################################################
#Product plots

# Group rare products as "Others"
top_categories <- c("Goat", "Sheep", "Cattle", "Chicken", "Fish", "Pigs")

livestock_counts <- combined_prods %>%
  mutate(P.Product = ifelse(P.Product %in% top_categories, P.Product, "Others")) %>%
  count(P.Product, name = "Count") %>%
  mutate(label = paste0(P.Product, " (", Count, ")"))


tree_plot<- ggplot(livestock_counts, aes(area = Count, fill = Count, label = label)) +
  geom_treemap(color = "white") +
  geom_treemap_text(
    colour = "black",
    place = "centre",
    grow = FALSE,       # Disable growing to avoid oversized text
    reflow = TRUE,
    size = 16            # Adjust this value to control the actual text size
  ) +
  scale_fill_distiller(palette = "Greens", direction = 1, guide = "none") +
  labs(title = "Distribution of Livestock Products") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    legend.position = "none"
  )
tree_plot


#############################################################################

library(patchwork)

# Combine side-by-side
combined_plot <- map + tree_plot + plot_layout(ncol = 2)

# Show the combined plot
combined_plot

