
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(plyr)
library(dplyr)
library(rgdal)
library(sf)

# WDPA package
# https://prioritizr.github.io/wdpar/
# devtools::install_github("prioritizr/wdpar")
library(wdpar)

# Directories
datadir <- "data/wdpa/data"
rasterdir <- "data/wdpa/data/annual_rasters"
plotdir <- "data/wdpa/figures/annual_rasters"

# Build data
################################################################################

# Fetch most recent version of the WDPA
# wdpa <- wdpa_fetch("global", download_dir=datadir)

# Load WDPA GeoDataBase
wdpa_file <- file.path(getwd(),"data/wdpa/data/WDPA_Feb2019_Public", "WDPA_Feb2019_Public.gdb")
st_layers(wdpa_file)
wdpa_orig <- sf::st_read(wdpa_file, layer="WDPA_poly_Feb2019", stringsAsFactors=F)# 17 minutes to load

# Inspect database
names(wdpa_orig)
table(wdpa_orig$STATUS) # Only use Designated, Inscribed, and Established (ignore Adopted, Proposed, Not Reported)
table(wdpa_orig$MARINE) # Only use 1=marine/terrestrial and 2=100% marine (0=100% terrestrial)
table(wdpa_orig$STATUS_YR) # Status year of zero doesn't make sense
# Are all of the important columns filled in?
sum(is.na(wdpa_orig$STATUS))
sum(is.na(wdpa_orig$STATUS))
sum(is.na(wdpa_orig$STATUS))

# Filter and format database
wdpa_use <- wdpa_orig %>% 
  filter(STATUS %in% c("Designated", "Established", "Inscribed") & 
           DESIG_ENG!="UNESCO-MAB Biosphere Reserve" &
           MARINE %in% c(1, 2) & STATUS_YR!=0)

# Plot establishment year
range(wdpa_use$STATUS_YR)
hist(wdpa_use$STATUS_YR, breaks=seq(1870,2020,5))

# Export data
################################################################################

# Export data
saveRDS(wdpa_use, file=file.path(datadir, "WDPA_Feb2019_polygons_use.Rds"))


# Build data
################################################################################

# Package
library(maptools)

# Projections
moll84 <- "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"

# Get world map
data(wrld_simpl)
world <- spTransform(wrld_simpl, moll84)

# Transform WDPA
wdpa <- st_transform(wdpa_use, moll84)

# Loop through years
for(i in 1900:2018){
  
  # Subset data
  yr <- 1970
  ydata <- filter(wdpa, STATUS_YR<=yr)
  
  # Build raster
  ras_temp <- raster::raster(raster::extent(ydata), resolution=1000, crs=sp::CRS(moll84))
  ras <- raster::rasterize(ydata, ras_temp, field=1, background=NA)
  
  # Plot MPA coverage
  png(file.path(plotdir, paste0(yr, ".png")), width=3.5, height=2, units="in", res=600)
  par(mar=rep(0,4))
  plot(world, col="grey", border="white", main=yr, lty=0.7, cex.main=0.8)
  plot(ras, add = TRUE, col="red", legend=F)
  dev.off()
  
}
