
# Install necessary packages (uncomment to run)
if ("rstudioapi" %in% installed.packages()) {
 rstudioapi::restartSession()
}
# After restart, run package installation again
install.packages(c("foreach", "ecmwfr", "raster", "sp", "sf", "geodata",
 "tidyr", "dplyr", "ncdf4", "ggplot2", "lubridate",
 "terra", "leaflet", "reshape2", "exactextractr"))

# Step 1: Print file summary

library(ncdf4) # For working with NetCDF files
library(raster) # For working with raster data
library(ggplot2) # For plotting
library(dplyr) # For data manipulation
library(lubridate) # For working with dates
library(terra) # For spatial data handling
library(leaflet) # For creating interactive maps
library(reshape2) # For reshaping data
library(sf) # For working with simple features and spatial data
library(exactextractr) # For exact extraction of raster values from polygons
library(ecmwfr) #to interface with the ECMWF


# Step 2: Extract Temperature Data
#################################################################
# setting working directory
setwd("C:/Users/Administrator/Desktop/IWR 2024/documents-export-2024-9-23/Climate data/Climate_tutorial/Climate_tutorial")

# Open NetCDF file using the actual path
temp_global <- nc_open("C:/Users/Administrator/Desktop/IWR 2024/documents-export-2024-9-23/Climate data/Climate_tutorial/Climate_tutorial/ERA5land_global_t2m_2023_daily_0.5.nc")

# Print the summary of the file to understand the variables and dimensions
print(temp_global)

# extracting the temperature data (mean) using the variable name in summary
temperature_data <- ncvar_get(temp_global, "t2m")

print(temperature_data) 

# Step 3: Extract Latitude and Longitude Information
#################################################################
## extract the latitude and longitude information of netcdf file
lat<-ncvar_get(temp_global,"lat") ## lat is the variable name in net cdf file
print(lat)

lon<-ncvar_get(temp_global,"lon") ## lon is the variable name in net cdf file
#print(lon)

# Step 4: Extract and Understand Time Variables
#################################################################
# now get the time variable (it is named as 'time')
time_units <- ncatt_get(temp_global, "time", "units")$value

print(time_units) # unit is days (daily data) and it starts from 1-1-2023

# now time variable
time_temp <- ncvar_get(temp_global, "time")
# print(time_temp)


# now time variable
time_temp <- ncvar_get(temp_global, "time")

## actual time corresponding to each time variable value
# origin is basically the starting time of data (time_units) in netcdf file
timestamp = as_datetime(c(time_temp*60*60*24),origin="2023-01-01")

print(timestamp) ## try to print it
# suppose instead of daily data, if netcdf file contains hourly data
# then actual time stamp for time varable containing
# 365*24 values (hourly data for 365 days) can be obtained by
# if Hours since origin is given

# Step 5: Check Temporal Coverage by Month
#################################################################

# checking the months covered in complete time frame
times_mon = month(timestamp)

print(times_mon) # try to print it

#timestamp = as_datetime(c(time_temp*60*60),origin="2023-01-01") ## origin from time_units

# Step 6: Close nc file
#################################################################
# Close the NetCDF file after extracting data to free up memory
nc_close(temp_global)

#################################################################
#################################################################
# Visualizing and Modifying the Climate Data
# Step 1: Loading and Inspecting the Raster Data
#################################################################
# Load the raster data using the path
temp_rast <- rast("C:/Users/Administrator/Desktop/IWR 2024/documents-export-2024-9-23/Climate data/Climate_tutorial/Climate_tutorial/ERA5land_global_t2m_2023_daily_0.5.nc")
print(temp_rast)

# Print information for a specific raster layer (e.g., 2nd Jan, 2023)
print(temp_rast[[2]])


# Step 2: Renaming Raster Layers with Timestamps
#################################################################
# Change the layer names to actual dates using timestamp
names(temp_rast) <- timestamp

print(temp_rast)

# Plotting the temperature data for all days
windows()
plot(temp_rast)

# Plotting data for a specific day (e.g., 2nd Jan, 2023)
plot(temp_rast[[7]])


# Step 3: Understanding Coordinate Reference Systems (CRS)
#################################################################
# Understanding the Coordinate Reference System (CRS)
crs_info <- crs(temp_rast)
print(crs_info)


# Step 4 Reprojecting to a Different CRS
#################################################################
# Reprojecting to a different CRS (here we use the same CRS for demonstration)
#"EPSG:4326" is an identifier for a specific CRS known as WGS84 (World Geodetic System 1984)
temp_rast_reproj <- project(temp_rast, "EPSG:4326")

#print(temp_rast_reproj)
plot(temp_rast_reproj[[2]])

# Reprojecting to crs NAD83 (to a different crd system)
temp_rast_nad83 <- project(temp_rast, "EPSG:4269")

print(temp_rast_nad83) # Now temp_rast_nad83 uses the NAD83 coordinate system (EPSG:4269)

# Step 5: Cropping the Raster to a Specific Extent
#################################################################
# Define the extent of Europe and crop the raster to this extent
## format to define extent is ((xmin, xmax, ymin, ymax) in raster plot)
europe_extent <- ext(-30.5, 70.5, 33.5, 75.5)
#It will crop the entire dataset for all days of year
europe_temp_rast <- crop(temp_rast, europe_extent)
#print(europe_temp_rast)
plot(europe_temp_rast)

plot(europe_temp_rast[[2]])

# Step 6: Cropping to more finer extent
#################################################################
# Define the extent of Heidelberg and crop the raster
Heidelberg_extent <- ext(8, 21, 49, 56)
Heidelberg_temp_rast <- crop(temp_rast, Heidelberg_extent)
#print(Heidelberg_temp_rast)
plot(Heidelberg_temp_rast)

# mean temperature in Heidelberg (at 0.5 degree resolution approx 55*55 km^2)
plot(Heidelberg_temp_rast[[2]])

# Define a specific location (IWR) and crop the raster further
IWR_extent <- extent(8.5, 9, 49, 49.5)
IWR_temp_rast <- crop(Heidelberg_temp_rast, IWR_extent)
#print(IWR_temp_rast) # Uncomment to see cropped extent details
plot(IWR_temp_rast)

lat_IWR <- 49.25 # falls within latitude extent (49,49.5)
lon_IWR <- 8.75 # falls within longitude extent (8.5,9)
values_at_IWR <- extract(temp_rast, matrix(c(lon_IWR, lat_IWR), ncol=2))
print(values_at_IWR) ## same as IWR_temp_ras

# Step 7: Plotting a Time Series of Temperature for IWR
#################################################################
# Plotting a time series for mean temperature for IWR
# plot(timestamp, values_at_IWR, type = "l", xlab = "Date", ylab = "Mean Temperature", pch = 26, col = "date")
# plot(timestamp, values_at_IWR, type = "l", xlab = "Date", ylab = "Mean Temperature", pch = 26, col = "da

#################################################################
# Spatial Aggregation of Climate Data
#################################################################
# Step 1: Concept
# Create a 4x4 matrix and convert it into a raster
m <- matrix(1:16, ncol = 4, nrow = 4)
r16 <- rast(m)
#print(r16)
# Function to visualize raster data
plot_raster <- function(r) {
plot(r, axes = FALSE, legend = FALSE)
plot(as.polygons(r, dissolve = FALSE, trunc = FALSE), add = TRUE)
text(r, digits = 2)
}

# Plot the created raster matrix
plot_raster(r16)


# Aggregate the raster to a 2x2 resolution using default aggregation function
r4 <- aggregate(r16, fact = 2)

#print(r4) # Uncomment to check new resolution
plot_raster(r4)

# Aggregate using the minimum value instead of the mean
plot_raster(aggregate(r16, fun = "min"))

# Step 2: Spatially aggregating climate data
#################################################################
# Aggregating the raster to 1-degree resolution
temp_rast_1 <- aggregate(temp_rast, fact = 2)
print(temp_rast_1) # Check new resolution (now 1-degree resolution i.e. approx 110*110 km^2)

plot(temp_rast_1[[2]])

#similarly aggregating our temp_rast to 1 degree resolution using minimun temperature of grid combined
temp_rast_1_min <- aggregate(temp_rast, fact = 2, fun = "min")
print(temp_rast_1_min) ## check the values and compare with print(temp_rast_1) where it was aggregated
plot(temp_rast_1_min[[2]]) # plot of 2nd layer


# Step 3: Disaggregating Data to Original Resolution
#################################################################
# Disaggregate back to 0.5-degree resolution from 1-degree resolution
temp_rast_dis <- disagg(temp_rast_1, fact = 2)

#print(temp_rast_dis) # Uncomment to check the new resolution
plot(temp_rast_dis[[2]])

# Step 4: Resampling Raster Data to Match Different Resolutions
#################################################################
temp_global_0.1_rast <- rast("C:/Users/Administrator/Desktop/IWR 2024/documents-export-2024-9-23/Climate data/Climate_tutorial/Climate_tutorial/ERA5land_global_t2m_2023_daily_0.5.nc")
print(temp_global_0.1_rast) ## check the resolution (0.1 * 0.1)

plot(temp_global_0.1_rast[[2]])

one_layer <- temp_global_0.1_rast[[2]]
plot(one_layer)

## now resampling one_layer to resolution temp_rast
temp_global_converted_rast <- resample(one_layer, temp_rast)
print(temp_global_converted_rast) ## check the resolution

#################################################################
# Temporal aggregation of Climate Data
#################################################################
# Step 1: Monthly Aggregation of daily climate Data
# Extract date information from the layer names
dates <- as.Date(names(temp_rast)) ## as done earlier in Chapter 2
#print(dates)
# Create a vector representing the months
months <- format(dates, "%Y-%m")
#print(months)

# Aggregate daily data to monthly means using the 'tapp' function
#tapp function (from terra package) aggregates across layers based on the grouping factor
#Here, 'months' is used as groupin factor
monthly_temp <- tapp(temp_rast, months, fun = mean)
# Check the resulting SpatRaster with monthly data
print(monthly_temp) ## check the temporal layears (3D matrix layer)

# Get and clean the current layer names
current_names <- names(monthly_temp)
# Remove the "X" prefix
#X prefix got introduced due to conversion from numeric format of names to character format
cleaned_names <- sub("^X", "", current_names)
# Assign the new names to the SpatRaster object
names(monthly_temp) <- cleaned_names
# Check the new names after cleaning
print(monthly_temp)

# Plot the monthly aggregated data
plot(monthly_temp)

##############################################
# Extracting Data and Converting into Working Data Frames
#############################################

print(temperature_data) ## temperature data of 365 layers

# Uncomment if needed to verify latitude and longitude assignment
print(lat) ## latitudes at 0.5 degree resolution
print(lon) ## longitudes at 0.5 degree resolutions
# Assign row names as longitude and column names as latitude to the temperature data matrix
colnames(temperature_data) <- lat
rownames(temperature_data) <- lon
# Create a data frame of all coordinates
temperature_data_df <- melt(temperature_data)
print(temperature_data_df)

# Filter rows where Var3 (time) equals 2 (2nd Jan, 2023)
temp_data_df2 <- temperature_data_df %>%
  filter(Var3 == 2)
# Remove the Var3 column (we don't need the time column now as we know that it is the data of 2nd Jan, 2023)
temp_data_df2 <- temp_data_df2 %>%
  select(-Var3)
# Print the resulting data frame to inspect
print(temp_data_df2) ### uncomment to print


# renaming the column names for rasterXYZ (function to convert dataframes to raster for visulization) compatibility
names(temp_data_df2)<-c("x","y","temperature")
# converting it to raster projection
ras_temp<-rasterFromXYZ(temp_data_df2)
print(ras_temp)

# assigning a coordinate system
proj4string(ras_temp)<-CRS("epsg:4326")
## plotting the data and visualizing it
plot(ras_temp)


##############################################################
# Integrating Shapefiles for Regional Climate Data Extraction and Analysis
##############################################################

# Step 1: Preparing the shapefile
# first reading the shapefile from the same directory (set the directory to tutorials folders)
shp_file <- "NUTS_RG_01M_2021_3035_LEVL_3.shp" ## name of the shapefile for EU NUTS3 region
shapefile1 <- st_read(shp_file)

#print(shapefile1) # Detailed explanation (uncomment to print)
## now since the CRS is in Easting - Northing system
## so we convert it to standard WGS84 ( for uniform crs throughout this tutorial)
shapefile <- st_transform(shapefile1, crs = "epsg:4326")
plot(shapefile$geometry)
# info(shapefile)

#Since shapefile also contain extended EU regions
# Hence, define the bounding box, so that shapefile is cropped only to main Europe
bbox <- c(xmin = -10, ymin = 29, xmax = 35, ymax = 72)
# Create the geographic extent object
cropped_region <- st_crop(x = shapefile$geometry, y = bbox)
plot(cropped_region)

# Step 2: Aligning crs of shapefile and raster object
##############################################################
## Recall from last chapter,
##Our ras_temp raster contains the raster information for mean temperature of 2nd Jan, 2023
print(ras_temp) ## refreshing the information

#Cropping the ras_temp to extent so that it matches with area covered by shapefile
## i.e. both have same lat long extent (as we have done in chapter 3)
temp_EU <- crop(ras_temp, extent(shapefile))
print(temp_EU) ## now ras_temp is reduced to extent of EU NUTS3 shapefile

# Step 3: Extracting climate data using shapefile
##############################################################
temp_data_NUTS3 = exact_extract(temp_EU,shapefile,
                                fun="weighted_mean",weights="area") ## weighted by area of grid
print(temp_data_NUTS3)

temp_data_NUTS3 = as.numeric(temp_data_NUTS3)
## convert the data to numeric values (only to be double sure)

# Step 4: Visualizing the extracted data
##############################################################
shapefile$values <- temp_data_NUTS3
#print(shapefile) ## uncomment if needed to inspect


# Define your custom color palette with three colors
custom_colors <- c("navyblue", "white", "red4")
# Set your upper limit for legend (any value above mx temperature)
upper_limit <- 35
# Set legend title
legend_title <- "Mean temperature"
# for no space after highest value in legend
# Plot the shapefile with output_data using a custom color scale
ggplot() +
  geom_sf(data = shapefile, aes(fill = ifelse(values > upper_limit, upper_limit, values))) +
  labs(title = "Mean temperature on 2nd january 2023 ") +
  scale_fill_gradientn(colors = custom_colors, name = legend_title) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank()
  )+
  coord_sf(xlim = c(-10, 29), ylim = c(35, 72), expand = FALSE)

# Step 5: Converting the data to working dataframes
##############################################################
# extracting shapefile details in a data frame
shp_to_df <- as.data.frame(shapefile)
head(shp_to_df) ## select only required columns

req_df <- shp_to_df[, c(1:9)] # only required information (columns) from shapefile (not geometry)
df_temp_shapefile <- cbind(req_df, shapefile$values) ## bind column wise corresponding values of mean temperature of regions
#print(df_temp_shapefile) # Uncomment if needed to inspect the final data frame
#Write output data to CSV file and save in your folder
write.csv(df_temp_shapefile, file = "EU_NUTS3_mean_temp.csv", row.names = FALSE) # set the directory and


############################################################################################################################
# Brazil
############################################################################################################################



##############################################################
# Integrating Shapefiles for Regional Climate Data Extraction and Analysis
##############################################################

# Step 1: Preparing the shapefile
# first reading the shapefile from the same directory (set the directory to tutorials folders)
shp_file_br <- "shape_brazil" ## name of the shapefile for Brazil region
shapefile1_br <- st_read(shp_file_br)
print(shapefile1_br)

# #print(shapefile1) # Detailed explanation (uncomment to print)
# ## now since the CRS is in Easting - Northing system
# ## so we convert it to standard WGS84 ( for uniform crs throughout this tutorial)
# shapefile <- st_transform(shapefile1, crs = "epsg:4326")
# plot(shapefile$geometry)
# # info(shapefile)

# #Since shapefile also contain extended Brazil regions
# # Hence, define the bounding box, so that shapefile is cropped only to Brazil
# bbox <- c(xmin = -73.99045, ymin = -33.7483, xmax = -32.39494, ymax = 5.271841)
# # Create the geographic extent object
# cropped_region <- st_crop(x = shapefile$geometry, y = bbox)
# plot(cropped_region)


plot(shapefile1_br)

# Step 2: Aligning crs of shapefile and raster object
##############################################################
## Recall from last chapter,
##Our ras_temp raster contains the raster information for mean temperature of 2nd Jan, 2023
print(ras_temp) ## refreshing the information

#Cropping the ras_temp to extent so that it matches with area covered by shapefile
## i.e. both have same lat long extent (as we have done in chapter 3)
temp_br <- crop(ras_temp, extent(shapefile1_br))
print(temp_br) ## now ras_temp is reduced to extent of EU NUTS3 shapefile

# Step 3: Extracting climate data using shapefile
##############################################################
temp_data_br = exact_extract(temp_br,shapefile1_br,
                                fun="weighted_mean",weights="area") ## weighted by area of grid
print(temp_data_br)

temp_data_br = as.numeric(temp_data_br)
print(temp_data_br)
## convert the data to numeric values (only to be double sure)

# Step 4: Visualizing the extracted data
##############################################################
shapefile1_br$values <- temp_data_br
print(shapefile1_br) ## uncomment if needed to inspect


# Define your custom color palette with three colors
custom_colors <- c("darkgreen", "yellow", "purple")
# Set your upper limit for legend (any value above mx temperature)
upper_limit <- 35
# Set legend title
legend_title <- "Mean temperature"
# for no space after highest value in legend
# Plot the shapefile with output_data using a custom color scale
ggplot() +
  geom_sf(data = shapefile1_br, aes(fill = ifelse(values > upper_limit, upper_limit, values))) +
  labs(title = "Mean temperature on 2nd january 2023 ") +
  scale_fill_gradientn(colors = custom_colors, name = legend_title) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank()
  )

# Step 5: Converting the data to working dataframes
##############################################################
# extracting shapefile details in a data frame
shp_to_df_br <- as.data.frame(shapefile1_br)
head(shp_to_df_br) ## select only required columns

req_df_br <- shp_to_df_br[, c(1)] # only required information (columns) from shapefile (not geometry)
df_temp_shapefile1_br <- cbind(req_df_br, shapefile1_br$values) ## bind column wise corresponding values of mean temperature of regions
#print(df_temp_shapefile) # Uncomment if needed to inspect the final data frame
#Write output data to CSV file and save in your folder
write.csv(df_temp_shapefile1_br, file = "BR_NUTS3_mean_temp.csv", row.names = FALSE) # set the directory and

