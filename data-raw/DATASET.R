# Load raw data from CCDC file
exampleData <- stars::read_stars("data-raw/exampleData.tif",n_proxy=10^10)
# Apply preprocessing...
# Save the cleaned data in the required R package location
usethis::use_data(exampleData,overwrite = T)

landcover <- sf::read_sf("data-raw/zonePolyExmpl.shp")

usethis::use_data(landcover,overwrite = T)

