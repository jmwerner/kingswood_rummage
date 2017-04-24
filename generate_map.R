library(rvest)
library(leaflet)
library(RJSONIO)
library(htmlwidgets)

listing_website = "http://kingswoodrummage.com/listings/"


# Robbed from here: http://stackoverflow.com/questions/32504880/street-address-to-geolocation-lat-long
# Thank you kind stranger

geocodeAddress <- function(address) {
    url <- "http://maps.google.com/maps/api/geocode/json?address="
    url <- URLencode(paste(url, address, "&sensor=false", sep = ""))
    x <- fromJSON(url, simplify = FALSE)
    if (x$status == "OK") {
        out <- c(x$results[[1]]$geometry$location$lng,
                 x$results[[1]]$geometry$location$lat)
    } else {
        out <- NA
    }
    Sys.sleep(0.2)  # API only allows 5 requests per second
    out
}


processDateString <- function(input) {
    str = gsub("PMA", "PM</p><p>A", input)
    str = gsub("AMA", "AM</p><p>A", str)
    paste0("<p>", str, "</p>")
}

createGoogleSearchLink <- function(query) {
    full_address = paste(query, "Sioux Falls, SD")
    link = paste0("http://maps.google.com/?q=", full_address)
    paste0('<a href=\"', link, '\">', query, '</a>')
}




## Driver ##
tables = read_html(listing_website) %>% html_nodes("table") %>% html_table(fill = TRUE, header = TRUE)

locations = tables[[2]]

names(locations) = c("Address", "Items", "Dates", "FamilyN")

locations$FamilyN = as.numeric(locations$FamilyN)
locations$FamilyN[is.na(locations$FamilyN)] = 1

geolocations = sapply(locations$Address, function(x){
    geocodeAddress(paste(x, "Sioux Falls, SD"))
})

locations$lon = geolocations[1,]
locations$lat = geolocations[2,]

locations$processedDates = sapply(locations$Dates, processDateString)

locations$label = mapply(function(x, y) {
    address = createGoogleSearchLink(x)
    paste(address, y)    
}, locations$Address, locations$processedDates)

m <- leaflet() %>%
         addTiles(layerId = "tiles") %>% 
         addCircleMarkers(lng = locations$lon, 
                          lat = locations$lat, 
                          radius = locations$FamilyN / 2.5 + 2,
                          popup = locations$label) %>%
         addProviderTiles(providers$Esri.NatGeoWorldMap)


saveWidget(m, file="map.html")


