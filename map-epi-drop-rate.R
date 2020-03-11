library(sf)
library(tidyverse)

# Load the epi data
epi_geo <- read.csv("so-dhis-epi-2017-9-geo.csv", stringsAsFactors = F)

# Load the shapefiles
regions <- read_sf("somalia/Som_Admbnda_Adm1_UNDP.shp")

# Calculate drop out rate and reshape the data
epi_wide <- epi_geo %>% 
    filter(Year == 2019 & Vaccine %in% c("Penta_1", "Penta_3")) %>% 
    group_by(OBJECTID, Region, Vaccine) %>% summarise(Dose=sum(Dose)) %>% 
    pivot_wider(names_from = 3, values_from = 4) %>%
    select(OBJECTID, Region, Penta_1, Penta_3) %>% 
    mutate(drop_out=ceiling((Penta_1-Penta_3)/Penta_1 *100))

# Merging the two datasets
epi_merge <-merge(regions, epi_wide, by="OBJECTID", all.x=TRUE)
ordered_epi <-epi_merge[order(epi_merge$drop_out), ]

# Create coordinates for the labels.
ordered_epi <- cbind(ordered_epi, st_coordinates(st_centroid(ordered_epi$geometry)))


# Visualize Drop Out Rate by Region
ggplot(data = ordered_epi) +
    geom_sf(aes(fill=drop_out), size=0.25, color="white") + 
    coord_sf() + 
    scale_fill_gradient(low = "white", high = "red", limits=c(0,40), na.value ="gray", name = "Drop Out Rate %")  + 
    ggtitle("Drop Out Rate for Penta 1 & 3 in 2019", subtitle = "Out of 10 reporting regions, half recorded more \nthan 20% drop out rate. The national average is \n23 percent.") + 
    geom_text(aes(x=X, y=Y, label = admin1Name), 
              size = 2, col = "black", fontface = "bold") + 
    theme_bw() + xlab("") + ylab("") +
    theme(legend.position = "top")
    
