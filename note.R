#note
library(tmap)
install.packages("tmap")
library(tmap)
install.packages("readr")
library(readr)
install.packages("sf")
library(sf)
install.packages("tidyverse")
library(tidyverse)
install.packages("raster")
library(raster)
######

#trying some stuff

item <- raster("./EAST_BURN_NEW.tif.aux.xml")
###########

#Everything from the raster cropping stuff witht he EVT burn
#####################################################3
library(tidyverse)
library(sf)
library(raster)
library(tmap)
library(ggplot2)
#####################################################

#Uploading new files

East_BURN <- raster("./EAST_BURN_NEW.tif")

HNF_SHAPE <- st_read("./HNF_SHAPE_NEW.shp")

E_HNF_EVT <- raster("./QGIS_test_HNF_EVT.tif")

#Visualize it  using plot and add = True to add second plot request to existing plot

plot(East_BURN)

plot(HNF_SHAPE, add = T)

# create extract list

r1_x <- East_Burn %>%
  raster::extract(HNF_SHAPE) %>%
  lapply(E_HNF_EVT)

##Bunch of crap below this

r1_xx <- East_Burn %>% raster::extract(HNF_SHAPE)

r1_xxx <- lapply(r1_xx)


# Trying something different

unique.count <- function(x) {
  y <- table(x)
  z <- as.data.frame(y)
  a <- arrange(z, desc(Freq)) %>% rename(ID = x)
  return(a) }

####Hold up

bps_df <- r1_xx[[1]]

#didnt work

# create extract list
DF_East_BURN <- East_BURN %>%
  raster::extract(HNF_SHAPE) %>%
  lapply(unique.count)

# swoopty to data frame

DF_East_BURN <- DF_East_BURN[[1]]


#golden code for the combining of rasters
# golden code
# this code "stacked the two rasters of intrest then pulled values with get values() then placed them as a data frame with as.data.frame() 
#then used plyr::count() 
#to summarize the frequencey of uiqe value combinations between the two columns, placing a summarary value into a third, new column in the data frame

EVT_burn <- stack(East_BURN, E_HNF_EVT) %>% getValues() %>% as.data.frame() %>% plyr::count()

# Pulling in EVT 200

EVT_200_AttributeTable <- read.csv("./US_200_EVT_Attribute_Table.csv")
#success

#using dplyr package to rename the columns of EVT_burn
EVT_burn <- rename(EVT_burn, EVT_Value = QGIS_test_HNF_EVT, Burn_Value = EAST_BURN_HNF) 

EVT_200_AttributeTable <- rename(EVT_200_AttributeTable, EVT_Value = VALUE)

#Now can Left Join using left_join

EVT_Join <-left_join(EVT_burn, EVT_200_AttributeTable)

#Filter out columns of intrest using select()

EVT_Join <- select(EVT_Join, Burn_Value, EVT_Value, freq, EVT_Name, EVT_PHYS, EVT_Fuel_N)


#Everything with pipes


NewData <- EVT_burn %>% 
  rename(EVT_Value = QGIS_test_HNF_EVT,
         Burn_Value = EAST_BURN_HNF,
         Frequency = freq) %>%
  left_join(EVT_200_AttributeTable) %>%
  select(Burn_Value, EVT_Value, freq, EVT_Name, EVT_PHYS, EVT_Fuel_N)




#Worked

EVT_Join <- EVT_Join %>% add_column(acres = .$freq/4047)


View(EVT_Join)

# Exporting as acsv to have graphs made in excel
write.csv(EVT_Join, "./EVT_and_Burn.csv")


EVT_Join %>% group_by(EVT_Name) %>% summarise(avg = mean(Burn_Value))

EVT_Join_Clean <- filter(EVT_Join, Burn_Value == 1:198)


