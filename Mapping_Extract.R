
# Bring in libraries
suppressMessages(library(raster))
suppressMessages(library(data.table))
suppressMessages(library(rgdal))
suppressMessages(library(sp))
suppressMessages(library(sf))
suppressMessages(library(dplyr))
suppressMessages(library(leaflet))

# Read in data
Area_Data <- readRDS("Area_Data.rds")
map_dat_1 <- readRDS("India_Level1_Shapefile.rds")
map_dat_2 <- readRDS("India_Level2_Shapefile.rds")

# Extract data from maps
dat1 <- as.data.table(map_dat_1@data)
dat2 <- as.data.table(map_dat_2@data)

# Explore merge
Mapping_Table <- dat2[, .N, keyby=.(NAME_1, NAME_2)]
Mapping_Table[, N := NULL]

# Mapping table
District_Table <- Area_Data[, .N, keyby=.(District)]
District_Table[, N := NULL]
District_Table[, District_Mod := District]
District_Table[District=="Ahmedabad", District_Mod := "Gujarat"]
District_Table[District=="Aravalli", District_Mod := "Gujarat"]
District_Table[District=="Banaskantha", District_Mod := "Gujarat"]
District_Table[District=="Baroda", District_Mod := "Vadodara"]
District_Table[District=="Botad", District_Mod := "Gujarat"]
District_Table[District=="Botad", District_Mod := "Gujarat"]
District_Table[District=="Chhota Udepur", District_Mod := "Gujarat"]
District_Table[District=="Dang", District_Mod := "Gujarat"]
District_Table[District=="Devbhumi Dwarka", District_Mod := "Gujarat"]
District_Table[District=="Gir Somanath", District_Mod := "Gujarat"]
District_Table[District=="Kutch", District_Mod := "Gujarat"]
District_Table[District=="Mahisagar", District_Mod := "Gujarat"]
District_Table[District=="Mehsana", District_Mod := "Gujarat"]
District_Table[District=="Morbi", District_Mod := "Gujarat"]
District_Table[District=="Panchamahal", District_Mod := "Gujarat"]
District_Table[District=="Porabandar", District_Mod := "Gujarat"]
District_Table[District=="Sabarkanta", District_Mod := "Gujarat"]
District_Table[District=="Tapi", District_Mod := "Gujarat"]

# Merge on names
Mapping_Table[District_Table, Ind1 := 1.0, on=.(NAME_1=District_Mod)]
Mapping_Table[District_Table, Ind2 := 1.0, on=.(NAME_2=District_Mod)]

# Clean up
Mapping_Table[!is.na(Ind2), NAME_C := NAME_2]
Mapping_Table[is.na(NAME_C) & !is.na(Ind1), NAME_C := NAME_1]
Mapping_Table[is.na(NAME_C), NAME_C := paste0(NAME_2,"__C")]
Mapping_Table[, c("Ind1","Ind2") := NULL]

# Confirm that this worked
District_Table[Mapping_Table, Check := 1.0, on=.(District_Mod=NAME_C)]

# Merge on final name
dat2[Mapping_Table, NAME_C := i.NAME_C, on=.(NAME_1, NAME_2)]
dat2[is.na(NAME_C)]
setDF(dat2)
map_dat_2@data <- dat2

# Consolidate shapefile
final_map <- st_as_sf(map_dat_2)
final_map <- final_map %>% 
    group_by(NAME_C) %>%
    summarise(geometry = sf::st_union(geometry)) %>%
    ungroup()

final_map <- as(final_map, 'Spatial')
final_map <- final_map[!(final_map$NAME_C %like% "__C"),]
plot(final_map)

# Test mapping
final_map_data <- as.data.table(final_map@data)

# Test
Test_Data <- Area_Data[, .(AvgYield=mean(Yield)), keyby=.(District)]
Test_Data[District_Table, District_Mod := i.District_Mod, on=.(District)]
Test_Data <- Test_Data[, .(AvgYield=mean(AvgYield)), keyby=.(District=District_Mod)]
final_map_data[Test_Data, AvgYield := i.AvgYield, on=.(NAME_C=District)]
final_map_data[, Label := paste0(NAME_C, " - ", scales::comma(AvgYield))]

# Make test map
final_map@data <- final_map_data
qpal <- colorQuantile("RdYlBu", final_map_data$AvgYield, n = 5)
leaflet_outplot <- leaflet(final_map) %>%
	addTiles() %>%
  addPolygons(
    fillColor=~qpal(AvgYield),
    weight = 2,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7,
    highlight = highlightOptions(
      weight = 5,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE),
    label = final_map$Label,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto")) %>%
   addLegend(pal = qpal, values = ~AvgYield, opacity = 1)

# Save plot and mapping table
District_Table[, DistrictRaw := gsub("\\s+","", District)]
final_map@data[, c("AvgYield","Label") := NULL]

# India 
out_list <- list(final_map, District_Table)
names(out_list) <- c("ind_map","district_mapping")
saveRDS(out_list, "India_Map.rds")
