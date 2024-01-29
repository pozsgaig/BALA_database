# from Sebastien modified by Gabor


Event_table <- unique(data.frame(eventID = BALA_all_data$eventID, #should this be id or eventID?
                          locationID = BALA_all_data$site_code,
                          island = BALA_all_data$island,
                          year = BALA_all_data$year,
                          eventDate = as.Date(paste(BALA_all_data$year, BALA_all_data$month, "01", sep="-"), 
                                              format = "%Y-%b-%d"),
                          stateProvince = as.factor("Azores"),
                          islandGroup = as.factor("Azores"),
                          countryCode = as.factor("PT"),
                          country = as.factor("Portugal"),
                          #locality = BALA_all_data$site_name,
                          locationRemarks = BALA_all_data$fragment_name,
                          habitat = as.factor(BALA_all_data$habitat_specific),
                          geodeticDatum = as.factor("WGS84"),
                          coordinateUncertaintyInMeters = as.factor("45"),
                          coordinatePrecision = as.factor("0.00001"),
                          georeferenceSources = as.factor("GPS"),
                          samplingProtocol = as.factor(BALA_all_data$sampling_method)))


unique(BALA_all_data$site_name)

length(unique(BALA_all_data$eventID)) # correct


Event_table <- merge(Event_table, 
                     select(site_coords, "Site_code", "Y_coord", "X_coord", "Altitude", "municipality"), 
                     by.x = "locationID", by.y = "Site_code", all.x = T)

Event_table <- Event_table %>% rename("minimumElevationInMeters" = "Altitude", 
                                      "decimalLatitude" = "Y_coord", 
                                      "decimalLongitude" = "X_coord")

Event_table$decimalLatitude <- round(as.numeric(Event_table$decimalLatitude),digits = 5)
Event_table$decimalLongitude<- round(as.numeric(Event_table$decimalLongitude), digits = 5)
    
    # #### Municipality ####
    # Event_table <- merge(x = Event_table, y = select(site_db, Site_code, Municipality), by.x = "locationID", by.y = "Site_code", all.x = T)
    # Event_table <- Event_table %>% rename("municipality" = "Municipality")


    # #### Published data ####
    # Event_table$PublishedSpider <- FALSE
    # Event_table$PublishedArthropod <- FALSE
    # Event_table$DOI <- NA_character_

Event_table<-unique(Event_table)

#Event_table$DatasetName<-"BALA Project"
Event_table$fieldNumber<-paste(sapply(strsplit(sapply(strsplit(Event_table$eventID, "_", fixed = T), "[", 2), "-", fixed = T),
                                "[", 2),
                               sapply(strsplit(sapply(strsplit(Event_table$eventID, "_", fixed = T), "[", 2), "-", fixed = T),
                                      "[", 3), sep="-")





write.table(Event_table, file = "../Calculated_data/BALA_event_table.csv", sep = "\t",
            fileEncoding = "utf8", row.names = F)

write.xlsx(Event_table, file = "../Calculated_data/BALA_event_table.xlsx",
            fileEncoding = "utf8", rowNames = F)