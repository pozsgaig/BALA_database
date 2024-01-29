#### Event ID ####

Occ_table <- BALA_all_data %>% 
  select(
    eventID = eventID,
    MF,
    scientificName,
    kingdom,
    phylum,
    class,
    order,
    family,
    genus,
    specificEpithet,
    infraspecificEpithet,
    taxonRank,
    scientificNameAuthorship,
    total_adults,
    total_abundance, 
    A, 
    AM, 
    AF, 
    J,
    OccurrenceRemarks = solution_plant,
    project,
    identifiedBy)


Occ_table$dateIdentified <- as.numeric(BALA_all_data$year + 1)
Occ_table$dynamicProperties = ifelse(is.na(BALA_all_data$IUCN),
                                     NA, 
                                     paste0("iucnStatus:", BALA_all_data$IUCN)) #ask PAULO

#### Fix information column ####
Occ_table$type <- "PhysicalObject"
Occ_table$license <- "https://creativecommons.org/licenses/by/4.0/legalcode"
Occ_table$institutionID <- "0a80feca-b411-428a-a34e-c5c2fa07cdba"
Occ_table$collectionID <- "1366b359-8936-4e40-be36-1f1e1eb6d2b0"
Occ_table$institutionCode <- "UAc"
Occ_table$collectionCode <- "DTP"
Occ_table$DatasetName <- "BALA project"
Occ_table$basisOfRecord <- "PreservedSpecimen"
Occ_table$occurrenceID <- UUIDgenerate(n = nrow(Occ_table))
Occ_table$organismQuantityType <- "individuals"
Occ_table$recordedBy <- "Borges, P.A.V."


    #### Sex ####
Occ_table$sex<-NA

temp_table_sex <- Occ_table[Occ_table$AF != 0 | Occ_table$AM != 0, ]

    # Females
temp_table_sex$females <- NA

temp_table_sex$females[as.numeric(temp_table_sex$AF) > 1] <- 
  paste(as.numeric(temp_table_sex$AF[as.numeric(temp_table_sex$AF) > 1]), "females", sep = " ")

temp_table_sex$females[as.numeric(temp_table_sex$AF) == 1] <- 
  paste(as.numeric(temp_table_sex$AF[as.numeric(temp_table_sex$AF) == 1]), "female", sep = " ")

    # Males
temp_table_sex$males <- NA

temp_table_sex$males[as.numeric(temp_table_sex$AM) > 1] <- 
  paste(as.numeric(temp_table_sex$AM[as.numeric(temp_table_sex$AM) > 1]), "males", sep = " ")
temp_table_sex$males[as.numeric(temp_table_sex$AM) == 1] <- 
  paste(as.numeric(temp_table_sex$AM[as.numeric(temp_table_sex$AM) == 1]), "male", sep = " ")

    # Combining
temp_table_sex$sex[!is.na(temp_table_sex$males) & !is.na(temp_table_sex$females)] <- 
  paste(temp_table_sex$males[!is.na(temp_table_sex$males) & !is.na(temp_table_sex$females)], 
        temp_table_sex$females[!is.na(temp_table_sex$males) & !is.na(temp_table_sex$females)], 
        sep = " | ")

temp_table_sex$sex[!is.na(temp_table_sex$males) & is.na(temp_table_sex$sex)] <- 
  temp_table_sex$males[!is.na(temp_table_sex$males) & is.na(temp_table_sex$sex)]

temp_table_sex$sex[!is.na(temp_table_sex$females) & is.na(temp_table_sex$sex)] <- 
  temp_table_sex$females[!is.na(temp_table_sex$females) & is.na(temp_table_sex$sex)]


# writing sex to the sex column when AM/AF are not 0
Occ_table[Occ_table$AF != 0 | Occ_table$AM != 0, "sex"]<-temp_table_sex$sex


    #### Life stage ####

    #Juveniles

Occ_table$juv <- NA

Occ_table$juv[Occ_table$J > 1] <- paste(as.numeric(Occ_table$J[Occ_table$J > 1]), "juveniles", sep = " ")
Occ_table$juv[Occ_table$J == 1] <- paste(as.numeric(Occ_table$J[Occ_table$J == 1]), "juvenile", sep = " ")

    # Adults
Occ_table$Ad <- NA

Occ_table$Ad[Occ_table$total_adults > 1] <- 
  paste(as.numeric(Occ_table$total_adults[Occ_table$total_adults > 1]), 
        "adults", sep = " ")
Occ_table$Ad[Occ_table$total_adults == 1] <- 
  paste(as.numeric(Occ_table$total_adults[Occ_table$total_adults == 1]), 
        "adult", sep = " ")

    # Combining
Occ_table$lifeStage <- paste(
  Occ_table$juv, 
  Occ_table$Ad, 
        sep = " | "
    )

Occ_table$lifeStage <- str_remove_all(Occ_table$lifeStage, "NA \\| ")
Occ_table$lifeStage <- str_remove_all(Occ_table$lifeStage, " \\| NA")


Occ_table <- Occ_table %>% 
  select(-juv, -Ad, -A, -AM,-AF, -J, -total_adults) %>% 
  rename("organismQuantity" = "total_abundance", "identificationRemarks" = "MF")



    #### Establishment Means ####
Occ_table$establishmentMeans <- as.character(BALA_all_data$establishmentMeans)
Occ_table$establishmentMeans[Occ_table$establishmentMeans == "E"] <- "endemic"
Occ_table$establishmentMeans[Occ_table$establishmentMeans == "I"] <- "introduced"
Occ_table$establishmentMeans[Occ_table$establishmentMeans == "Indeterminate"] <- "indeterminate"
Occ_table$establishmentMeans[Occ_table$establishmentMeans == "N"] <- "native"


    #### Final building ####
    Occ_table <- select(Occ_table,
        type,
        license,
        institutionID,
        collectionID,
        collectionCode,
        institutionCode,    
        DatasetName,
        basisOfRecord,
        recordedBy,
        occurrenceID,
        organismQuantity,
        organismQuantityType,
        sex,
        lifeStage,
        establishmentMeans,    
        eventID,
        identifiedBy,
        dateIdentified,
        scientificName,
        kingdom,
        phylum,
        class,
        order,
        family,
        genus,
        specificEpithet,
        infraspecificEpithet,
        taxonRank,
        scientificNameAuthorship,
        identificationRemarks,
        dynamicProperties, 
        project)


Occ_table$identifiedBy[Occ_table$project=="BALA3"] <-"Borges, P.A.V. | Abrão Leite"
Occ_table$recordedBy[Occ_table$project=="PhD_CG"] <-"C. Gaspar | Borges, P.A.V."


Occ_table$project<-NULL

# Occ_table$identificationRemarks<-substring(Occ_table$identificationRemarks, 3, nchar(Occ_table$identificationRemarks))
# aaa<-Occ_table[is.na(Occ_table$establishmentMeans),] # only NPIs

write.table(Occ_table, file = "../Calculated_data/BALA_occ_table.csv", sep = "\t",
            fileEncoding = "utf8", row.names = F)

write.xlsx(Occ_table, file = "../Calculated_data/BALA_occ_table.xlsx", 
            fileEncoding = "utf8", rowNames = F)
