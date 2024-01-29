####################################################################################################
#### Writing to MySQL ####
####################################################################################################

# Establish a connection to the MySQL database
con <- dbConnect(RMySQL::MySQL(), 
                 dbname = "bala_arthropods",
                 host = "localhost",
                 port = 3306,
                 user = "root",
                 password = "")


dbExecute(con, "SET FOREIGN_KEY_CHECKS=0")
dbRemoveTable(con,"bala_1_3_dat")
dbRemoveTable(con,"sites")
dbRemoveTable(con,"spp_data")


# Create the 'site_coords' table
dbExecute(con, "
  CREATE TABLE sites (
    `Site_code` VARCHAR(255) PRIMARY KEY,
    `Island` VARCHAR(255),
    `Fragment_Name` VARCHAR(255),
    `Site_number` VARCHAR(255),
    `Grid_zone` VARCHAR(255),
    `Habitat_gen` VARCHAR(255),
    `Habitat_spec` VARCHAR(255),
    `Project` VARCHAR(255),
    `Area` VARCHAR(255),
    `Perimeter` VARCHAR(255),
    `Altitude` FLOAT,
    `Y_coord` FLOAT,
    `X_coord` FLOAT,
    `UTM_E` FLOAT,
    `UTM_N` FLOAT,
    `layer` VARCHAR(255),
    `municipality` VARCHAR(255),
    `Notes` TEXT
  )
")


# Insert data into the 'site_coords' table
dbWriteTable(con, "sites", site_coords, append = TRUE, row.names = FALSE)


# Create the 'spp_data' table
dbExecute(con, "
  CREATE TABLE spp_data (
    `MF` VARCHAR(255) PRIMARY KEY,
    `scientificname` VARCHAR(255),
    `taxonrank` VARCHAR(255),
    `kingdom` VARCHAR(255),
    `phylum` VARCHAR(255),
    `class` VARCHAR(255),
    `order` VARCHAR(255),
    `family` VARCHAR(255),
    `genus` VARCHAR(255),
    `specificepithet` VARCHAR(255),
    `infraspecificepithet` VARCHAR(255),
    `scientificnameauthorship` VARCHAR(255),
    `establishmentmeans` VARCHAR(255),
    `ind_nonind` VARCHAR(255),
    `trophic` VARCHAR(1),
    `IUCN` VARCHAR(255)
  )
")

# Insert data into the 'spp_data' table
dbWriteTable(con, "spp_data", spp_data, append = TRUE, row.names = FALSE)

# Create the 'BALA_1_3_dat' table
dbExecute(con, "
  CREATE TABLE bala_1_3_dat (
    `eventID` VARCHAR(255) PRIMARY KEY,
    `project` VARCHAR(255),
    `site_code` VARCHAR(255),
    `solution_plant` VARCHAR(255),
    `year` INT,
    `month` VARCHAR(255),
    `sampling_method` VARCHAR(255),
    `sample_number` VARCHAR(255),
    `sample_code` VARCHAR(255),
    `MF` VARCHAR(255),
    `A` INT,
    `AM` INT,
    `AF` INT,
    `J` INT,
    `total_abundance` INT,
    `total_adults` INT,
    FOREIGN KEY (site_code) REFERENCES sites(Site_code),
    FOREIGN KEY (MF) REFERENCES spp_data(MF)
  )
")


# Insert data into the 'BALA_1_3_dat' table
dbWriteTable(con, "bala_1_3_dat", BALA_1_3_dat, append = TRUE,
             row.names = FALSE)

dbExecute(con, "SET FOREIGN_KEY_CHECKS=1")


# Close the database connection
dbDisconnect(con)


####################################################################################################