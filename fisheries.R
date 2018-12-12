library(DBI)
library(bigrquery)

BQ_connection <-  dbConnect(bigquery(), 
                            project = 'global-fishing-watch',
                            dataset = "global_footprint_of_fisheries", 
                            billing = "milliechapman", # your billing account name
                            use_legacy_sql = FALSE) # specify we are using Standard SQL

DBI::dbListTables(BQ_connection)

library(tidyverse) #loads dplyr and friends

fishing_vessels <- dplyr::tbl(BQ_connection, 
                              "fishing_vessels") # connects to a table

fishing_vessels


# install.packages('devtools')
library(rfisheries)
of_landings()
of_landings(country = "USA")
of_landings(species = "SKJ")
of_species_codes()

library(rfishbase)
mangrove<- habitatSearch("mangrove")
reef <- which_fish("reef", "habitat", fish.data)
