
my_packages <- c("tidyverse",
                 "ggplot2",
                 "ggpubr",
                 "paletteer",
                 "rio",
                 "sf") # create vector of packages
invisible(lapply(my_packages, require, character.only = TRUE)) # load multiple packages

  # get data ------------------------------------------------------------
  
  # Data from Massachusetts Office of Campaign and Political Finance
  # https://www.ocpf.us/Home/Index
  # This data has a subsample for Boston.
linkBoston = "https://github.com/DACSS-Visual/SpatialData/raw/refs/heads/main/data/BostonContrib.xlsx"
bostonCont = rio::import(linkBoston)

  # Map of zipcodes
linkZips = 'https://raw.githubusercontent.com/DACSS-Visual/SpatialData/refs/heads/main/data/zip_codes.json'
bostonZips = sf::read_sf(linkZips)

  # Compare the contributions of two tender types (you choose which), 
  # by zip zode. The results can be interactive, but it is NOT compulsory.
  # 
  # NOTE: You need to aggregate the data from the excel file, 
  # and then merged that aggregated data into the map. 
  # Then, you can plot the chloropleths with the data as it is (countinuous) 
  # or discretize it before plotting.
  # 
  # From Jose: 
  # Remember that the map need NOT be interactive. 
  # Also keep in mind that since I am requesting  TWO tender types, you need to use FACETS.
  # If you decide to go for an interactive version, the justification will be 
  # to use tooltips, interactive legends, and the use of library(leafsync).
  # Please, follow my request on NON USING raw counts, you should normalize the variable.

summary(bostonCont$Amount)
tapply(bostonCont$Amount,bostonCont$`Tender Type Description`,summary)


