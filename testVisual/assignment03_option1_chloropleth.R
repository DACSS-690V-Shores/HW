
my_packages <- c("tidyverse",
                 "ggplot2",
                 "ggpubr",
                 "paletteer",
                 "rio",
                 "sf",
                 "patchwork") # create vector of packages
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
  # Also keep in mind that since I am requesting TWO tender types, you need to use FACETS.
  # If you decide to go for an interactive version, the justification will be 
  # to use tooltips, interactive legends, and the use of library(leafsync).
  # Please, follow my request on NON USING raw counts, you should normalize the variable.

summary(bostonCont$Amount)
tapply(bostonCont$Amount,bostonCont$`Tender Type Description`,summary)

  # aggregate sum of contributions (Amount) grouped by zip and tender
  # Filtered by my tenders of choice: check and transfer
bostonAggDF <- bostonCont %>%
  group_by(Zip, `Tender Type Description`) %>%
  summarize_at(vars(Amount),
               list(contributionCount = length,
                    amountPerDonor = mean,
                    totalContribution = sum)) %>%
  rename(zip = Zip, 
         tender = `Tender Type Description`) %>%
  filter(tender %in% c('Check', 'Credit Card'))

  # calculate proportion of tender by zip
bostonAggDF <- bostonAggDF %>%
  group_by(zip) %>%
  mutate(contributionPercentage = 
           contributionCount / sum(contributionCount) * 100) %>%
  select(-contributionCount)

  # merge with shape file data
  # map to the left!
  # never: merge(contribsWA_agg,zipMap...)
contrib_zipMap = merge(bostonZips, bostonAggDF, by.x='ZIP5', by.y='zip')

# facet map 1 for contributionPercentage
base1 = ggplot() + theme_void() +
  labs(title = 'Most donors prefer credit card',
       subtitle = 'Boston political campaign contributions in 2024',
       caption = 'Source: Massachusetts Office of Campaign and Political Finance') +
  #ggtitle('Most donors prefer credit card') +
  #theme(strip.text = element_blank()) # remove facet labels
  theme(strip.text = element_text(size = 12)) # increase facet label font size
base1 = base1 + geom_sf(data = contrib_zipMap,
                        aes(fill = contributionPercentage)) + 
  scale_fill_viridis_c(option = "cividis",
                       direction = -1,
                       na.value = 'white') + # missing in white
  facet_grid(tender ~ ., switch = "y") + # 'switch' moves facet labels to the left
  labs(fill='Tender Percentage') 

  # facet map 2 for amountPerDonor
base2 = ggplot() + theme_void() +
  ggtitle('Most large contributions are by check') +
  theme(strip.text = element_blank()) # remove facet labels
base2 = base2 + geom_sf(data = contrib_zipMap,
               aes(fill = amountPerDonor)) + 
  scale_fill_viridis_c(option = "cividis",
                       direction = -1,
                       na.value = 'white', # missing in white
                       labels = scales::dollar) + # added currency formatting
  facet_grid(tender ~ .) +
  labs(fill='Contribution Per Donor') 

# facet map 3 for contribution impact by tender (tender % * contribution)
base3 = ggplot() + theme_void() +
  ggtitle('Campaign impact is concentrated in a few zip codes') +
  theme(strip.text = element_text(size = 14)) # increase facet label font size
base3 = base3 + geom_sf(data = contrib_zipMap,
                        aes(fill = totalContribution)) + 
  scale_fill_viridis_c(option = "cividis",
                       direction = -1,
                       na.value = 'white', # missing in white
                       labels = scales::dollar) +
  facet_grid(tender ~ .) +
  labs(fill='Campaign Totals') 

  # Combine the two plots side by side and print
combined_plot = base1 + base2 + base3 + 
  plot_layout(ncol = 3) +
  plot_annotation(title = 'Contribution tender and geography can guide campaign fundraising strategy',
                  theme = theme(plot.title = element_text(size = 20)))

# write to an R data serialization file
saveRDS(base1, file = "assignment03_option2_chloropleth.rds")
