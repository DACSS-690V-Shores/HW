
my_packages <- c("tidyverse",
                 "ggplot2",
                 "ggpubr",
                 "rio") # create vector of packages
invisible(lapply(my_packages, require, character.only = TRUE)) # load multiple packages

# get data ------------------------------------------------------------

# Massachusetts State Police arrest details.
linkMass="https://github.com/DACSS-Visual/tabular_bivar_catcat/raw/refs/heads/main/data/MSP%20DFS%20Arrests%2019-20Q1.xlsx"

#see it
arrests = rio::import(linkMass,which = 1)
head(arrests)

# Use the columns Arrest Type and Age to make a visual. 
# The excel file has the data in the first sheet; 
# the second one has the Arrest Type codes (5 codes).


#

rm(list = ls()) # clean memory

location = "https://github.com/DACSS-Visual/tabular_bivar_catcat/raw/main/data/"
file = 'crime.RData'
link = paste0(location,file)

#getting the data TABLE from the file in the cloud:
load(file = url(link))

# filter for specific years
yearsSelected = 2011:2015
subCrime = crime[crime$year %in% yearsSelected, ]
subCrime = subCrime[complete.cases(subCrime), ]

# The goal is to find a visualization to represent the behavior of 
# a numerical variable in each level of a categorical variable. 
# Let’s choose one of each and see the descriptives:
# tapply(subCrime$DaysToReport, subCrime$Precinct, summary)

# As there seems to be lots of asymmetry, let’s explore with boxplots 
# several lengths using ggarrange from ggpubr:

baseDay = ggplot(data = subCrime,
                 aes(x = Precinct,
                     y = DaysToReport))
boxDay = baseDay + geom_boxplot() + 
  labs(title = "daily")
baseWeek = ggplot(data = subCrime[subCrime$DaysToReport > 7, ],
                  aes(x = Precinct, 
                      y = DaysToReport))
boxWeek = baseWeek + geom_boxplot() +
  labs(title = "> week")
baseMonth = ggplot(data = subCrime[subCrime$DaysToReport > 30, ],
                   aes(x = Precinct,
                       y = DaysToReport))
boxMonth = baseMonth + geom_boxplot() + 
  labs(title = "> month")
baseYear = ggplot(data = subCrime[subCrime$DaysToReport > 365, ],
                  aes(x = Precinct,
                      y = DaysToReport))
boxYear = baseYear + geom_boxplot() +
  labs(title = "> year")

ggarrange(boxDay, boxWeek, boxMonth, boxYear)

# Let’s build our visual from the crimes that took ONE year or longer to report.
crimePrecinct = subCrime[subCrime$DaysToReport >= 365, ]
crimePrecinct$yearsToReport = crimePrecinct$DaysToReport / 365

# In general, we want to see if the distribution is different across levels:
kruskal.test(yearsToReport ~ Precinct, data = crimePrecinct)

# There is a significant probability (0.1) that some precinct is different 
# from another; this can be identified here:
pairwise.wilcox.test(crimePrecinct$yearsToReport, crimePrecinct$Precinct)
# Arguably, EAST might differ from NORTH; and WEST from NORTH. 
# What plot may help us show that?

# Let’s redo the boxplot and histograms:

baseBox = ggplot(data = crimePrecinct,
                 aes(y = yearsToReport))
baseBox + geom_boxplot(aes(x = reorder(Precinct, yearsToReport, median))) +
  coord_flip()

# Density plots?
ggplot(crimePrecinct) + 
  geom_density(aes(x = yearsToReport), show.legend = F) +
  facet_grid(reorder(Precinct, yearsToReport, median) ~ .)

# Histogram?
baseHist = ggplot(data = crimePrecinct, 
                  aes(x = yearsToReport))
baseHist + geom_histogram() + facet_grid(reorder(Precinct, yearsToReport, median) ~ .)
