
my_packages <- c("tidyverse",
                 "ggplot2",
                 "ggpubr",
                 "forcats",
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

arrests <- arrests %>%
  select("Arrest Type", Age) %>%
  rename(Arrest_Type = "Arrest Type")


tapply(arrests$Age,arrests$Arrest_Type, summary)

# unable to find a data dictionary that defines Arrest Type,
# but here's my best guess:
# F = Felony without warrant
# M = Misdemeanor
# O = On-view
# W = Warrant

  # set NA Ages to median age
arrests$Age[is.na(arrests$Age)] <- median(arrests$Age, na.rm = TRUE)
  # set NA Arrest Types to "Unknown"
arrests$Arrest_Type[is.na(arrests$Arrest_Type)] <- "Unknown"

# Replace values in the column
arrests$Arrest_Type <- recode(arrests$Arrest_Type,
                              "F" = "Felony",
                              "O" = "Other",
                              "M" = "Misdemeanor",
                              "W" = "Warrant")

  # Boxplot
baseArrest = ggplot(data = arrests,
                    aes(x = Arrest_Type, 
                        y = Age)) 
boxArrest = baseArrest + geom_boxplot() + labs(title = "woof")

  # Density  
ggplot(arrests) + 
  geom_density(aes(x = Age),show.legend = F) + 
  facet_grid(reorder(Arrest_Type, Age, median) ~ .) 

  # Jitter w/ error bars
  # for colors see https://r-charts.com/color-palettes/#discrete
baseMEANs = ggplot(arrests, aes(x = fct_infreq(Arrest_Type), # forcats to reorder
                                y = Age)) 
jitterMEANs = baseMEANs + geom_jitter(color = "#009292",
                                     alpha = 0.1 #transparency
                                     )
  # layer a boxplot over the jitter point plot
  # the notch shows confidence interval
  # when the notches between two groups don't overlap then
  # it suggests medians are significantly different
  # 
jitterMEANs = jitterMEANs + 
  geom_boxplot(alpha = 0.2, # Adjust alpha for transparency
               fill = "#FFFF6D",
               color = "#924900",  
               linewidth = 1,
               notch = TRUE,
               notchwidth = 0.75) +
    # use μ to indicate where mean is
  geom_text(stat = "summary", 
            label = "μ",
            color = "#924900",
            size = 4,
            fontface = "bold")


# Load up plot information variables
titleText = 'Arrests centered on 30-40 year olds'
sub_titleText = 'Misdemeanor and warrant arrests lead in Massachusetts 2019-2020'
sourceText = 'Source: Massachusetts State Police'
x.AxisText = 'Arrest Type'
y.AxisText = 'Age of Arrestee'
legTitle = 'Greater than \nstatewide mean'
meanAnnotation = paste("Statewide STR mean =", round(meanSTRatio, 2))

# Decorate with contextual info
jitterMEANs = jitterMEANs + 
  labs(title = titleText,
       subtitle = sub_titleText,
       x = x.AxisText,
       y = y.AxisText, 
       caption = sourceText,
       colour = legTitle) +
  theme(plot.caption = element_text(hjust = 0),
        plot.title = element_text(hjust = 0),
        legend.title = element_text(size = ),
        legend.text = element_text(size = 8))
