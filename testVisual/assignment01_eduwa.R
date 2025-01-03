

my_packages <- c("tidyverse",
                 "ggplot2") # create vector of packages
invisible(lapply(my_packages, require, character.only = TRUE)) # load multiple packages

# get data ------------------------------------------------------------

rm(list = ls()) # clean memory

location = 'https://github.com/DACSS-Visual/tabular_univar_cat/raw/main/data/'
file = 'eduwa.rda'
link = paste0(location,file)

#getting the data TABLE from the file in the cloud:
load(file = url(link))


# see data ----------------------------------------------------------

dim(eduwa)
names(eduwa)
head(eduwa)


# see data types ----------------------------------------------------------

str(eduwa, width = 70, strict.width = 'cut')

###

# Calculate mean Student Teacher Ratio for entire Washington data set
meanSTRatio = summary(eduwa$Student.Teacher.Ratio)[[4]]

# Create new df with just the Rural locales, get rid of empty factor levels
ruralEduwa = eduwa[eduwa$LocaleType == 'Rural',]
ruralEduwa$LocaleSub = droplevels(ruralEduwa$LocaleSub)
ruralEduwa$LocaleType = droplevels(ruralEduwa$LocaleType)

# Let's not assume 'unknowns' are rural
ruralEduwa = ruralEduwa %>% drop_na(LocaleType)

# means of Student Teacher ratio for each sub
ruralEduwa_STRatio = ruralEduwa %>%
  group_by(LocaleSub) %>%
  summarise_at(vars(Student.Teacher.Ratio), list(name = mean), na.rm = TRUE)

# Rename variables
names(ruralEduwa_STRatio) = c("RuralLocale", "mean_Student.Teacher.Ratio")

# Remove the string "Rural: " from factor levels
levels(ruralEduwa_STRatio$RuralLocale) <- c("Fringe","Distant","Remote")

# Calculate difference of rural STR from total STR
ruralEduwa_STRatio$difference = 
  ruralEduwa_STRatio$mean_Student.Teacher.Ratio - meanSTRatio

# a new column for color: Greater Than Mean (gtMean)
ruralEduwa_STRatio$gtMean <-
  ifelse(ruralEduwa_STRatio$difference > 0, "Yes", "No")
ruralEduwa_STRatio$gtMean <- 
  factor(ruralEduwa_STRatio$gtMean,
         levels = c("Yes", "No"))

# Load up plot information variables
titleText = 'Rural public schools show smaller classroom sizes'
sub_titleText = 'Student teacher ratios (STR) of rural schools in Washington, 2019'
sourceText = 'Source: US Department of Education'
x.AxisText = 'Rural sub locales'
y.AxisText = 'Difference from statewide STR mean'
legTitle = 'Greater than \nstatewide mean'
meanAnnotation = paste("Statewide STR mean =", round(meanSTRatio, 2))


# Initialize base plot, reorder by mean
# Color-blind colors chosen from:
# https://davidmathlogic.com/colorblind/
base = ggplot(data = ruralEduwa_STRatio, 
              aes(x = reorder(RuralLocale, difference), 
                  y = difference,
                  color = gtMean,
                  label = paste("mean\n",
                                round(mean_Student.Teacher.Ratio, 1))),
              hjust = 0) +
  scale_color_manual(values = 
                       c(Yes = "#882255", 
                         No = "#117733")) +
  geom_text(nudge_x = 0.3, # to the right
            size = 3,
            show.legend = FALSE) 
base = base + theme_classic()

# lollipop style
base = base + 
  geom_segment(aes(y = 0,
                   x = reorder(RuralLocale, difference),
                   yend = difference,
                   xend = reorder(RuralLocale, difference)),
               color = "grey50") + 
  geom_point(size = 5)

# Draw horizontal line (yintercept) representing mean Student.Teacher.Ratio
# from the original data set, 
# including all locales (City, Suburb, Town, Unknown) in addition to Rural.
base = base + 
  geom_hline(yintercept = 0, 
             linewidth = 1.5, #thickness
             alpha = 0.5) + #transparency
  annotate(geom = "text", 
           x = 0.5, 
           y = 0.3, 
           label = meanAnnotation,
           hjust = 0,
           vjust = 1,
           size = 3)

# Lollipop labels mean we don't need Y-axis

# base = base + 
#  theme(axis.title.y = element_blank(),
#        axis.text.y = element_blank(),
#        axis.line.x = element_blank(), # can also tidy up x-axis
#        axis.ticks.x = element_blank())

base = base + 
  theme(axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank())

# Decorate with contextual info
base = base + 
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


# write to an R data serialization file
saveRDS(base, file = "assignment01_eduwa.rds")






