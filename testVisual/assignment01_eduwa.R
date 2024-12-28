

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
titleText = 'Student Teacher Ratios in Rural Public Schools'
sub_titleText = 'Difference from the mean STR of all schools in Washington, 2019'
sourceText = 'Source: US Department of Education'
x.AxisText = "Rural Sub Locations"
y.AxisText = "Difference from mean STR"
legTitle = "Greater than Mean"

# Initialize base plot, reorder by mean
base = ggplot(data = ruralEduwa_STRatio, 
              aes(x = reorder(RuralLocale, difference), 
                  y = difference,
                  color = gtMean,
                  label = round(mean_Student.Teacher.Ratio,1))) +
  scale_color_manual(values = 
                       c(Yes = "#882255", 
                         No = "#117733")) +
  geom_text(nudge_x = 0.3, # to the right
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
base = base + geom_hline(yintercept = 0, 
                         linetype = "dashed", 
                         linewidth = 1.5, #thickness
                         alpha = 0.5) #transparency

# Decorate with contextual info
base = base + 
  labs(title = titleText,
       subtitle = sub_titleText,
       x = x.AxisText,
       y = y.AxisText, 
       caption = sourceText,
       colour = legTitle) +
  theme(plot.caption = element_text(hjust = 0),
        plot.title = element_text(hjust = 0))








