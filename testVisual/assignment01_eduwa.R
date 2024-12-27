
library(ggplot2)

# get data ------------------------------------------------------------

rm(list = ls()) # clean memory

location='https://github.com/DACSS-Visual/tabular_univar_cat/raw/main/data/'
file='eduwa.rda'
link=paste0(location,file)

#getting the data TABLE from the file in the cloud:
load(file=url(link))


# see data ----------------------------------------------------------

dim(eduwa)
names(eduwa)
head(eduwa)


# see data types ----------------------------------------------------------

str(eduwa, width = 70, strict.width = 'cut')

###

head(eduwa$LocaleType, 20)

# absolute values
absoluteT=table(eduwa$LocaleType,
                exclude = 'nothing') #include all values!
absoluteT

names(absoluteT)[5] = "Unknown"

# percent values 

propT=prop.table(absoluteT)*100
propT

# relative values
prop.table(absoluteT)

# pie(absoluteT) # pies are not the first option.

# as data frame
(tableFreq = as.data.frame(absoluteT))
names(tableFreq) = c("Locale", "Count")

# adding percents:
tableFreq$Percent = as.vector(propT)

#base GGPLOT2 starts with a "base", telling WHAT VARIABLES TO PLOT
base = ggplot(data = tableFreq, 
             aes(x = Locale, # horizontal
                 y = Count)) #vertical

plot1 = base + geom_bar(fill ="gray",
                        stat = 'identity') # notice the "stat"
plot1

titleText='Where are Public Schools located?'
sub_titleText='Washington State - 2019'
sourceText='Source: US Department of Education'
# are these obvious?
x.AxisText="Locations"
y.AxisText="Count"

plot2 = plot1 + labs(title = titleText,
                     subtitle = sub_titleText,
                     x = x.AxisText,
                     y = NULL, #y.AxisText
                     caption = sourceText) 
plot2

# begin again with Percent col

base = ggplot(data = tableFreq, 
             aes(x = Locale,
                 y = Percent)) 

plot1 = base + geom_bar(fill ="gray",
                        stat = 'identity') 

plot2 = plot1 + labs(title = titleText,
                     x = NULL, 
                     y = NULL,
                     caption = sourceText)

plot3 = plot2 + geom_hline(yintercept = 25, #where
                           linetype = "dashed", 
                           linewidth = 1.5, #thickness
                           alpha = 0.5) #transparency
plot3

# customize Y axis
# Custom Y-axis labels 
labels <- function(x) {
  paste(x, "%")
}

plot4 = plot3 + scale_y_continuous(breaks = c(0, 10, 25, 40),
                                   limits = c(0, 40), 
                                   labels = labels) 
plot4

#positions: 0 left / 1 right / 0.5 center
plot5 = plot4 + theme(plot.caption = element_text(hjust = 0), 
                      plot.title = element_text(hjust = 0.5))
plot5

bar_labels = paste0(round(tableFreq$Percent,2), '%')

plot6 = plot5 + geom_text(vjust=0, #hjust if flipping
                          size = 5,
                          aes(y = Percent,
                              label = bar_labels))
plot6 # + coord_flip() # wanna flip the plot?

# left off at the text, 
# Bar plots are the default option for categorical variables. In general, you see the distribution of the classification, which allows you to identify concentration. For that reason, ordering the bars by height can be helpful. Let´s redo everything, but reordering the bars by percent values: