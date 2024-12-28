
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

ruralEduwa = eduwa[eduwa$LocaleType=='Rural',]
ruralEduwa$LocaleSub = droplevels(ruralEduwa$LocaleSub)
table(ruralEduwa$LocaleSub)
ruralEduwa$LocaleType = droplevels(ruralEduwa$LocaleType)
head(ruralEduwa$LocaleType, 20)

# absolute values
absoluteS = table(ruralEduwa$LocaleSub)
absoluteS

# percent values 
propS = prop.table(absoluteS) * 100
propS

# relative values
prop.table(absoluteS)

# as data frame
(tableFreq = as.data.frame(absoluteS))
names(tableFreq) = c("RuralLocale", "Count", "Percent")

# adding percents:
tableFreq$Percent = as.vector(propS)

# Remove "Rural: " from factor levels
levels(tableFreq$RuralLocale) <- c("Fringe","Distant","Remote")

# Initialize base plot, reorder by percentage
base = ggplot(data = tableFreq, 
              aes(x = reorder(RuralLocale, Percent), 
                  y = Percent))
base = base + theme_classic()

# lollipop style
base = base + geom_segment(aes(y = 0,
                               x = reorder(RuralLocale,Percent),
                               yend = Percent,
                               xend = reorder(RuralLocale,Percent)), 
                               color = "grey50") 

# Custom Y-axis labels: Decorate with percent symbol
labels <- function(x) {
  paste(x, "%")
}
base = base + scale_y_continuous(breaks = c(0, 10, 25, 40),
                                 limits = c(0, 40),
                                 labels = labels)





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


plot4 = plot3 
plot4

#positions: 0 left / 1 right / 0.5 center
plot5 = plot4 + theme(plot.caption = element_text(hjust = 0), 
                      plot.title = element_text(hjust = 0.5))
plot5



plot6 = plot5 + geom_text(vjust=0, #hjust if flipping
                          size = 5,
                          aes(y = Percent,
                              label = bar_labels))
plot6 # + coord_flip() # wanna flip the plot?

