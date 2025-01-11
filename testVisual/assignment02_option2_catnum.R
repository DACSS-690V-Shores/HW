
my_packages <- c("tidyverse",
                 "ggplot2",
                 "ggpubr",
                 #"forcats",
                 "paletteer",
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

  # set NA Ages to median age
arrests$Age[is.na(arrests$Age)] <- median(arrests$Age, na.rm = TRUE)
  # set NA Arrest Types to "Unknown"
arrests$Arrest_Type[is.na(arrests$Arrest_Type)] <- "Unknown"
  # Replace values in the column
  # (codebook in source spreadsheet)
arrests$Arrest_Type <- recode(arrests$Arrest_Type,
                              "F" = "Felony",
                              "O" = "Other",
                              "M" = "Misdemeanor",
                              "W" = "Warrant")

  # Calculate counts for each Arrest_Type
counts <- arrests %>%
  group_by(Arrest_Type) %>%
  summarize(Count = n())


  # Reorder Arrest_Type by Count (greatest to least)
arrests$Arrest_Type <- factor(arrests$Arrest_Type, 
                              levels = counts %>% 
                                arrange(desc(Count)) %>% 
                                pull(Arrest_Type))

# Load up plot information variables
titleText = 'Misdemeanors and warrants lead Massachusetts arrests'
sub_titleText = 'State police arrests in 2019-2020 centered on 30-35 year olds'
sourceText = 'Source: Massachusetts State Police'
x.AxisText = 'Age of Arrestee'

  # Get the number of unique arrest types
  # (Only for color scale below - can remove this when remove color scale)
n_types <- length(unique(arrests$Arrest_Type))

base = ggplot(arrests, aes(x = Age,
                           y = reorder(Arrest_Type, Arrest_Type, function(x) length(x)),
                           fill = Arrest_Type)) +
    # uses order by Arrest_Type count (descending) to fill scale and create legend
    # colors from https://r-charts.com/color-palettes/#discrete
  scale_fill_manual(values = 
                      paletteer::paletteer_d("colorBlindness::LightBlue2DarkBlue7Steps")[1:n_types],
                    labels = 
                      paste0(counts %>% arrange(desc(Count)) %>% pull(Arrest_Type), 
                             " (n=", 
                             counts %>% arrange(desc(Count)) %>% pull(Count), 
                             ")")) +
  guides(fill = guide_legend(title = "Arrest Type (Count)")) +
  theme(legend.position = "right")




jitter = base + geom_jitter(color = "#6DB6FF",
                            alpha = 0.2, #transparency
                            size = 0.5)
  # layer a boxplot over the jitter point plot
  # the notch shows confidence interval
  # when the notches between two groups don't overlap then
  # it suggests medians are significantly different
  # 
box = jitter + 
  geom_boxplot(alpha = 0.7, # Adjust alpha for transparency
               color = "gray50", 
               linewidth = 0.7,
               notch = TRUE,
               notchwidth = 0.75,
               width = 0.6) +
    # Adds spacing above and below
  scale_y_discrete(expand = expansion(mult = c(0.2, 0)))  

  # calculate first quantile by type to figure out position for in-graph labels
q1_values <- arrests %>%
  group_by(Arrest_Type) %>%
  summarize(Q1 = quantile(Age, 0.25))

  # Testing: Update the plot with "Hello!" labels at Q1 positions
q1labs <- box + 
  geom_text(data = q1_values,
            aes(x = Q1, y = Arrest_Type, label = "Hello!"),
            color = "black",
            size = 3,
            hjust = 0)  # Set hjust = 0 to align left edge of text with Q1

# Decorate with contextual info
final = q1labs + 
  labs(title = titleText,
       subtitle = sub_titleText,
       x = x.AxisText,
       caption = sourceText) +
  scale_x_continuous(breaks = seq(min(arrests$Age), max(arrests$Age), by = 10)) +
  theme(plot.caption = element_text(hjust = 0),
        plot.title = element_text(hjust = 0),
        plot.subtitle = element_text(size = 10,
                                     color = "gray50"),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_line(color = "gray80", linewidth = 0.5),
        panel.background = element_blank(),  # Remove panel background
        plot.background = element_blank(),  # Remove plot background
        axis.title.y = element_blank(),   # Remove y-axis label
        axis.text.y = element_blank(),    # Remove y-axis elements
        panel.grid.major.x = element_line(color = "gray80", linewidth = 0.5),  # Add vertical grid lines
        legend.title = element_text(size = 8),  # Adjust title size
        legend.text = element_text(size = 8),    # Adjust text size
          # Move x-axis label to the left
        axis.title.x = element_text(hjust = 0.04, 
                                    size = 10,
                                    color = "gray50"))

# write to an R data serialization file
saveRDS(final, file = "assignment02_option2_catnum.rds")

