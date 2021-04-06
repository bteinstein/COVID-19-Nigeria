---
  title: "00. Learning"
author: "Babatunde Adebayo"
date: "8/21/2020"
output: html_document
---
  
  ```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## Basic Idea
()
The basic idea of this viz is circular barplot. I saw this a part of #TidyTuesday challenge contribution by Joseph Pope (Aug 18, 2020)
on endangered plant.

## Recreation Attempt
```{r setup}
library(tidyverse)
library(extrafont)
library(ggthemes)
library(kableExtra)
```

```{r data}
tuesdata <- tidytuesdayR::tt_load(2020, week = 34)
threats <- tuesdata$threats

# readr::write_csv(tuesdata$plants, path = here::here("data",'plant.csv'))
# readr::write_csv(tuesdata$actions, path = here::here("data",'actions.csv'))
# readr::write_csv(tuesdata$threats, path = here::here("data",'threats.csv'))

# plants <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-18/plants.csv')
# actions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-18/actions.csv')
# threats <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-18/threats.csv')

rm(tuesdata)
```

### Explore Data
Attempt to use {skimr} package to get a sense of overview about the size of your data, missing values, mean rates, and a basic histogram to show the distribution of numerical data-type columns.
```{r exploration}
skimr::skim(threats)
```

- We have 500 unique plants in this dataset base on their Specie name (Genus+species) comprising of 6 taxonomic groups
- They type of threats for these plants are grouped into 12 different classes
- Data collected are from 72 countries in 6 continents
- All variable/column are complete except year_last_seen with 180 (3%) missing values
- threatened, which is the only numerical variable out of 8 variables in total is binary in nature. 1 indicates the plant is endangered (threatened with extinction) and 0 otherwise


#### Exploring futher
```{r type-of-threat-vs-threat-status}
threats %>% group_by(threat_type) %>%  count(threatened)
table(threats$threat_type, threats$threatened)
```
The data shows that the top threat to this plants are related to Agriculture & Aquaculture, Biological Resources Use, Transportation Corridor and Natural System Modification. Interestingly, Pollution is found to be the least threat factor to threaten - As expected fellow plants are really protecting themeselves in detoxification.


### Data Processing
Getting data ready for visualization. 
- Here the focus here are threatened plants by country and continent
- Saint Helena will be used as a truncted name for the Country Saint Helena, Ascension and Tristan da Cunha is too long, 

Basic idea here is to filter just only threatened plant, group by continents then country and count.

I created two versions, one scaled count between 0-100 and unscaled count
```{r data_prep}
# data_scaled 
data <- threats %>% 
  filter(threatened == 1) %>% 
  group_by(continent, country) %>% 
  count(name = "threatened_plants", sort = TRUE) %>% 
  ungroup() %>% 
  mutate(continent = factor(continent),
         country = ifelse(str_detect(country, "Saint Helena"), "Saint Helena", country),
         threatened_plants = scales::rescale(threatened_plants, to = c(0, 100))) %>% 
  filter(threatened_plants != 0)



data_unscaled <- threats %>% 
  filter(threatened == 1) %>% 
  group_by(continent, country) %>% 
  count(name = "threatened_plants", sort = TRUE) %>% 
  ungroup() %>% 
  mutate(continent = factor(continent),
         country = ifelse(str_detect(country, "Saint Helena"), "Saint Helena", country))
data_unscaled
```

### Plotting Setup
The goal is to create circular barplot plot with extra-sauce of grouped format structure. The extra-sauce here include some extra row space to separate groups, arc line for group labelling and smooth grid for y-axis labelling. 
Hence, there is a need for some data preps and geometry calculations (not to worry on this data-to-viz to the rescue).

The basic steps are as follow:
  1. Create empty bar rows in the data to allow grouping 
2. Build labels and add to data. In this case, we will label the country per bar and use continent for the grouping. More importantly for angular geometry for labels rotation.
3. Base Data - Dataframe handles continent labelling and line arc aesthetics. 
4. Grid Data - A dataframe to structure y-axis scale (It basically acts as the y-axis label) -  in the case of the scaled data it will handle bar of 0-20-40-60-80 

```{r plot_setup}
# Step 1
# Set a number of 'empty bar' to add at the end of each group
empty_bar=3
to_add = data.frame( matrix(NA, empty_bar*nlevels(data$continent), ncol(data)) )

colnames(to_add) = colnames(data)
to_add$continent = rep(levels(data$continent), each=empty_bar)

data= full_join(data, to_add)   %>%  #rbind(data, to_add)
  arrange(continent) %>% 
  mutate(id = row_number())
# Step 2
# Get the name and the y position of each label
label_data=data
number_of_bar=nrow(label_data)
angle= 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_data$hjust<-ifelse( angle < -90, 1, 0)
label_data$angle<-ifelse(angle < -90, angle+180, angle)

# step 3
# prepare a data frame for base lines
base_data=data %>% 
  group_by(continent) %>% 
  summarize(start=min(id), end=max(id) - empty_bar) %>% 
  rowwise() %>% 
  mutate(title=mean(c(start, end)))

# Step 4
# prepare a data frame for grid (scales)
grid_data = base_data
grid_data$end = grid_data$end[ c( nrow(grid_data), 1:nrow(grid_data)-1)] + 1
grid_data$start = grid_data$start - 1
grid_data=grid_data[-1,]
```
### Build Plot
Most of the code are patterned on that of sample provided by data-to-viz https://www.data-to-viz.com/graph/circularbarplot.html
with few modification - removing redundate code, margin correction and theme

#### Scaled Data
```{r build_plot, fig.height=9, fig.width=8}
p <- ggplot(data, aes(x=as.factor(id), y=threatened_plants, fill=continent)) + #NB id variable is a factor
  geom_bar(stat = "identity", alpha =0.5) +
  # Add a val=100/75/50/25 lines. I do it at the beginning to make sur barplots are OVER it.
  geom_segment(data=grid_data, aes(x = end, y = 80, xend = start, yend = 80), colour = "white", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 60, xend = start, yend = 60), colour = "white", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 40, xend = start, yend = 40), colour = "white", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 20, xend = start, yend = 20), colour = "white", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  
  # Add text showing the value of each 100/75/50/25 lines # Modify this accordingly for an unscaled data
  annotate("text", x = rep(max(data$id),4), y = c(20, 40, 60, 80), label = c("20", "40", "60", "80") , color="white", size=3 , angle=0, fontface="bold", hjust=1) +
  # geom_bar(aes(x=as.factor(id), y=threatened_plants, fill=continent), stat="identity", alpha=0.5) +
  ylim(-100,120) +
  labs(
    title = "Number of Threatened Plants by Geo",
    subtitle = "Scaled from 0-100",
    caption = "Joseph Pope | @joepope44"
  ) + 
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_blank(), # Modify this to keep x or y axis text
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.background = element_rect(fill = "#a3b18a"),
    plot.title = element_text(hjust = 0.5, size = 24, family  = "Garamond"),
    plot.subtitle = element_text(hjust = 0.5, family  = "Garamond"),
    plot.caption = element_text(hjust = 0.5, family = "Garamond")
    # plot.margin = unit(rep(-1,4), "cm") <- this hides the plot title if not commented out
  ) + 
  coord_polar() + # Make it a circle
  # add labels - save to remove inherit.aes = FALSE (soft-deprecated I believe)
  geom_text(data=label_data, aes(x=id, y=threatened_plants+10, label=country, hjust=hjust), color="black", fontface="bold",alpha=0.7, size=3.5, angle= label_data$angle) +
  # Add base line information - Arc (a line in a polar coordinate) Then Label ( gridText Can be use to enhance text placement - say for a two line display)
  geom_segment(data=base_data, aes(x = start, y = -5, xend = end, yend = -5), colour = "black", alpha=0.8, size=0.6 , inherit.aes = FALSE )  +
  geom_text(data=base_data, aes(x = title, y = -18, label=continent),
            hjust=c(1,1,.5,0,0,0), colour = "black", alpha=0.8, size=4, 
            fontface="bold", inherit.aes = FALSE) 

```



### Unscaled Data
```{r}
empty_bar=3
to_add = data.frame( matrix(NA, empty_bar*nlevels(data_unscaled$continent), ncol(data_unscaled)) )

colnames(to_add) = colnames(data_unscaled)
to_add$continent = rep(levels(data_unscaled$continent), each=empty_bar)

data_uscl <- full_join(data_unscaled, to_add)   %>%  #rbind(data, to_add)
  arrange(continent) %>% 
  mutate(id = row_number())


# Step 2
# Get the name and the y position of each label
label_data=data_uscl
number_of_bar=nrow(label_data)
angle= 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_data$hjust<-ifelse( angle < -90, 1, 0)
label_data$angle<-ifelse(angle < -90, angle+180, angle)

# step 3
# prepare a data frame for base lines
base_data=data_uscl %>% 
  group_by(continent) %>% 
  summarize(start=min(id), end=max(id) - empty_bar) %>% 
  rowwise() %>% 
  mutate(title=mean(c(start, end)))

# Step 4
# prepare a data frame for grid (scales)
grid_data = base_data
grid_data$end = grid_data$end[ c( nrow(grid_data), 1:nrow(grid_data)-1)] + 1
grid_data$start = grid_data$start - 1
grid_data=grid_data[-1,]


```

```{r build_plot, fig.height=8, fig.width=7}
ggplot(data_uscl, aes(x=as.factor(id), y=threatened_plants, fill=continent)) + #NB id variable is a factor
  geom_bar(stat = "identity", alpha =0.5) +
  # Add a val=100/75/50/25 lines. I do it at the beginning to make sure barplots are OVER it.
  geom_segment(data=grid_data, aes(x = end, y = 200, xend = start, yend = 200), colour = "white", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 150, xend = start, yend = 150), colour = "white", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 100, xend = start, yend = 100), colour = "white", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 50, xend = start, yend = 50), colour = "white", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  # Add text showing the value of each 100/75/50/25 lines # Modify this accordingly for an unscaled data
  annotate("text", x = rep(max(data$id),4), y = c(50, 100, 150, 200), label = c("50", "100", "150", "200") , color="white", size=3 , angle=0, fontface="bold", hjust=1) +
  # geom_bar(aes(x=as.factor(id), y=threatened_plants, fill=continent), stat="identity", alpha=0.5) +
  ylim(-320,300) + # Modify Y Lim
  labs(
    title = "Number of Threatened Plants by Geo",
    subtitle = "Scaled from 0-100",
    caption = "Joseph Pope | @joepope44"
  ) + 
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_blank(), # Modify this to keep x or y axis text
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.background = element_rect(fill = "#a3b18a"),
    plot.title = element_text(hjust = 0.5, size = 24, family  = "Garamond"),
    plot.subtitle = element_text(hjust = 0.5, family  = "Garamond"),
    plot.caption = element_text(hjust = 0.5, family = "Garamond")
    # plot.margin = unit(rep(-1,4), "cm") <- this hides the plot title if not commented out
  ) + 
  coord_polar() + # Make it a circle
  # add labels - save to remove inherit.aes = FALSE (soft-deprecated I believe)
  geom_text(data=label_data, aes(x=id, y=threatened_plants+10, label=country, hjust=hjust), color="black", fontface="bold",alpha=0.7, size=3.5, angle= label_data$angle) +
  # Add base line information - Arc (a line in a polar coordinate) Then Label ( gridText Can be use to enhance text placement - say for a two line display)
  geom_segment(data=base_data, aes(x = start, y = -5, xend = end, yend = -5), colour = "black", alpha=0.8, size=0.6 , inherit.aes = FALSE )  +
  geom_text(data=base_data, aes(x = title, y = -18, label=continent),
            hjust=c(1,1,.5,0,0,0), colour = "black", alpha=0.8, size=4, 
            fontface="bold", inherit.aes = FALSE) 

```

