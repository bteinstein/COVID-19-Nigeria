library(tidyverse)
# library(gganimate)
# library(ggthemes)
library(viridis)
library(RColorBrewer)
# Load data
africa_data_covid <- readxl::read_excel("data/africa/Africa.xlsx")


world_map_d <- map_data("world2")
wrd_contr <- unique(world_map_d$region)

`%nin%` = Negate(`%in%`)
sum(africa_data_covid$Countries %nin% wrd_contr)
africa_data_covid$Countries[africa_data_covid$Countries %nin% wrd_contr]

# "DRC"        "Cabo Verde" "Eswatini"   "Congo"      "CAR"        "Réunion" 


africa_data_covid <- africa_data_covid %>% 
  mutate(Countries = case_when(
    Countries == 'DRC' ~ "Democratic Republic of the Congo",
    Countries == "Cabo Verde" ~ "Cape Verde",
    Countries =="Eswatini"  ~ "Swaziland",
    Countries == "Congo" ~ "Republic of Congo", 
    Countries == "CAR"~ "Central African Republic",
    Countries == "Réunion"~ "Reunion", 
    TRUE ~ as.character(Countries)
  ))


# Merge dataset
df.map1 <- right_join(africa_data_covid, world_map_d, by = c("Countries"="region")) #%>% 
            # filter(Tot_Case > 0)

# Grouping 
quantile(africa_data_covid$Tot_Case, seq(0, 1, .10))
# 0%      10%      20%      30%      40%      50%      60%      70%      80%      90%     100% 
# 10.0    512.6   1810.2   2681.4   4089.2   5423.0   7786.6  11077.0  19320.8  49241.6 686891.0 
# "0 - 500", "501 - 5,000","5,001 - 10,000","20,001 - 50,000","50,000 - 100,000" ,"100,001 - 500,000" ,"500,001+" 


df.map2 <- df.map1 %>% 
  filter(Tot_Case > 0) %>%
  mutate(
    cc_range = case_when(
      Tot_Case <= 500 ~ "0 - 500",
      Tot_Case <= 5000 ~ "501 - 5,000",
      Tot_Case <= 10000 ~ "5,001 - 10,000",
      Tot_Case <= 50000 ~ "20,001 - 50,000",
      Tot_Case <= 100000 ~ "50,000 - 100,000" ,
      Tot_Case <= 500000 ~ "100,001 - 500,000" ,
      Tot_Case > 500000 ~ "500,001+" 
    ),
    cc_range = factor(cc_range,levels = c("0 - 500", "501 - 5,000","5,001 - 10,000","20,001 - 50,000",
                                          "50,000 - 100,000" ,"100,001 - 500,000" ,"500,001+" )),
    label_color = case_when(
      Countries == "Nigeria" ~ "white",
      TRUE ~ "grey40"
    )
  )


########################### PLOT ##############
custom_black_theme <- theme_void() +
  theme(
    panel.background = element_rect(color =NA,fill='black'),
    plot.background =  element_rect(color =NA,fill='black'),
    plot.margin = margin(t = 1,r = -25,b = 1,l = -25,unit = 'cm'),
    # Legend
    legend.position = "top",
    legend.justification = 'center',
    legend.text = element_text(family = "poppins", face = 'bold',
                               color = "white", size = 10),
    legend.key.width = unit(5.2, "lines"),
    legend.key.height = unit(0.8, "lines") 
  )

col_range = c('#FFFFFF',colorRampPalette(brewer.pal(7,"Blues"))(7)[2:6],"#001F47")

cnames <- aggregate(cbind(long, lat) ~ Countries, data=df.map2, 
                    FUN=function(x)mean(range(x)))
ggplot(data = df.map2) +
  geom_polygon(aes(x = long, y = lat, fill = cc_range, group = group),
               color = "grey50") +
  expand_limits(x = c(min(df.map2$long), max(df.map2$long)), 
                y = c(min(df.map2$lat), max(df.map2$lat))) +
  scale_fill_manual(values = col_range) +
  guides(fill = guide_legend(title.position = "top", 
                             title.hjust = 0.7, nrow = 1,
                             label.position = "top")) +
  # geom_text(data=cnames, aes(long, lat, label = Countries), size=2)+
  # scale_color_identity() +
  coord_sf(expand = F) +
  custom_black_theme

ggsave(filename = "./Poster/png/Africa_unlabled.png", device = 'png')

