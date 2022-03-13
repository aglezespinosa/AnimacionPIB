##############################################################################
# Grafica Animada del PIB por año                                            #
# (c) Angel Gonzalez Espinosa 2022                                           #
# aglezespinosa@gmail.com                                                    #
##############################################################################

library(tidyverse)
library(janitor)
library(gganimate)

#gdp <- read_csv("./data/GDP_Data.csv")
#select required columns
#gdp <- gdp %>% select(3:15) 
#filter only country rows
#gdp <- gdp[1:217,]
#gdp_tidy <- gdp %>% 
 # mutate_at(vars(contains("YR")),as.numeric) %>% 
#  gather(year,value,3:13) %>% 
#  janitor::clean_names() %>% 
#  mutate(year = as.numeric(stringr::str_sub(year,1,4)))
#write_csv(gdp_tidy,"./data/gdp_tidy.csv")

gdp_tidy <- read_csv("GDP_tidy.csv")

gdp_formatted <- gdp_tidy %>%
  group_by(year) %>%
  # The * 1 makes it possible to have non-integer ranks while sliding
  mutate(rank = rank(-value),
         Value_rel = value/value[rank==1],
         Value_lbl = paste0(" ",round(value/1e9))) %>%
  group_by(country_name) %>% 
  filter(rank <=10) %>%
  ungroup()

# Inicia la graficación

anim <- ggplot(gdp_formatted, aes(rank, group = country_name, 
                                  fill = as.factor(country_name), color = as.factor(country_name))) +
  geom_tile(aes(y = value/2,
                height = value,
                width = 0.9), alpha = 0.8, color = NA) +
  geom_text(aes(y = 0, label = paste(country_name, " ")), vjust = 0.2, hjust = 1) +
  geom_text(aes(y=value,label = Value_lbl, hjust=0)) +
  coord_flip(clip = "off", expand = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_reverse() +
  guides(color = FALSE, fill = FALSE) +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.grid.major.x = element_line( size=.1, color="grey" ),
        panel.grid.minor.x = element_line( size=.1, color="grey" ),
        plot.title=element_text(size=25, hjust=0.5, face="bold", colour="grey", vjust=-1),
        plot.subtitle=element_text(size=18, hjust=0.5, face="italic", color="grey"),
        plot.caption =element_text(size=8, hjust=0.5, face="italic", color="grey"),
        plot.background=element_blank(),
        plot.margin = margin(2,2, 2, 4, "cm")) +
  
  transition_states(year, transition_length = 4, state_length = 1) +
  view_follow(fixed_x = TRUE)  +
  labs(title = 'PIB por Año : {closest_state}',  
       subtitle  =  "10 Principales Paises",
       caption  = "PIB en Billions USD | Fuente de Datos: Banco Mundial") 

# Se genera el GIF Animado
animate(anim, 200, fps = 20,  width = 1200, height = 1000, 
        renderer = gifski_renderer("gganim.gif"))

#Este código tiene mejor vista

p <- gdp_tidy %>%
  # build rank, labels and relative values
  group_by(year) %>%
  mutate(rank = rank(-value),
         Value_rel = value/value[rank==1],
         Value_lbl = paste0(" ",round(value/1e9)))  %>%
  group_by(country_name) %>%
  # keep top 10
  filter(rank <= 10) %>%
  
  # plot
  ggplot(aes(-rank,Value_rel, fill = country_name)) +
  geom_col(width = 0.8, position="identity") +
  coord_flip() + 
  geom_text(aes(-rank,y=0,label = country_name,hjust=0)) +       #country label
  geom_text(aes(-rank,y=Value_rel,label = Value_lbl, hjust=0)) + # value label
  
  coord_flip(clip = "off", expand = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_reverse() +
  guides(color = FALSE, fill = FALSE) +
  
  theme_minimal() +
  theme(legend.position = "none",axis.title = element_blank()) +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.grid.major.x = element_line( size=.1, color="grey" ),
        panel.grid.minor.x = element_line( size=.1, color="grey" ),
        plot.title=element_text(size=25, hjust=0.5, face="bold", colour="red", vjust=-1),
        plot.subtitle=element_text(size=18, hjust=0.5, face="italic", color="black"),
        plot.caption =element_text(size=18, hjust=0.5, face="italic", color="red"),
        plot.background=element_blank(),
        plot.margin = margin(2,2, 2, 4, "cm")) +
  
  
  # animate along Year
  #transition_states(year,4,1)
  transition_states(year, transition_length = 4, state_length = 1) +
  view_follow(fixed_x = TRUE)  +
  labs(title = 'PIB por Año : {closest_state}',  
       subtitle  =  "10 Principales Paises",
       caption  = "PIB en Billions USD | Fuente de Datos: Banco Mundial") 


#animate(p, 100, fps = 25, duration = 20, width = 800, height = 600)
# animate(plot, nframes, fps, duration, detail, renderer,
#device, ref_frame, start_pause, end_pause, rewind, ...)

animate(p, 200, fps = 20,  duration = 20, width = 800, height = 600, 
        renderer = gifski_renderer("pibanim.gif"))

# Se genera el MP4
#animate(anim, 200, fps = 20,  width = 1200, height = 1000, 
#        renderer = ffmpeg_renderer()) -> for_mp4

#anim_save("animation.mp4", animation = for_mp4 )