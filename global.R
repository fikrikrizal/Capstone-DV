# dashboarding
library(shiny)
library(shinydashboard)
library(DT) # datatable

options(scipen = 99) # me-non-aktifkan scientific notation
library(tidyverse) # koleksi beberapa package R
library(plotly) # plot interaktif
library(glue) # setting tooltip
library(scales) # mengatur skala pada plot
library(shinyWidgets)


# --------- READ DATA 

epl <- read_csv("EPL Standings 2000-2022.csv")

# --------- DATA PREPARATION

epl_clean <- epl %>% 
  mutate(Season=as.factor(Season),
         Team=as.factor(Team)) %>% 
  rename(Win = W,
         Lost = L,
         Draw = D,
         Played = Pld,
         Goal = GF,
         Conceded = GA,
         Different = GD)
  
most_champion <- epl_clean %>% filter(Pos==1) %>% 
  group_by(Team) %>% summarise(count=n()) %>% 
  top_n(1) %>% select(Team)

last_champion <-  epl_clean %>% 
  filter(Season=="2021-22" & Pos==1) %>% 
  select(Team)

epl_clean %>% filter(Team=="Manchester United") %>% 
  select(Season, Team, Pos) %>% 
  ggplot(mapping = aes(x=Season, y=-Pos,group=1))+
  geom_line()

epl_winner <- epl_clean %>% filter(Pos==1) %>% 
  select(Season, Team, Win, Draw, Lost) %>% ungroup()
epl_winner$Year <- 2001:2022  
epl_winner <- epl_winner %>% 
  pivot_longer(cols = c("Win","Draw","Lost"))

epl_winner <- 
  epl_winner %>% 
  mutate(label = glue("Winner: {Team}
                          {name}: {value}"))

epl_winner %>% filter(name %in% "Win") %>% 
  ggplot(mapping = aes(x=factor(Year), y=value,
                       text = label,
                       group=name))+
  geom_line(aes(color=name),position = "dodge")+
  geom_point(aes(color=name))+
  scale_y_continuous(limits = c(0,35), breaks = c(1:35))+
  labs(x="Year End of Season",
       y="Matchs")



