suppressWarnings(suppressPackageStartupMessages({library(tidycensus)
library(tidyverse)
library(grid)
library(rlist)
library(usmap)}))

options(scipen = 999)

Wapo_Colors <- c("#72a379", "#75a8cb", "#5c5252", "#c2af23", "#af6464", "#8b53a1","#6566c2","#f1afad")
Wapo_Colors_Reversed <- list.reverse(Wapo_Colors)

#Need extra space over the top for the graphic

Wapo_Format <- function(chart) {
  chart +
    geom_col(width = .8) +
    scale_fill_manual(name = "Units per Structure", values = Wapo_Colors_Reversed, guide = "none") +
    geom_vline(xintercept = c(.25, .5, .75), linetype = "dashed", linewidth = .1) +
    theme(axis.title.y = element_blank(),
          axis.text = element_text(size = 6),
          plot.margin = unit(c(0, 0, 0, 0), "npc"))
  }

Place_DF <- read_rds("Data/Place_DF.rds")
Place_Geometry <- read_rds("Data/Place Geometry.rds")

CCA_DF <- read_rds("Data/CCA_DF.rds")
CCA_Geometry <- read_sf("Data/CCA Geometry/Chicago_Community_Areas.shp") %>% 
  select(cca_num = area_numbe,
         geometry)

#Parameters
#-How many entries to include
#-Which variable set to color by
#-Which variable to sort on
#-Community Areas vs. Places

#Recreate WaPo graph-------------------------

DF <- c("Place_DF", "CCA_DF")
Rankby_UnitsPer <- c("1, detached", "1, attached", "2", "3 or 4", "5 to 9", "10 to 19", "20+", "Alternatives")
Entries <- 20
Most_or_Least <- c("Most", "Least")

Wapo_Bar_Generate <- function(DF = Place_DF, Rankby = "1, detached", Entries = 20, Order = "Descending") {

  if(Order == "Descending") {
    
  Plot_DF <- DF %>% 
    filter(Race == "All") %>% 
    group_by(Geography, Race) %>% 
    mutate(Sort = sum(Percent[Units_per_Structure %in% Rankby])) %>% 
    ungroup() %>% 
    mutate(Rank = dense_rank(-Sort)) %>% 
    filter(Rank <= Entries)
  
  } else {
    
    Plot_DF <- DF %>% 
      filter(Race == "All") %>% 
      group_by(Geography, Race) %>% 
      mutate(Sort = sum(Percent[Units_per_Structure %in% Rankby])) %>% 
      ungroup() %>% 
      mutate(Rank = dense_rank(Sort)) %>% 
      filter(Rank <= Entries)
  }   
  
  ggplot(Plot_DF, aes(x = Percent, y = fct_reorder(Geography, -Rank), fill = fct_rev(Units_per_Structure))) %>% 
    Wapo_Format()
  
}

Wapo_Map_Generate <- function(DF = Place_DF, Rankby = "1, detached", Entries = 20, Order = "Descending") {

  if(Order == "Descending") {
    
    Plot_DF <- DF %>% 
      filter(Race == "All") %>% 
      group_by(Geography, Race) %>% 
      mutate(Sort = sum(Percent[Units_per_Structure %in% Rankby])) %>% 
      ungroup() %>% 
      mutate(Rank = dense_rank(-Sort)) %>% 
      filter(Rank <= Entries) %>% 
      inner_join(Place_Geometry, by = "GEOID")
    
  } else {
    
    Plot_DF <- DF %>% 
      filter(Race == "All") %>% 
      group_by(Geography, Race) %>% 
      mutate(Sort = sum(Percent[Units_per_Structure %in% Rankby])) %>% 
      ungroup() %>% 
      mutate(Rank = dense_rank(Sort)) %>% 
      filter(Rank <= Entries) %>% 
      inner_join(Place_Geometry, by = "GEOID")
  }   
  
  States <- Plot_DF %>% 
    pull(State_Abb) %>% 
    unique()
  
  Map_States <- us_map("state", include = States)
  
  ggplot() +
    geom_sf(data = Map_States$geom) +
    geom_sf(data = Plot_DF$Centroid, color = alpha("blue", .2))
  
}

Wapo_Map_Generate_CCA <- function(DF = CCA_DF, Rankby = "1, detached", Entries = 20, Order = "Descending") {
  
  if(Order == "Descending") {
    
    Plot_DF <- DF %>% 
      filter(Race == "All") %>% 
      group_by(Geography, Race) %>% 
      mutate(Sort = sum(Percent[Units_per_Structure %in% Rankby])) %>% 
      ungroup() %>% 
      mutate(Rank = dense_rank(-Sort)) %>% 
      filter(Rank <= Entries) %>% 
      inner_join(CCA_Geometry)
    
  } else {
    
    Plot_DF <- DF %>% 
      filter(Race == "All") %>% 
      group_by(Geography, Race) %>% 
      mutate(Sort = sum(Percent[Units_per_Structure %in% Rankby])) %>% 
      ungroup() %>% 
      mutate(Rank = dense_rank(Sort)) %>% 
      filter(Rank <= Entries) %>% 
      inner_join(CCA_Geometry)
  }   
  
  ggplot() +
    geom_sf(data = CCA_Geometry$geometry) +
    geom_sf(data = Plot_DF$geometry, fill = alpha("lightblue", .2))
  
}

Wapo_Bar_Generate(Order = "Ascending")
ggsave("images/Ascending.png", units = "in", width = 4, height = 6)

Wapo_Bar_Generate(Rankby = "20+")
ggsave("images/20+.png", units = "in", width = 4, height = 6)

Wapo_Bar_Generate(Rankby = "1, attached")
ggsave("images/1attached.png", units = "in", width = 4, height = 6)
Wapo_Map_Generate(Rankby = "1, attached", Entries = 19)
ggsave("images/1attachedmap.png", units = "in", width = 4, height = 4.72)

Wapo_Bar_Generate(Rankby = "1, detached")
ggsave("images/1detached.png", units = "in", width = 4, height = 6)

Wapo_Bar_Generate(Rankby = c("2", "3 or 4", "5 to 9"))
ggsave("images/middle.png", units = "in", width = 4, height = 6)

Wapo_Bar_Generate(DF = CCA_DF, Order = "Ascending", Entries = 10)
ggsave("images/Ascending_CCA.png", units = "in", width = 4, height = 3)
Wapo_Map_Generate_CCA(Order = "Ascending", Entries = 10)
ggsave("images/Ascending_CCA_Map.png", units = "in", width = 4, height = 4.72)

Wapo_Bar_Generate(DF = CCA_DF, Order = "Ascending", Entries = 10)
ggsave("images/Ascending_CCA.png", units = "in", width = 4, height = 3)
Wapo_Map_Generate_CCA(Order = "Ascending", Entries = 10)
ggsave("images/Ascending_CCA_Map.png", units = "in", width = 4, height = 4.72)

Wapo_Bar_Generate(DF = CCA_DF, Rankby = "1, detached", Entries = 10)
ggsave("images/1detached_CCA.png", units = "in", width = 4, height = 3)
Wapo_Map_Generate_CCA(Rankby = "1, detached", Entries = 10)
ggsave("images/1detached_CCA_Map.png", units = "in", width = 4, height = 4.72)

Wapo_Bar_Generate(DF = CCA_DF, Rankby = c("2", "3 or 4", "5 to 9"), Entries = 10)
ggsave("images/middle_CCA.png", units = "in", width = 4, height = 3)
Wapo_Map_Generate_CCA(Rankby = c("2", "3 or 4", "5 to 9"), Entries = 10)
ggsave("images/middle_CCA_Map.png", units = "in", width = 4, height = 4.72)
