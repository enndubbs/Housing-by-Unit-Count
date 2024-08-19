library(tidyverse)
library(tidycensus)
library(rlist)
library(sf)

ACSVars2022 <- load_variables(2022, "acs5")
State_Lookup <- data.frame(State = state.name,
                           State_Abb = state.abb) %>% 
  bind_rows(data.frame(State = "District of Columbia",
                       State_Abb = "DC"))

Data_Directory <- "Data/"

Data_Files_DF <- data.frame(Filenames = list.files(Data_Directory)) %>% 
  mutate(Filepaths = str_c(Data_Directory, Filenames),
         Filenames = str_remove_all(Filenames, ".csv")) %>% 
  filter(str_detect(Filenames, "Housing"))

Population_by_Place <- read_csv(str_c(Data_Directory, "Population_Place.csv"))

Data_Filenames <- Data_Files_DF %>%
  pull(Filenames)

Data_Files <- map(Data_Files_DF$Filepaths, read_csv) 

Unit_Processed <- map(Data_Files, function(x) {

  x %>% 
    inner_join(ACSVars2022 %>% rename(variable = name))  %>% 
      filter(!str_detect(variable, "_001")) %>% 
      mutate(Race = str_remove_all(concept, "Units in Structure \\("), 
             Race = str_remove_all(Race, " Householder\\)"),
             Race = str_replace_all(Race, "Units in Structure", "All"),
             Units_per_Structure = str_remove_all(label, "Estimate!!Total:!!"),
             Units_per_Structure = if_else(str_detect(Units_per_Structure, "Boat|Mobile"), "Alternatives", Units_per_Structure),
             Units_per_Structure = if_else(str_detect(Units_per_Structure, "20 to|50"), "20+", Units_per_Structure),
             Units_per_Structure = factor(Units_per_Structure, c("1, detached", "1, attached", "2", "3 or 4", "5 to 9", "10 to 19", "20+", "Alternatives"), ordered = TRUE))
})

names(Unit_Processed) <- Data_Filenames

list2env(Unit_Processed, envir = .GlobalEnv)

CCA_DF <- Housing_Unit_CCA %>% 
  group_by(cca_num, cca_name, Race, Units_per_Structure) %>% 
  summarize(Units = sum(Units),
            moe = sqrt(sum(moe^2))) %>% 
  ungroup() %>% 
  mutate(High_Error = moe/Units > .2) %>% 
  group_by(cca_num, cca_name, Race) %>% 
  mutate(Geography = cca_name,
         Percent = Units/sum(Units)) %>% 
  ungroup()

write_rds(CCA_DF, "Data/CCA_DF.rds")

Over50k <- Population_by_Place %>% 
  filter(estimate > 50000) %>% 
  arrange(desc(estimate)) %>% 
  mutate(PopRank = row_number()) %>% 
  select(GEOID, Population = estimate, PopRank)

Place_DF <- Housing_Unit_Place %>% 
  inner_join(Over50k) %>% 
  mutate(CityState = str_remove_all(NAME, " city| zona urbana| CDP| municipality| town|"),
         CityState = if_else(str_detect(CityState, "Nashville-Davidson"), "Nashville, Tennessee",
                        if_else(str_detect(CityState, "Louisville/"), "Lousiville, Kentucky", 
                                if_else(str_detect(CityState, "Indianapolis"), "Indianapolis, Indiana", CityState)))) %>% 
  separate(CityState, c("City", "State"), sep = ", ") %>% 
  group_by(GEOID, City, State, Race, Units_per_Structure, Population, PopRank) %>% 
  summarize(Units = sum(estimate),
            moe = sqrt(sum(moe^2))) %>% 
  ungroup() %>% 
  mutate(High_Error = moe/Units > .2) %>% 
  group_by(GEOID, City, State, Race) %>% 
  mutate(Percent = Units/sum(Units)) %>% 
  ungroup() %>% 
  inner_join(State_Lookup) %>% 
  mutate(Geography = str_c(City, ", ", State_Abb))

write_rds(Place_DF, "Data/Place_DF.rds")
