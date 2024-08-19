library(tidyverse)
library(tidycensus)
library(cmapgeo)
library(sf)

ACSVars2022 <- load_variables(2022, "acs5")

All_Housing_Vars <- ACSVars2022 %>% 
  filter(str_detect(name, "B25032|B25024"),
         !str_detect(name, "B25032_"))

Vars <- Housing_Vars %>% 
  pull(name) %>% 
  unique()

Housing_Data_by_Place <- get_acs(year = 2022, geography = "place", variables = Vars, geometry = FALSE) %>% 
  mutate(High_Error = ((moe/1.645)/estimate)>.2)

Population_by_Place <- get_acs(year = 2022, geography = "place", variables = "B01001_001", geometry = FALSE) %>% 
  mutate(High_Error = ((moe/1.645)/estimate)>.2)

Place_Geometry <- get_acs(year = 2022, geography = "place", variables = "B01001_001", geometry = TRUE) %>% 
  filter(estimate > 50000) %>% 
  select(GEOID, geometry) %>% 
  mutate(Centroid = st_centroid(geometry))

write_rds(Place_Geometry, "Data/Place Geometry.rds")

Housing_Data_by_Place_Github <- Housing_Data_by_Place %>% 
  semi_join(Population_by_Place %>% 
              filter(estimate > 50000) %>% 
              select(GEOID)) %>% 
  filter(str_detect(variable, "B25024"))

# Had to restrict Housing_Unit_Place to keep the filesize down for Github To
# pull all housing unit categories by race as well as for lower populations
# (though be warned the error rate goes through the roof), use
# Housing_Data_by_Place instead of Housing_data_by_Place_Github

write_csv(Housing_Data_by_Place_Github, str_c("Data/Housing_Unit_Place.csv", na=""))
write_csv(Population_by_Place, str_c("Data/Population_Place.csv", na=""))

Housing_Data_by_Tract <- get_acs(year = 2022, geography = "tract", state = "IL", variables = Vars, geometry = FALSE) %>% 
  mutate(High_Error = ((moe/1.645)/estimate)>.2)

Population_by_Tract <- get_acs(year = 2022, geography = "tract", state = "IL", variables = "B01001_001", geometry = FALSE) %>% 
  mutate(High_Error = ((moe/1.645)/estimate)>.2)

Housing_Data_by_CCA <- Housing_Data_by_Tract %>% 
  inner_join(xwalk_tract2cca %>% mutate(GEOID = geoid_tract)) %>% 
  mutate(Adjusted_Units = estimate * hu_pct) %>% 
  group_by(cca_num, variable) %>% 
  summarize(Units = sum(Adjusted_Units, na.rm = TRUE),
            moe = sqrt(sum(moe^2, na.rm = TRUE))) %>% 
  ungroup() %>% 
  mutate(High_Error = moe/Units > .2) %>% 
  inner_join(cca_sf %>% data.frame() %>% select(cca_name, cca_num))

Population_by_CCA <- Population_by_Tract %>% 
  inner_join(xwalk_tract2cca %>% mutate(GEOID = geoid_tract)) %>% 
  mutate(Adjusted_Pop = estimate * pop_pct) %>% 
  group_by(cca_num) %>% 
  summarize(Population = sum(Adjusted_Pop)) %>% 
  ungroup() %>% 
  inner_join(cca_sf %>% data.frame() %>% select(cca_name, cca_num))

write_csv(Housing_Data_by_CCA, str_c("Data/Housing_Unit_CCA.csv", na=""))
write_csv(Population_by_CCA, str_c("Data/Population_CCA.csv", na=""))
