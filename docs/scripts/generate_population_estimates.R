
### MH Briefing - script to generate CCG population counts
# Author RG

require(readxl)
require(dplyr)
require(tidyr)
require(stringr)
require(here)


# Set up ------------------------------------------------------------------

### Import CCG lookup 
ccg_lookup <- read.csv(here('inputs','ccg_lookup.csv')) 

### Import population data

# Mid-2020 CCG population estimates (the latest available CCG estimates) ---------------------------------------
# Note: ONS population statistics imported below are mid-year estimates while NHS data is for the financial year 

pop_2020 <- read_excel(here('inputs','pop_2020.xlsx'), sheet = "Mid-2020 Persons", range = "A7:CT143") %>% 
  rename(ccg = 'CCG Name',
         ccg_cd = 'CCG Code') %>% 
  filter(!is.na(ccg)) %>% 
  rowwise() %>% 
  mutate(pop_2020 = sum(c_across('0':'17'), na.rm=TRUE),
         ccg = toupper(ccg)) %>% 
  select(ccg, pop_2020)

# bind on ccg lookup to find new CCG name after mergers
pop_2020 <- pop_2020 %>% left_join(ccg_lookup %>% select(-region), by=c("ccg")) 

# check for NAs 
pop_2020 %>% filter(is.na(ccg_new))

# remove duplicates - should be 106 CCGs
pop_2020 <- pop_2020 %>% 
  unique() %>% 
  group_by(ccg_new) %>% 
  mutate(pop_2020 = sum(pop_2020, na.rm = TRUE)) %>% 
  select(ccg_new, pop_2020) %>%
  unique() 


## Mid-2019 CCG population estimates
pop_2019 <- read_excel(here('inputs','pop_2019.xlsx'), sheet = "Mid-2019 Persons", range = "A7:CT199") %>% 
  rename(ccg = 'CCG Name',
         ccg_cd = 'CCG Code') %>% 
  filter(!is.na(ccg)) %>%
  rowwise() %>% 
  mutate(pop_2019 = sum(c_across('0':'17'), na.rm=TRUE),
         ccg = toupper(ccg)) %>% 
  select(ccg, pop_2019)

# bind on ccg lookup to find new CCG name after mergers
pop_2019 <- pop_2019 %>% left_join(ccg_lookup %>% select(-region), by=c("ccg")) 

# check for NAs 
pop_2019 %>% filter(is.na(ccg_new))

# remove duplicates - should be 106 CCGs
pop_2019 <- pop_2019 %>% 
  unique() %>% 
  group_by(ccg_new) %>% 
  mutate(pop_2019 = sum(pop_2019, na.rm = TRUE)) %>% 
  select(ccg_new, pop_2019) %>%
  unique() 


## Mid-2018 CCG population estimates
pop_2018 <- read_excel(here('inputs','pop_2018.xlsx'), sheet = "Mid-2018 Persons", range = "A7:CP221") %>%  
  rename(ccg = 'Area Names',
         ccg_cd = 'Area Codes') %>% 
  filter(!is.na(ccg)) %>% 
  rowwise() %>% 
  mutate(pop_2018 = sum(c_across('0':'17'), na.rm=TRUE),
         ccg = toupper(ccg)) %>%  
  select(ccg, pop_2018) %>% 
  filter(str_detect(ccg, "CCG"))


# bind on ccg lookup to find new CCG name after mergers
pop_2018 <- pop_2018 %>% left_join(ccg_lookup, by=c("ccg"))
  
# check for NAs 
pop_2018 %>% filter(is.na(ccg_new))

# remove duplicates - should be 106 CCGs
pop_2018 <- pop_2018 %>% 
  unique() %>% 
  group_by(ccg_new) %>% 
  mutate(pop_2018 = sum(pop_2018, na.rm = TRUE)) %>% 
  select(ccg_new, pop_2018) %>%
  unique() 


### Bind all years together
pop_ccg <- pop_2018 %>% 
  left_join(pop_2019, by = c("ccg_new")) %>% 
  left_join(pop_2020, by = c("ccg_new")) %>% 
  rename(ccg = ccg_new)


### Create England population counts

# Mid-2020
pop_2020_eng <- pop_2020 %>% 
  group_by() %>% 
  summarise(pop_2020 = sum(pop_2020, na.rm = TRUE)) %>% 
  mutate(ccg = "ENGLAND")

# Mid-2019
pop_2019_eng <- pop_2019 %>% 
  group_by() %>% 
  summarise(pop_2019 = sum(pop_2019, na.rm = TRUE)) %>% 
  mutate(ccg = "ENGLAND")

# Mid-2018
pop_2018_eng <- read_excel(here('inputs','pop_2018.xlsx'), sheet = "Mid-2018 Persons", range = "A7:CP221") %>%  
  rename(ccg = 'Area Names',
         ccg_cd = 'Area Codes') %>% 
  filter(!is.na(ccg)) %>% 
  rowwise() %>% 
  mutate(pop_2018 = sum(c_across('0':'17'), na.rm=TRUE)) %>%  
  select(ccg, pop_2018) %>% 
  filter(ccg %in% c("ENGLAND"))

### Join England population tables together
pop_england <- pop_2018_eng %>% 
  left_join(pop_2019_eng, by=c("ccg")) %>% 
  left_join(pop_2020_eng, by=c("ccg"))


# export tables
write.csv(pop_ccg, here('intermediate','pop_ccg.csv'), row.names = FALSE)
write.csv(pop_england, here('intermediate','pop_england.csv'), row.names = FALSE)



