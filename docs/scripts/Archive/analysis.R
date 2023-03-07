
### Author: Rebecca Gilhooly
### Title: Cleaning and analysis script for the MH Briefing (2021/22 data)
### Date: 18/01/2023

library(here)
library(dplyr)
library(ggplot2)
library(occresearch)
library(tidyr)
library(janitor)
library(haven)
library(stringr)
library(showtext)
library(kableExtra)


#####    Set up    #####

# Import fonts

font_add("HindVadodara", here('inputs','HindVadodara-Regular.ttf'), bold = here('inputs','HindVadodara-Bold.ttf'))
showtext_auto()
windows()

# Function to copy data frames

writetable <- function(x){
  
  x %>% write.table(file="clipboard", sep="\t", row.names=F)}


# Function to format dataframes 
format <- function(x){ 
  x %>% 
    mutate(ccg = toupper(ccg)) %>% 
    mutate(across(starts_with('t4'), ~ifelse(. %in% c("","*"),NA, .))) %>% 
    mutate(across(starts_with('t4'), ~as.numeric(.))) %>% 
    mutate(across(starts_with('adultward'), ~ifelse(. %in% c("","*"), NA, .))) %>% 
    mutate(across(starts_with('adultward'), ~as.numeric(.))) %>% 
    mutate(across(starts_with('contacts'), ~as.numeric(.))) 
}

### Import CCG lookup ###

ccg_lookup <- read.csv(here('inputs','ccg_lookup.csv'))

### Import data  ###

### NHS England (NHSE)

# NHSE send their data on separate tabs in an Excel spreadsheet.
# Data needs compiling into one dataframe for R - aka the "cleandata.csv" file below

df_nhse <- read.csv(here('inputs','nhsengland_cleandata.csv'))

# Trend data across all indicators is labelled by year/quarter - column names need to be renamed after import.
# Copy (old) column names into excel using writetable function to create a look up file with new names (nhse_names.csv)

#writetable(names(df_nhse)) 

# Import names file and replace column names
nhse_names <- read.csv(here('inputs','nhse_names.csv'))
names(df_nhse) <- nhse_names$New

# Fix formatting issues
df_nhse <- format(df_nhse)
str(df_nhse)


### NHS Digital (NHSD) ###

# NHSD data is presented in an unhelpful format - needs copying into a clean spreadsheet with only values.
df_nhsd_full <- read.csv(here('inputs', 'nhsdigital_cleandata.csv'))
nhsd_names <- read.csv(here('inputs','nhsd_names.csv'))
names(df_nhsd_full) <- nhsd_names$New

# Fix formatting issues
df_nhsd_full <- format(df_nhsd_full)
str(df_nhsd_full)

# Clean table (without the UNKNOWN row)
df_nhsd_2122 <- df_nhsd_full %>% filter(!ccg %in% c("UNKNOWN"))

### NHSD 2020/21
df_nhsd_2021 <- read.csv(here('inputs','MH tables 202021.csv')) %>% 
  select(-Region, -CCG.code, -org_cd)

# rename columns
df_nhsd_2021 <- df_nhsd_2021 %>% 
  rename(ccg = 'Clinical.Commissioning.Group..CCG.',
         perc_referred_2021 = 'X..percentage.of.children.referred.to.CYPMHS',
         avg_wait_days_2021 = 'Average.waiting.time.before.treatment..days.',
         referrals_closed_2021 = 'X..of.referrals.closed.before.treatment',
         overall_score_2021 = 'CCG.overall.score')

cols_2021 <- names(df_nhsd_2021)[2:6]

# join ccg lookup and summarise table by new ccg names
df_nhsd_2021 <- df_nhsd_2021 %>% 
  left_join(ccg_lookup, by= c("ccg"="ccg")) %>% 
  group_by(ccg_new) %>% 
  mutate(across(cols_2021,  ~ mean(.x, na.rm = TRUE))) %>% 
  select(-ccg) %>% unique()


# NHSD 2019/20
df_nhsd_1920 <- read.csv(here('inputs','MH tables 201920.csv')) %>% 
  select(-region, -ccg_cd, -perc_access, -X..still.waiting)

df_nhsd_1920 <- df_nhsd_1920 %>% 
  rename(perc_referred_1920 = perc_referral,
         avg_wait_days_1920 = avg_wait,
         referrals_closed_1920 = perc_closed,
         overall_score_1920 = overall_score)

# join lookup for new ccg name
cols_1920 <- names(df_nhsd_1920)[2:6]

df_nhsd_1920 <- df_nhsd_1920 %>%
  left_join(ccg_lookup, by= c("ccg"="ccg")) %>% 
  group_by(ccg_new) %>% 
  mutate(across(cols_1920,  ~ mean(.x, na.rm = TRUE))) %>% 
  select(-ccg) %>% unique() 


### join nhsd tables together 
df_nhsd <- df_nhsd_2122 %>% 
  left_join(df_nhsd_2021, by=c("ccg"="ccg_new")) %>% 
  left_join(df_nhsd_1920 %>% select(!region), by=c("ccg"="ccg_new"))

# Combine nhse and nhsd tables
df <- df_nhse %>%  
  select(-region,-org_cd) %>% 
  left_join(df_nhsd, by=c("ccg"="ccg"))


##### Analysis #####

# create spc_year, perc_budget, perc_referred (divide by pop), perc_closed, total CCG allocation (mhspend_allages/mhspend_perc)
# note that the latest population estimates for CCGs was published in 2022

### Spend per child
df <- df %>%
  mutate(spc_202122 = mhspend_cyp_2122/pop_2020,
         spc_202021 = mhspend_cyp_2021/pop_2020,
         spc_201920 = mhspend_cyp_1920/pop_2019)


### Percentage of YP population referred 
df <- df %>%
  mutate(percreferred_202122 = no_referred/pop_2020,
         percreferred_202021 = no_referred_2021/pop_2020,
         percreferred_201920 = no_referred_1920/pop_2019)

### Calculate total budget allocation per ccg (not just MH)

df <- df %>%
  mutate(total_budget_202122 = mhspend_allages_2122/mhspend_perc_allocation_2122) %>% 
  mutate(total_budget_202021 = mhspend_allages_2021/mhspend_perc_allocation_2021) %>%
  mutate(total_budget_201920 = mhspend_allages_1920/mhspend_perc_allocation_1920) %>% 
  mutate(total_budget_201819 = mhspend_allages_1819/mhspend_perc_allocation_1819) %>% 
  mutate(total_budget_201718 = mhspend_allages_1718/mhspend_perc_allocation_1718) 

### Calculate proportion of budget allocated to CYPMHS

df <- df %>% 
  mutate(pbudget_camhs_2122 = mhspend_cyp_2122/total_budget_202122) %>% 
  mutate(pbudget_camhs_2021 = mhspend_cyp_2021/total_budget_202021) %>% 
  mutate(pbudget_camhs_1920 = mhspend_cyp_1920/total_budget_201920) %>% 
  mutate(pbudget_camhs_1819 = mhspend_cyp_1819/total_budget_201819) %>% 
  mutate(pbudget_camhs_1718 = mhspend_cyp_1718/total_budget_201718) 

###  Create separate England and CCGs only table

england <- df %>% 
  filter(ccg=="ENGLAND")

ccg_df <- df %>%
  filter(ccg!="ENGLAND")

### Assign quintile to each CCG for each 
# spend per child, perc budget, avg wait, perc referred, referrals closed

ccg_df <- ccg_df %>%
  mutate(spc_2122_quintile = ntile(spc_202122, 5)) %>% 
  mutate(pbudget_2122_quintile = ntile(pbudget_camhs_2122, 5)) %>% 
  mutate(avgwait_2122_quintile = ntile(-avg_wait_days_2122, 5)) %>% 
  mutate(percreferred_2122_quintile = ntile(percreferred_202122, 5)) %>% 
  mutate(referralsclosed_2122_quintile = ntile(-referrals_closed_2122, 5))
  

### Sum quintile scores to calculated overall ccg score

ccg_df <- ccg_df %>% 
  mutate(overall_score_2122 = spc_2122_quintile + pbudget_2122_quintile + 
  avgwait_2122_quintile + percreferred_2122_quintile + referralsclosed_2122_quintile)

quintile_cols <- c("ccg","spc_2122_quintile","pbudget_2122_quintile","avgwait_2122_quintile",
                   "percreferred_2122_quintile","referralsclosed_2122_quintile",
                   "overall_score_2122")

# Check scores
ccg_df %>% select(all_of(quintile_cols)) 

# Overall score dataframe
df_summary <- ccg_df %>% 
       select(ccg,spc_202122,pbudget_camhs_2122,avg_wait_days_2122,percreferred_202122,
              referrals_closed_2122,overall_score) %>% 
       mutate(pbudget_camhs_2122 = round(pbudget_camhs_2122*100,1),
              percreferred_202122 = round(percreferred_202122*100,1))


#### Tier 4 analysis 

# select columns
t4_days <- ccg_df %>% select(starts_with(c("t4beddays","ccg")))

# Tier 4 bed days data is quarterly - calculate value per year
t4_days 
test<- t4_days %>% 
  mutate(t4beddays_1718 = sum(t4beddays_Q1_201718:t4beddays_Q4_201718),na.rm=TRUE)

df_nhse %>% select(ccg, tier4_beddays,tier4_admissions) %>% View(.)
                                                                    

