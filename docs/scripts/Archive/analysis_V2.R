
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

# Create summary dataframe
summary_cols <- c("ccg","spc_202122","pbudget_camhs_2122","avg_wait_days_2122",
                  "percreferred_202122","referrals_closed_2122","overall_score_2122")

df_summary <- ccg_df %>% 
  select(all_of(summary_cols)) %>% 
  mutate(pbudget_camhs_2122 = round(pbudget_camhs_2122*100,1),
         percreferred_202122 = round(percreferred_202122*100,1))

# Export summary table for use in the interactive map script
write.csv(df_summary, here('intermediate','summarytables_202122.csv'), row.names=FALSE)


#### Tier 4 analysis ####

# select columns
t4_days <- ccg_df %>% select(starts_with(c("ccg","t4beddays")))

# Tier 4 bed days data is quarterly - calculate value per year
t4_days <- t4_days %>% 
  rowwise() %>% 
  mutate(t4beddays_1718 = sum(c_across(t4beddays_Q1_201718:t4beddays_Q4_201718),na.rm=TRUE)) %>% 
  mutate(t4beddays_1819 = sum(c_across(t4beddays_Q1_201819:t4beddays_Q4_201819),na.rm=TRUE)) %>% 
  mutate(t4beddays_1920 = sum(c_across(t4beddays_Q1_201920:t4beddays_Q4_201920),na.rm=TRUE)) %>% 
  mutate(t4beddays_2021 = sum(c_across(t4beddays_Q1_202021:t4beddays_Q4_202021),na.rm=TRUE)) %>%
  mutate(t4beddays_2122 = sum(c_across(t4beddays_Q1_202122:t4beddays_Q4_202122),na.rm=TRUE))

  
### How many bed days did CYP have in tier 4 mental health wards? ###

# England level
t4_england <- t4_days %>% 
  select(t4beddays_1718:t4beddays_2122) %>%  
  group_by() %>% 
  summarise_at(vars(everything()),~sum(.x,na.rm=T))



# CCG level
t4_ccg <- t4_days %>% 
  select(ccg, t4beddays_1718:t4beddays_2122)

#### Adult ward analysis ####

# select columns
adultward_days <- ccg_df %>% select(starts_with(c("ccg","adultward")))

# Adult ward bed days data is quarterly - calculate value per year
adultward_days <- adultward_days %>% 
  rowwise() %>% 
  mutate(adultward_2021 = sum(c_across(adultward_Q1_202021:adultward_Q4_202021),na.rm=TRUE)) %>%
  mutate(adultward_2122 = sum(c_across(adultward_Q1_202122:adultward_Q4_202122),na.rm=TRUE))

# England level
adultward_england <- adultward_days %>% 
  select(adultward_2021:adultward_2122) %>%  
  group_by() %>% 
  summarise_at(vars(everything()),~sum(.x,na.rm=T))

# CCG level
adultward_ccg <- adultward_days %>% 
  select(ccg, adultward_2021:adultward_2122)


#### Outputs ####

# Tier 4 bed days 10 worst CCGs table
t4_top10 <- t4_ccg %>%
  select(ccg, t4beddays_2021, t4beddays_2122) %>% 
  mutate(Change = t4beddays_2122 - t4beddays_2021) %>% 
  arrange(desc(t4beddays_2122)) %>% 
  rename('Clinical Commissioning Group'=ccg,
         'No. bed days in 2020/21' = t4beddays_2021,
         'No. bed days in 2021/22' = t4beddays_2122) %>% 
  head(n=10)

write.csv(t4_top10, here('outputs','t4_top10.csv'), row.names = FALSE)

# Tier 4 England trends
write.csv(t4_england, here('outputs','t4_england.csv'), row.names = FALSE)

# Tier 4 bed days annual trend chart - 2017/18 -2021/22
# t4_england %>% 
#   rename('2017/18'=t4beddays_1718,
#          '2018/19'=t4beddays_1819,
#          '2019/20'=t4beddays_1920,
#          '2020/21'=t4beddays_2021,
#          '2021/22'=t4beddays_2122) %>% 
#   pivot_longer(cols = '2017/18':'2021/22')


# Adult ward bed days 10 worst CCGs table
adultward_top10 <- adultward_ccg %>%
  mutate(Change = adultward_2122 - adultward_2021) %>% 
  arrange(desc(adultward_2122)) %>% 
  rename('Clinical Commissioning Group'=ccg,
         'No. bed days in 2020/21' = adultward_2021,
         'No. bed days in 2021/22' = adultward_2122) %>% 
  head(n=10)
write.csv(adultward_top10, here('outputs','adultward_top10.csv'), row.names = FALSE)


# Top 10 % referred to CAMHS

ccg_df %>% 
  mutate(percreferred_change = percreferred_202122 - percreferred_202021)

percreferred_top10 <- ccg_df %>%
  mutate(percreferred_change = percreferred_202122 - percreferred_202021) %>% 
  select(ccg,percreferred_202122,percreferred_change) %>% 
  arrange(desc(percreferred_202122)) %>% 
  rename('Clinical Commissioning Group'=ccg,
         '% referred to CYPMHS' = percreferred_202122) %>% 
  head(n=10)
write.csv(percreferred_top10, here('outputs','percreferred_top10.csv'), row.names = FALSE)

# Bottom 10 % referred to CAMHS

percreferred_bottom10 <- df_summary %>%
  select(ccg,percreferred_202122) %>% 
  arrange(percreferred_202122) %>% 
  rename('Clinical Commissioning Group'=ccg,
         '% referred to CYPMHS' = percreferred_202122) %>% 
  head(n=10)
write.csv(percreferred_bottom10, here('outputs','percreferred_bottom10.csv'), row.names = FALSE)

# Top 10 % referrals closed

refsclosed_top10 <- df_summary %>%
  select(ccg,referrals_closed_2122) %>% 
  arrange(desc(referrals_closed_2122)) %>% 
  rename('Clinical Commissioning Group'=ccg,
         '% referred closed before treatment' = referrals_closed_2122) %>% 
  head(n=10)
write.csv(refsclosed_top10, here('outputs','refsclosed_top10.csv'), row.names = FALSE)


# Bottom 10 % referrals closed

refsclosed_bottom10 <- df_summary %>%
  select(ccg,referrals_closed_2122) %>% 
  arrange(referrals_closed_2122) %>% 
  rename('Clinical Commissioning Group'=ccg,
         '% referred closed before treatment' = referrals_closed_2122) %>% 
  head(n=10)
write.csv(refsclosed_bottom10, here('outputs','refsclosed_bottom10.csv'), row.names = FALSE)


# Top 10 - waiting time in days (worst ccgs - longest waiting time)
avgwait_top10 <- df_summary %>%
  select(ccg,avg_wait_days_2122) %>% 
  arrange(desc(avg_wait_days_2122)) %>% 
  rename('Clinical Commissioning Group'=ccg,
         'Average wait time (days)' = avg_wait_days_2122) %>% 
  head(n=10)
write.csv(avgwait_top10, here('outputs','avgwait_top10.csv'), row.names = FALSE)

# Bottom 10 waiting time in days (best ccgs - shortest wait time)

avgwait_bottom10 <- df_summary %>%
  select(ccg,avg_wait_days_2122) %>% 
  arrange(avg_wait_days_2122) %>% 
  rename('Clinical Commissioning Group'=ccg,
         'Average wait time (days)' = avg_wait_days_2122) %>% 
  head(n=10)

write.csv(avgwait_bottom10, here('outputs','avgwait_bottom10.csv'), row.names = FALSE)

# Top spend per child
spc_top10 <- df_summary %>%
  select(ccg,spc_202122) %>% 
  arrange(desc(spc_202122)) %>%
  mutate(spc_202122 = round(spc_202122)) %>%
  rename('Clinical Commissioning Group'=ccg,
         'Spend per child (£)' = spc_202122) %>%
  head(n=10)
write.csv(spc_top10, here('outputs','spc_top10.csv'), row.names = FALSE)


# Bottom spend per child
spc_bottom10 <- df_summary %>%
  select(ccg,spc_202122) %>% 
  arrange(spc_202122) %>% 
  mutate(spc_202122 = round(spc_202122)) %>%
  rename('Clinical Commissioning Group'=ccg,
         'Spend per child (£)' = spc_202122) %>% 
  head(n=10)
write.csv(spc_bottom10, here('outputs','spc_bottom10.csv'), row.names = FALSE)


# Top 10 - percentage of ccg budget

percbudget_top10 <- df_summary %>%
  select(ccg,pbudget_camhs_2122) %>% 
  arrange(desc(pbudget_camhs_2122)) %>%
  mutate(pbudget_camhs_2122 = round(pbudget_camhs_2122,1)) %>%
  rename('Clinical Commissioning Group'=ccg,
         '% of CCG budget (£)' = pbudget_camhs_2122) %>%
  head(n=10)
write.csv(percbudget_top10, here('outputs','percbudget_top10.csv'), row.names = FALSE)


# Bottom 10 - percentage of ccg budget
percbudget_bottom10 <- df_summary %>%
  select(ccg,pbudget_camhs_2122) %>% 
  arrange(pbudget_camhs_2122) %>% 
  mutate(pbudget_camhs_2122 = round(pbudget_camhs_2122,1)) %>%
  rename('Clinical Commissioning Group'=ccg,
         '% of CCG budget (£)' = pbudget_camhs_2122) %>% 
  head(n=10)
write.csv(percbudget_bottom10, here('outputs','percbudget_bottom10.csv'), row.names = FALSE)


# Top 20 - overall score
overall_top20 <- df_summary %>%
  select(ccg,overall_score_2122) %>% 
  arrange(desc(overall_score_2122)) %>%
  mutate(overall_score_2122 = round(overall_score_2122)) %>%
  rename('Clinical Commissioning Group'=ccg,
         'CCG overall score' = overall_score_2122) %>%
  head(n=20)
write.csv(overall_top20, here('outputs','overall_top20.csv'), row.names = FALSE)

# Bottom 20 - overall score
overall_bottom20 <- df_summary %>%
  select(ccg,overall_score_2122) %>% 
  arrange(overall_score_2122) %>% 
  mutate(overall_score_2122 = round(overall_score_2122)) %>%
  rename('Clinical Commissioning Group'=ccg,
         'CCG overall score' = overall_score_2122) %>% 
  head(n=20)
write.csv(overall_bottom20, here('outputs','overall_bottom20.csv'), row.names = FALSE)
















