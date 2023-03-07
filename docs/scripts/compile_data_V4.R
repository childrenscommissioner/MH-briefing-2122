# Compile MH briefing data

library(readxl)
library(here)
library(dplyr)
library(stringr)

# Function to format dataframes 
format_mh <- function(x){ 
  x %>% 
    mutate(across(starts_with('ccg'), ~ toupper(ccg))) %>% 
    mutate(across(starts_with('t4'), ~ifelse(. %in% c("","*"),NA, .))) %>% 
    mutate(across(starts_with('t4'), ~as.numeric(.))) %>% 
    mutate(across(starts_with('adultward'), ~ifelse(. %in% c("","*"), NA, .))) %>% 
    mutate(across(starts_with('adultward'), ~as.numeric(.))) %>% 
    mutate(across(starts_with('contacts'), ~as.numeric(.))) %>% 
    mutate(across(starts_with('wait_'), ~ifelse(. %in% c("","*"), NA, .))) %>% 
    mutate(across(starts_with('wait_'), ~as.numeric(.)))
}

### NHS England data ###

# Source: sent to CCO directly by NHS England. Also available on the NHS Five Year Forward View for Mental Health dashboard
# Source link: https://www.england.nhs.uk/publication/nhs-mental-health-dashboard/
# Note: There are known discrepancies between the data sent to us and that published due to reporting - NHS has confirmed our data is more accurate

#import metadata
nhse_indicatornames <- read_excel(here('inputs/Raw data','OCC data file and trend_2022 - v2.1 JS NEW.xlsx'),sheet="Metadata") %>% 
  filter(!Code %in% c("Children and Young People (CYP) Mental Health","Meeting commitment to increase mental health funding"))

#import access figures - 2+ contacts
print(nhse_indicatornames %>% filter(Code=="CYP (i.c)") %>% select(`Indicator name`))
nhse_access <-  read_excel(here('inputs/Raw data','OCC data file and trend_2022 - v2.1 JS NEW.xlsx'),sheet="Trend CYP (i.c)") %>% 
  rename(access_2021=`2020/21`,
         access_2122=`2021/22`,
         ccg=`Organisation Name`,
         org_cd=`Organisation Code`)
nhse_access <- format_mh(nhse_access) 

#import access figures - 1+ contacts
print(nhse_indicatornames %>% filter(Code=="CYP(i.a)") %>% select(`Indicator name`))
nhse_access_1contact <-  read_excel(here('inputs/Raw data','OCC data file and trend_2022 - v2.1 JS NEW.xlsx'),sheet="Trend CYP (i.a)") %>% 
  rename(access_2021_1c=`2020/21`,
         access_2122_1c=`2021/22`,
         ccg=`Organisation Name`,
         org_cd=`Organisation Code`)
nhse_access_1c <- format_mh(nhse_access) 

nhse_access_1c <- nhse_access_1c %>% 
  rename(access_2021_1c = access_2021,
         access_2122_1c = access_2122)

#import mhspend_perc_allocation figures
print(nhse_indicatornames %>% filter(Code=="MHF (i)") %>% select(`Indicator name`))
nhse_mhspend_perc_allocation <-  read_excel(here('inputs/Raw data','OCC data file and trend_2022 - v2.1 JS NEW.xlsx'),sheet="Trend MHF (i and ii)",range="A3:H109") %>% 
  rename(mhspend_perc_allocation_1718=`2017/18`,
         mhspend_perc_allocation_1819=`2018/19`,
         mhspend_perc_allocation_1920=`2019/20`,
         mhspend_perc_allocation_2021=`2020/21`,
         mhspend_perc_allocation_2122=`2021/22`,
         ccg=`Organisation name`,
         org_cd=`Organisation code`) 
nhse_mhspend_perc_allocation <- format_mh(nhse_mhspend_perc_allocation)


#import mhspend_allages figures
print(nhse_indicatornames %>% filter(Code=="MHF (ii)") %>% select(`Indicator name`))
nhse_mhspend_allages <-  read_excel(here('inputs/Raw data','OCC data file and trend_2022 - v2.1 JS NEW.xlsx'),sheet="Trend MHF (i and ii)",range="J3:Q109") %>% 
  rename(mhspend_allages_1718=`2017/18`,
         mhspend_allages_1819=`2018/19`,
         mhspend_allages_1920=`2019/20`,
         mhspend_allages_2021=`2020/21`,
         mhspend_allages_2122=`2021/22`,
         ccg=`Organisation name`,
         org_cd=`Organisation code`) 
nhse_mhspend_allages <- format_mh(nhse_mhspend_allages)

#import mhspend_perc_allocation figures
print(nhse_indicatornames %>% filter(Code=="CYP(v)") %>% select(`Indicator name`))
nhse_mhspend_cyp <-  read_excel(here('inputs/Raw data','OCC data file and trend_2022 - v2.1 JS NEW.xlsx'),sheet="Trend CYP (v and vi)",range="A3:I109") %>% 
  rename(mhspend_cyp_1718=`2017/18`,
         mhspend_cyp_1819=`2018/19`,
         mhspend_cyp_1920=`2019/20`,
         mhspend_cyp_2021=`2020/21`,
         mhspend_cyp_2122=`2021/22`,
         ccg=`Organisation name`,
         org_cd=`Organisation code`) 
nhse_mhspend_cyp <- format_mh(nhse_mhspend_cyp)

#import ED figures
print(nhse_indicatornames %>% filter(Code=="CYP(vi)") %>% select(`Indicator name`))
nhse_EDspend <-  read_excel(here('inputs/Raw data','OCC data file and trend_2022 - v2.1 JS NEW.xlsx'),sheet="Trend CYP (v and vi)",range="L3:S109") %>% 
  rename(EDspend_1718=`2017/18`,
         EDspend_1819=`2018/19`,
         EDspend_1920=`2019/20`,
         EDspend_2021=`2020/21`,
         EDspend_2122=`2021/22`,
         ccg=`Organisation name`,
         org_cd=`Organisation code`) 
nhse_EDspend <- format_mh(nhse_EDspend)

### Import Tier 4 and adult ward data 
# Data is quarterly and difficult to rename manually (as above) -> create names lookup

#function to export df names into excel to create variable names lookup
writetable <- function(x){
  x %>% write.table(file="clipboard", sep="\t", row.names=F)
}

### import number of Tier 4 bed days
nhse_t4beddays <-  read_excel(here('inputs/Raw data','OCC data file and trend_2022 - v2.1 JS NEW.xlsx'),sheet="Trend CYP (iii)",range="A4:W110") 
writetable(names(nhse_t4beddays))

## import t4 bed days names df and replace
names_t4beddays <- read_excel(here('inputs','t4_adult_names.xlsx'),sheet="t4_beddays")
names(nhse_t4beddays) <- names_t4beddays$New
nhse_t4beddays <- format_mh(nhse_t4beddays)

### import number of Tier 4 admissions
nhse_t4admissions <-  read_excel(here('inputs/Raw data','OCC data file and trend_2022 - v2.1 JS NEW.xlsx'),sheet="Trend CYP (iii)",range="A114:W220")
writetable(names(nhse_t4admissions))
names_t4admissions <- read_excel(here('inputs','t4_adult_names.xlsx'),sheet="t4_admissions")
names(nhse_t4admissions) <- names_t4admissions$New
nhse_t4admissions <- format_mh(nhse_t4admissions)

### import number of adult ward bed days
nhse_adultbeddays <-  read_excel(here('inputs/Raw data','OCC data file and trend_2022 - v2.1 JS NEW.xlsx'),sheet="Trend CYP (iv)",range="A3:K109")
writetable(names(nhse_adultbeddays))
names_adultbeddays <- read_excel(here('inputs','t4_adult_names.xlsx'),sheet="adult_beddays")
names(nhse_adultbeddays) <- names_adultbeddays$New
nhse_adultbeddays <- format_mh(nhse_adultbeddays)

#combine nhse all data
df_nhse <- nhse_access %>% 
  left_join(nhse_access_1contact %>% select(-ccg, -'Period Type'), by= c("org_cd")) %>% 
  left_join(nhse_mhspend_perc_allocation %>% select(-ccg, -'Period type'), by= c("org_cd")) %>% 
  left_join(nhse_mhspend_allages %>% select(-ccg, -'Period type'), by= c("org_cd")) %>% 
  left_join(nhse_mhspend_cyp %>% select(-ccg, -'Period type', -'Ledger Code (Reporting)'), by= c("org_cd")) %>% 
  left_join(nhse_EDspend %>% select(-ccg, -'Period type'), by= c("org_cd")) %>% 
  left_join(nhse_t4beddays %>% select(-ccg, -'period_type'), by= c("org_cd")) %>% 
  left_join(nhse_t4admissions %>% select(-ccg, -'period_type'), by= c("org_cd")) %>%
  left_join(nhse_adultbeddays %>% select(-ccg, -'period_type'), by= c("org_cd"))



# NHS Digital data --------------------------------------------------------

# Source: Sent directly to us by NHS Digital - published later on their website
# Previous years' data can be found on NHS Digital website under the title "Waiting times for children and young people's mental health services"

# Import CCG lookup to track CCG mergers over time
ccg_lookup <- read.csv(here('inputs','ccg_lookup.csv'))


# NHSD 2021-22 ------------------------------------------------------------

#import nhsd data
df_nhsd_2122_raw <- read_excel(here('inputs/Raw data','NHS D_additional_cyp_wait_times_2021-22.xlsx'),sheet="1b",skip=16) %>% 
  na.omit()

#import nhsd names
names_nhsd_2122 <- read_excel(here('inputs','nhsd_names.xlsx'),sheet="2021-22")
names(df_nhsd_2122_raw ) <- names_nhsd_2122$New

# format table
df_nhsd_2122_raw  <- format_mh(df_nhsd_2122_raw )

# create england df
england <- df_nhsd_2122_raw  %>%  filter(ccg=="ENGLAND")

# filter out data on STPs/Region/unknowns
df_nhsd_2122 <- df_nhsd_2122_raw  %>%  filter(str_detect(ccg, "CCG|ENGLAND"),
                                         !ccg %in% c("EAST OF ENGLAND COMMISSIONING REGION")) 


# NHSD 2020-21 ------------------------------------------------------------

# 2020-21 source link: https://digital.nhs.uk/supplementary-information/2021/waiting-times-for-children-and-young-peoples-mental-health-services-2020-2021

df_nhsd_2021_raw  <- read_excel(here('inputs/Raw data','additional_cyp_wait_times_2020-21_v2.xlsx'),sheet="1b",skip=16) %>% 
  na.omit()

#import nhsd names
names_nhsd_2021 <- read_excel(here('inputs','nhsd_names.xlsx'),sheet="2020-21")
names(df_nhsd_2021_raw) <- names_nhsd_2021$New

# format table
df_nhsd_2021_raw <- format_mh(df_nhsd_2021_raw)

# create england df
england <- england %>% left_join(df_nhsd_2021_raw %>%  filter(ccg=="ENGLAND") %>% select(-org_cd), by= c("ccg"))

# filter out data on STPs/Region/unknowns
df_nhsd_2021 <- df_nhsd_2021_raw  %>%  filter(str_detect(ccg, "CCG|ENGLAND"),
                                         !ccg %in% c("EAST OF ENGLAND COMMISSIONING REGION"))

# Pull in overall score from 20-21 MH briefing
mhb2021_tables <- read.csv(here('inputs','MH tables 202021.csv')) %>% 
  select('Clinical.Commissioning.Group..CCG.','CCG.overall.score') %>% 
  rename(overall_score_2021 = 'CCG.overall.score',
         ccg = 'Clinical.Commissioning.Group..CCG.') 

# Bind overall score onto df_nhsd_2021
df_nhsd_2021 <- df_nhsd_2021 %>% left_join(mhb2021_tables, by=c("ccg"))

# check for CCGs with NAs - England is meant to be NA
df_nhsd_2021 %>% filter(is.na(overall_score_2021)) 


## Specify columns for calculating means/totals for CCGs that merged in previous years
# need avg wait days, perc refs closed, overall score for mean
# no_referred, wait weeks numbers, ref closed number, sill waiting numbers, contacts before referral numbers for sums

df_nhsd_2021 <- df_nhsd_2021 %>% 
  mutate(wait_days_2021 = avg_wait_days_2021 * no_referred_2021)

sum_cols_2021 <- c("no_referred_2021","wait_0to4_weeks_2021","wait_4to6_weeks_2021","wait_6to8_weeks_2021","wait_8to10_weeks_2021","wait_10to12_weeks_2021","wait_over12_weeks_2021" ,"referrals_closed_number_2021","stillwaiting_nocontact_2021","stillwaiting_onecontact_2021","contacts_before_referral_2021","wait_days_2021")
mean_cols_2021 <- c( "overall_score_2021")


# create separate table for sum columns - summarise by ccg name 
sum_2021 <- df_nhsd_2021 %>% select(ccg, all_of(sum_cols_2021)) %>% 
  left_join(ccg_lookup %>%  unique() %>% select(-region), by = c("ccg"="ccg")) %>% #View(.)
  unique() %>%
  group_by(ccg_new) %>% 
  summarise(across(all_of(sum_cols_2021),  ~ sum(.x, na.rm = TRUE))) %>% 
  mutate(avg_wait_days_2021 = round(wait_days_2021 / no_referred_2021),
         referrals_closed_2021 = round(100 * referrals_closed_number_2021 / no_referred_2021, 0))

# select columns to calculate mean
cols_2021 <- names(df_nhsd_2021)[2:4]

# join ccg lookup and summarise table by new ccg names
mean_2021 <- df_nhsd_2021 %>% 
  # remove no_referred so that the below mean function can work for CCGs that merged. 
  select(-no_referred_2021) %>% 
  left_join(ccg_lookup %>% unique(), by= c("ccg"="ccg")) %>% 
  group_by(ccg_new) %>% 
  summarise(across(all_of(mean_cols_2021),  ~ mean(.x, na.rm = TRUE))) %>% 
  mutate( overall_score_2021 = round(overall_score_2021))

# bind on 2020/2021 figures
df_nhsd_2021 <- mean_2021 %>% 
  left_join(sum_2021, by = c("ccg_new")) 
str(df_nhsd_2021)

# check for NAs
df_nhsd_2021 %>% filter(is.na(no_referred_2021))


# NHSD 2019/20 ------------------------------------------------------------

# 2019-20 source link: https://digital.nhs.uk/supplementary-information/2020/waiting-times-for-children-and-young-peoples-mental-health-services-2019---2020-additional-statistics

# import data
df_nhsd_1920_raw  <- read_excel(here('inputs/Raw data','additional_cyp_wait_times_1920.xlsx'),sheet="1b",skip=17) %>% 
  na.omit()

# import nhsd names
names_nhsd_1920 <- read_excel(here('inputs','nhsd_names.xlsx'),sheet="2019-20")
names(df_nhsd_1920_raw) <- names_nhsd_1920$New

# format table
df_nhsd_1920_raw  <- format_mh(df_nhsd_1920_raw )

# create england df
england <- england %>% left_join(df_nhsd_1920_raw  %>%  filter(ccg=="ENGLAND") %>% select(-org_cd), by= c("ccg")) 

# filter out data on STPs/Region/unknowns
df_nhsd_1920 <- df_nhsd_1920_raw  %>%  filter(str_detect(ccg, "CCG|ENGLAND"),
                                         !ccg %in% c("EAST OF ENGLAND"))

# import 1920 overall score 
mhb1920_tables <- read.csv(here('inputs','MH tables 201920.csv')) %>%  
  select(ccg, overall_score)

# rename columns
mhb1920_tables <- mhb1920_tables %>% 
  rename(overall_score_1920 = overall_score)

# bind overall score onto df_nhsd_1920
df_nhsd_1920 <- df_nhsd_1920 %>% left_join(mhb1920_tables, by=c("ccg"))

# check for CCGs with NAs - England is meant to be NA
df_nhsd_1920 %>% filter(is.na(overall_score_1920)) 

## Specify columns for calculating means/totals for CCGs that merged in previous years
df_nhsd_1920 <- df_nhsd_1920 %>% 
  mutate(wait_days_1920 = avg_wait_days_1920 * no_referred_1920)

sum_cols_1920 <- c("no_referred_1920","wait_0to4_weeks_1920","wait_4to6_weeks_1920","wait_6to8_weeks_1920","wait_8to10_weeks_1920","wait_10to12_weeks_1920","wait_over12_weeks_1920" ,"referrals_closed_number_1920","stillwaiting_nocontact_1920","stillwaiting_onecontact_1920","contacts_before_referral_1920","wait_days_1920")
mean_cols_1920 <- c("overall_score_1920")


# create separate table for sum columns - summarise by ccg name 
sum_1920 <- df_nhsd_1920 %>% select(ccg, all_of(sum_cols_1920)) %>% 
  left_join(ccg_lookup %>%  unique() %>% select(-region), by = c("ccg"="ccg")) %>% #View(.)
  unique() %>%
  group_by(ccg_new) %>% 
  summarise(across(all_of(sum_cols_1920),  ~ sum(.x, na.rm = TRUE))) %>% 
  mutate(avg_wait_days_1920 = round(wait_days_1920 / no_referred_1920),
         referrals_closed_1920 = round(100 * referrals_closed_number_1920 / no_referred_1920, 0))

# select columns to calculate mean
cols_1920 <- names(df_nhsd_1920)[2:4]

# join ccg lookup and summarise table by new ccg names
mean_1920 <- df_nhsd_1920 %>% 
  # remove no_referred so that the below mean function can work for CCGs that merged. 
  select(-no_referred_1920) %>% 
  left_join(ccg_lookup %>% unique(), by= c("ccg"="ccg")) %>% 
  group_by(ccg_new) %>% 
  summarise(across(all_of(mean_cols_1920),  ~ mean(.x, na.rm = TRUE)))

# bind on 2020/1920 figures
df_nhsd_1920 <- mean_1920 %>% 
  left_join(sum_1920, by = c("ccg_new")) 
str(df_nhsd_1920)

# check for NAs
df_nhsd_1920 %>% filter(is.na(no_referred_1920))

### join nhsd tables together 
df_nhsd <- df_nhsd_2122 %>% 
  left_join(df_nhsd_2021, by=c("ccg"="ccg_new")) %>% 
  left_join(df_nhsd_1920, by=c("ccg"="ccg_new")) 
str(df_nhsd)


# Sense checks ------------------------------------------------------------

# 2020-21
after_2021 <- apply(df_nhsd[,sum_cols_2021[1:10]], 2, sum, na.rm = TRUE)

before_2021 <- df_nhsd_2021_raw %>% 
  filter(str_detect(ccg, "CCG|ENGLAND"),
         !ccg %in% c("EAST OF ENGLAND COMMISSIONING REGION")) %>% 
  select(all_of(sum_cols_2021[1:10])) %>% 
  apply(2, sum, na.rm = TRUE)

all(after_2021 == before_2021)

# 2019/20
after_1920 <- apply(df_nhsd[,sum_cols_1920[1:10]], 2, sum, na.rm = TRUE)

before_1920 <- df_nhsd_1920_raw %>% 
  filter(str_detect(ccg, "CCG|ENGLAND"),
         !ccg %in% c("EAST OF ENGLAND")) %>% 
  select(all_of(sum_cols_1920[1:10])) %>% 
  apply(2, sum, na.rm = TRUE)

all(after_1920 == before_1920)


# Export data -------------------------------------------------------------

write.csv(df_nhse,here('intermediate','nhse_clean.csv'), row.names = FALSE)
write.csv(df_nhsd,here('intermediate','nhsd_clean.csv'), row.names = FALSE)
write.csv(england,here('intermediate','england_df.csv'), row.names = FALSE)
