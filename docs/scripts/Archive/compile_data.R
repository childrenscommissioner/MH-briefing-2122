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
    mutate(across(starts_with('contacts'), ~as.numeric(.))) 
}

### NHS England data ###

#import metadata
nhse_indicatornames <- read_excel(here('inputs/Raw data','OCC data file and trend_2022 - v2.1 JS NEW.xlsx'),sheet="Metadata") %>% 
  filter(!Code %in% c("Children and Young People (CYP) Mental Health","Meeting commitment to increase mental health funding"))

#import access figures
print(nhse_indicatornames %>% filter(Code=="CYP (i.c)") %>% select(`Indicator name`))
nhse_access <-  read_excel(here('inputs/Raw data','OCC data file and trend_2022 - v2.1 JS NEW.xlsx'),sheet="Trend CYP (i.c)") %>% 
  rename(access_2021=`2020/21`,
         access_2122=`2021/22`,
         ccg=`Organisation Name`,
         org_cd=`Organisation Code`)
nhse_access <- format_mh(nhse_access) 

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
print(nhse_indicatornames %>% filter(Code=="CYP(iii)") %>% select(`Indicator name`))
nhse_t4beddays <-  read_excel(here('inputs/Raw data','OCC data file and trend_2022 - v2.1 JS NEW.xlsx'),sheet="Trend CYP (iii)",range="A4:W110") 
#writetable(names(nhse_t4beddays))
#import t4 bed days names df and replace
names_t4beddays <- read_excel(here('inputs','t4_adult_names.xlsx'),sheet="t4_beddays")
names(nhse_t4beddays) <- names_t4beddays$New
nhse_t4beddays <- format_mh(nhse_t4beddays)


### import number of Tier 4 admissions
print(nhse_indicatornames %>% filter(Code=="CYP(iii)") %>% select(`Indicator name`))
nhse_t4admissions <-  read_excel(here('inputs/Raw data','OCC data file and trend_2022 - v2.1 JS NEW.xlsx'),sheet="Trend CYP (iii)",range="A114:W220")
#writetable(names(nhse_t4admissions))
names_t4admissions <- read_excel(here('inputs','t4_adult_names.xlsx'),sheet="t4_admissions")
names(nhse_t4admissions) <- names_t4admissions$New
nhse_t4admissions <- format_mh(nhse_t4admissions)


### import number of adult ward bed days
print(nhse_indicatornames %>% filter(Code=="CYP(iv)") %>% select(`Indicator name`))
nhse_adultbeddays <-  read_excel(here('inputs/Raw data','OCC data file and trend_2022 - v2.1 JS NEW.xlsx'),sheet="Trend CYP (iv)",range="A3:K109")
#writetable(names(nhse_adultbeddays))
names_adultbeddays <- read_excel(here('inputs','t4_adult_names.xlsx'),sheet="adult_beddays")
names(nhse_adultbeddays) <- names_adultbeddays$New
nhse_adultbeddays <- format_mh(nhse_adultbeddays)


#combine nhse all data
df_nhse <- nhse_access %>% 
  left_join(nhse_mhspend_perc_allocation %>% select(-ccg, -'Period type'), by= c("org_cd")) %>% 
  left_join(nhse_mhspend_allages %>% select(-ccg, -'Period type'), by= c("org_cd")) %>% 
  left_join(nhse_mhspend_cyp %>% select(-ccg, -'Period type', -'Ledger Code (Reporting)'), by= c("org_cd")) %>% 
  left_join(nhse_EDspend %>% select(-ccg, -'Period type'), by= c("org_cd")) %>% 
  left_join(nhse_t4beddays %>% select(-ccg, -'period_type'), by= c("org_cd")) %>% 
  left_join(nhse_t4admissions %>% select(-ccg, -'period_type'), by= c("org_cd")) %>%
  left_join(nhse_adultbeddays %>% select(-ccg, -'period_type'), by= c("org_cd"))


### NHS Digital data ###

# Import CCG lookup to track CCG mergers over time
ccg_lookup <- read.csv(here('inputs','ccg_lookup.csv'))

## NHSD 2021-22 ##

#import nhsd data
df_nhsd_2122 <- read_excel(here('inputs/Raw data','NHS D_additional_cyp_wait_times_2021-22.xlsx'),sheet="1b",skip=16) %>% 
  na.omit()

#import nhsd names
nhsd_names <- read.csv(here('inputs','nhsd_names.csv'))
names(df_nhsd_2122) <- nhsd_names$New

# format table
df_nhsd_2122 <- format_mh(df_nhsd_2122)

# filter out data on STPs/Region/unknowns
df_nhsd_2122 <- df_nhsd_2122 %>%  filter(str_detect(ccg, "CCG|ENGLAND"),
                    !ccg %in% c("EAST OF ENGLAND COMMISSIONING REGION")) 


## NHSD 2020-21 ##
df_nhsd_2021 <- read.csv(here('inputs','MH tables 202021.csv')) %>% 
  select(-Region, -CCG.code, -org_cd, -'X..percentage.of.children.referred.to.CYPMHS')

# rename columns
df_nhsd_2021 <- df_nhsd_2021 %>% 
  rename(ccg = 'Clinical.Commissioning.Group..CCG.',
         avg_wait_days_2021 = 'Average.waiting.time.before.treatment..days.',
         referrals_closed_2021 = 'X..of.referrals.closed.before.treatment',
         overall_score_2021 = 'CCG.overall.score')

# format columns
df_nhsd_2021 <- format_mh(df_nhsd_2021)
str(df_nhsd_2021) # no.referred will be made numeric/commas removed in next section

# create separate table for number of children referred - summarise by ccg name 
no_referred_2021 <- df_nhsd_2021 %>% select(ccg, no_referred_2021) %>% 
  left_join(ccg_lookup %>% select(-region), by = c("ccg"="ccg")) %>% 
  mutate(no_referred_2021 = str_remove(no_referred_2021,",")) %>% 
  mutate(no_referred_2021 = as.numeric(no_referred_2021)) %>% 
  group_by(ccg_new) %>% 
  mutate(no_referred_2021 = sum(no_referred_2021, na.rm = TRUE)) %>% 
  select(-ccg) %>% 
  unique()

# select columns to calculate mean
cols_2021 <- names(df_nhsd_2021)[2:4]

# join ccg lookup and summarise table by new ccg names
df_nhsd_2021 <- df_nhsd_2021 %>% 
  # remove no_referred so that the below mean function can work for CCGs that merged. 
  select(-no_referred_2021) %>% 
  left_join(ccg_lookup, by= c("ccg"="ccg")) %>% 
  group_by(ccg_new) %>% 
  mutate(across(all_of(cols_2021),  ~ mean(.x, na.rm = TRUE))) %>% 
  select(-ccg) %>% unique()

# bind on 2020/2021 figures
df_nhsd_2021 <- df_nhsd_2021 %>% 
  left_join(no_referred_2021, by = c("ccg_new")) 
str(df_nhsd_2021)

# check for NAs
df_nhsd_2021 %>% filter(is.na(no_referred_2021))


## NHSD 2019/20 ##
df_nhsd_1920 <- read.csv(here('inputs','MH tables 201920.csv')) %>%  
  select(-region, -ccg_cd, -perc_access, -X..still.waiting)

# rename columns
df_nhsd_1920 <- df_nhsd_1920 %>% 
  rename(perc_referred_1920 = perc_referral,
         avg_wait_days_1920 = avg_wait,
         referrals_closed_1920 = perc_closed,
         overall_score_1920 = overall_score)

# format columns
df_nhsd_1920 <- format_mh(df_nhsd_1920)
str(df_nhsd_1920)

# separate df for number of referrals
no_referred_1920 <- df_nhsd_1920 %>% select(ccg, no_referred_1920) %>% 
  left_join(ccg_lookup %>% select(-region), by = c("ccg"="ccg")) %>% 
  mutate(no_referred_1920 = str_remove(no_referred_1920,",")) %>% 
  mutate(no_referred_1920 = as.numeric(no_referred_1920)) %>% 
  group_by(ccg_new) %>% 
  mutate(no_referred_1920 = sum(no_referred_1920, na.rm = TRUE)) %>% 
  select(-ccg) %>% unique()

# join lookup for new ccg name
cols_1920 <- names(df_nhsd_1920)[2:5]

df_nhsd_1920 <- df_nhsd_1920 %>%
  select(-no_referred_1920) %>% 
  left_join(ccg_lookup, by= c("ccg"="ccg")) %>% 
  group_by(ccg_new) %>% 
  mutate(across(all_of(cols_1920),  ~ mean(.x, na.rm = TRUE))) %>% 
  select(-ccg) %>% unique()

# bind on no_referrals table
df_nhsd_1920 <- df_nhsd_1920 %>% 
  left_join(no_referred_1920, by=c("ccg_new"))
str(df_nhsd_1920)

# check for NAs
df_nhsd_1920 %>% filter(is.na(no_referred_1920))

### join nhsd tables together 
df_nhsd <- df_nhsd_2122 %>% 
  left_join(df_nhsd_2021, by=c("ccg"="ccg_new")) %>% 
  left_join(df_nhsd_1920 %>% select(!region), by=c("ccg"="ccg_new")) 
str(df_nhsd)


#write data
write.csv(df_nhse,here('intermediate','nhse_clean.csv'), row.names = FALSE)
write.csv(df_nhsd,here('intermediate','nhsd_clean.csv'), row.names = FALSE)



