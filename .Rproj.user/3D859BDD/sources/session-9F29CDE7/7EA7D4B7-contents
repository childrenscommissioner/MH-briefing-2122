
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
library(readxl)
library(scales)


# Set up ------------------------------------------------------------------

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
    mutate(across(starts_with('ccg'), ~ toupper(ccg))) %>% 
    mutate(across(starts_with('t4'), ~ifelse(. %in% c("","*"),NA, .))) %>% 
    mutate(across(starts_with('t4'), ~as.numeric(.))) %>% 
    mutate(across(starts_with('adultward'), ~ifelse(. %in% c("","*"), NA, .))) %>% 
    mutate(across(starts_with('adultward'), ~as.numeric(.))) %>% 
    mutate(across(starts_with('contacts'), ~as.numeric(.))) 
}

# Import CCG lookup
ccg_lookup <- read.csv(here('inputs','ccg_lookup.csv'))


# Import data -------------------------------------------------------------

# NHS England (NHSE)
df_nhse <- read.csv(here('intermediate','nhse_clean.csv')) %>% 
  mutate(ccg = str_replace(ccg, "NHS FYLDE & WYRE CCG", "NHS FYLDE AND WYRE CCG")) %>% 
  mutate(ccg = str_replace(ccg, "NHS NORFOLK & WAVENEY CCG", "NHS NORFOLK AND WAVENEY CCG"))

# NHS Digital (NHSD) ###
df_nhsd <- read.csv(here('intermediate','nhsd_clean.csv')) # nhsd has an extra row because it comes with one for England

# England level data frame (both NHSE and NHSD)
england <- read.csv(here('intermediate','england_df.csv'))


# Create CCG level dataframe ----------------------------------------------

# Combine nhse and nhsd tables
ccg_df <- df_nhse %>%  
  select(-org_cd) %>% 
  left_join(df_nhsd, by=c("ccg"="ccg"))
str(ccg_df)

# Import CCG/England population estimates 
pop_england <- read.csv(here('intermediate','pop_england.csv'))
pop_ccg <- read.csv(here('intermediate','pop_ccg.csv'))

# bind on CCG population estimates
ccg_df <- ccg_df %>%  left_join(pop_ccg, by=c("ccg")) 

# check for NAs/structure
ccg_df %>% filter(is.na(pop_2018)) 
ccg_df %>% filter(is.na(referrals_closed_1920))
ccg_df %>% filter(is.na(referrals_closed_2021)) %>% select(ccg, referrals_closed_2021)
str(ccg_df)

# format CCG names so that they're not all caps
ccg_df <- ccg_df %>% 
  mutate(across(starts_with('ccg'), ~str_to_title(ccg))) %>%
  mutate(across(starts_with('ccg'), ~str_replace(ccg, "Nhs","NHS"))) %>%
  mutate(across(starts_with('ccg'), ~str_replace(ccg, "Ccg","CCG"))) %>%
  mutate(across(starts_with('ccg'), ~str_replace(ccg, "And","and"))) %>%
  mutate(across(starts_with('ccg'), ~str_replace(ccg, "With","with")))


# Create England level dataframe -----------------------------------------

## Import NHSE data for England (not provided in the data sent to us)
# Data from the NHS mental health dashboard: https://www.england.nhs.uk/publication/nhs-mental-health-dashboard/

# Import metadata
nhse_indicatornames <- read_excel(here('inputs/Raw data','nhsmh-dashboard-q4-21-22-v3.2.xlsm'),sheet="Metadata") %>% 
  filter(!Code %in% c("Children and Young People (CYP) Mental Health","Meeting commitment to increase mental health funding"))


# percentage of NHS budget allocation on mental health 
eng_spend <-  read_excel(here('inputs/Raw data','nhsmh-dashboard-q4-21-22-v3.2.xlsm'),sheet="Dashboard",range="B82:K88") %>% 
  select(-'...2',-'...9') %>% 
  select('Meeting commitment to increase mental health funding','CCG spend on MH as a % of CCG base allocations')  %>% 
  rename(indicator = 'Meeting commitment to increase mental health funding',
         perc_totalallocation = 'CCG spend on MH as a % of CCG base allocations') %>% 
  mutate(year = case_when(indicator %in% c("Total actual spend - 2016/17") ~ "1617",
                          indicator %in% c("Total actual spend - 2017/18") ~ "1718",
                          indicator %in% c("Total actual spend - 2018/19 ^") ~ "1819",
                          indicator %in% c("Total actual spend - 2019/20 ^") ~ "1920",
                          indicator %in% c("Total actual spend - 2020/21 ^") ~ "2021",
                          indicator %in% c("Total actual spend - 2021/22 ^") ~ "2122")) %>% 
  mutate(label_percallocation = "mhspend_perc_allocation_")

# Percentage of NHS budget allocation spent on MH (all ages)
eng_percallocation <- eng_spend %>% 
  select(label_percallocation, year, perc_totalallocation) %>% 
  mutate(label = paste0(label_percallocation,year)) %>% 
  select(label, perc_totalallocation) %>% 
  pivot_wider(names_from = label, values_from = perc_totalallocation) %>% 
  mutate(ccg = "ENGLAND")

# Total NHS spend on mental health all ages (£m) and 
eng_mhtotalspend <- df_nhse %>% select(mhspend_allages_1718:mhspend_allages_2122) %>% 
  group_by() %>% 
  summarise_at(vars(everything()),~sum(.x,na.rm=T)) %>% 
  mutate(ccg = "ENGLAND")

# sum ccg spend on CYPMHS to get England total
eng_cypspend <- df_nhse %>% select(mhspend_cyp_1718:mhspend_cyp_2122) %>% 
  group_by() %>% 
  summarise_at(vars(everything()),~sum(.x,na.rm=T)) %>% 
  mutate(ccg = "ENGLAND")

# sum numbers accessing CYPMHS (1+ contacts)
eng_access <- df_nhse %>% select(access_2021:access_2122) %>% 
  group_by() %>% 
  summarise_at(vars(everything()),~sum(.x,na.rm=T)) %>% 
  mutate(ccg = "ENGLAND")

# Bind above tables and population estimates onto england df
england <- england %>% 
  left_join(eng_mhtotalspend, by=c("ccg")) %>% 
  left_join(eng_percallocation, by=c("ccg")) %>% 
  left_join(eng_cypspend, by=c("ccg")) %>% 
  left_join(eng_access, by=c("ccg")) %>% 
  left_join(pop_england, by=c("ccg"))


# Analysis ----------------------------------------------------------------

# Create functions for basic calculations and run for both CCG and England level DFs

# create spc_year, perc_budget, perc_referred (divide by pop), perc_closed, total CCG allocation (mhspend_allages/mhspend_perc)
# note that the latest population estimates for CCGs was published in 2022

### Spend per child
# function - easy calculations for both ccg and england level df
calc_spc <- function(x){ 
  x %>% 
    mutate(spc_202122 = mhspend_cyp_2122/pop_2020,
           spc_202021 = mhspend_cyp_2021/pop_2020,
           spc_201920 = mhspend_cyp_1920/pop_2019)
}

ccg_df <- calc_spc(ccg_df)
england <- calc_spc(england)

### Percentage of YP population referred 
calc_percreferred <- function(x){
  x %>% mutate(percreferred_202122 = no_referred_2122/pop_2020,
               percreferred_202021 = no_referred_2021/pop_2020,
               percreferred_201920 = no_referred_1920/pop_2019)
}

ccg_df <- calc_percreferred(ccg_df)
england <- calc_percreferred(england)

### Calculate total budget allocation per ccg (not just MH)
calc_totalbudget <- function(x){
  x %>% 
    mutate(total_budget_202122 = mhspend_allages_2122/mhspend_perc_allocation_2122) %>% 
    mutate(total_budget_202021 = mhspend_allages_2021/mhspend_perc_allocation_2021) %>%
    mutate(total_budget_201920 = mhspend_allages_1920/mhspend_perc_allocation_1920) %>% 
    mutate(total_budget_201819 = mhspend_allages_1819/mhspend_perc_allocation_1819) %>% 
    mutate(total_budget_201718 = mhspend_allages_1718/mhspend_perc_allocation_1718) 
}

ccg_df <- calc_totalbudget(ccg_df)
england <- calc_totalbudget(england) # England level

### Calculate proportion of budget allocated to CYPMHS
calc_pbudget <- function(x){
  x %>% 
    mutate(pbudget_camhs_2122 = mhspend_cyp_2122/total_budget_202122) %>% 
    mutate(pbudget_camhs_2021 = mhspend_cyp_2021/total_budget_202021) %>% 
    mutate(pbudget_camhs_1920 = mhspend_cyp_1920/total_budget_201920) %>% 
    mutate(pbudget_camhs_1819 = mhspend_cyp_1819/total_budget_201819) %>% 
    mutate(pbudget_camhs_1718 = mhspend_cyp_1718/total_budget_201718) 
}

ccg_df <- calc_pbudget(ccg_df) 
england <- calc_pbudget(england)


##### CCG level summary scores #####

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
ccg_df %>% select(all_of(quintile_cols)) %>% View(.)

# Create summary dataframe - CCGs
summary_cols <- c("ccg","spc_202122","pbudget_camhs_2122","avg_wait_days_2122",
                  "percreferred_202122","referrals_closed_2122","overall_score_2122")

df_summary <- ccg_df %>% 
  select(all_of(summary_cols)) %>% 
  mutate(pbudget_camhs_2122 = round(pbudget_camhs_2122*100,1),
         percreferred_202122 = round(percreferred_202122*100,1),
         spc_202122 = round(spc_202122))

# Summary for England
summary_cols_eng <- c("ccg","spc_202122","pbudget_camhs_2122","avg_wait_days_2122",
                  "percreferred_202122","referrals_closed_2122")

df_summary_england <- england %>% 
  select(all_of(summary_cols_eng)) %>% 
  mutate(spc_202122 = round(spc_202122),
         pbudget_camhs_2122 = round(pbudget_camhs_2122*100,1),
         percreferred_202122 = round(percreferred_202122*100,1))

# Export summary table for use in the interactive map script
write.csv(df_summary, here('intermediate','summarytables_202122.csv'), row.names=FALSE)
write.csv(df_summary_england, here('intermediate','summaryengland_202122.csv'), row.names=FALSE)

# Export england and ccg_df for ad hoc ccg level analysis
write.csv(ccg_df, here('intermediate','ccg_df.csv'), row.names=FALSE)
write.csv(england, here('intermediate','summarytables_202122.csv'), row.names=FALSE)

##### Tier 4 analysis #####

# select columns
t4_days <- ccg_df %>% select(starts_with(c("ccg","pop","t4beddays")))

# Tier 4 bed days data is quarterly - calculate value per year
t4_days <- t4_days %>% 
  rowwise() %>% 
  mutate(t4beddays_1718 = sum(c_across(t4beddays_Q1_201718:t4beddays_Q4_201718),na.rm=TRUE)) %>% 
  mutate(t4beddays_1819 = sum(c_across(t4beddays_Q1_201819:t4beddays_Q4_201819),na.rm=TRUE)) %>% 
  mutate(t4beddays_1920 = sum(c_across(t4beddays_Q1_201920:t4beddays_Q4_201920),na.rm=TRUE)) %>% 
  mutate(t4beddays_2021 = sum(c_across(t4beddays_Q1_202021:t4beddays_Q4_202021),na.rm=TRUE)) %>%
  mutate(t4beddays_2122 = sum(c_across(t4beddays_Q1_202122:t4beddays_Q4_202122),na.rm=TRUE))


# Calculate Tier 4 bed days per 1000 CYP - 2020 is the latest available pop estimate for CCGs
t4_days <- t4_days %>% 
  mutate(t4bedrate_1819 = round(t4beddays_1819/(pop_2018/1000)),
         t4bedrate_1920 = round(t4beddays_1920/(pop_2019/1000)),
         t4bedrate_2021 = round(t4beddays_2021/(pop_2020/1000)),
         t4bedrate_2122 = round(t4beddays_1920/(pop_2020/1000))) 


### How many bed days did CYP have in tier 4 mental health wards? ###

# England level
t4_england <- t4_days %>% 
  select(t4beddays_1718:t4beddays_2122) %>%  
  group_by() %>% 
  summarise_at(vars(everything()),~sum(.x,na.rm=T))

t4_england_rate <- t4_days %>% 
  select(t4bedrate_1819:t4bedrate_2122) %>% 
  group_by() %>% 
  summarise_at(vars(everything()), ~mean(.x,na.rm=T))

# CCG level
t4_ccg <- t4_days %>% 
  select(ccg, t4beddays_1718:t4bedrate_2122)


#### Adult ward analysis ####

# select columns
adultward_days <- ccg_df %>% select(starts_with(c("ccg","pop","adultward")))

# Adult ward bed days data is quarterly - calculate value per year
adultward_days <- adultward_days %>% 
  rowwise() %>% 
  mutate(adultward_2021 = sum(c_across(adultward_Q1_202021:adultward_Q4_202021),na.rm=TRUE)) %>%
  mutate(adultward_2122 = sum(c_across(adultward_Q1_202122:adultward_Q4_202122),na.rm=TRUE))

# Calculate adult ward bed days per 1000 CYP - 2020 is the latest available pop estimate for CCGs
adultward_days <- adultward_days %>% 
  mutate(adultwardrate_2021 = round(adultward_2021/(pop_2020/1000)),
         adultwardrate_2122 = round(adultward_2122/(pop_2020/1000))) 


# England level
adultward_england <- adultward_days %>% 
  select(adultward_2021:adultward_2122) %>%  
  group_by() %>% 
  summarise_at(vars(everything()),~sum(.x,na.rm=T))

# CCG level
adultward_ccg <- adultward_days %>% 
  select(ccg, adultward_2021:adultwardrate_2122)


##### Outputs #####

# Tier 4- NUMBER of bed days - top 10 worst CCGs table
t4_top10 <- t4_ccg %>%
  select(ccg, t4beddays_2021, t4beddays_2122) %>% 
  mutate(Change = t4beddays_2122 - t4beddays_2021) %>% 
  arrange(desc(t4beddays_2122)) %>% 
  rename('Clinical Commissioning Group'=ccg,
         'No. bed days in 2020-21' = t4beddays_2021,
         'No. bed days in 2021-22' = t4beddays_2122) %>% 
  head(n=10)
write.csv(t4_top10, here('outputs','t4_top10.csv'), row.names = FALSE)

### Tier 4 bed days RATE - 10 worst CCGs table ###
t4rate_top10 <- t4_ccg %>%
  select(ccg, t4beddays_2122, t4bedrate_2021, t4bedrate_2122) %>% 
  mutate(Change = t4bedrate_2122 - t4bedrate_2021) %>% 
  arrange(desc(t4bedrate_2122)) %>% 
  rename('Clinical Commissioning Group'=ccg,
         'Total number of bed days in 2021-22' = t4beddays_2122,
         'Number of bed days per 1000 CYP in 2020-21' = t4bedrate_2021,
         'Number of bed days per 1000 CYP in 2021-22' = t4bedrate_2122) %>% 
  head(n=10)
write.csv(t4rate_top10, here('outputs','t4rate_top10.csv'), row.names = FALSE)


# Tier 4 England trends
write.csv(t4_england, here('outputs','t4_england.csv'), row.names = FALSE)


### PLOT: Histogram/boxplot to show distribution of tier 4 bed days across CCGs #####

# long version of t4_ccg dataframe
t4_ccg_long <- t4_ccg %>% 
  select(starts_with(c("ccg","t4bedrate"))) %>% 
  pivot_longer(cols= t4bedrate_1819:t4bedrate_2122, values_to = "rate", names_to = "name") %>% 
  mutate(year = case_when(name %in% c("t4bedrate_1819") ~ "2018-19",
                          name %in% c("t4bedrate_1920") ~ "2019-20",
                          name %in% c("t4bedrate_2021") ~ "2020-21",
                          name %in% c("t4bedrate_2122") ~ "2021-22")) %>% 
  select(-name)

# Tier 4 facet histogram 
t4_ccg_long %>% 
  filter(year %in% c("2018-19","2021-22")) %>% 
  ggplot(aes(x=rate)) +
  geom_histogram(binwidth=3, colour="black", fill=occ_pal[2])+
   #geom_vline(aes(xintercept = mean(rate)),col="#202B51",size=2)+
  facet_wrap(~year, ncol = 1)+
 
  theme(text=element_text(family="HindVadodara",size=70,colour="#202B51"),
        strip.text = element_text(family="HindVadodara", size = 100, color = "#202B51"))+
  theme_occ() +
 
  labs(y= "Count of CCGs", x = "Number of Tier 4 bed days per 1000 CYP")
ggsave(here('outputs','t4rate_hist.png'),width=5,height=5)

# create box plot
ggplot(t4_ccg_long, aes(x=year, y=rate)) + 
  geom_boxplot()+
  theme_occ()


# Top 10 CCGs with the largest increases in Tier 4 bed days.
t4changes_top10 <- t4_ccg %>%
  select(ccg, t4beddays_2122,t4bedrate_2021, t4bedrate_2122) %>% 
  mutate(Change = t4bedrate_2122 - t4bedrate_2021) %>% 
  arrange(desc(Change)) %>% 
  rename('Clinical Commissioning Group'=ccg,
         'Number of bed days in 2021-22' = t4beddays_2122,
         'Rate of bed days per 1000 CYP in 2020-21' = t4bedrate_2021,
         'Rate of bed days per 1000 CYP in 2021-22' = t4bedrate_2122) %>% 
  head(n=10)
write.csv(t4changes_top10, here('outputs','t4rate_changes.csv'), row.names = FALSE)


# Tier 4 bed days annual trend chart - 2017/18 -2021/22 
t4_eng_long <- t4_england %>% 
  pivot_longer(cols=1:5, values_to = "beddays", names_to = "name") %>% 
  mutate(Year = case_when(name %in% c("t4beddays_1718") ~ "2017-18",
                           name %in% c("t4beddays_1819") ~ "2018-19",
                           name %in% c("t4beddays_1920") ~ "2019-20",
                           name %in% c("t4beddays_2021") ~ "2020-21",
                           name %in% c("t4beddays_2122") ~ "2021-22"))


# PLOT: create bar chart
t4_eng_long %>% 
  ggplot(aes(x=Year, y= beddays))+
  geom_col(fill=occ_pal[2])+
  ylab("Number of bed days in Tier 4 wards") + xlab("Year") +
  geom_text(aes(label=beddays), position=position_dodge(width=0.9), vjust=-0.25, size=3.5,colour=occ_pal[1])+
  theme(text=element_text(family="HindVadodara",size=100,colour=occ_pal[1]))+
  theme_occ()
ggsave(here('outputs','T4beddays_trends.png'),width=5,height=5)


### Adult ward bed days 10 worst CCGs table ###
adultward_top10 <- adultward_ccg %>%
  select(-adultwardrate_2021,-adultwardrate_2122) %>% 
  mutate(Change = adultward_2122 - adultward_2021) %>% 
  arrange(desc(adultward_2122)) %>% 
  rename('Clinical Commissioning Group'=ccg,
         'No. bed days in 2020-21' = adultward_2021,
         'No. bed days in 2021-22' = adultward_2122) %>% 
  head(n=10)
write.csv(adultward_top10, here('outputs','adultward_top10.csv'), row.names = FALSE)



######   Main analysis (not tier 4/inpatient settings)   ######

### Percentage referred to CAMHS ###

# Check distribution of perc referred with box plot/or summary
# shows mean and distribution of CCGs
perc_referred_long <- ccg_df %>%
  select(starts_with(c("ccg","percreferred_20"))) %>%
#  mutate(percreferred_201920 = percreferred_201920*100,
#         percreferred_202021 = percreferred_202021*100,
#         percreferred_202122 = percreferred_202122*100) %>% 
  select(ccg, percreferred_201920,percreferred_202021,percreferred_202122) %>% #summary()
  pivot_longer(cols=percreferred_201920:percreferred_202122, names_to = "name", values_to = "perc") %>% 
  mutate(year = case_when(name %in% c("percreferred_201920") ~ "2019-20",
                          name %in% c("percreferred_202021") ~ "2020-21",
                          name %in% c("percreferred_202122") ~ "2021-22"))


perc_referred_long %>% 
  ggplot(aes(x=year, y=perc))+
    geom_boxplot(fill=occ_pal[2])+
  theme_occ()+
  theme(text=element_text(family="HindVadodara",size=30,colour="#202B51"),
        axis.text.x=element_text(family="HindVadodara",size=30,colour="#202B51"))+
  scale_y_continuous(label=scales::percent,limits=c(0, max(perc_referred_long$perc)+0.01)) +
  ylab("% of children referred") + xlab("Year")
ggsave(here('outputs','preferred_box.png'),width=7,height=4)

## NB: SC adding in below: percentage referred at national level
round(sum(ccg_df$no_referred_2122)/sum(ccg_df$pop_2020)*100)
round(sum(ccg_df$no_referred_2021)/sum(ccg_df$pop_2020)*100)
round(sum(ccg_df$no_referred_1920)/sum(ccg_df$pop_2019)*100)

### PLOT: Trend bar chart of children referred

# England long table - older figures taken from old briefings and collated in a table
no_referred_eng <- read.csv(here('inputs','no_referred_trend.csv'))
  
no_referred_eng %>% 
  rename(no_referred = 'No..children.referred') %>% 
  ggplot(aes(x=Year, y= no_referred))+
  geom_col(fill=occ_pal[2])+
  ylab("Number of children referred to CYPMHS") + xlab("Year") +
  geom_text(aes(label=scales::comma(no_referred)), position=position_dodge(width=0.9), vjust=-0.25, size=10,colour=occ_pal[1])+
  theme_occ() +
  theme(text=element_text(family="HindVadodara",size=30,colour=occ_pal[1]))+
  scale_y_continuous(label=scales::comma)
ggsave(here('outputs','no_referred_trends.png'),width=7.5,height=3)


# number of CCGs decrease referral rate
ccg_df %>% 
  select(starts_with(c("ccg","percreferred_20"))) %>%
  mutate(percreferred_202021 = round(percreferred_202021*100),
         percreferred_202122 = round(percreferred_202122*100),
    Change = round((percreferred_202122-percreferred_202021)*100)) %>% 
  select(ccg,percreferred_202021,percreferred_202122,Change) %>% 
  filter(Change < 0) %>% 
  tally()  

# calculate percentage
print((0/106)*100)


# Top 10 % referred to CAMHS  
percreferred_top10 <- ccg_df %>%
  select(starts_with(c("ccg","percreferred_20"))) %>%
  mutate(Change = round((percreferred_202122-percreferred_202021)*100),
         percreferred_202021 = round(percreferred_202021*100),
         percreferred_202122 = round(percreferred_202122*100)) %>% 
  select(ccg,percreferred_202021,percreferred_202122,Change) %>% 
  arrange(desc(percreferred_202122)) %>% 
  rename('Clinical Commissioning Group'=ccg,
         '% referred to CYPMHS in 2020-21' = percreferred_202021,
         '% referred to CYPMHS in 2021-22' = percreferred_202122) %>% 
  head(n=10)
write.csv(percreferred_top10, here('outputs','percreferred_top10.csv'), row.names = FALSE)


# Bottom 10 % referred to CAMHS
percreferred_bottom10 <- ccg_df %>%
  select(starts_with(c("ccg","percreferred_20"))) %>%
  mutate(Change = round((percreferred_202122-percreferred_202021)*100),
         percreferred_202021 = round(percreferred_202021*100),
         percreferred_202122 = round(percreferred_202122*100)) %>% 
  select(ccg,percreferred_202021,percreferred_202122,Change) %>% 
  arrange(percreferred_202122) %>% 
  rename('Clinical Commissioning Group'=ccg,
         '% referred to CYPMHS in 2020-21' = percreferred_202021,
         '% referred to CYPMHS in 2021-22' = percreferred_202122) %>% 
  head(n=10)
write.csv(percreferred_bottom10, here('outputs','percreferred_bottom10.csv'), row.names = FALSE)


##### Referrals closed #####

# Check distribution of % referrals closed with box plot/or summary

# England total of children with referrals closed
print(england %>% select(referrals_closed_number_2122))

# show mean and distribution of CCGs - take # off of "summary(")" on line 536 to switch from boxplot
ccg_df %>%
  select(starts_with(c("ccg","referrals_closed"))) %>%
  select(ccg, referrals_closed_1920,referrals_closed_2021,referrals_closed_2122) %>% #summary()
  pivot_longer(cols=referrals_closed_1920:referrals_closed_2122, names_to = "name", values_to = "perc") %>% 
  mutate(year = case_when(name %in% c("referrals_closed_1920") ~ "2019-20",
                          name %in% c("referrals_closed_2021") ~ "2020-21",
                          name %in% c("referrals_closed_2122") ~ "2021-22")) %>% 
  mutate(perc=perc/100) %>% 
  ggplot(aes(x=year, y=perc))+
  geom_boxplot(fill="white")+
  theme_occ()+
  theme(text=element_text(family="HindVadodara",size=30,colour="#202B51"),
        axis.text.x=element_text(family="HindVadodara",size=30,colour="#202B51"))+
  scale_y_continuous(labels=percent,limits = c(0,0.5))+
  ylab("% of children who had referrals closed") + xlab("Year")
ggsave(here('outputs','refsclosed_box.png'),width=7,height=3)


### PLOT: Trend bar chart for % of referrals closed before treatment

# England long table - older figures taken from old briefings and collated in a table
refsclosed_trend <- read.csv(here('inputs','refsclosed_trend.csv'))

refsclosed_trend %>% 
  ggplot(aes(x=Year, y= perc))+
  geom_col(fill=occ_pal[2])+
  ylab("% of referrals closed before treatment") + xlab("Year") +
  geom_text(aes(label=perc), position=position_dodge(width=0.9), vjust=-0.25, size=8,colour=occ_pal[1])+
  theme_occ()+
  theme(text=element_text(family="HindVadodara",size=30,colour=occ_pal[1]))
  #scale_y_continuous(label=scales::percent)
ggsave(here('outputs','refsclosed_trends.png'),width=5,height=3)


# number of CCGs increased - their referral closure rate
ccg_df %>% 
  select(starts_with(c("ccg","referrals_closed"))) %>%
  mutate(Change = round((referrals_closed_2122-referrals_closed_2021))) %>% 
  select(ccg,referrals_closed_2021,referrals_closed_2122,Change) %>% 
  filter(Change > 0) %>% 
  tally() 

print((96/106)*100)


# Highest 10 % referrals closed (worst)
refsclosed_top10 <- ccg_df %>%
  select(starts_with(c("ccg","referrals_closed"))) %>% 
  mutate(Change = round((referrals_closed_2122-referrals_closed_2021))) %>% 
  select(ccg,referrals_closed_2021,referrals_closed_2122,Change) %>% 
  arrange(desc(referrals_closed_2122)) %>% 
  rename('Clinical Commissioning Group'=ccg,
         '% closed before treatment in 2020-21' = referrals_closed_2021,
         '% closed before treatment in 2021-22' = referrals_closed_2122) %>% 
  head(n=10)
write.csv(refsclosed_top10, here('outputs','refsclosed_top10.csv'), row.names = FALSE)

 
# Lowest 10 % referrals closed (best)
refsclosed_bottom10 <- ccg_df %>%
  select(starts_with(c("ccg","referrals_closed"))) %>%
  mutate(Change = round((referrals_closed_2122-referrals_closed_2021))) %>% 
  select(ccg,referrals_closed_2021,referrals_closed_2122,Change) %>% 
  arrange(referrals_closed_2122) %>% 
  rename('Clinical Commissioning Group'=ccg,
         '% closed before treatment in 2020-21' = referrals_closed_2021,
         '% closed before treatment in 202-22' = referrals_closed_2122) %>% 
  head(n=10)
write.csv(refsclosed_bottom10, here('outputs','refsclosed_bottom10.csv'), row.names = FALSE)

# Which CCGs increased closure rate the most/ decreased the most
refsclosed_change <- ccg_df %>%
  select(starts_with(c("ccg","referrals_closed"))) %>%
  mutate(Change = round((referrals_closed_2122-referrals_closed_2021))) %>% 
  select(ccg,referrals_closed_2021,referrals_closed_2122,Change) %>% 
  arrange(desc(Change)) %>% 
  rename('Clinical Commissioning Group'=ccg,
         '% closed before treatment in 2020-21' = referrals_closed_2021,
         '% closed before treatment in 2021-22' = referrals_closed_2122) %>% 
  filter(Change > 0) %>% 
  #filter(Change < 0) %>% # for biggest decreases
  head(n=10)
write.csv(refsclosed_change, here('outputs','refsclosed_change.csv'), row.names = FALSE)


#### Waiting times and outcomes ####

# Check distribution of average waiting times with box plot/or summary

# England average waiting time
print(england %>% select(avg_wait_days_2122))

# avg wait dataframe
wait_df <- ccg_df %>% 
  select(starts_with(c("ccg","avg_wait_days"))) %>% 
  mutate(avg_wait_days_1920 = round(avg_wait_days_1920),
         avg_wait_days_2021 = round(avg_wait_days_2021)) %>% #View(.)
  select(ccg, avg_wait_days_1920, avg_wait_days_2021,avg_wait_days_2122) 
  
# Check mean and distribution of CCGs 
summary(wait_df)

# boxplot
wait_df %>% 
  pivot_longer(cols=2:4, names_to = "name", values_to = "days") %>% 
  mutate(year = case_when(name %in% c("avg_wait_days_1920") ~ "2019-20",
                          name %in% c("avg_wait_days_2021") ~ "2020-21",
                          name %in% c("avg_wait_days_2122") ~ "2021-22")) %>% 
  ggplot(aes(x=year, y=days))+
  geom_boxplot()+
  theme(text=element_text(family="HindVadodara",size=100,colour="#202B51"),
        axis.text.x=element_text(family="HindVadodara",size=100,colour="#202B51"))+
  theme_occ()+
  ylab("Average wait time (days)") + xlab("Year")
ggsave(here('outputs','wait_box.png'),width=5,height=5)


### bar chart - annual trends in average wait (days) ###

# create england wait time df - full data only available for 2019-2022
  # create df with older stats sourced from previous MH briefings
wait_1719 <- data.frame(ccg = "England",                     
                    days = c("57","53"),
                    year = c("2017-18","2018-19"))
print(wait_1719)

wait_england <- england %>% 
  select(starts_with(c("ccg","avg_wait_days"))) %>% 
  mutate(avg_wait_days_1920 = round(avg_wait_days_1920),
         avg_wait_days_2021 = round(avg_wait_days_2021)) %>% #View(.)
  select(ccg, avg_wait_days_1920, avg_wait_days_2021,avg_wait_days_2122) %>% 
  pivot_longer(cols=2:4, names_to = "name", values_to = "days") %>% 
  mutate(year = case_when(name %in% c("avg_wait_days_1920") ~ "2019-20",
                          name %in% c("avg_wait_days_2021") ~ "2020-21",
                          name %in% c("avg_wait_days_2122") ~ "2021-22")) %>% 
  select(ccg,days,year)


# rbind() the full table together
wait_england <- wait_england %>% rbind(wait_1719) %>% 
  arrange(year)
print(wait_england)

# create bar chart
wait_england %>% 
  mutate(days = as.numeric(days)) %>% 
  ggplot(aes(x=year, y=days))+
  geom_col(fill=occ_pal[2])+
  ylab("Average wait time (days)") + xlab("Year") +
  geom_text(aes(label=days), position=position_dodge(width=0.9), vjust=-0.25, size=6,colour=occ_pal[1])+
  theme_occ()+
  theme(text=element_text(family="HindVadodara",size=25,colour=occ_pal[1]))+
  scale_y_continuous(limit=c(0,60))
ggsave(here('outputs','wait_trends.png'),width=5,height=3)


# number of CCGs increased waiting times
wait_df %>%
  mutate(Change = avg_wait_days_2122-avg_wait_days_2021) %>% 
  select(ccg, avg_wait_days_2021,avg_wait_days_2122, Change) %>% 
  filter(Change > 0) %>% 
  tally() 
print((80/106)*100)

### national stacked bar chart on outcomes 

# 21-22 waiting times data
wait_cols_2122 <- c("ccg","no_referred_2122","wait_0to4_weeks_2122","wait_4to6_weeks_2122","wait_6to8_weeks_2122",
               "wait_8to10_weeks_2122","wait_10to12_weeks_2122","wait_over12_weeks_2122","referrals_closed_number_2122",
               "stillwaiting_nocontact_2122","stillwaiting_onecontact_2122","contacts_before_referral_2122")

# number and percentage of children by waiting time (within 4 weeks, over 4 weeks, still waiting, referrals closed)
wait_outcomes_2122 <- df_nhsd %>% filter(ccg %in% c("ENGLAND")) %>% 
  select(all_of(wait_cols_2122)) %>% 
  mutate(ccg = str_replace(ccg, "ENGLAND","England")) %>% 
  rowwise() %>% 
  mutate(within_4w = sum(wait_0to4_weeks_2122,contacts_before_referral_2122),
         over_4w = sum(wait_4to6_weeks_2122, wait_6to8_weeks_2122, wait_8to10_weeks_2122, wait_10to12_weeks_2122, wait_over12_weeks_2122),
         still_waiting = sum(stillwaiting_nocontact_2122,stillwaiting_onecontact_2122)) %>%
  mutate(within_year = within_4w + over_4w,
         within_4w_perc = (within_4w/no_referred_2122)*100,
         over_4w_perc = (over_4w/no_referred_2122)*100,
         still_waiting_perc =  (still_waiting/no_referred_2122)*100,
         refsclosed_perc = (referrals_closed_number_2122/no_referred_2122)*100,
         within_year_perc = (within_year/no_referred_2122)*100) 

# percentage of children seen within year
print(wait_outcomes_2122$within_year_perc)

# create long table for 2021-22 data.
wait_long <- wait_outcomes_2122 %>% 
  select(ccg, within_4w_perc:refsclosed_perc) %>% 
  pivot_longer(cols = within_4w_perc:refsclosed_perc, names_to="outcome", values_to ="Percentage") %>% 
  mutate(Year = paste0("2021-22"))


## 20-21 waiting times data
wait_cols_2021 <- c("ccg","no_referred_2021","wait_0to4_weeks_2021","wait_4to6_weeks_2021","wait_6to8_weeks_2021",
                    "wait_8to10_weeks_2021","wait_10to12_weeks_2021","wait_over12_weeks_2021","referrals_closed_number_2021",
                    "stillwaiting_nocontact_2021","stillwaiting_onecontact_2021","contacts_before_referral_2021")

# number and percentage of children by waiting time (within 4 weeks, over 4 weeks, still waiting, referrals closed)
wait_outcomes_2021 <- df_nhsd %>% filter(ccg %in% c("ENGLAND")) %>% 
  select(all_of(wait_cols_2021)) %>% 
  mutate(ccg = str_replace(ccg, "ENGLAND","England")) %>% 
  rowwise() %>% 
  mutate(within_4w = sum(wait_0to4_weeks_2021,contacts_before_referral_2021),
         over_4w = sum(wait_4to6_weeks_2021, wait_6to8_weeks_2021, wait_8to10_weeks_2021, wait_10to12_weeks_2021, wait_over12_weeks_2021),
         still_waiting = sum(stillwaiting_nocontact_2021,stillwaiting_onecontact_2021)) %>%
  mutate(within_year = within_4w + over_4w,
         within_4w_perc = (within_4w/no_referred_2021)*100,
         over_4w_perc = (over_4w/no_referred_2021)*100,
         still_waiting_perc =  (still_waiting/no_referred_2021)*100,
         refsclosed_perc = (referrals_closed_number_2021/no_referred_2021)*100,
         within_year_perc = (within_year/no_referred_2021)*100)


# create long table for 2020-21 data.
wait_long_2021 <- wait_outcomes_2021 %>% 
  select(ccg, within_4w_perc:refsclosed_perc) %>% 
  pivot_longer(cols = within_4w_perc:refsclosed_perc, names_to="outcome", values_to ="Percentage") %>% 
  mutate(Year = paste0("2020-21"))


### 19-20 waiting times data
wait_cols_1920 <- c("ccg","no_referred_1920","wait_0to4_weeks_1920","wait_4to6_weeks_1920","wait_6to8_weeks_1920",
                    "wait_8to10_weeks_1920","wait_10to12_weeks_1920","wait_over12_weeks_1920","referrals_closed_number_1920",
                    "stillwaiting_nocontact_1920","stillwaiting_onecontact_1920","contacts_before_referral_1920")

# number and percentage of children by waiting time (within 4 weeks, over 4 weeks, still waiting, referrals closed)
wait_outcomes_1920 <- df_nhsd %>% filter(ccg %in% c("ENGLAND")) %>% 
  select(all_of(wait_cols_1920)) %>% 
  mutate(ccg = str_replace(ccg, "ENGLAND","England")) %>% 
  rowwise() %>% 
  mutate(within_4w = sum(wait_0to4_weeks_1920,contacts_before_referral_1920),
         over_4w = sum(wait_4to6_weeks_1920, wait_6to8_weeks_1920, wait_8to10_weeks_1920, wait_10to12_weeks_1920, wait_over12_weeks_1920),
         still_waiting = sum(stillwaiting_nocontact_1920,stillwaiting_onecontact_1920)) %>%
  mutate(within_year = within_4w + over_4w,
         within_4w_perc = (within_4w/no_referred_1920)*100,
         over_4w_perc = (over_4w/no_referred_1920)*100,
         still_waiting_perc =  (still_waiting/no_referred_1920)*100,
         refsclosed_perc = (referrals_closed_number_1920/no_referred_1920)*100,
         within_year_perc = (within_year/no_referred_1920)*100)

# create long table for 2019-20 data.
wait_long_1920 <- wait_outcomes_1920 %>% 
  select(ccg, within_4w_perc:refsclosed_perc) %>% 
  pivot_longer(cols = within_4w_perc:refsclosed_perc, names_to="outcome", values_to ="Percentage") %>% 
  mutate(Year = paste0("2019-20"))

# Bind r bind wait_long tables together
wait_long_full <- wait_long %>% rbind(wait_long_2021) %>% rbind(wait_long_1920)

# create stacked bar chart
wait_long_full %>% 
  mutate(label = case_when(outcome %in% c("within_4w_perc") ~ "Entered treatment in 4 weeks or less",
                           outcome %in% c("over_4w_perc") ~ "Entered treatment in over 4 weeks",
                           outcome %in% c("still_waiting_perc") ~ "Still waiting",
                           outcome %in% c("refsclosed_perc") ~ "Referrals closed" )) %>% 
ggplot(aes(fill=str_wrap(label,20), y=Percentage, x=Year, label = round(Percentage))) + 
  geom_bar(position="stack", stat="identity")+
  scale_fill_manual(values=c(occ_pal[1],occ_pal[2],occ_pal[3],occ_pal[5]))+
    geom_text(position=position_stack(vjust=0.5),size=9,colour="white", fontface="bold")+
  #scale_y_continuous(labels = scales::percent)+
  coord_flip() +
    theme_occ()+
  theme(text=element_text(family="HindVadodara",size=30,colour=occ_pal[1]),
        legend.title = element_blank(),
        legend.text = element_text(lineheight = 0.3), legend.key.height = unit(1, "cm"))+
  ylab("Percentage (%)")
ggsave(here('outputs','wait_stackbar4w.png'),width=6,height=4)


### wait outcomes by CCG ###
wait_outcomes_ccg <- df_nhsd %>% 
  select(all_of(wait_cols_2122)) %>% 
  rowwise() %>% 
  mutate(within_4w = sum(wait_0to4_weeks_2122,contacts_before_referral_2122),
         over_4w = sum(wait_4to6_weeks_2122, wait_6to8_weeks_2122, wait_8to10_weeks_2122, wait_10to12_weeks_2122, wait_over12_weeks_2122),
         still_waiting = sum(stillwaiting_nocontact_2122,stillwaiting_onecontact_2122)) %>%
  mutate(within_year = within_4w + over_4w,
         within_4w_perc = round((within_4w/no_referred_2122)*100),
         over_4w_perc = round((over_4w/no_referred_2122)*100),
         still_waiting_perc =  round((still_waiting/no_referred_2122)*100),
         refsclosed_perc = round((referrals_closed_number_2122/no_referred_2122)*100),
         within_year_perc = round((within_year/no_referred_2122)*100)) 

# % children still waiting - mean, top and bottom ccg
summary(wait_outcomes_ccg)
wait_outcomes_ccg %>% arrange(desc(still_waiting_perc)) %>% select(ccg, still_waiting, still_waiting_perc) %>% head(n=10)

# Top 10 - waiting time in days (worst ccgs - longest waiting time)

# Longest 10 waiting time (worst)
avgwait_top10 <- ccg_df %>%
  select(starts_with(c("ccg","avg_wait_days"))) %>%
  mutate(Change = avg_wait_days_2122- avg_wait_days_2021) %>% 
  select(ccg,avg_wait_days_2021,avg_wait_days_2122,Change) %>% 
  arrange(desc(avg_wait_days_2122)) %>% 
  rename('Clinical Commissioning Group'=ccg,
         'Average wait time (days) in 2020-21' = avg_wait_days_2021,
         'Average wait time (days) in 2021-22' = avg_wait_days_2122) %>% 
  head(n=10)
write.csv(avgwait_top10, here('outputs','avgwait_top10.csv'), row.names = FALSE)

# Bottom 10 waiting time in days (best ccgs - shortest wait time)
avgwait_bottom10 <- ccg_df %>%
  select(starts_with(c("ccg","avg_wait_days"))) %>%
  mutate(Change = avg_wait_days_2122- avg_wait_days_2021) %>% 
  select(ccg,avg_wait_days_2021,avg_wait_days_2122,Change) %>% 
  arrange(avg_wait_days_2122) %>% 
  rename('Clinical Commissioning Group'=ccg,
         'Average wait time (days) in 2020-21' = avg_wait_days_2021,
         'Average wait time (days) in 2021-22' = avg_wait_days_2122) %>% 
  head(n=10)
write.csv(avgwait_bottom10, here('outputs','avgwait_bottom10.csv'), row.names = FALSE)


# Which CCGs increased waiting times the most/ decreased the most
avgwait_change <- ccg_df %>%
  select(starts_with(c("ccg","avg_wait_days"))) %>%
  mutate(Change = avg_wait_days_2122- avg_wait_days_2021) %>% 
  select(ccg,avg_wait_days_2021,avg_wait_days_2122,Change) %>% 
  arrange(desc(Change)) %>% 
  rename('Clinical Commissioning Group'=ccg,
         'Average wait time (days) in 2020-21' = avg_wait_days_2021,
         'Average wait time (days) in 2021-22' = avg_wait_days_2122) %>% #View(.)
  head(n=10)
write.csv(avgwait_change, here('outputs','avgwait_change.csv'), row.names = FALSE)

# CCGs with waiting times of 21 days or less
wait_df %>% filter(avg_wait_days_2122 <22) %>% tally()
print((11/106)*100)

# Number/percentage of CCGs improved waiting time
wait_df %>%
  mutate(Change = avg_wait_days_2122- avg_wait_days_2021) %>% 
  filter(Change < 0) %>% tally()
print((22/106)*100)


# CCG spend on CAMHS ------------------------------------------------------


# Calculate real spend values ---------------------------------------------
# ALL SPEND DATA SOURCED FROM NHS ENGLAND
# GDP deflators sourced from ONS

# Total NHS Budget 
england %>% 
  select(starts_with("total_budget")) %>% print()

# import GDP deflators
gdpdeflators <-  read_excel(here('inputs','GDP deflators 092022.xlsx'),sheet="30092022 deflator update",range="B7:F79") %>% 
  rename(deflator = `2021-22 = 100`,
         year = 'Financial year') %>% 
  select(year, deflator)

# create ENGLAND level long data frame for total spend
spend_trend <- england %>% 
  select(starts_with(c("ccg","mhspend_cyp"))) %>% 
  pivot_longer(cols=2:6, values_to = "spend", names_to = "names") %>% 
  mutate(year = case_when(names %in% c("mhspend_cyp_1718") ~ "2017-18",
                          names %in% c("mhspend_cyp_1819") ~ "2018-19",
                          names %in% c("mhspend_cyp_1920") ~ "2019-20",
                          names %in% c("mhspend_cyp_2021") ~ "2020-21",
                          names %in% c("mhspend_cyp_2122") ~ "2021-22"))
# join on GDP deflators
spend_trend <- spend_trend %>% left_join(gdpdeflators, by=c("year"))

# Reference year deflator = 19/20 pre-COVID
reference_deflator_value <- spend_trend %>% 
  filter(year == "2019-20") %>% 
  pull(deflator)

spend_trend <- spend_trend %>% 
  mutate(reference_deflator = reference_deflator_value)

spend_trend <- spend_trend %>% 
  mutate(deflator = as.numeric(deflator),
         reference_deflator = as.numeric(reference_deflator),
         deflated_value = spend/deflator) %>% 
  mutate(real_spend = round(deflated_value*reference_deflator))

### bar chart showing real spend trends

spend_trend %>% 
  mutate(label = round(real_spend/1000000)) %>% 
  ggplot(aes(x=year, y=label))+
  geom_col(fill=occ_pal[2])+
  ylab("Real spend on CYPMHS (£ millions)") + xlab("Year") +
  geom_text(aes(label=label), position=position_dodge(width=0.9), vjust=-0.25, size=5,colour=occ_pal[1])+
  theme_occ()+
  theme(axis.text = element_text(family="HindVadodara",size=15,colour=occ_pal[1]),
        axis.title = element_text(family="HindVadodara",size=15,colour=occ_pal[1]))
ggsave(here('outputs','realspend_trends.png'), width=5,height=3, dpi=300)


# Top spend per child
spc_top10 <- ccg_df %>%
  select(starts_with(c("ccg","spc"))) %>% #names()
  mutate(spc_201920 = round(spc_201920),
         spc_202021 = round(spc_202021),
         spc_202122 = round(spc_202122),
         Change = spc_202122- spc_202021) %>% 
  select(ccg,spc_202021,spc_202122,Change) %>% 
  arrange(desc(spc_202122)) %>% 
  rename('Clinical Commissioning Group'=ccg,
         'Spend per child (£) in 2020-21' = spc_202021,
         'Spend per child (£) in 2021-22' = spc_202122) %>% 
  head(n=10)
write.csv(spc_top10, here('outputs','spc_top10.csv'), row.names = FALSE)


# Bottom spend per child
spc_bottom10 <- ccg_df %>%
  select(starts_with(c("ccg","spc"))) %>% #names()
  mutate(spc_201920 = round(spc_201920),
         spc_202021 = round(spc_202021),
         spc_202122 = round(spc_202122),
         Change = spc_202122- spc_202021) %>% 
  select(ccg,spc_202021,spc_202122,Change) %>% 
  arrange(spc_202122) %>% 
  rename('Clinical Commissioning Group'=ccg,
         'Spend per child (£) in 2020-21' = spc_202021,
         'Spend per child (£) in 2021-22' = spc_202122) %>% 
  head(n=10)
write.csv(spc_bottom10, here('outputs','spc_bottom10.csv'), row.names = FALSE)

# Which CCGs increased spend per child the most/ decreased the most
spc_topchange <- ccg_df %>%
  select(starts_with(c("ccg","spc"))) %>% #names()
  mutate(spc_201920 = round(spc_201920),
         spc_202021 = round(spc_202021),
         spc_202122 = round(spc_202122),
         Change = spc_202122- spc_202021) %>% 
  select(ccg,spc_202021,spc_202122,Change) %>% 
  arrange(desc(Change)) %>% 
  rename('Clinical Commissioning Group'=ccg,
         'Spend per child (£) in 2020-21' = spc_202021,
         'Spend per child (£) in 2021-22' = spc_202122) %>% 
  head(n=10)
write.csv(spc_topchange, here('outputs','spc_topchange.csv'), row.names = FALSE)

# Which CCGs decreased spend per child the most/ decreased the most
spc_bottomchange <- ccg_df %>%
  select(starts_with(c("ccg","spc"))) %>% #names()
  mutate(spc_201920 = round(spc_201920),
         spc_202021 = round(spc_202021),
         spc_202122 = round(spc_202122),
         Change = spc_202122- spc_202021) %>% 
  select(ccg,spc_202021,spc_202122,Change) %>% 
  arrange(Change) %>% 
  rename('Clinical Commissioning Group'=ccg,
         'Spend per child (£) in 2020-21' = spc_202021,
         'Spend per child (£) in 2021-22' = spc_202122) %>% 
  head(n=10)
write.csv(spc_bottomchange, here('outputs','spc_bottomchange.csv'), row.names = FALSE)

# Percentage of CCGs where spend per child has increased
ccg_df %>% 
  select(starts_with(c("ccg","spc"))) %>% #names()
  mutate(spc_201920 = round(spc_201920),
         spc_202021 = round(spc_202021),
         spc_202122 = round(spc_202122),
         Change = spc_202122- spc_202021) %>% 
  select(ccg,spc_202021,spc_202122,Change) %>% 
  filter(Change > 0) %>% 
  tally()

print((80/106)*100)


## How many CCGs spent more than 1% of their budget on CYPMHS

# 2021-22
ccg_df %>% filter(pbudget_camhs_2122 < 0.01) %>% select(ccg, pbudget_camhs_2122) %>% tally()
print((58/106)*100)

# 2020-21
ccg_df %>% filter(pbudget_camhs_2021 < 0.01) %>% select(ccg, pbudget_camhs_2021) %>% tally()
print((74/106)*100)


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

# % of total spend on adult mental services 
england <- england %>% 
  mutate(mhspend_adult_2122 = mhspend_allages_2122-mhspend_cyp_2122,
         pbudget_adult_2122 = mhspend_adult_2122/total_budget_202122) %>% 
  mutate(pbudget_adult_2122 = round(pbudget_adult_2122*100))

print(england$pbudget_adult_2122)

#### OVERALL CCG SCORES ###

# Top 20 - overall score
overall_top20 <- df_summary %>%
  #select(ccg,overall_score_2122) %>% 
  arrange(desc(overall_score_2122)) %>%
  mutate(overall_score_2122 = round(overall_score_2122),
         spc_202122 = round(spc_202122)) %>%
  rename('Clinical Commissioning Group'=ccg,
         'CCG overall score' = overall_score_2122,
         'Spend per child (£)' = spc_202122,
         '% of budget spent on CYPMHS' = pbudget_camhs_2122,
         'Avg. wait in days' = avg_wait_days_2122,
         '% referred to CYPMHS' = percreferred_202122,
         '% referrals closed before treatment' = referrals_closed_2122) %>%
  head(n=20)
write.csv(overall_top20, here('outputs','overall_top20.csv'), row.names = FALSE)

# Bottom 20 - overall score
overall_bottom20 <- df_summary %>%
  #select(ccg,overall_score_2122) %>% 
  arrange(overall_score_2122) %>% 
  mutate(overall_score_2122 = round(overall_score_2122),
         spc_202122 = round(spc_202122)) %>%
  rename('Clinical Commissioning Group'=ccg,
         'CCG overall score' = overall_score_2122,
         'Spend per child (£)' = spc_202122,
         '% of budget spent on CYPMHS' = pbudget_camhs_2122,
         'Avg. wait in days' = avg_wait_days_2122,
         '% referred to CYPMHS' = percreferred_202122,
         '% referrals closed before treatment' = referrals_closed_2122) %>%
  head(n=20)
write.csv(overall_bottom20, here('outputs','overall_bottom20.csv'), row.names = FALSE)

# Full summary table for appendix

summary_table <- df_summary %>%
  arrange(desc(overall_score_2122)) %>% 
  mutate(overall_score_2122 = round(overall_score_2122),
         spc_202122 = round(spc_202122)) %>%
  rename('Clinical Commissioning Group'=ccg,
         'CCG overall score' = overall_score_2122,
         'Spend per child (£)' = spc_202122,
         '% of budget spent on CYPMHS' = pbudget_camhs_2122,
         'Avg. wait in days' = avg_wait_days_2122,
         '% referred to CYPMHS' = percreferred_202122,
         '% referrals closed before treatment' = referrals_closed_2122) 
write.csv(summary_table, here('outputs','summary_table.csv'), row.names = FALSE)












