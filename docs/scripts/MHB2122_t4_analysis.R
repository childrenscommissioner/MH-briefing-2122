
### Author: Rebecca Gilhooly
### Title: MHB 2021-22, Tier 4/adult wards analysis script (2021/22 data)
### Date: 02/03/2023

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
  
  x %>% write.table(file="clipboard-10000", sep="\t", row.names=F)}

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

# "and" still capitalised for Bath and North East Somerset, Swindon And Wiltshire CCG
ccg_df <- ccg_df %>% 
  mutate(ccg = str_replace(ccg,"NHS Bath and North East Somerset, Swindon And Wiltshire CCG","NHS Bath and North East Somerset, Swindon and Wiltshire CCG"))

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
# note that the latest population estimates for CCGs was published in 2020

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


# CCG level summary scores ------------------------------------------------

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
write.csv(england, here('intermediate','england_202122.csv'), row.names=FALSE)

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

