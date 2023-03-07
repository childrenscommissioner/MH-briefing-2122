#Trend in CYP receiving treatment

library(readxl)
library(here)
library(dplyr)

CYPia <- read_excel(here('inputs/Raw data','NHS E_OCC data file and trend_2022 - v2 JS.xlsx'),sheet="Trend CYP (i.a)")

#HARD CODED TEMPORARY ASSUMPTIONS
#for both below inputs we ideally want 2015-16 - 2025-2026
#ideally every year but every 3 years (dataframe with one column year and one column estimate prevalence - make assumptions that previous year holds where no new data)
prevalence <- 1/6

# create prevalence by year df using NHS MHCYP report.
# prevalence data before 2016-17 was published in 2004 and not comparable to current stats

prevalence <- data.frame(year = c("2016-17","2017-18","2018-19","2019-20","2020-21","2021-22","2022-23","2023-24","2024-25","2025-26"),
           #assumption that prevalence rate will hold for year from 2017-2019 and after 21-22
           prev_7to16 = c("0.121","0.121","0.121","0.167 ","0.178","0.180","0.180","0.180","0.180","0.180"),
           prev_17to25 = c("0.101","0.101","0.101","0.177","0.174","0.257","0.257","0.257","0.257","0.257"))
print(prevalence)

#we need dataframe with CYP population each year (England total) 
population <- 11800000

df <- CYPia %>% 
  group_by() %>% 
  summarise(CYPtreated2021=sum(`2020/21`),
            CYPtreated2122=sum(`2021/22`)) %>% 
  mutate(CYPtreated_growthrate_2021_to_2122=CYPtreated2122/CYPtreated2021-1,
         
         CYPtreated2223_est=CYPtreated2122*(1+CYPtreated_growthrate_2021_to_2122),
         
         share_CYPtreated2021=CYPtreated2021/(prevalence*population),
         share_CYPtreated2122=CYPtreated2122/(prevalence*population),
         share_CYPtreated2223_est=CYPtreated2223_est/(prevalence*population))

# plot the share CYPtreated