require(dplyr)
require(occresearch)
require(tidyr)
require(here)

### Import CCG lookup ###
ccg_lookup <- read.csv(here('inputs','ccg_lookup.csv'))

# 2018 ccg population estimates
pop_2018 <- read.csv(here('inputs','pop_2018.csv')) %>% 
  mutate(ccg = toupper(ccg),
         pop_2018 = str_remove(pop_2018,","),
         pop_2018 = as.numeric(pop_2018))

# Bind lookup to find new ccg names
pop_2018 <- pop_2018 %>% 
  left_join(ccg_lookup %>% select(!region), by=c("ccg")) %>% 
  group_by(ccg_new) %>% 
  mutate(pop_2018 = sum(pop_2018, na.rm = TRUE)) %>% 
  select(-ccg) %>% unique() %>% 
  rename(ccg = ccg_new)

# Export to intermediate folder
write.csv(pop_2018, here("intermediate/pop_2018.csv"), row.names=F)

# 2019 population estimates 
df <- read.csv(here("inputs/pop_2019_oldnames.csv"))

read.excel <- function(header=TRUE,...) {
  read.table("clipboard",sep="\t",header=header,...)
}

#df=read.excel()

pop_2019 <- df %>% group_by(ccg.new) %>% summarise(sum(pop_2019)) 

write.csv(pop_2019, here("intermediate/pop_2019_newnames.csv"),row.names=F)

