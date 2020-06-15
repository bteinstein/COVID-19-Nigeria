## packages
library(tidyverse)
library(readxl)




# Data
df_nigeria_state <- read_excel("data/Count/Daily Count By State_final.xlsx") %>% 
                    select(Date_mmddyyyy, States,  Samples_Tested,
                           Confirmed_Cases, Active,
                           Recovered,    Death, Geopolitical_Zone) %>% 
                    rename(Date = Date_mmddyyyy)

# Subsetting, Transformations and Summaries
# Data as at 4th of June, 2020
df_nigeria_05062020 <- df_nigeria_state %>% 
                  filter(Date == max(Date)) 
              # Thinking to append Cross-River

##### Cummulative ####
df_daily_cum <- df_nigeria_state %>% 
                  # select(2,5:9) %>% 
                  group_by(Date) %>% 
                  summarise(
                    cum_st = sum(Samples_Tested),
                    cum_cc = sum(Confirmed_Cases),
                    cum_ac = sum(Active),
                    cum_rc = sum(Recovered),
                    cum_dc = sum(Death, na.rm = T),
                  )

##### Daily Cases (Test, Confirmed Cases, Recoveries and Death) #####
df_daily_case <-  df_daily_cum %>% select(1,2:6) %>% 
                      transform(
                        DailyTestCases = ave(cum_st, FUN=function(x) x - c(0, head(x, -1))),
                        DailyConfirmedCases = ave(cum_cc, FUN=function(x) x - c(0, head(x, -1))),
                        #DailyActiveCases = ave(cum_ac, FUN=function(x) x - c(0, head(x, -1))),
                        DailyRecoveryCases = ave(cum_rc, FUN=function(x) x - c(0, head(x, -1))),
                        DailyDeaths = ave(cum_dc, FUN=function(x) x - c(0, head(x, -1)))
                      ) %>% 
                      select(1,7:10)




#### Nation Wide Average ####
## calculate nationwide average
current_nationwide_cc_avg <- mean(df_nigeria_05062020$Confirmed_Cases)
current_nationwide_cc_median <- median(df_nigeria_05062020$Confirmed_Cases)

nationwide_daily_cc_avg <- mean(df_daily_case$DailyConfirmedCases)
nationwide_daily_cc_median <- median(df_daily_case$DailyConfirmedCases)


##### Aggregate By State #####
df_daily_case_by_state <-  df_nigeria_state %>% 
                                select(1,2,4,5,6,7) %>% 
                              complete(States, Date, 
                                       fill = list(
                                                   Confirmed_Cases = 0,
                                                   Recovered = 0,
                                                   Death = 0,
                                                   Active = 0
                                                   )) %>% 
                              group_by(States) %>% 
                              mutate(
                                #DailyTestCases = ave(Samples_Tested, FUN=function(x) x - c(0, head(x, -1))),
                                DailyConfirmedCases = ave(Confirmed_Cases, FUN=function(x) x - c(0, head(x, -1))),
                                DailyRecoveryCases = ave(Recovered, FUN=function(x) x - c(0, head(x, -1))),
                                DailyDeaths = ave(Death, FUN=function(x) x - c(0, head(x, -1)))
                              )

sum(df_daily_case_by_state$DailyConfirmedCases)



### By Geoploliticl Zone
df_zones <- df_daily_case_by_state %>% 
  group_by(States) %>% 
  summarise(Confirmed_Cases_cum = sum(DailyConfirmedCases)) %>% 
  ungroup() %>% 
  left_join(distinct(df_nigeria_state, States,Geopolitical_Zone)) %>% 
  group_by(Geopolitical_Zone)%>%
  mutate(zone_avg = mean(Confirmed_Cases_cum)) %>% 
  ungroup() %>% 
  mutate(Geopolitical_Zone = fct_reorder(Geopolitical_Zone, zone_avg))

zone_summary <- df_zones %>% 
  group_by(Geopolitical_Zone) %>% 
  summarise(region_sum = sum(Confirmed_Cases_cum)) %>% 
  ungroup() %>% 
  mutate(pct_region_sum = (region_sum/sum(region_sum))*100) %>% 
  arrange(-pct_region_sum)

df_daily_case_by_state_region <-  df_daily_case_by_state %>% left_join(df_zones) 

