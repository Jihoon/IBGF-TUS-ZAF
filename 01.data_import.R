library(tidyverse)
library(fs)

options("scipen"=100, "digits"=4)

#### Read TUS data ####
datapath_2010 = "2010 raw"
datapath_2000 = "2000 raw"
fn_indv = "tus-2010-person-v1.2.csv"
fn_hh = "tus-2010-household-v1.2.csv"
fn_all = "tus-2010-allpersons-v1.2.csv"
fn_act = "tus-2010-activities-v1.2.csv"

df_hh = read_csv(path(datapath_2010, fn_hh))
df_indv = read_csv(path(datapath_2010, fn_indv)) %>% 
  filter(Q117Age > 14) 
df_all = read_csv(path(datapath_2010, fn_all))
df_act_raw = read_csv(path(datapath_2010, fn_act)) %>% 
  filter(Q117Age > 14) %>%
  # read_csv has an encoding issue with some special letters.
  mutate(Activity_code = gsub("\\x96", "-", Activity_code, useBytes = TRUE)) %>%
  mutate(Activity_code = gsub("\\x92", "'", Activity_code, useBytes = TRUE)) 

#### Re-code missing Activity_code in df_act ####
df_act = df_act_raw %>% 
  mutate(
    Activity_code = case_when(
      Activity_code == "930" ~ "Listening to music/radio",
      Activity_code == "940" ~ "Accessing information by computer",
      Activity_code == "950" ~ "Visiting library",
      Activity_code == "980" ~ "Travel related to mass media use and entertainment",
      Activity_code == "990" ~ "Mass media use and entertainment not elsewhere classified",
      Activity_code == "88"  ~ "Waiting:Travel related to personal care and self-maintenance",
      Activity_code == "188" ~ "Waiting:Travel to/from work and seeking employment in establishments",
      Activity_code == "288" ~ "Waiting:Travel related to primary production activities (not for establishments)",
      Activity_code == "388" ~ "Waiting:Travel related to services for income and other production of goods (not for establishments)",
      Activity_code == "488" ~ "Waiting:Travel related to household maintenance, management and shopping",
      Activity_code == "588" ~ "Waiting:Travel related to care of children, the sick, elderly and disabled in the household",
      Activity_code == "788" ~ "Waiting:Travel related to learning",
      Activity_code == "888" ~ "Waiting:Travel related to social, cultural and recreational activities",
      .default = Activity_code
    )
  )
      
# #### Read QoL data ####
# datapath_QoL = path("..", "QoL ZAF", "2011")
# fn_QoL = "gcro 2011_28feb12nogis-v1-s10.csv"
# df_QoL = read_csv(path(datapath_QoL, fn_QoL))

#### 1. Data cleansing ####
# 1.1. Multiple identical person IDs in one hh -> Remove all
# When merging to df_act, it is by hh/person + some other vars (gender/age/education/etc).
# e.g., 171039160000005312 / 2
df_exclude = df_indv %>% 
  count(UQNO, PERSONNO, Q116Gender, Q117Age, Q118Population, 
        Q22HighestSchool, Q23MaritalStatus, Weight) %>% filter(n>1)
df_indv = df_indv %>% left_join(df_exclude) %>%
  filter(is.na(n)) %>% select(-n)
df_act = df_act %>% left_join(df_exclude) %>%
  filter(is.na(n)) %>% select(-n)
rm(df_exclude)

# Identify person with more than 1440 hours/week
# TODO: Include these people in df_exclude
aa= df_act_raw %>%
  left_join(df_indv) %>%
  group_by(UQNO, PERSONNO, Q116Gender, Q117Age, Q118Population,
           Q22HighestSchool, Q23MaritalStatus, Weight) %>%
  summarise(TotHours=sum(Timeper)) %>%
  filter(TotHours > 1440)

# Add individual unique identifier number & numerical income (personal)
library(matsbyname)
df_indv = df_indv %>%  # Remove minors (different decision making)
  arrange(UQNO, PERSONNO, Q116Gender, Q117Age, Q118Population, 
          Q22HighestSchool, Q23MaritalStatus, Weight) %>%
  mutate(PersonUQNO = cur_group_rows()) %>%
  mutate(
    # point value estimated based on https://www.unicef.org/southafrica/media/8181/file/ZAF-Decent-standard-of-living-South-Africa-2023.pdf#page=23.24
    MonthlyIncomeIndiv = case_when( 
      Q46TotIncome == "None" ~ 1,
      Q46TotIncome == "R1 - R200" ~ 200/3*2,
      Q46TotIncome == "R201 - R500" ~ 350,
      Q46TotIncome == "R501 - R1 000" ~ logmean(501,1000),
      Q46TotIncome == "R1 001 - R1 500" ~ logmean(1001,1500),
      Q46TotIncome == "R1 501 - R2 500" ~ logmean(1501,2500),
      Q46TotIncome == "R2 501 - R3 500" ~ logmean(2501,3500),
      Q46TotIncome == "R3 501 - R4 500" ~ logmean(3501,4500),
      Q46TotIncome == "R4 501 - R6 000" ~ logmean(4501,6000),
      Q46TotIncome == "R6 001 - R8 000" ~ logmean(6501,8000),
      Q46TotIncome == "R8 001 - R11 000" ~ logmean(501,1000),
      Q46TotIncome == "R11 001 or more" ~ 11000*2,
      .default = NA  # "Don't know" or "Refuse"
    )
  )

# 2010 median monthly income per worker: R2800 (http://www.statssa.gov.za/publications/P02112/P021122010.pdf)
med_inc = 2800 
# Impute NA's with observed median
library(spatstat.univar)
df_indv = df_indv %>% 
  replace_na(
    list(MonthlyIncomeIndiv = 
           weighted.median(df_indv$MonthlyIncomeIndiv,
                           df_indv$Weight, na.rm=TRUE)
         )
    )

# 1.2. Multiple identical household IDs -> Keep only the 1st one (e.g., 817001870000018816)
df_hh = df_hh %>% group_by(UQNO) %>% 
  slice(1)


#### 2. Add categorical vars ####
df_indv = df_indv %>%
  mutate(
    weekend = case_when(
      Q52DayDiary %in% c("Saturday", "Sunday") ~ TRUE,
      .default = FALSE
    )
  )

df_act = df_act %>% 
  mutate(
    timetype = case_when(
      Activity_code %in% c("Collecting fuel, firewood or dung",
                           "Collecting water") ~ "unpaid",
      Activ %in% c("Work in establishments", 
                   "Work in non-establishment", 
                   "Primary production") ~ "paid",
      Activ %in% c("Mass media use", 
                   "Learning",
                   "Social and cultural activities") ~ "leisure",
      Activ %in% c("Personal care") ~ "personal",
      .default = "unpaid"
    )
  ) %>%
  mutate(
    TravType = case_when(
      Location1 == "Travelling or waiting to travel" & timetype %in% c("unpaid", "paid") ~ "Committed",
      Location1 == "Travelling or waiting to travel" & timetype %in% c("leisure", "personal") ~ "Uncommitted",
      .default = "NoTravel"
    )
  ) 

#### 3. Aggregate time by type (paid/unpaid, leisure, personal) #### 
# df_master = df_act %>% 
#   left_join(df_indv) %>% 
#   left_join(df_hh)

# Aggregate four main category totals ("leisure", "paid", "unpaid", "personal")
df_agg = df_act %>% 
  left_join(df_indv) %>%  # df_act still has persons with hours > 1440
  group_by(UQNO, PersonUQNO, weekend, timetype) %>%
  # These fields jointly/uniquely identify a person. (And assume their hh ids are correct..)
  # group_by(UQNO, PERSONNO, weekend, timetype,
  #          Q116Gender, Q117Age, Q118Population, Q22HighestSchool, Q23MaritalStatus) %>%
  summarise(Fulltime=sum(Fulltime), Timeper=sum(Timeper)) %>%
  ungroup() %>%
  drop_na(weekend) %>%
  # Add back individual characteristics
  left_join(df_hh) %>% left_join(df_indv %>% select(-weekend)) %>%
  mutate(Q43Hours = as.numeric(Q43Hours),   # %>% # Work hours / week
         MonthlyIncomeHH = case_when(
           Q113Income == "None" ~ 1,
           Q113Income == "R1 - R200" ~ 200/3*2,
           Q113Income == "R201 - R500" ~ 350,
           Q113Income == "R501 - R1 000" ~ logmean(501,1000),
           Q113Income == "R1 001 - R1 500" ~ logmean(1001,1500),
           Q113Income == "R1 501 - R2 500" ~ logmean(1501,2500),
           Q113Income == "R2 501 - R3 500" ~ logmean(2501,3500),
           Q113Income == "R3 501 - R4 500" ~ logmean(3501,4500),
           Q113Income == "R4 501 - R6 000" ~ logmean(4501,6000),
           Q113Income == "R6 001 - R8 000" ~ logmean(6501,8000),
           Q113Income == "R8 001 - R11 000" ~ logmean(501,1000),
           Q113Income == "R11 001 or more" ~ 11000*2,
           .default = NA  # "Don't know" or "Refuse"
         )
       )

# Create a wide format to visualize/compare patterns
df_agg_wide = df_agg %>% 
  select(PersonUQNO, timetype, Fulltime, Timeper, everything()) %>%
  pivot_wider(id_cols = PersonUQNO, names_from=timetype, values_from = c(Fulltime, Timeper), unused_fn=first) 

df_agg_wide = df_agg_wide %>%
  mutate(across(Fulltime_leisure:Timeper_unpaid, ~replace_na(.,0))) %>%
  mutate(SumCheck_Fulltime = Fulltime_leisure + Fulltime_paid + Fulltime_unpaid + Fulltime_personal,
         SumCheck_Timeper = Timeper_leisure + Timeper_paid + Timeper_unpaid + Timeper_personal) #%>%

# Impute NA's with observed median
df_agg = df_agg %>% 
  replace_na(
    list(MonthlyIncomeHH = 
           weighted.median(df_agg_wide$MonthlyIncomeHH, 
                           df_agg_wide$Weight, na.rm=TRUE)
    )
  )


# Aggregate specific categories of time use
# 1. Travel (committed vs. uncommitted)
df_trav_wide = df_act %>% 
  left_join(df_indv) %>% 
  group_by(UQNO, PersonUQNO, weekend, TravType) %>% 
  summarise(TimeTrav=sum(Timeper)) %>% 
  pivot_wider(id_cols = PersonUQNO, names_from=TravType, 
              values_from = TimeTrav, unused_fn=first) %>%
  mutate(across(Committed:Uncommitted, ~replace_na(.,0)))

# df_cook_wide = df_act %>% 
#   left_join(df_indv) %>% 
#   group_by(UQNO, PersonUQNO, weekend, Cooktime) %>% 
#   summarise(CookTime = sum(Timeper)) %>%
#   mutate(CookTime=ifelse(CookTime==1440, 0, CookTime)) %>%
#   filter(Cooktime | CookTime==0) %>% select(-Cooktime)

# 2. Cooking
df_cook_wide = df_act %>% 
  left_join(df_indv) %>% 
  group_by(UQNO, PersonUQNO, weekend) %>% 
  filter(Activity_code == "Cooking, making drinks, setting and serving tables, washing up") %>%
  summarise(CookTime = sum(Timeper)) %>%
  mutate(CookTime=ifelse(CookTime==1440, 0, CookTime)) 
# Fill in 0 for those without waiting activity
df_cook_wide = df_indv %>% left_join(df_cook_wide) %>% 
  mutate(CookTime = coalesce(CookTime, 0)) %>%
  select(c("UQNO", "PersonUQNO", "weekend"), CookTime)

# FillIndividualNA <- function(df, value=0) {
#   vars = names(df)
#   var = tail(vars, 1)
#   df_fill = df_indv %>% left_join(df) %>% 
#     select(all_of(vars))
#   df_fill[var] = coalesce(df_fill[var], 
#                           tibble(rep(0, dim(df_indv)[1]))
#                           )
#   
#   return(df_fill)
# }

# 3. Waiting
df_wait_wide = df_act %>% 
  left_join(df_indv) %>% 
  group_by(UQNO, PersonUQNO, weekend) %>% 
  filter(grepl("Wait|wait", Activity_code)) %>%
  summarise(WaitTime = sum(Timeper)) 
# Fill in 0 for those without waiting activity
df_wait_wide = df_indv %>% left_join(df_wait_wide) %>% 
  mutate(WaitTime = coalesce(WaitTime, 0)) %>%
  select(c("UQNO", "PersonUQNO", "weekend"), WaitTime)




df_agg_wide = df_agg_wide %>% 
  left_join(df_trav_wide) %>% 
  left_join(df_cook_wide) %>%
  left_join(df_wait_wide) %>%
  mutate(Committed.adj = Committed * med_inc / MonthlyIncomeHH,
         Unpaid.adj = Timeper_unpaid * med_inc / MonthlyIncomeHH,
         Leisure.adj = Timeper_leisure * med_inc / MonthlyIncomeHH)

# Should I remove HH's with 'none' monthly income?
df_agg_wide = df_agg_wide %>% filter(Q113Income != "None")


#### 4. Some descriptive plots ####
focus = c("Timeper_leisure", "Timeper_paid", "Timeper_unpaid", "Timeper_personal",
          "Committed", "Uncommitted", "CookTime", "WaitTime")
# flist = list()

for (f in focus) {
  p = df_agg_wide %>% select(weekend, Q116Gender, as.name(f)) %>%
    ggplot(aes(f)) + 
    geom_histogram(bins = 70) +
    facet_wrap(~weekend+Q116Gender) +
    labs(title=f)
  print(p)
}
df_agg %>% filter(timetype=="paid") %>%  ggplot(aes(Q43Hours)) +
  geom_histogram(bins = 70) 
df_agg %>% filter(timetype=="personal") %>% count(Q45SourceIncome)
# Q: How can I deal with those mainly relying on non-wage income (e.g., Remittance, invest)
# Q: Only non-retired adults? Or minors?
# TODO: Add 0 for all non-existing timetypes (using coales)
# TODO: scatterplot different times (unpaid vs paid vs leisure)
df_agg_wide %>% ggplot(aes(x=Timeper_paid, y=Timeper_leisure)) +
  geom_jitter(size=0.1) + facet_wrap(.~weekend+ Q116Gender)
df_agg_wide %>% ggplot(aes(x=Timeper_unpaid, y=Timeper_leisure)) +
  geom_jitter(size=0.1) + facet_wrap(.~weekend+ Q116Gender) 
df_agg_wide %>% ggplot(aes(x=Timeper_unpaid, y=Timeper_paid)) +
  geom_jitter(size=0.1) + facet_wrap(.~weekend+ Q116Gender) 

df_agg_wide %>% ggplot(aes(Committed)) + 
  geom_histogram(bins = 70) + facet_wrap(.~weekend+ Q116Gender) +
  lims(x=c(0, 600), y=c(0, 2000))
df_agg_wide %>% ggplot(aes(Uncommitted)) + 
  geom_histogram(bins = 70) + facet_wrap(.~weekend+ Q116Gender) +
  lims(x=c(0, 600), y=c(0, 2000))
df_agg_wide %>% ggplot(aes(TotTravelTime )) + 
  geom_histogram(bins = 70) + facet_wrap(.~weekend+ Q116Gender) +
  lims(x=c(0, 600), y=c(0, 2000))
df_agg_wide %>% ggplot(aes(Committed.adj)) + 
  geom_histogram(bins = 70) + facet_wrap(.~weekend+ Q116Gender) +
  lims(x=c(0, 600), y=c(0, 2000))
df_agg_wide %>% ggplot(aes(Timeper_unpaid)) + 
  geom_histogram(bins = 70) + facet_wrap(.~weekend+ Q116Gender) +
  lims(x=c(0, 1400), y=c(0, 2000))
df_agg_wide %>% ggplot(aes(Timeper_unpaid)) + 
  geom_histogram(bins = 70) + facet_wrap(.~Q113Income) +
  lims(x=c(0, 1400), y=c(0, 2000))
df_agg_wide %>% ggplot(aes(Unpaid.adj)) + 
  geom_histogram(bins = 100) + facet_wrap(.~weekend+ Q116Gender) +
  lims(x=c(0, 1400), y=c(0, 2000))
df_agg_wide %>% ggplot(aes(Timeper_leisure)) + 
  geom_histogram(bins = 70) + facet_wrap(.~weekend+ Q116Gender) +
  lims(x=c(0, 1400), y=c(0, 2000))
df_agg_wide %>% ggplot(aes(Leisure.adj)) + 
  geom_histogram(bins = 70) + facet_wrap(.~weekend+ Q116Gender) +
  lims(x=c(0, 1400), y=c(0, 2000))
df_agg_wide %>% ggplot(aes(CookTime)) + 
  geom_histogram(bins = 70) + facet_wrap(.~weekend+ Q116Gender) +
  lims(x=c(0, 500), y=c(0, 2000))
df_agg_wide %>% ggplot(aes(MonthlyIncomeHH)) + geom_histogram(bins = 70)
df_indv %>% ggplot(aes(MonthlyIncomeIndiv)) + geom_histogram(bins = 70)

# plotly somehow not working
library(plotly)
fig = plot_ly(df_agg_wide %>% filter(weekend==FALSE) , type="scatter3d",
              x = ~Timeper_unpaid, y = ~Timeper_paid, z = ~Timeper_leisure,
              color = ~Q116Gender, size=3,
              mode = "markers")  %>%
  add_markers()
fig

# library(rgl)
# colors <- c('royalblue1', 'red')
# colors <- colors[ as.factor(df_agg_wide$Q116Gender) ]
# df_3d = df_agg_wide %>% filter(weekend=FALSE) %>% select(Timeper_unpaid, Timeper_paid, Timeper_leisure)
# plot3d( 
#   x=df_3d$Timeper_unpaid, y=df_3d$Timeper_paid, z=df_3d$Timeper_leisure, 
#   col = colors, 
#   type = 's', 
#   radius = .1,
#   xlab="unpaid", ylab="paid", zlab="leisure")


#### 5. Tobit runs ####
library(AER)

tob_mod1 = tobit(Timeper_unpaid ~ weekend + Q116Gender + Q23MaritalStatus + 
                   MonthlyIncomeIndiv + Geo_Type +
                   Q19Train + Q19Bus + Q19Taxi + Q110Shop +
                   Q17FarWater + Q14FarWood +
                   Q29Child06HH + 
                   Q31PdWrk +
                   Q31OwnBusns +
                   Q12WashingMachine + Q12Refrigerator + Q12Car + 
                   Q12DishWasher + Q12Microwave + Q13Cooking +
                   Q11DwellType,
                 data = df_agg_wide)
tob_mod2 = tobit(Unpaid.adj ~ weekend + Q116Gender + Q23MaritalStatus + 
                   MonthlyIncomeIndiv + Geo_Type +
                   Q19Train + Q19Bus + Q19Taxi + Q110Shop +
                   Q17FarWater + Q14FarWood +
                   Q29Child06HH + 
                   Q31PdWrk +
                   Q31OwnBusns +
                   Q12WashingMachine + Q12Refrigerator + Q12Car + 
                   Q12DishWasher + Q12Microwave + Q13Cooking +
                   Q11DwellType,
                 data = df_agg_wide)
tob_mod3 = tobit(Timeper_leisure ~ weekend + Q116Gender + Q23MaritalStatus + 
                   MonthlyIncomeIndiv + Geo_Type +
                   Q19Train + Q19Bus + Q19Taxi + Q110Shop +
                   Q17FarWater + Q14FarWood +
                   Q29Child06HH + 
                   Q31PdWrk +
                   Q31OwnBusns +
                   Q12WashingMachine + Q12Refrigerator + Q12Car + 
                   Q12DishWasher + Q12Microwave + Q13Cooking +
                   Q11DwellType,
                 data = df_agg_wide)
tob_mod4 = tobit(Committed ~ weekend + Q116Gender + Q23MaritalStatus + 
                   MonthlyIncomeIndiv + Geo_Type +
                   Q19Train + Q19Bus + Q19Taxi + Q110Shop +
                   Q17FarWater + Q14FarWood +
                   Q29Child06HH + 
                   Q31PdWrk +
                   Q31OwnBusns +
                   Q12WashingMachine + Q12Refrigerator + Q12Car + 
                   Q12DishWasher + Q12Microwave + Q13Cooking +
                   Q11DwellType,
                 data = df_agg_wide)
tob_mod5 = tobit(CookTime ~ weekend + Q116Gender + Q23MaritalStatus + 
                   MonthlyIncomeIndiv + Geo_Type +
                   Q19Train + Q19Bus + Q19Taxi + Q110Shop +
                   Q17FarWater + Q14FarWood +
                   Q29Child06HH + 
                   Q31PdWrk +
                   Q31OwnBusns +
                   Q12WashingMachine + Q12Refrigerator + Q12Car + 
                   Q12DishWasher + Q12Microwave + Q13Cooking +
                   Q11DwellType,
                 data = df_agg_wide)
summary(tob_mod1)
summary(tob_mod2)
summary(tob_mod3)
summary(tob_mod4)
summary(tob_mod5)


#Create a function for pseudoR2 calculation 
pseudoR2 <- function(obj) 1 - as.vector(logLik(obj)/logLik(update(obj, . ~ 1)))
pseudoR2(tob_mod1)
pseudoR2(tob_mod2)
pseudoR2(tob_mod3)
pseudoR2(tob_mod4)




# for (f in focus) {
#   df_sub = df_agg %>% filter(timetype==f)
#   
#   # Fill in zero for those having no time (=0) for this focus category
#   df_no_obs = df_master%>% 
#     group_by(UQNO, PERSONNO, weekend, 
#              Q116Gender, Q117Age, Q118Population, Q22HighestSchool, Q23MaritalStatus) %>% 
#     count() %>%
#     left_join(df_sub %>% select(
#       UQNO, PERSONNO, weekend, timetype,
#       Q116Gender, Q117Age, Q118Population, Q22HighestSchool, Q23MaritalStatus
#     )) %>% 
#     filter(is.na(timetype)) %>%
#     mutate(timetype=f, Fulltime=0, Timeper=0) %>% select(-n) %>%
#     left_join(df_hh) %>% left_join(df_indv %>% select(-weekend)) 
#   
#   df_analysis = df_sub %>% 
#     filter(timetype==f) %>% rbind(df_no_obs) %>% 
#     arrange(UQNO, PERSONNO, weekend, Q116Gender, Q117Age, 
#             Q118Population, Q22HighestSchool, Q23MaritalStatus, timetype) 
#   
#   # Person with the longest unpaid work hours
#   # aa=df_master %>% filter(UQNO==171026660000017600, PERSONNO==1)
#   
#   #### Try a model estimation ####
#   library(AER)
#   
#   tob_mod_timetype = tobit(Timeper ~ weekend + Q116Gender*Q23MaritalStatus + 
#                      Q46TotIncome + Geo_Type +
#                      Q19Train + Q19Bus + Q19Taxi + Q110Shop +
#                      Q17FarWater + 
#                      Q29Child06HH + 
#                      Q31PdWrk:Q116Gender +
#                      # Q31OwnBusns +
#                      Q12WashingMachine + Q12Refrigerator + Q12Car + Q12DishWasher ,
#                    data = df_analysis)
#   summary(tob_mod_timetype)
#   
#   flist[[f]] = tob_mod_timetype
# }
# 
# 
# 
# 
# 
