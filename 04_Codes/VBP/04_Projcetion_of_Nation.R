# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  Bluebook 2021-VBP
# Purpose:      Projection of nation
# programmer:   Zhe Liu
# Date:         2021-03-11
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


##---- Readin data ----
## city tier
city.tier <- read.xlsx("02_Inputs/pchc_city_tier.xlsx") %>% 
  group_by(city) %>% 
  mutate(tier = if_else(is.na(city_tier), first(na.omit(city_tier)), city_tier)) %>% 
  ungroup() %>% 
  mutate(tier = if_else(is.na(tier), 5, tier)) %>% 
  distinct(city, tier)

## nation CHC
pchc.universe.n <- pchc.universe %>% 
  distinct(province = gsub('省|市', '', `省`), 
           city = gsub('市', '', `地级市`), 
           district = `区[县/县级市]`, 
           pchc = `新版PCHC_Code`, 
           pop = `人口`, 
           est = `其中：西药药品收入（千元）`) %>% 
  filter(pop > 0, est > 0) %>% 
  group_by(pchc) %>% 
  summarise(province = first(na.omit(province)), 
            city = first(na.omit(city)), 
            district = first(na.omit(district)), 
            pop = sum(pop, na.rm = TRUE), 
            est = sum(est, na.rm = TRUE)) %>% 
  ungroup()

hospital.nation <- bind_rows(imp.total, pchc.universe.n) %>% 
  group_by(pchc) %>% 
  summarise(province = first(province),
            city = first(na.omit(city)),
            district = first(na.omit(district)), 
            pop = first(na.omit(pop)), 
            est = first(na.omit(est))) %>% 
  ungroup() %>% 
  filter(pop > 0, est > 0)


##---- Projection ----
source('04_Codes/functions/ProjectNation.R', encoding = 'UTF-8')

proj.nation <- ProjectNation(proj.sample.total = proj.total, 
                             pchc.universe = hospital.nation, 
                             city.tier = city.tier)

write.xlsx(proj.nation, '03_Outputs/VBP/04_Bluebook_2020_VBP_Projection_of_Nation.feather')
