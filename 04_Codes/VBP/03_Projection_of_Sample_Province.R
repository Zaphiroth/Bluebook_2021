# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  Bluebook 2021-VBP
# Purpose:      Projection of sample province
# programmer:   Zhe Liu
# Date:         2021-03-11
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


##---- Readin data ----
## city segment
proj.segment <- read.xlsx('02_Inputs/seg_45cities.xlsx') %>% 
  mutate(seg_city = if_else(city == '上海', stri_paste(city, district), city)) %>% 
  select(seg_city, segment = seg_up)

## universe PCHC
pchc.universe <- read.xlsx("02_Inputs/2020_PCHC_Universe更新维护.xlsx", 
                           sheet = "2020 CHC universe", cols = 1:19)

pchc.universe.m <- pchc.universe %>% 
  distinct(province = gsub('省|市', '', `省`), 
           city = gsub('市', '', `地级市`), 
           district = `区[县/县级市]`, 
           pchc = `新版PCHC_Code`, 
           est = `其中：西药药品收入（千元）`) %>% 
  filter(est > 0) %>% 
  group_by(pchc) %>% 
  summarise(province = first(na.omit(province)), 
            city = first(na.omit(city)), 
            district = first(na.omit(district)), 
            est = sum(est, na.rm = TRUE)) %>% 
  ungroup()

hospital.universe <- bind_rows(imp.total, pchc.universe.m) %>% 
  group_by(pchc) %>% 
  summarise(province = first(province),
            city = first(na.omit(city)),
            district = first(na.omit(district)), 
            est = first(na.omit(est))) %>% 
  ungroup() %>% 
  filter(!is.na(province), !is.na(city), !is.na(district), !is.na(est)) %>% 
  mutate(flag_sample = if_else(pchc %in% unique(imp.total$pchc), 1, 0)) %>% 
  left_join(proj.segment, by = c('city' = 'seg_city'))


##---- Projection ----
## projection universe
universe.data <- imp.total %>% 
  distinct(year, quarter, date, province, city, market, atc4, molecule, packid) %>% 
  left_join(hospital.universe, by = c('province', 'city')) %>% 
  full_join(imp.total, by = c('year', 'quarter', 'date', 'province', 'city', 'district', 
                              'pchc', 'market', 'atc4', 'molecule', 'packid')) %>% 
  mutate(sales = if_else(is.na(sales) & flag_sample == 1, 0, sales))

## projection parameter
proj.parm <- data.table(universe.data)[, {
  ux <- mean(est, na.rm = TRUE)
  uy <- mean(sales, na.rm = TRUE)
  slope <- uy / ux
  intercept <- 0
  predict_sales = est * slope
  spearman_cor <- cor(sales, predict_sales, method = 'spearman')
  list(slope = slope, intercept = intercept, spearman_cor = spearman_cor)
}, by = list(date, segment, packid)]

## result
proj.total <- universe.data %>% 
  left_join(proj.parm, by = c('date', 'segment', 'packid')) %>% 
  mutate(predict_sales = est * slope, 
         predict_sales = if_else(predict_sales < 0, 0, predict_sales), 
         final_sales = if_else(is.na(sales), predict_sales, sales)) %>% 
  filter(final_sales > 0) %>% 
  select(year, quarter, date, province, city, district, pchc, market, atc4, 
         molecule, packid, sales = final_sales)

write_feather(proj.total, '03_Outputs/VBP/03_Bluebook_2020_VBP_Projection_of_Sample_Province.feather')
