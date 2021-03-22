# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  Bluebook 2021-VBP
# Purpose:      Price
# programmer:   Zhe Liu
# Date:         2021-03-12
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


##---- Readin data ----
## pack en
product.en <- read.xlsx('02_Inputs/ims_chpa_to20Q4.xlsx', cols = 1:21, startRow = 4) %>% 
  distinct(Pack_ID, Brand_EN = Prd_desc, Manufacture_EN = Mnf_Desc, Pck_Desc)

## pack cn
product.cn <- read.xlsx('02_Inputs/Product standardization master data-A-S-0106_updated.xlsx') %>% 
  distinct(PACK_ID, Brand_CN = PROD_NAME_CH, Manufacture_CN = MNF_NAME_CH)

## 4+7 pack
pack47 <- read.xlsx('02_Inputs/4+7VBP__pack.xlsx') %>% 
  mutate(Pack.ID = stri_pad_left(Pack.ID, 7, 0))


##---- Result ----
bluebook.delivery <- proj.price %>% 
  group_by(year, date, province, city, market, atc4, molecule, packid) %>% 
  summarise(units = sum(units, na.rm = TRUE),
            sales = sum(sales, na.rm = TRUE)) %>% 
  ungroup() %>% 
  left_join(product.en, by = c('packid' = 'Pack_ID')) %>% 
  mutate(pos = stri_locate_last(Pck_Desc, regex = '\\s')[,1], 
         packsize = as.integer(str_squish(stri_sub(Pck_Desc, pos, -1))), 
         Volume = units * packsize, 
         Channel = 'CHC') %>% 
  left_join(product.cn, by = c('packid' = 'PACK_ID')) %>% 
  select(Province = province, City = city, Channel, Molecule = molecule, 
         Brand_EN, Brand_CN, Manufacture_EN, Manufacture_CN, Pack = packid, 
         Year = year, Date = date, Market = market, Volume, Value = sales)

write_feather(bluebook.delivery, '03_Outputs/VBP/06_Bluebook_2020_VBP_Summary.feather')
write.xlsx(bluebook.delivery, '03_Outputs/VBP/06_Bluebook_2020_VBP_Summary.xlsx')
