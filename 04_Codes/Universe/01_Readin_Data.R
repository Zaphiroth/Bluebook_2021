# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  Bluebook 2021-Universe
# Purpose:      Readin
# programmer:   Zhe Liu
# Date:         2021-03-12
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


##---- Readin raw data ----
## PCHC code
pchc.mapping <- read.xlsx('02_Inputs/Universe_PCHCCode_20210303.xlsx', sheet = 'PCHC')

pchc.mapping1 <- pchc.mapping %>% 
  filter(!is.na(`单位名称`), !is.na(PCHC_Code)) %>% 
  group_by(province = `省`, city = `地级市`, district = `区[县/县级市】`, hospital = `单位名称`) %>% 
  summarise(pchc = first(PCHC_Code)) %>% 
  ungroup()

pchc.mapping2 <- pchc.mapping %>% 
  filter(!is.na(ZS_Servier.name), !is.na(PCHC_Code)) %>% 
  group_by(province = `省`, city = `地级市`, district = `区[县/县级市】`, hospital = `ZS_Servier.name`) %>% 
  summarise(pchc = first(PCHC_Code)) %>% 
  ungroup()

pchc.mapping3 <- bind_rows(pchc.mapping1, pchc.mapping2) %>% 
  distinct(province, city, district, hospital, pchc)

pchc.mapping4 <- pchc.mapping3 %>% 
  group_by(pchc) %>% 
  summarise(province = first(na.omit(province)),
            city = first(na.omit(city)),
            district = first(na.omit(district))) %>% 
  ungroup()


##---- Formatting ----
raw.100.19 <- read_csv('02_Inputs/data/shequ_100_bjjszj_19_packid_moleinfo.csv', 
                       locale = locale(encoding = 'GB18030'))
raw.100.20 <- read_csv('02_Inputs/data/shequ_100_bjjszj_20_packid_moleinfo.csv', 
                       locale = locale(encoding = 'GB18030'))

raw.total <- bind_rows(raw.100.19, raw.100.20) %>% 
  distinct(year = as.character(Year), 
           quarter = Quarter, 
           date = gsub('/', '', Month), 
           province = gsub('省|市', '', Province), 
           city = if_else(City == '市辖区', '北京', gsub('市', '', City)), 
           district = County, 
           hospital = Hospital_Name, 
           market = 'Universe', 
           atc2 = stri_sub(ATC4_Code, 1, 3), 
           atc3 = stri_sub(ATC4_Code, 1, 4), 
           atc4 = ATC4_Code, 
           molecule = Molecule_Desc, 
           packid = stri_pad_left(packcode, 7, 0), 
           units = Volume, 
           sales = Value) %>% 
  filter(!is.na(packid), !is.na(molecule)) %>% 
  left_join(pchc.mapping3, by = c('province', 'city', 'district', 'hospital')) %>% 
  filter(!is.na(pchc)) %>% 
  group_by(year, quarter, date, province, city, district, pchc, market, atc4, molecule, packid) %>% 
  summarise(units = sum(units, na.rm = TRUE), 
            sales = sum(sales, na.rm = TRUE)) %>% 
  ungroup() %>% 
  filter(units > 0, sales > 0)

write_feather(raw.total, '03_Outputs/Universe/01_Bluebook_2020_Universe_Raw.feather')
