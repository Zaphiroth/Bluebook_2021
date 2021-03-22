# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  Bluebook 2021-VBP
# Purpose:      Readin
# programmer:   Zhe Liu
# Date:         2021-03-11
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


##---- Readin raw data ----
## PCHC code
pchc.mapping <- read.xlsx("02_Inputs/Universe_PCHCCode_20210303.xlsx", sheet = "PCHC")

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

## product info
nfc.info <- read.xlsx("02_Inputs/Product standardization master data-A-S-0106_updated.xlsx") %>% 
  distinct(packid = stri_pad_left(PACK_ID, 7, 0), 
           nfc1 = NFC1_NAME_CH)


##---- Raw data ----
## Community 100
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
           atc2 = stri_sub(ATC4_Code, 1, 3), 
           atc3 = stri_sub(ATC4_Code, 1, 4), 
           atc4 = ATC4_Code, 
           molecule = Molecule_Desc, 
           packid = stri_pad_left(packcode, 7, 0), 
           units = Volume, 
           sales = Value) %>% 
  mutate(
    market = case_when(
      atc2 %in% c("C10", "C11") ~ 'LIP', 
      atc2 %in% c("C02", "C03", "C07", "C08", "C09") ~ "HTN", 
      atc4 == "L01H2" & molecule %in% c("ERLOTINIB", "GEFITINIB", "ICOTINIB", "AFATINIB") ~ 
        "ONG-TKI", 
      atc4 == "N03A0" & molecule %in% c("CARBAMAZEPINE", "LAMOTRIGINE", "LEVETIRACETAM", 
                                        "OXCARBAZEPINE", "VALPROIC ACID") ~ 
        "EPI",
      atc4 == "J01D1" & molecule %in% c("CEFDINIR", "CEFIXIME", "CEFACLOR", 
                                        "CEFPROZIL", "CEFUROXIME", "CEFADROXIL", 
                                        "CEFUROXIME AXETIL") ~ 
        "ORAL CEF", 
      TRUE ~ NA_character_
    )
  ) %>% 
  filter(!is.na(market)) %>% 
  left_join(pchc.mapping3, by = c('province', 'city', 'district', 'hospital')) %>% 
  filter(!is.na(pchc)) %>% 
  group_by(year, quarter, date, province, city, district, pchc, market, atc4, molecule, packid) %>% 
  summarise(units = sum(units, na.rm = TRUE), 
            sales = sum(sales, na.rm = TRUE)) %>% 
  ungroup() %>% 
  filter(units > 0, sales > 0)

# chk <- raw.total %>% 
#   filter(is.na(pchc), 
#          grepl('中心', hospital), 
#          grepl('社区', hospital), 
#          !grepl('卫生院|卫生室|卫生站|服务站|社区站|医院', hospital)) %>% 
#   distinct(province, city, district, hospital) %>% 
#   arrange(province, city, district, hospital)

write_feather(raw.total, "03_Outputs/VBP/01_Bluebook_2020_VBP_Raw.feather")
