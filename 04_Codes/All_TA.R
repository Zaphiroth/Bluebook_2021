# CHC data ----------------------------------------------------------------

# readin the sample city projection data ----------------------------------

require(rlang)

## sample raw data
# total.imp <- read_feather("03_Outputs/02_Inside_Imputation.feater")
# gz <- read_csv("02_Inputs/01_Servier_CHC_Guangzhou_Raw.csv",
#                locale = locale(encoding = "GB18030")) 
# gz_m <- gz %>%
#   select(-c(X1, district, market, price)) %>%
#   rename(month = date, molecule = molecule_desc, atc4 = atc3) %>%
#   mutate(year = as.character(year),
#          month = as.character(month))
# 
# total.imp_m <- bind_rows(total.imp, gz_m)
total.imp_m <- imp.total %>% 
  rename(month = date)

# chk <- total.imp_m %>%
#   filter(city == "广州") %>%
#   left_join(chpa_info_m[, c("Pack_Id", "Molecule_Desc", "Prd_desc",
#                             "Mnf_Desc", "Pck_Desc", "ATC4_Code")],
#             by = c("packid" = "Pack_Id")) %>%
#   mutate(mkt = case_when(substr(ATC4_Code, 1, 3) %in% c("C10", "C11") ~ "LIP",
#                          substr(ATC4_Code, 1, 3) %in% c("C02", "C03", "C07", "C08", "C09") ~ "HTN",
#                          Molecule_Desc %in% c("ERLOTINIB", "GEFITINIB", "ICOTINIB", "AFATINIB") ~ "ONG_TKI",
#                          Molecule_Desc %in% c("CEFDINIR", "CEFIXIME", "CEFACLOR", "CEFPROZIL",
#                                          "CEFUROXIME",  "CEFADROXIL") ~ "ORAL_CEF",
#                          Molecule_Desc %in% c("CARBAMAZEPINE", "LAMOTRIGINE", 
#                                          "LEVETIRACETAM", "OXCARBAZEPINE",
#                                          "VALPROIC ACID") ~ "EPI", 
#                          TRUE ~ "Others"),
#          Year = substr(quarter, 1, 4)) %>%
#   filter(mkt != "Others") %>%
#   group_by(province,mkt, Year) %>%
#   summarise(Value = sum(sales, na.rm = TRUE)) %>%
#   ungroup() %>%
#   spread(Year, Value)
# 
# 
# chk1 <- total.imp_m %>%
#   filter(city == "广州") %>%
#   mutate(Year = substr(quarter, 1, 4)) %>%
#   group_by(province, Year) %>%
#   summarise(Value = sum(sales, na.rm = TRUE))



## sample projection data
# chc_sample_prj_data_files <- paste("./03_Outputs/",
#                                    c("03_Projection_2017.feather", 
#                                      "03_Projection_2018.feather",
#                                      "03_Projection_2019.feather"), 
#                                    sep = "")
# chc_all_ta_prj_data <- map_df(chc_sample_prj_data_files, read_feather)

proj.price <- imp.total %>% 
  filter(units > 0) %>% 
  group_by(packid, year, date, province, city) %>% 
  summarise(price = sum(sales, na.rm = TRUE) / sum(units, na.rm = TRUE)) %>% 
  ungroup()

chc_all_ta_prj_data <- proj.total %>% 
  left_join(proj.price, by = c('province', 'city', 'year', 'date', 'packid')) %>% 
  mutate(units = sales / price) %>% 
  rename(month = date)


# preparetion for the nation wide projection ------------------------------

## city tier information
# city_tier <- read_excel("03_Outputs/pchc_city_tier.xlsx")
city_tier <- read_excel("02_Inputs/pchc_city_tier.xlsx")

city_tier_m <- city_tier %>%
  group_by(city) %>%
  mutate(city_tier_m = ifelse(is.na(city_tier), first(city_tier), city_tier)) %>%
  ungroup() %>%
  mutate(city_tier_m = ifelse(is.na(city_tier_m), 5, city_tier_m)) %>%
  select(pchc, city, district, city_tier = city_tier_m)

packid_info <- chc_all_ta_prj_data %>%
  mutate(TA = "ALL TA") %>%
  select(packid) %>%
  distinct()

# pchc.universe <- read_excel("02_Inputs/Universe_PCHCCode_20200328.xlsx",
#                             sheet = "PCHC")
pchc.universe <- read.xlsx("02_Inputs/2020_PCHC_Universe更新维护.xlsx", 
                           sheet = "2020 CHC universe", cols = 1:19) %>% 
  mutate(`省` = gsub('省|市', '', `省`), 
         `地级市` = gsub('市', '', `地级市`)) %>% 
  rename(`区[县/县级市】` = `区[县/县级市]`, 
         PCHC_Code = `新版PCHC_Code`)

## projection to nation wide function
projection_model_nat <- function(data, metric) {
  # @data:
  # @metric:
  
  
  # data = total.imp_m
  # metric = "sales"
  
  
  ## here is the meta-programming
  metric = enquo(metric)
  
  hospital.universe <- pchc.universe %>% 
    group_by(pchc = PCHC_Code) %>% 
    summarise(province = dplyr::first(na.omit(`省`)),
              city = dplyr::first(na.omit(`地级市`)),
              district = dplyr::first(na.omit(`区[县/县级市】`)),
              pop = dplyr::first(na.omit(`人口`)),
              est = dplyr::first(na.omit(`其中：西药药品收入（千元）`))) %>% 
    ungroup() %>% 
    filter(!is.na(est), !is.na(pop)) %>%
    ungroup()
  
  pchc_district <- pchc.universe  %>%
    select(pchc = PCHC_Code,
           # city = 地级市, 
           district = `区[县/县级市】`) %>%
    distinct() %>%
    group_by(pchc) %>%
    filter(row_number() == 1) %>%
    left_join(city_tier_m[, c("pchc", "city_tier")], by = c("pchc"))
  
  mkt_data <- data %>%
    left_join(pchc_district, by = "pchc") %>%
    # the observations missing city tier are all in GZ
    mutate(city_tier = ifelse(is.na(city_tier), 1, city_tier)) %>%
    group_by(year, month, quarter, province, city, city_tier,
             # district,
             # TA,
             atc4, molecule, packid) %>%
    summarise(units = sum(units, na.rm = TRUE),
              sales = sum(sales, na.rm = TRUE)) %>%
    ungroup()
  
  
  
  mkt_data_m <- mkt_data %>%
    filter(!is.na(province)) %>%
    arrange(province, 
            city,
            # district,
            packid,
            month) %>%
    select(province, 
           city,
           # district,
           # PHA,
           city_tier,
           packid, 
           month, 
           !!metric) %>%
    spread(month, !!metric) %>%
    mutate(flag = 1)
  
  mkt_data_m1 <- mkt_data_m %>%
    filter(province %in% c("江苏",  "浙江", "广东"))
  
  
  universe_profile_m <- hospital.universe %>%
    left_join(pchc_district[, c("pchc", "city_tier")], by = c("pchc")) %>%
    group_by(province, city, 
             # district, 
             city_tier) %>%
    summarise(pop = sum(pop, na.rm = TRUE),
              est = sum(est, na.rm = TRUE)) %>%
    ungroup()
  
  mkt_data_m1 <- universe_profile_m %>%
    #rename(PHA = 新版ID) %>%
    inner_join(mkt_data_m1, by = c('province', 'city', 'city_tier'))
  
  mkt_universe <- universe_profile_m %>%
    #rename(PHA = 新版ID) %>%
    # filter(!(province %in% c("江苏", "浙江", "北京", "上海"))) 
    filter(!(city %in% c("北京","杭州", "南京", "宁波",  "苏州", "台州", "无锡", "徐州", "镇江"))) 
  
  region_projection <- vector("list", length = nrow(mkt_universe))
  
  pb <- txtProgressBar(min = 1, max = nrow(mkt_universe), initial = 1) 
  
  for (i in 1:nrow(mkt_universe)) {
    setTxtProgressBar(pb, i)
    
    tmp_universe <- mkt_universe[i, ]
    
    region_projection[[i]] <- tmp_universe %>% 
      left_join(mkt_data_m1,
                by = c(#"Hosp_level",
                  #"性质",
                  "city_tier"
                  #"Specialty_1_标准化", 
                  #"Specialty_2_标准化"
                )) %>%
      mutate(Est_DrugIncome_RMB_gap = abs(est.x - est.y)) %>%
      arrange(Est_DrugIncome_RMB_gap) %>%
      group_by(Est_DrugIncome_RMB_gap) %>%
      mutate(cnt = n()) %>%
      ungroup() %>%
      filter(row_number() <= cnt) %>%
      mutate(factor = ifelse(is.infinite(est.x / est.y) |
                               is.na(est.x / est.y) |
                               is.nan(est.x / est.y), 1, 
                             est.x / est.y)) %>%
      gather(date, value, -setdiff(1:ncol(.), starts_with("20"))) %>%
      select(province = province.x,
             city = city.x, 
             # Prefecture = Prefecture.x,
             # PHA = PHA.x,
             # Hosp_level,
             # 性质,
             city_tier, 
             # Specialty_1_标准化, 
             # Specialty_2_标准化,
             Est_DrugIncome_RMB = est.x,  
             Est_DrugIncome_RMB.y = est.y,
             factor,
             # 医生数 = 医生数.x, 
             # 床位数 = 床位数.x, 
             packid, date, value)
  }
  
  
  region_projection_m <- region_projection %>%
    bind_rows() %>%
    filter(!is.na(packid)) %>%
    mutate(factor = ifelse(factor > quantile(factor, 0.9),
                           quantile(factor, 0.9), factor)) %>% #remove outlier
    mutate(value = factor * value) %>%
    group_by(province, city, packid, date) %>%
    summarise(value = sum(value, na.rm = TRUE)) %>%
    ungroup() %>%
    left_join(packid_info, by = c("packid"))
  # %>%
  # mutate(Mkt = mkt_name) %>%
  # select(Mkt, Pack_ID = packcode, Province, Date = date, ATC3, 
  #        Molecule_Desc, 分子, Prod_Desc = Prd_desc, Pck_Desc, Mnf_Desc, value) %>%
  # arrange(Province, Date)
  
  if (as_name(enquo(metric)) == "sales") {
    region_projection_m <- region_projection_m %>% 
      rename(Sales = value)
  }
  
  if (as_name(enquo(metric)) == "units") {
    region_projection_m <- region_projection_m %>% 
      rename(Units = value)
  }
  
  
  return(region_projection_m)
}
projection_model_sh <- function(data, metric) {
  # @data:
  # @metric:
  
  
  # data = total.imp_m
  # metric = "sales"
  
  
  ## here is the meta-programming
  metric = enquo(metric)
  
  hospital.universe <- pchc.universe %>% 
    group_by(pchc = PCHC_Code) %>% 
    summarise(province = dplyr::first(na.omit(`省`)),
              city = dplyr::first(na.omit(`地级市`)),
              district = dplyr::first(na.omit(`区[县/县级市】`)),
              pop = dplyr::first(na.omit(`人口`)),
              est = dplyr::first(na.omit(`其中：西药药品收入（千元）`))) %>% 
    ungroup() %>% 
    filter(!is.na(est), !is.na(pop)) %>%
    ungroup()
  
  pchc_district <- pchc.universe  %>%
    select(pchc = PCHC_Code,
           # city = 地级市, 
           district = `区[县/县级市】`) %>%
    distinct() %>%
    group_by(pchc) %>%
    filter(row_number() == 1) %>%
    left_join(city_tier_m[, c("pchc", "city_tier")], by = c("pchc"))
  
  mkt_data <- data %>%
    left_join(pchc_district, by = "pchc") %>%
    # the observations missing city tier are all in GZ
    mutate(city_tier = ifelse(is.na(city_tier), 1, city_tier)) %>%
    group_by(year, month, quarter, province, city, city_tier,
             # district,
             # TA,
             atc4, molecule, packid) %>%
    summarise(units = sum(units, na.rm = TRUE),
              sales = sum(sales, na.rm = TRUE)) %>%
    ungroup()
  
  
  
  mkt_data_m <- mkt_data %>%
    filter(!is.na(province)) %>%
    arrange(province, 
            city,
            # district,
            packid,
            month) %>%
    select(province, 
           city,
           # district,
           # PHA,
           city_tier,
           packid, 
           month, 
           !!metric) %>%
    spread(month, !!metric) %>%
    mutate(flag = 1)
  
  mkt_data_m1 <- mkt_data_m %>%
    filter(province %in% c("北京"))
  
  
  universe_profile_m <- hospital.universe %>%
    left_join(pchc_district[, c("pchc", "city_tier")], by = c("pchc")) %>%
    group_by(province, city, 
             # district, 
             city_tier) %>%
    summarise(pop = sum(pop, na.rm = TRUE),
              est = sum(est, na.rm = TRUE)) %>%
    ungroup()
  
  mkt_data_m1 <- universe_profile_m %>%
    #rename(PHA = 新版ID) %>%
    inner_join(mkt_data_m1)
  
  mkt_universe <- universe_profile_m %>%
    #rename(PHA = 新版ID) %>%
    filter((province %in% c("上海"))) 
  
  region_projection <- vector("list", length = nrow(mkt_universe))
  
  # pb <- txtProgressBar(min = 1, max = nrow(mkt_universe), initial = 1) 
  # 
  # for (i in 1:nrow(mkt_universe)) {
  #   setTxtProgressBar(pb, i)
  
  tmp_universe <- mkt_universe[1, ]
  
  region_projection[[1]] <- tmp_universe %>% 
    left_join(mkt_data_m1,
              by = c(#"Hosp_level",
                #"性质",
                "city_tier"
                #"Specialty_1_标准化", 
                #"Specialty_2_标准化"
              )) %>%
    mutate(Est_DrugIncome_RMB_gap = abs(est.x - est.y)) %>%
    arrange(Est_DrugIncome_RMB_gap) %>%
    group_by(Est_DrugIncome_RMB_gap) %>%
    mutate(cnt = n()) %>%
    ungroup() %>%
    filter(row_number() <= cnt) %>%
    mutate(factor = ifelse(is.infinite(est.x / est.y) |
                             is.na(est.x / est.y) |
                             is.nan(est.x / est.y), 1, 
                           est.x / est.y)) %>%
    gather(date, value, -setdiff(1:ncol(.), starts_with("20"))) %>%
    select(province = province.x,
           city = city.x, 
           # Prefecture = Prefecture.x,
           # PHA = PHA.x,
           # Hosp_level,
           # 性质,
           city_tier, 
           # Specialty_1_标准化, 
           # Specialty_2_标准化,
           Est_DrugIncome_RMB = est.x,  
           Est_DrugIncome_RMB.y = est.y,
           factor,
           # 医生数 = 医生数.x, 
           # 床位数 = 床位数.x, 
           packid, date, value)
  # }
  
  
  region_projection_m <- region_projection %>%
    bind_rows() %>%
    filter(!is.na(packid)) %>%
    mutate(factor = ifelse(factor > quantile(factor, 0.9),
                           quantile(factor, 0.9), factor)) %>% #remove outlier
    mutate(value = factor * value) %>%
    group_by(province, city, packid, date) %>%
    summarise(value = sum(value, na.rm = TRUE)) %>%
    ungroup() %>%
    left_join(packid_info, by = c("packid"))
  # %>%
  # mutate(Mkt = mkt_name) %>%
  # select(Mkt, Pack_ID = packcode, Province, Date = date, ATC3, 
  #        Molecule_Desc, 分子, Prod_Desc = Prd_desc, Pck_Desc, Mnf_Desc, value) %>%
  # arrange(Province, Date)
  
  if (as_name(enquo(metric)) == "sales") {
    region_projection_m <- region_projection_m %>% 
      rename(Sales = value)
  }
  
  if (as_name(enquo(metric)) == "units") {
    region_projection_m <- region_projection_m %>% 
      rename(Units = value)
  }
  
  
  return(region_projection_m)
}

data_sales_nat_without_sh <-
  projection_model_nat(data = total.imp_m, metric = "sales")
data_units_nat_without_sh <-
  projection_model_nat(data = total.imp_m, metric = "units")

data_sales_sh <- projection_model_sh(data = total.imp_m, metric = "sales")
data_units_sh <- projection_model_sh(data = total.imp_m, metric = "units")


oth_provice_without_sh_data <- data_sales_nat_without_sh %>%
  left_join(data_units_nat_without_sh)

sh_data <- data_sales_sh %>%
  left_join(data_units_sh)

# data1_m <- data1 %>%
#   mutate(year = substr(date, 1, 4)) %>%
#   group_by(TA, province, year) %>%
#   summarise(Sales = sum(Sales, na.rm = TRUE)) %>%
#   ungroup()

chc_all_ta_prj_data_m <- chc_all_ta_prj_data %>%
  group_by(province, city, packid, date = month) %>%
  summarise(Sales = sum(sales, na.rm = TRUE),
            Units = sum(units, na.rm = TRUE)) %>%
  ungroup()


all_chc_data <- bind_rows(chc_all_ta_prj_data_m,
                          oth_provice_without_sh_data, sh_data)

# write.xlsx(all_data, "03_Outputs/chc_test1.xlsx")
chk <- all_chc_data %>%
  mutate(Year = substr(date, 1, 4)) %>%
  group_by(province, Year) %>%
  summarise(Sales = sum(Sales, na.rm = TRUE)) %>%
  ungroup() %>%
  spread(Year, Sales)


# product.info <- read.xlsx("02_Inputs/Product standardization master data-A-S-0313.xlsx")
city_info <- read_excel("02_Inputs/4+7+省会名单.xlsx") %>%
  mutate(城市 = gsub("市", "", 城市 )) %>%
  select(城市, 类别)

chpa_info_m <- read.xlsx('02_Inputs/ims_chpa_to20Q4.xlsx', cols = 1:21, startRow = 4) %>%
  select(Pack_Id:Pck_Desc) %>%
  distinct()

all_chc_data_m <- all_chc_data %>%
  left_join(chpa_info_m[, c("Pack_Id", "Molecule_Desc", "Prd_desc",
                            "Mnf_Desc", "Pck_Desc", "ATC4_Code")],
            by = c("packid" = "Pack_Id")) %>%
  mutate(Quarter = case_when(substr(date, 5, 6) %in% c("01", "02", "03") ~ 
                               paste(substr(date, 1, 4), "Q1", sep = ""),
                             substr(date, 5, 6) %in% c("04", "05", "06") ~ 
                               paste(substr(date, 1, 4), "Q2", sep = ""),
                             substr(date, 5, 6) %in% c("07", "08", "09") ~ 
                               paste(substr(date, 1, 4), "Q3", sep = ""),
                             TRUE ~ paste(substr(date, 1, 4), "Q4", sep = "")),
         PACK = as.integer(str_extract(Pck_Desc, "\\d{1,}$")),
         Year = substr(date, 1, 4)) %>%
  left_join(city_info, by = c("city" = "城市")) %>%
  mutate(Channel = "CHC") %>%
  mutate(`Volume（片）` = Units * PACK) %>%
  group_by(Province = province, City = city,
           Channel, ATC4_Code, Molecule = Molecule_Desc, Brand = Prd_desc,
           Manufacture = Mnf_Desc, Pack = Pck_Desc, PackID = packid, 
           Year, Quarter,
           Market = "ALL TA") %>%
  summarise( `Volume（片）` = sum(`Volume（片）`, na.rm = TRUE),
             Value = sum(Sales, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(mkt = case_when(substr(ATC4_Code, 1, 3) %in% c("C10", "C11") ~ "LIP",
                         substr(ATC4_Code, 1, 3) %in% c("C02", "C03", "C07", "C08", "C09") ~ "HTN",
                         Molecule %in% c("ERLOTINIB", "GEFITINIB", "ICOTINIB", "AFATINIB") ~ "ONG_TKI",
                         Molecule %in% c("CEFDINIR", "CEFIXIME", "CEFACLOR", "CEFPROZIL",
                                         "CEFUROXIME",  "CEFADROXIL") ~ "ORAL_CEF",
                         Molecule %in% c("CARBAMAZEPINE", "LAMOTRIGINE", 
                                         "LEVETIRACETAM", "OXCARBAZEPINE",
                                         "VALPROIC ACID") ~ "EPI", 
                         TRUE ~ "Others"),
         Year = substr(Quarter, 1, 4)) 

write.xlsx(all_chc_data_m, '03_Outputs/Bluebook_2020_Universe_Nation.xlsx')


## chc adjustment according to VBP
vbp_data <- read_excel("05_Internal_Review/v9/blue_book_II_5mkts_20200429.xlsx")

vbp_data_m <- vbp_data %>% 
  filter(Channel == "CHC") %>%
  group_by(Province, Market, Year) %>%
  summarise(Value = sum(Value, na.rm = TRUE)) %>%
  ungroup() %>%
  spread(Year, Value)


chc_chk <- all_chc_data_m %>%
  mutate(mkt = case_when(substr(ATC4_Code, 1, 3) %in% c("C10", "C11") ~ "LIP",
                         substr(ATC4_Code, 1, 3) %in% c("C02", "C03", "C07", "C08", "C09") ~ "HTN",
                         Molecule %in% c("ERLOTINIB", "GEFITINIB", "ICOTINIB", "AFATINIB") ~ "ONG_TKI",
                         Molecule %in% c("CEFDINIR", "CEFIXIME", "CEFACLOR", "CEFPROZIL",
                                         "CEFUROXIME",  "CEFADROXIL") ~ "ORAL_CEF",
                         Molecule %in% c("CARBAMAZEPINE", "LAMOTRIGINE", 
                                         "LEVETIRACETAM", "OXCARBAZEPINE",
                                         "VALPROIC ACID") ~ "EPI", 
                         TRUE ~ "Others"),
         Year = substr(Quarter, 1, 4)) %>%
  filter(mkt != "Others") %>%
  # filter(Province %in% c("北京", "浙江", "江苏", "上海")) %>%
  group_by(Province, mkt, Year) %>%
  # group_by( mkt, Year) %>%
  summarise(Value = sum(Value, na.rm = TRUE)) %>%
  ungroup() %>%
  # group_by(Province, Year) %>%
  # group_by(Year) %>%
  # mutate(total = sum(Value, na.rm = TRUE)) %>%
  # mutate(ratio = Value / total) %>%
  # select(-c(Value, total)) %>%
  spread(Year, Value) %>%
  mutate(Province = ifelse(Province == "黑龙江", "黑龙", Province))

# write.xlsx(chc_chk, "03_Outputs/ALL_TA_CHC_nat_share.xlsx")
# write.xlsx(chc_chk, "03_Outputs/ALL_TA_CHC_nat_value.xlsx")



vbp_data_m1 <- vbp_data_m %>%
  left_join(chc_chk, by = c("Province", "Market" = "mkt")) %>%
  mutate(factor_2017 = `2017.x` / `2017.y`,
         factor_2018 = `2018.x` / `2018.y`,
         factor_2019 = `2019.x` / `2019.y`) %>%
  mutate(factor_2017 = ifelse(is.na(factor_2017) | is.infinite(factor_2017), 1, factor_2017),
         factor_2018 = ifelse(is.na(factor_2018) | is.infinite(factor_2018), 1, factor_2018),
         factor_2019 = ifelse(is.na(factor_2019) | is.infinite(factor_2019), 1, factor_2019)) %>%
  mutate(factor = (factor_2017 + factor_2018 + factor_2019) / 3,
         factor = ifelse(is.na(factor), 1, factor)) %>%
  select(Province, Market, weight = `2019.x`, factor) %>%
  group_by(Province) %>%
  mutate(Others = sum(weight * factor, na.rm = TRUE) / sum(weight, na.rm = TRUE)) %>%
  ungroup() %>%
  select(-weight) %>%
  spread(Market, factor) %>%
  gather(Market, Factor, -Province) %>%
  mutate(Province = ifelse(Province == "黑龙", "黑龙江", Province))

all_chc_data_m1 <- all_chc_data_m %>%
  left_join(vbp_data_m1, by = c("Province", "mkt" = "Market")) %>%
  mutate(`Volume（片）` = `Volume（片）` * Factor,
         Value = Value * Factor) %>%
  select(-Factor)

# chk <- all_chc_data_m1 %>%filter(is.na(factor))
prov_chk <- all_chc_data_m1 %>%
  # mutate(mkt = case_when(substr(ATC4_Code, 1, 3) %in% c("C10", "C11") ~ "LIP",
  #                        substr(ATC4_Code, 1, 3) %in% c("C02", "C03", "C07", "C08", "C09") ~ "HTN",
  #                        Molecule %in% c("ERLOTINIB", "GEFITINIB", "ICOTINIB", "AFATINIB") ~ "ONG_TKI",
  #                        Molecule %in% c("CEFDINIR", "CEFIXIME", "CEFACLOR", "CEFPROZIL",
  #                                        "CEFUROXIME",  "CEFADROXIL") ~ "ORAL_CEF",
  #                        Molecule %in% c("CARBAMAZEPINE", "LAMOTRIGINE", 
  #                                        "LEVETIRACETAM", "OXCARBAZEPINE",
  #                                        "VALPROIC ACID") ~ "EPI", 
  #                        TRUE ~ "Others"),
  #        Year = substr(Quarter, 1, 4)) %>%
  group_by(mkt, Year) %>%
  summarise(Value = sum(Value, na.rm = TRUE)) %>%
  ungroup() %>%
  spread(Year, Value)


all_chc_data_m2 <- all_chc_data_m1 %>%
  group_by(#Province = province, City = city,
    Channel, ATC4_Code, Molecule , Brand, Manufacture, Pack, PackID,
    Year, Quarter,
    Market = "ALL TA") %>%
  summarise( `Volume（片）` = sum(`Volume（片）`, na.rm = TRUE),
             Value = sum(Value, na.rm = TRUE)) %>%
  ungroup()

chc_atc_chk <- all_chc_data_m2 %>%
  mutate(ATC1 = substr(ATC4_Code, 1, 1)) %>%
  group_by(ATC1, Year) %>%
  summarise(Value = sum(Value, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(Year) %>%
  mutate(Total = sum(Value, na.rm = TRUE)) %>%
  mutate(chc_atc1_percentage = Value / Total)


write.xlsx(all_chc_data_m2, "03_Outputs/all_ta_chc_0430.xlsx")


# City hospital ALL TA ----------------------------------------------------

chpa_data <- read_excel("02_Inputs/ims_chpa_to19Q4.xlsx")

chpa_data_m <- chpa_data %>%
  select(Pack_Id:Pack_ID, dplyr::starts_with("CU"), dplyr::starts_with("RENMINBI")) %>%
  gather(Date, Value, -c(Pack_Id:Pack_ID)) %>%
  separate(Date, c("measurement", "date")) %>%
  spread(measurement, Value) %>%
  filter(grepl("2017|2018|2019", date))


city_hosp_ratio <- read_excel("02_Inputs/AZ-Sanofi_intrim_bpeng.xlsx",
                              sheet = "Sheet1")


chpa_data_m1 <- chpa_data_m %>%
  left_join(city_hosp_ratio, by = c("ATC1_Code" = "IMS"))


# chk <- chpa_data_m1 %>%
#   filter(is.na(city_hosp_ratio))

random_noise <- rnorm(nrow(chpa_data_m1), 0, 0.01)

chpa_data_m1$city_hosp_ratio <- chpa_data_m1$city_hosp_ratio + random_noise

chpa_data_m2 <- chpa_data_m1 %>%
  mutate(CU = CU * city_hosp_ratio,
         RENMINBI = RENMINBI * city_hosp_ratio,
         Channel = "城市医院",
         Year = substr(date, 1, 4)) %>%
  rename(`Volume（片）` = CU,
         Value = RENMINBI) %>% 
  group_by(#Province = province, City,
    Channel, ATC4_Code, Molecule = Molecule_Desc, Brand = Prd_desc,
    Manufacture = Mnf_Desc, Pack = Pck_Desc, PackID = Pack_Id, 
    Year, Quarter = date,
    Market = "ALL TA") %>%
  summarise( `Volume（片）` = sum(`Volume（片）`, na.rm = TRUE),
             Value = sum(Value, na.rm = TRUE)) %>%
  ungroup()

write.xlsx(chpa_data_m2, "03_Outputs/all_ta_city_hosp_0430.xlsx")

# city_chk <- chpa_data_m2 %>%
#   mutate(mkt = case_when(substr(ATC4_Code, 1, 3) %in% c("C10", "C11") ~ "LIP",
#                          substr(ATC4_Code, 1, 3) %in% c("C02", "C03", "C07", "C08", "C09") ~ "HTN",
#                          Molecule %in% c("ERLOTINIB", "GEFITINIB", "ICOTINIB", "AFATINIB") ~ "ONG_TKI",
#                          Molecule %in% c("CEFDINIR", "CEFIXIME", "CEFACLOR", "CEFPROZIL",
#                                          "CEFUROXIME",  "CEFADROXIL") ~ "ORAL_CEF",
#                          Molecule %in% c("CARBAMAZEPINE", "LAMOTRIGINE", 
#                                          "LEVETIRACETAM", "OXCARBAZEPINE",
#                                          "VALPROIC ACID") ~ "EPI", 
#                          TRUE ~ "Others"),
#          Year = substr(Quarter, 1, 4)) %>%
#   group_by(mkt, Year) %>%
#   summarise(Value = sum(Value, na.rm = TRUE)) %>%
#   ungroup() %>%
#   spread(Year, Value)
# 
# 
city_atc_chk <- chpa_data_m2 %>%
  mutate(ATC1 = substr(ATC4_Code, 1, 1)) %>%
  group_by(ATC1, Year) %>%
  summarise(Value = sum(Value, na.rm = TRUE)) %>%
  group_by(Year) %>%
  mutate(Total = sum(Value, na.rm = TRUE)) %>%
  mutate(city_atc1_percentage = Value / Total) %>%
  left_join(chc_atc_chk[, c("ATC1", "Year", "chc_atc1_percentage")]) %>%
  left_join(county_atc_chk[, c("ATC1", "Year", "county_atc1_percentage")])




# combine 3 channel together and repalce the vbp part ---------------------

# vbp_data <- read.xlsx("05_Internal_Review/v9/blue_book_II_5mkts_20200429.xlsx")
vbp_data <- read.xlsx("05_Internal_Review/v9/blue_book_II_5mkts_20200506.xlsx")
vbp_m <- vbp_data %>%
  select("Market", "Channel", "Molecule", "Brand", "Manufacture",
         "Pack" , "Quarter", "Volume（片）", "Value",   "PackID","ATC4_Code" = "ATC4") %>%
  mutate(Market = "ALL TA") %>%
  group_by(Market, Channel, Molecule, Brand, Manufacture, Pack, Quarter, PackID, ATC4_Code) %>%
  summarise(`Volume（片）` = sum(`Volume（片）`, na.rm = TRUE),
            Value = sum(Value, na.rm = TRUE)) %>%
  ungroup()

vbp_packid <- vbp_m %>%
  select(PackID) %>%
  distinct() %>%
  unlist()

# county_data_all_ta_m3 <- read.xlsx("03_Outputs/all_ta_county_0430.xlsx")
all_ta_county <- county_data_all_ta_m3 %>%
  rename(PackID = Pack_Id)

all_ta_chc <- read.xlsx("03_Outputs/all_ta_chc_0430.xlsx")
all_ta_city <- read.xlsx("03_Outputs/all_ta_city_hosp_0430.xlsx")

all_ta_city_m <- all_ta_city %>%
  mutate(Market = "ALL TA")

## append city hospital with beds less than 100
# hosp_universe <- read.xlsx("02_Inputs/2019年Universe更新维护2.4_200210.xlsx",
#                             sheet = "Universe2019")
# colnames(hosp_universe)[which(colnames(hosp_universe) %in%
#                           c("新版ID", "医生数", "年诊疗人次", "门诊诊次"))] <-
#   paste(colnames(hosp_universe)[which(colnames(hosp_universe) %in%
#                                         c("新版ID", "医生数", "年诊疗人次", "门诊诊次"))],
#         1:8)
# 
# hosp_universe_m <- hosp_universe %>%
#   mutate(bed100_flag = ifelse(床位数 >= 100, "床位数大于等于100", "床位数小于100")) %>%
#   group_by(bed100_flag) %>%
#   summarise(Est_DrugIncome_RMB = sum(Est_DrugIncome_RMB, na.rm = TRUE)) %>%
#   ungroup() %>%
#   filter(!is.na(bed100_flag)) %>%
#   spread(bed100_flag, Est_DrugIncome_RMB) %>%
#   mutate(ratio = `床位数小于100` / `床位数大于等于100`)

all_ta_city_m1 <- all_ta_city_m %>%
  mutate(`Volume（片）` = `Volume（片）` * 1.0822,
         Value = Value * 1.0822)

## adjust county hospital

all_ta_county_m <- all_ta_county %>%
  mutate(`Volume（片）` = `Volume（片）` * 1.06,
         Value = Value * 1.06)

chk <- all_ta_county_m %>%
  filter(Brand %in% c("HERCEPTIN          GTC",
                      "LI PU SU           JNE",
                      "AVASTIN            GTC",
                      "LANTUS             AVS",
                      "BEHRING            CSB",
                      "TPIAO              S3S",
                      "MABTHERA           ROC")) %>%
  group_by(Brand, Year = substr(Quarter, 1, 4)) %>%
  summarise(Value = sum(Value, na.rm = TRUE)) %>%
  ungroup() %>%
  spread(Year, Value)

all_ta_county_m1 <- all_ta_county_m %>%
  mutate(`Volume（片）` = ifelse(Brand == "HERCEPTIN          GTC",
                              `Volume（片）` * 1.71,
                              ifelse(Brand == "LI PU SU           JNE",
                                     `Volume（片）` * 1.23,
                                     ifelse(Brand == "AVASTIN            GTC",
                                            `Volume（片）` * 1.88,
                                            ifelse(Brand == "LANTUS             AVS",
                                                   `Volume（片）` * 1.22,
                                                   ifelse(Brand == "BEHRING            CSB",
                                                          `Volume（片）` * 5.52,
                                                          ifelse(Brand == "TPIAO              S3S",
                                                                 `Volume（片）` * 3.23,
                                                                 ifelse(Brand == "MABTHERA           ROC",
                                                                        `Volume（片）` * 3.06,
                                                                        `Volume（片）`))))))),
         Value = ifelse(Brand == "HERCEPTIN          GTC",
                        Value * 1.71,
                        ifelse(Brand == "LI PU SU           JNE",
                               Value * 1.23,
                               ifelse(Brand == "AVASTIN            GTC",
                                      Value * 1.88,
                                      ifelse(Brand == "LANTUS             AVS",
                                             Value * 1.22,
                                             ifelse(Brand == "BEHRING            CSB",
                                                    Value * 5.52,
                                                    ifelse(Brand == "TPIAO              S3S",
                                                           Value * 3.23,
                                                           ifelse(Brand == "MABTHERA           ROC",
                                                                  Value * 3.06,
                                                                  Value))))))))


all_ta_3channel <- bind_rows(all_ta_county_m1, all_ta_chc, all_ta_city_m1) %>%
  select(-Year)

all_ta_3channel_m <- all_ta_3channel %>%
  filter(!(PackID %in% vbp_packid)) %>%
  bind_rows(vbp_m)

# write_csv(all_ta_3channel_m, "05_Internal_Review/v9/全域_3渠道_0430.csv")

all_ta_chc <- all_ta_3channel_m %>%
  filter(Channel == "CHC") %>%
  filter(!((`Volume（片）` == 0 & Value != 0) | (`Volume（片）` != 0 & Value == 0)))

all_ta_county <- all_ta_3channel_m %>%
  filter(Channel == "County") %>%
  filter(!((`Volume（片）` == 0 & Value != 0) | (`Volume（片）` != 0 & Value == 0)))

all_ta_city <- all_ta_3channel_m %>%
  filter(Channel == "城市医院") %>%
  filter(!((`Volume（片）` == 0 & Value != 0) | (`Volume（片）` != 0 & Value == 0)))



wb <- createWorkbook()
addWorksheet(wb, "CHC")
addWorksheet(wb, "County")
addWorksheet(wb, "City")

writeData(wb, "CHC", all_ta_chc)
writeData(wb, "County", all_ta_county)
writeData(wb, "City", all_ta_city)

saveWorkbook(wb, "05_Internal_Review/v9/全域_3渠道_0508_m.xlsx", overwrite = TRUE)




## extract packid that upjohn request 
upjohn_pack <- read_excel("02_Inputs/普强packid.xlsx") %>%
  unlist() %>%
  unique()

upjohn_county <- all_ta_3channel_m %>%
  filter(Channel == "County") %>%
  filter(PackID %in% upjohn_pack )


upjohn_county_chk <- upjohn_county %>%
  select(Molecule, PackID  ) %>%
  distinct()


write.xlsx(upjohn_county, "03_Outputs/upjohn_county_data_0430.xlsx")

