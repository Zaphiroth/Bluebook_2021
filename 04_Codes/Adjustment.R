# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  Bluebook 2021
# Purpose:      CHC projection
# programmer:   Zhe Liu
# Date:         2021-03-22
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


##---- Readin ----
## Bluebook 2019 & 2020
all_chc_data_m <- read.xlsx('03_Outputs/Bluebook_2020_Universe_Nation.xlsx')

## factor
adj.factor <- read.xlsx('02_Inputs/CHC_Final_Factor_2017_2019.xlsx')


##---- Adjustment ----
all_chc_data_adj <- all_chc_data_m %>% 
  # mutate(Market = case_when(substr(ATC4_Code, 1, 3) %in% c("C10", "C11") ~ "LIP", 
  #                           substr(ATC4_Code, 1, 3) %in% c("C02", "C03", "C07", "C08", "C09") ~ "HTN", 
  #                           Molecule %in% c("ERLOTINIB", "GEFITINIB", "ICOTINIB", "AFATINIB") ~ "ONG_TKI", 
  #                           Molecule %in% c("CEFDINIR", "CEFIXIME", "CEFACLOR", "CEFPROZIL", 
  #                                           "CEFUROXIME",  "CEFADROXIL") ~ "ORAL_CEF", 
  #                           Molecule %in% c("CARBAMAZEPINE", "LAMOTRIGINE", 
  #                                           "LEVETIRACETAM", "OXCARBAZEPINE", 
  #                                           "VALPROIC ACID") ~ "EPI", 
  #                           TRUE ~ "Others")) %>% 
  left_join(adj.factor, by = c('Province', 'mkt' = 'Market')) %>% 
  mutate(Factor = if_else(is.na(Factor), 1, Factor), 
         `Volume（片）` = `Volume（片）` * Factor, 
         Value = Value * Factor) %>% 
  select(-Factor)

write.xlsx(all_chc_data_adj, '03_Outputs/Bluebook_2020_Universe_Nation_Adj.xlsx')
