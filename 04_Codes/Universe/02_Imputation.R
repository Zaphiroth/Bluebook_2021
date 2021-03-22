# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  Bluebook 2021-Universe
# Purpose:      Imputation
# programmer:   Zhe Liu
# Date:         2021-03-12
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


##---- Imputation ----
source('./04_Codes/functions/MonthImpFunc.R', encoding = 'UTF-8')

imp.q1 <- MonthImpFunc(raw.total, past = '2019', current = '2020', quar = "Q1")
imp.q2 <- MonthImpFunc(raw.total, past = '2019', current = '2020', quar = "Q2")
imp.q3 <- MonthImpFunc(raw.total, past = '2019', current = '2020', quar = "Q3")
imp.q4 <- MonthImpFunc(raw.total, past = '2019', current = '2020', quar = "Q4")

imp.total <- bind_rows(imp.q1, imp.q2, imp.q3, imp.q4)

write_feather(imp.total, "03_Outputs/Universe/02_Bluebook_2020_Universe_Imputation.feater")
