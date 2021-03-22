# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  Bluebook 2021-VBP
# Purpose:      Imputation function
# programmer:   Zhe Liu
# Date:         2021-03-11
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


MonthImpFunc <- function(raw.total = raw.total, 
                         past = '2019', 
                         current = '2020', 
                         quar = 'Q1') {
  
  ## quarter data
  quar.raw <- raw.total %>% 
    filter(stri_sub(quarter, 5, 6) == quar)
  
  ## quarterly month continuity
  month.continuity <- quar.raw %>% 
    distinct(province, city, district, pchc, market, year, date) %>% 
    count(province, city, district, pchc, market, year) %>% 
    pivot_wider(names_from = year, 
                values_from = n, 
                values_fill = 0) %>% 
    mutate(cnt_min = pmin(!!sym(past), !!sym(current)),
           cnt_max = pmax(!!sym(past), !!sym(current)))
  
  ## city molecule yearly growth
  city.growth <- month.continuity %>% 
    filter(cnt_min >= 2) %>% 
    inner_join(quar.raw, by = c('province', 'city', 'district', 'pchc', 'market')) %>% 
    filter(year %in% c(past, current)) %>% 
    group_by(year, province, city, market, atc4, molecule) %>% 
    summarise(sales = sum(sales, na.rm = TRUE), 
              units = sum(units, na.rm = TRUE)) %>% 
    ungroup() %>% 
    pivot_wider(id_cols = c(province, city, market, atc4, molecule), 
                names_from = year, 
                values_from = c(units, sales), 
                values_fill = 0) %>% 
    mutate(growth_units = !!sym(paste0('units_', current)) / !!sym(paste0('units_', past)), 
           growth_sales = !!sym(paste0('sales_', current)) / !!sym(paste0('sales_', past)), 
           growth_units = ifelse(growth_units < 0.1 | growth_units > 10, 1, growth_units),
           growth_sales = ifelse(growth_sales < 0.1 | growth_sales > 10, 1, growth_sales)) %>% 
    select(province, city, market, atc4, molecule, growth_units, growth_sales)
  
  ## imputing values
  imputing <- month.continuity %>% 
    filter(cnt_max >= 2) %>% 
    left_join(quar.raw, by = c('province', 'city', 'district', 'pchc', 'market')) %>% 
    filter(year %in% c(past, current)) %>% 
    mutate(m = stri_sub(date, 5, 6)) %>% 
    pivot_wider(id_cols = c(year, quarter, date, province, city, district, pchc, 
                            market, atc4, molecule, packid), 
                names_from = year, 
                values_from = c(units, sales), 
                values_fill = -1) %>% 
    filter(!!sym(paste0('units_', past)) > 0, 
           !!sym(paste0('sales_', past)) > 0, 
           !!sym(paste0('units_', current)) == -1, 
           !!sym(paste0('sales_', current)) == -1) %>% 
    left_join(city.growth, by = c('province', 'city', 'market', 'atc4', 'molecule')) %>% 
    mutate(growth_units = ifelse(is.na(growth_units), 1, growth_units), 
           growth_sales = ifelse(is.na(growth_sales), 1, growth_sales), 
           units_imp = !!sym(paste0('units_', past)) * growth_units, 
           sales_imp = !!sym(paste0('sales_', past)) * growth_sales) %>% 
    mutate(year = current, 
           quarter = paste0(current, quar), 
           date = stri_paste(current, stri_sub(date, 5, 6)), 
           flag = 1) %>% 
    select(year, quarter, date, province, city, district, pchc, market, atc4, 
           molecule, packid, units_imp, sales_imp, flag)
  
  ## imputation result
  imputed <- quar.raw %>% 
    # filter(year == current) %>% 
    full_join(imputing, by = c('year', 'quarter', 'date', 'province', 'city', 'district', 
                               'pchc', 'market', 'atc4', 'molecule', 'packid')) %>% 
    mutate(units = ifelse(is.na(units), units_imp, units),
           sales = ifelse(is.na(sales), sales_imp, sales),
           flag = ifelse(is.na(flag), 0, flag)) %>% 
    select(year, quarter, date, province, city, district, pchc, market, atc4, 
           molecule, packid, units, sales, flag)
  
  return(imputed)
}
