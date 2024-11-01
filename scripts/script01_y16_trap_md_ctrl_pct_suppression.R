#============================================================================
# script1_y16_trap_md_ctrl_pct_suppression.R
#
# PARTS
# 1. Import data (line 28)
# 2. MD (all) vs control (line 46)
# 3. Kruskal test ctrl vs md by period (line 92)
# 
#============================================================================

library(tidyverse) 
library(FSA)

#---------------------------------------------------------------------------#
#-- 1. Import data ----------------------------------------------------------
all <- readRDS("./data/y16_md_trap_suppression.Rds")
all
# A tibble: 84 × 16
#     Rep PlotID TrtID TrtLabel TreceID PerAcre DatePlcd  DateRtrvd Lure     P1    P2    P3    P4 Period Plotsum Blend
#   <dbl>  <dbl> <dbl> <chr>      <dbl>   <dbl> <chr>     <chr>     <chr> <dbl> <dbl> <dbl> <dbl>  <dbl>   <dbl> <chr>
# 1     1    100     7 G              1       0 21-Sep-16 28-Sep-16 L2H      51    48    38    36      1     173 None 
# 2     2    200     7 G              1       0 21-Sep-16 28-Sep-16 L2H      23    44    52    32      1     151 None 
# 3     3    300     7 G              1       0 21-Sep-16 28-Sep-16 L2H      32    49    66    76      1     223 None 

### Note that one replicate block went one day longer than the others in
### the second monitoring period. Since this was done by replicate block,
### and since it turns out that zero males were captured on 21 of 24 plots
### in the second monitoring period (female baited), this is deemed a 
### non-issue.

### Drop unneeded variable
all <- select(all, -DatePlcd, -DateRtrvd, -P1, -P2,-P3,-P4) 

### Display data in revised form
all
# A tibble: 84 × 10
#     Rep PlotID TrtID TrtLabel TreceID PerAcre Lure  Period Plotsum Blend
#   <dbl>  <dbl> <dbl> <chr>      <dbl>   <dbl> <chr>  <dbl>   <dbl> <chr>
# 1     1    100     7 G              1       0 L2H        1     173 None 
# 2     2    200     7 G              1       0 L2H        1     151 None 

### Save data in cleaned form
write.csv(all, "./Data/intermediate/Suppression_cleaned.csv", row.names = FALSE)

#---------------------------------------------------------------------------#
#--2. MD (all) vs control ---------------------------------------------------

### Compare numbers between the 1 non-MD (reference) plot and the six
### MD plots per rep (Ald and Alt+TCP)*(7, 12, and 28 dispensers/acre)

## Create a variable for mating disruption
all <- all %>% 
  mutate(MdStatus = ifelse(PerAcre == 0, "Ctrl", "MD"))

## Set this as a factor
all$MdStatus <- as.factor(all$MdStatus)

## Get mean of Ctrl and MD by period (three periods: phero1, fem, phero2)
## Per period, Ctrl is n = 1 and MD is n = 6. Gives overall view of
## level of moth abundance and suppression for the three monitoring
## periods.

Md_per <- all %>% 
  group_by(Period, MdStatus) %>%
  summarise(n = sum(!is.na(Plotsum)),
            mean = mean(Plotsum, na.rm = TRUE), 
            se = FSA::se(Plotsum),
            prop0 = sum(Plotsum == 0)/n)

#----- Table of mean, SE, and per cent zero

Md_per
# A tibble: 6 × 6
# Groups:   Period [3]
# Period MdStatus     n    mean     se prop0
#    <dbl> <fct>    <int>   <dbl>  <dbl> <dbl>
# 1      1 Ctrl         4 260.    79.6   0    
# 2      1 MD          24  30.2    6.46  0    
# 3      2 Ctrl         4  63     31.3   0    
# 4      2 MD          24   0.208  0.134 0.875
# 5      3 Ctrl         4 248.    72.7   0    
# 6      3 MD          24   5.5    1.30  0.125

### Trap suppresion, Period 1
100*(1 - 30.2/260) #88.3%
### Trap suppresion, Period 2
100*(1 - 0.208/63) #99.7%
### Trap suppresion, Period 1
100*(1 - 5.5/248) #97.8%

#---------------------------------------------------------------------------#
#--3. Kruskal test ctrl vs md by period -------------------------------------

### Statistical sig, Period 1
per1 <- all %>% 
  filter(Period == 1)

kruskal.test(per1$Plotsum,per1$MdStatus)
# 
# Kruskal-Wallis rank sum test
# 
# data:  per1$Plotsum and per1$MdStatus
# Kruskal-Wallis chi-squared = 9.9501, df = 1, p-value = 0.001608

### Statistical sig, Period 2
per2 <- all %>% 
  filter(Period == 2)

kruskal.test(per2$Plotsum,per2$MdStatus)
# 
# Kruskal-Wallis rank sum test
# 
# data:  per2$Plotsum and per2$MdStatus
# Kruskal-Wallis chi-squared = 17.174, df = 1, p-value = 3.411e-05


### Statistical sig, Period 3
per3 <- all %>% 
  filter(Period == 3)

kruskal.test(per3$Plotsum,per3$MdStatus)
# 
# Kruskal-Wallis rank sum test
# 
# data:  per3$Plotsum and per3$MdStatus
# Kruskal-Wallis chi-squared = 10.063, df = 1, p-value = 0.001513


all <- readRDS("./data/y16_md_trap_suppression.Rds")
