#===========================================================================#
# script05a_y16_plot_variability_per3.R
#
# 1. Load data and trim to Period 1 
# 2. Compare control and treatment sums by plot
# 3. Examine only the treatment plots: 2-way ANOVA
# 4. Plot count vs dispenser density
#
#===========================================================================#


library(tidyverse)
library(car)

#---------------------------------------------------------------------------#
#-- 1. Load data and trim to Period 1 ---------------------------------------

all <- readRDS("./data/y16_md_trap_suppression.Rds")
all
# # A tibble: 84 × 16
#     Rep PlotID TrtID TrtLabel TreceID PerAcre DatePlcd  DateRtrvd Lure     P1    P2    P3    P4 Period Plotsum
#   <dbl>  <dbl> <dbl> <chr>      <dbl>   <dbl> <chr>     <chr>     <chr> <dbl> <dbl> <dbl> <dbl>  <dbl>   <dbl>
# 1     1    100     7 G              1       0 21-Sep-16 28-Sep-16 L2H      51    48    38    36      1     173
# 2     2    200     7 G              1       0 21-Sep-16 28-Sep-16 L2H      23    44    52    32      1     151
# 3     3    300     7 G              1       0 21-Sep-16 28-Sep-16 L2H      32    49    66    76      1     223

per3 <- all %>% filter(Period == 3) # from 84 to 28 obs

per3 <- per3 %>% 
  mutate(md_status = ifelse(PerAcre == 0, "ctrl","md")) 

#---------------------------------------------------------------------------#
#-- 2. Compare control and treatment sums by plot ----------------------------

per3%>% 
  group_by(Rep,md_status) %>% 
  summarise(Count = sum(Plotsum)) %>% 
  pivot_wider(names_from = md_status,values_from = Count)
# A tibble: 4 × 3
# # Groups:   Rep [4]
#     Rep  ctrl    md
#   <dbl> <dbl> <dbl>
# 1     1   137    16
# 2     2   167    24
# 3     3   228    42
# 4     4   458    50
### For Period 3, ranks are the same for all 4 plots

#---------------------------------------------------------------------------#
#-- 3. Examine only the treatment plots: 2-way ANOVA -----------------------

# Trim to treatments only
per3_trt <- per3 %>% filter(TrtID < 7) # 24 observations

summary(per3_trt$Plotsum) 
DescTools::Desc(per3_trt$Plotsum)  # Three of 24 zeroes, that's nice

per3_trt <- per3_trt %>% 
  mutate(trans_log = log10(Plotsum + 1))

summary(per3_trt$trans_log)
hist(per3_trt$trans_log)
# Not Gausian, but at least even

# Make factors of independent vars that should be factors
per3_trt$Rep <- factor(per3_trt$Rep, levels = c(1,2,3,4))
per3_trt$TrtLabel <- factor(per3_trt$TrtLabel, levels = c("A","B","C","D","E","F"))

# Fit to two-way ANOVA
model <- lm(trans_log ~ Rep + TrtLabel, data = per3_trt)

summary(model)

plot(model)

par(mfrow = c(2,2))
plot(model)

Anova(model, type = "II")

# Response: trans_log
#            Sum Sq Df F value  Pr(>F)  
# Rep       0.34557  3  0.8021 0.51191  
# TrtLabel  1.63300  5  2.2742 0.09987 .
# Residuals 2.15419 15                  
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#---------------------------------------------------------------------------#
#-- 4. Plot count vs dispenser density... -----------------------------------

# w rep identified, blends separate over/under panels
ggplot(per3_trt, aes(x = as.factor(PerAcre), y = Plotsum, colour = Rep, group = Rep)) +
  geom_point() +
  geom_line() +
  facet_grid(Blend ~ .)

