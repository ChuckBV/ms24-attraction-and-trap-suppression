#===========================================================================#
# script03a_y16_plot_variability_per1.R
#
# 1. Load data and trim to Period 1 
# 2. Compare control and treatment sums by plot
# 3. Examine only the treatment plots: 2-way ANOVA
# 4. Plot count vs dispenser density
#
#===========================================================================#


library(tidyverse)
#library(car)

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

per1 <- all %>% filter(Period == 1) # from 84 to 28 obs

per1 <- per1 %>% 
  mutate(md_status = ifelse(PerAcre == 0, "ctrl","md")) 

#---------------------------------------------------------------------------#
#-- 2. Compare control and treatment sums by plot ----------------------------

per1%>% 
  group_by(Rep,md_status) %>% 
  summarise(Count = sum(Plotsum)) %>% 
  pivot_wider(names_from = md_status,values_from = Count)
# # A tibble: 4 × 3
# # Groups:   Rep [4]
#     Rep  ctrl    md
#   <dbl> <dbl> <dbl>
# 1     1   173   185
# 2     2   151   156
# 3     3   223   311
# 4     4   495    73

### Plots 1 to 3 were in the same 160 acre block, plot 4 was in a different
### plot. The rank level of control and treatment was the same in plots
### 1 to 3, but different in plot 4.

#---------------------------------------------------------------------------#
#-- 3. Examine only the treatment plots: 2-way ANOVA -----------------------

# Trim to treatments only
per1_trt <- per1 %>% filter(TrtID < 7) # 24 observations

summary(per1_trt$Plotsum)  # No zeroes, that's nice
hist(per1_trt$Plotsum)

per1_trt <- per1_trt %>% 
  mutate(trans_sqrt = sqrt(Plotsum),
         trans_log = log10(Plotsum + 1))

summary(per1_trt$trans_sqrt)
hist(per1_trt$trans_sqrt)

summary(per1_trt$trans_log)
hist(per1_trt$trans_log)
# Log seems to give nearest to a Gaussian frequency distribution

# Make factors of independent vars that should be factors
per1_trt$Rep <- factor(per1_trt$Rep, levels = c(1,2,3,4))
per1_trt$TrtLabel <- factor(per1_trt$TrtLabel, levels = c("A","B","C","D","E","F"))

# Fit to two-way ANOVA
model <- lm(trans_log ~ Rep + TrtLabel, data = per1_trt)

summary(model)

plot(model)

par(mfrow = c(2,2))
plot(model)

Anova(model, type = "II")

# Anova Table (Type II tests)
# 
# Response: trans_log
#           Sum Sq Df F value    Pr(>F)    
# Rep       0.8541  3  9.6035 0.0008737 ***
# TrtLabel  4.6934  5 31.6623 1.848e-07 ***
# Residuals 0.4447 15                      
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#---------------------------------------------------------------------------#
#-- 4. Plot count vs dispenser density... -----------------------------------

# w rep identified, blends separate over/under panels
ggplot(per1_trt, aes(x = as.factor(PerAcre), y = Plotsum, colour = Rep, group = Rep)) +
  geom_point() +
  geom_line() +
  facet_grid(Blend ~ .)

### Isolate and test by dispenser density

dens_07 <- per1_trt %>% 
  filter(PerAcre == 7)

t.test(Plotsum ~ Blend, data = dens_07)

dens_12 <- per1_trt %>% 
  filter(PerAcre == 12)

t.test(Plotsum ~ Blend, data = dens_12)

dens_28 <- per1_trt %>% 
  filter(PerAcre == 28)

t.test(Plotsum ~ Blend, data = dens_28)

#-- ns across the board