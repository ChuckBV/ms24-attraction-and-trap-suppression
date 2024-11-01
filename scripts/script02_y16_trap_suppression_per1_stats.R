#============================================================================
# script2_y16_trap_suppression_per1_stats.R
#
# Analysis of the factorial model reveals a significant interaction beteen
# blend and dispenser density (Wald chisq = 8.898, df = 2; P = 0.0117)
#
# 1-way ANOVA of Ald only found signficant differences (Wald chisqr = 48.758,
# df = 2; P < 0.0001); sep was a,b,b
#
# 1-way ANOVA of Ald + TCP found signficant differences (Wald chisqr = 76.664,
# df = 2; P < 0.0001); sep was a,b,c
#
# PARTS
# 1. Read data as found in the SAS scrip (line 117)
# 2. Test factorial GLMM w NB (line 38)
# 3. 1-way ANOVA for Aldehyde only (line 103)
# 4. Perform non-parametric ANOVA (line 94)
# 
#============================================================================

library(tidyverse)

#---------------------------------------------------------------------------#
#-- 1. Read data as found in the SAS script ---------------------------------

dat <- readRDS("./data/y16_md_trap_suppression.Rds")

# Get mean and SE by group 
dat <- dat %>% 
  mutate(PerHa = case_when(PerAcre == 0 ~ 0,
                           PerAcre == 7 ~ 17,
                           PerAcre == 12 ~ 30,
                           .default = 69)) %>% 
  filter(Period == 1 & PerHa > 0) %>% 
  select(-c(P1:P4, ))

dat 

#--------------------------------------------------------------------------#
#-- 2. Test factorial GLMM w NB --------------------------------------------

## Restart R

### Full factorial model
library(lme4)
library(MASS)
library(car)
install.packages("emmeans") 
library(emmeans)

dat$PerHa <- factor(dat$PerHa, levels = unique(dat$PerHa))

model <- lme4::glmer.nb(Plotsum ~ Blend * PerHa + (1 | Rep), data = dat)

summary(model)
# Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
#  Family: Negative Binomial(12.9411)  ( log )
# Formula: Plotsum ~ Blend * PerHa + (1 | Rep)
#    Data: dat
# 
# AIC      BIC   logLik deviance df.resid 
# 182.2    191.6    -83.1    166.2       16 
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -1.2651 -0.7052 -0.1679  0.3344  2.0406 
# 
# Random effects:
#   Groups Name        Variance Std.Dev.
#   Rep    (Intercept) 0.2231   0.4724  
# Number of obs: 24, groups:  Rep, 4
# 
# Fixed effects:
#                      Estimate Std. Error z value Pr(>|z|)    
# (Intercept)            3.9192     0.2835  13.822  < 2e-16 ***
# BlendAld+TCP           0.1237     0.2201   0.562  0.57413    
# PerHa30               -0.4855     0.2274  -2.135  0.03279 *  
# PerHa69               -2.1005     0.2883  -7.285 3.21e-13 ***
# BlendAld+TCP:PerHa30  -0.9092     0.3388  -2.684  0.00728 ** 
# BlendAld+TCP:PerHa69  -0.9490     0.4607  -2.060  0.03939 *  
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#             (Intr) BlA+TCP PerH30 PerH69 BA+TCP:PH3
# BlndAld+TCP -0.392                                 
# PerHa30     -0.375  0.486                          
# PerHa69     -0.297  0.381   0.370                  
# BA+TCP:PH30  0.251 -0.650  -0.674 -0.243           
# BA+TCP:PH69  0.188 -0.477  -0.229 -0.623  0.306    

car::anova_result <- Anova(model, type = "II")
print(anova_result)
# Analysis of Deviance Table (Type II Wald chisquare tests)
# 
# Response: Plotsum
#                Chisq Df Pr(>Chisq)    
# Blend         4.9775  1    0.02568 *  
# PerHa       127.1350  2    < 2e-16 ***
# Blend:PerHa   8.8980  2    0.01169 *  
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#----------------------------------------------------------------------------#
#-- 3. 1-way ANOVA for Aldehyde only -----------------------------------------

### Model for Ald only
ald_only <- dat[dat$Blend == "Ald",]

m2 <- lme4::glmer.nb(Plotsum ~ PerHa + (1 | Rep), data = ald_only)

summary(m2)

anova_result2 <- Anova(m2, type = "II")
print(anova_result2)
# Analysis of Deviance Table (Type II Wald chisquare tests)
# 
# Response: Plotsum
#        Chisq Df Pr(>Chisq)    
# PerHa 48.758  2  2.584e-11 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Means separation using emmeans
emmeans_nb2 <- emmeans(m2, "PerHa")
# pairs(emmeans_nb2)
# contrast          estimate    SE  df z.ratio p.value
# PerHa17 - PerHa30    0.504 0.245 Inf   2.058  0.0988
# PerHa17 - PerHa69    2.115 0.304 Inf   6.956  <.0001
# PerHa30 - PerHa69    1.611 0.310 Inf   5.194  <.0001
# 
# Results are given on the log (not the response) scale. 
# P value adjustment: tukey method for comparing a family of 3 estimates 


#----------------------------------------------------------------------------#
#-- 4. 1-way ANOVA for Aldehyde + TCP ----------------------------------------

### Model for Ald + TCP
ald_tcp <- dat[dat$Blend == "Ald+TCP",]

m3 <- glmer.nb(Plotsum ~ PerHa + (1 | Rep), data = ald_tcp)

summary(m3)

anova_result3 <- Anova(m3, type = "II")
print(anova_result3)
# Analysis of Deviance Table (Type II Wald chisquare tests)
# 
# Response: Plotsum
# Chisq Df Pr(>Chisq)    
# PerHa 76.664  2  < 2.2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Means separation using emmeans
emmeans_nb3 <- emmeans(m3, "PerHa")
pairs(emmeans_nb3)
# contrast          estimate    SE  df z.ratio p.value
# PerHa17 - PerHa30     1.42 0.266 Inf   5.329  <.0001
# PerHa17 - PerHa69     3.02 0.369 Inf   8.196  <.0001
# PerHa30 - PerHa69     1.61 0.387 Inf   4.147  0.0001
# 
# Results are given on the log (not the response) scale. 
# P value adjustment: tukey method for comparing a family of 3 estimates 

