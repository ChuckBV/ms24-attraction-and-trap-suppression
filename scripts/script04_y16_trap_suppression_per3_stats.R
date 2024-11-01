#============================================================================
# script4_y16_trap_suppression_per3_stats.R
#
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
  filter(Period == 3 & PerHa > 0) %>% 
  select(-c(P1:P4, ))

dat 


#--------------------------------------------------------------------------#
#-- 2. Test factorial GLMM w NB --------------------------------------------

## Restart R

### Full factorial model
library(lme4)
library(MASS)
library(car)
library(emmeans)

dat$PerHa <- factor(dat$PerHa, levels = unique(dat$PerHa))

model <- glmer.nb(Plotsum ~ Blend * PerHa + (1 | Rep), data = dat)

summary(model)
# Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
# Family: Negative Binomial(1.5808)  ( log )
# Formula: Plotsum ~ Blend * PerHa + (1 | Rep)
# Data: dat
# 
# AIC      BIC   logLik deviance df.resid 
# 137.2    146.6    -60.6    121.2       16 
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -1.0959 -0.7756 -0.2797  0.4846  2.4111 
# 
# Random effects:
#   Groups Name        Variance  Std.Dev. 
# Rep    (Intercept) 1.218e-12 1.104e-06
# Number of obs: 24, groups:  Rep, 4
# 
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)            2.1691     0.4321   5.020 5.17e-07 ***
#   BlendAld+TCP           0.3365     0.6044   0.557   0.5777    
# PerHa30               -0.5596     0.6284  -0.891   0.3732    
# PerHa69               -1.3581     0.6753  -2.011   0.0443 *  
#   BlendAld+TCP:PerHa30  -0.6242     0.8934  -0.699   0.4848    
# BlendAld+TCP:PerHa69  -1.1474     1.0211  -1.124   0.2611    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#   (Intr) BlA+TCP PerH30 PerH69 BA+TCP:PH3
# BlndAld+TCP -0.715                                 
# PerHa30     -0.688  0.492                          
# PerHa69     -0.640  0.458   0.440                  
# BA+TCP:PH30  0.484 -0.676  -0.703 -0.309           
# BA+TCP:PH69  0.423 -0.592  -0.291 -0.661  0.400    
# optimizer (Nelder_Mead) convergence code: 0 (OK)
# boundary (singular) fit: see help('isSingular')

anova_result <- Anova(model, type = "II")
print(anova_result)
# Analysis of Deviance Table (Type II Wald chisquare tests)
# 
# Response: Plotsum
# Chisq Df Pr(>Chisq)    
# Blend        0.1356  1  0.7127346    
# PerHa       13.9174  2  0.0009503 ***
# Blend:PerHa  1.3363  2  0.5126509    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#----------------------------------------------------------------------------#
#-- 3. 1-way ANOVA for Aldehyde only -----------------------------------------

### Model for Ald only
ald_only <- dat[dat$Blend == "Ald",]

m2 <- glmer.nb(Plotsum ~ PerHa + (1 | Rep), data = ald_only)

summary(m2)

anova_result2 <- Anova(m2, type = "II")
print(anova_result2)
# Analysis of Deviance Table (Type II Wald chisquare tests)
# 
# Response: Plotsum
#        Chisq Df Pr(>Chisq)
# PerHa 3.1926  2     0.2026

# Results are given on the log (not the response) scale. 
# P value adjustment: tukey method for comparing a family of 3 estimates 


#----------------------------------------------------------------------------#
#-- 4. 1-way ANOVA for Aldehyde + TCP ----------------------------------------

### Model for Ald + TCP
ald_tcp <- dat[dat$Blend == "Ald+TCP",]

m3 <- glmer.nb(Plotsum ~ PerHa + (1 | Rep), data = ald_tcp)

summary(m3)
# Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
# Family: Negative Binomial(3.2214)  ( log )
# Formula: Plotsum ~ PerHa + (1 | Rep)
# Data: ald_tcp
# 
# AIC      BIC   logLik deviance df.resid 
# 66.5     68.9    -28.2     56.5        7 
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -1.2060 -0.8965 -0.0163  0.6193  1.7471 
# 
# Random effects:
#   Groups Name        Variance Std.Dev.
# Rep    (Intercept) 0        0       
# Number of obs: 12, groups:  Rep, 4
# 
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)   2.5055     0.3131   8.003 1.21e-15 ***
#   PerHa30      -1.1838     0.4922  -2.405 0.016176 *  
#   PerHa69      -2.5055     0.6524  -3.840 0.000123 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#   (Intr) PerH30
# PerHa30 -0.636       
# PerHa69 -0.480  0.305
# optimizer (Nelder_Mead) convergence code: 0 (OK)
# boundary (singular) fit: see help('isSingular')

anova_result3 <- Anova(m3, type = "II")
print(anova_result3)
# Analysis of Deviance Table (Type II Wald chisquare tests)
# 
# Response: Plotsum
# Chisq Df Pr(>Chisq)    
# PerHa 16.425  2  0.0002712 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# > # Means separation using emmeans
#   > emmeans_nb3 <- emmeans(m3, "PerHa")
# > pairs(emmeans_nb3)
# contrast          estimate    SE  df z.ratio p.value
# PerHa17 - PerHa30     1.18 0.492 Inf   2.405  0.0427
# PerHa17 - PerHa69     2.51 0.652 Inf   3.840  0.0004
# PerHa30 - PerHa69     1.32 0.687 Inf   1.924  0.1319
# 
# Results are given on the log (not the response) scale. 
# P value adjustment: tukey method for comparing a family of 3 estimates 

