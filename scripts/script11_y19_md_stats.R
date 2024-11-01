#============================================================================
# script8_y19_nonmd_stats.R
#
#
#============================================================================

## Restart R

### Full factorial model
library(lme4)
#library(MASS)
library(car)
library(emmeans)

# Load data
dat <- readRDS("./data/y19_md_trapsums.Rds")
#     NB lure and row both are and ought to be factors
head(dat)
#         lure row Sum
# 1 NowBiolure   1   0
# 2 NowBiolure   2   0
# 3 NowBiolure   3   0
# 4 NowBiolure   4   0
# 5 NowBiolure   5   1
# 6 NowBiolure   6   0

unique(dat$lure)
# [1] NowBiolure      CidetrakNOW_1in CidetrakNOW_4in CidetrakNOW_8in AldTCP_1in      AldTCP_4in     
# [7] AldTCP_12in     BiolurePpo     
# 8 Levels: NowBiolure CidetrakNOW_1in CidetrakNOW_4in CidetrakNOW_8in AldTCP_1in AldTCP_4in ... BiolurePpo

# run model
model <- glmer.nb(Sum ~ lure + (1 | row), data = dat)

summary(model)
# Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
# Family: Negative Binomial(1.8913)  ( log )
# Formula: Sum ~ lure + (1 | row)
# Data: dat
# 
# AIC      BIC   logLik deviance df.resid 
# 397.8    419.4   -188.9    377.8       54 
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -1.3344 -0.5619 -0.2922  0.5422  2.9075 
# 
# Random effects:
# Groups Name        Variance Std.Dev.
# row    (Intercept) 0.02439  0.1562  
# Number of obs: 64, groups:  row, 8
# 
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)           -2.095      1.035  -2.024  0.04295 *  
# lureCidetrakNOW_1in    1.386      1.176   1.179  0.23842    
# lureCidetrakNOW_4in    3.362      1.081   3.111  0.00186 ** 
# lureCidetrakNOW_8in    3.169      1.084   2.924  0.00346 ** 
# lureAldTCP_1in         4.805      1.070   4.489 7.15e-06 ***
# lureAldTCP_4in         5.917      1.066   5.552 2.83e-08 ***
# lureAldTCP_12in        5.446      1.067   5.106 3.29e-07 ***
# lureBiolurePpo         6.025      1.067   5.645 1.65e-08 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
# (Intr) lCNOW_1 lCNOW_4 lCNOW_8 lrATCP_1 lATCP_4 lATCP_12
# lrCdtrNOW_1 -0.876                                                  
# lrCdtrNOW_4 -0.953  0.839                                           
# lrCdtrNOW_8 -0.950  0.837   0.910                                   
# lrAldTCP_1n -0.960  0.847   0.922   0.920                           
# lrAldTCP_4n -0.967  0.851   0.926   0.923   0.934                   
# lrAldTCP_12 -0.967  0.850   0.924   0.922   0.932    0.938          
# lureBiolrPp -0.968  0.850   0.924   0.921   0.929    0.938   0.938  

anova_result <- Anova(model, type = "II")
print(anova_result)
# Analysis of Deviance Table (Type II Wald chisquare tests)
# 
# Response: Sum
#       Chisq Df Pr(>Chisq)    
# lure 146.84  7  < 2.2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

emmeans_nb <- emmeans(model, "lure")
pairs(emmeans_nb)
# contrast                          estimate    SE  df z.ratio p.value
# NowBiolure - CidetrakNOW_1in        -1.386 1.176 Inf  -1.179  0.9381
# NowBiolure - CidetrakNOW_4in        -3.362 1.081 Inf  -3.111  0.0393
# NowBiolure - CidetrakNOW_8in        -3.169 1.084 Inf  -2.924  0.0679
# NowBiolure - AldTCP_1in             -4.805 1.070 Inf  -4.489  0.0002
# NowBiolure - AldTCP_4in             -5.917 1.066 Inf  -5.552  <.0001
# NowBiolure - AldTCP_12in            -5.446 1.067 Inf  -5.106  <.0001
# NowBiolure - BiolurePpo             -6.025 1.067 Inf  -5.645  <.0001
# CidetrakNOW_1in - CidetrakNOW_4in   -1.976 0.646 Inf  -3.057  0.0464
# CidetrakNOW_1in - CidetrakNOW_8in   -1.782 0.652 Inf  -2.735  0.1121
# CidetrakNOW_1in - AldTCP_1in        -3.419 0.630 Inf  -5.426  <.0001
# CidetrakNOW_1in - AldTCP_4in        -4.530 0.621 Inf  -7.294  <.0001
# CidetrakNOW_1in - AldTCP_12in       -4.060 0.623 Inf  -6.520  <.0001
# CidetrakNOW_1in - BiolurePpo        -4.639 0.624 Inf  -7.438  <.0001
# CidetrakNOW_4in - CidetrakNOW_8in    0.194 0.458 Inf   0.423  0.9999
# CidetrakNOW_4in - AldTCP_1in        -1.443 0.424 Inf  -3.400  0.0155
# CidetrakNOW_4in - AldTCP_4in        -2.554 0.413 Inf  -6.185  <.0001
# CidetrakNOW_4in - AldTCP_12in       -2.084 0.418 Inf  -4.990  <.0001
# CidetrakNOW_4in - BiolurePpo        -2.663 0.419 Inf  -6.362  <.0001
# CidetrakNOW_8in - AldTCP_1in        -1.637 0.431 Inf  -3.794  0.0037
# CidetrakNOW_8in - AldTCP_4in        -2.748 0.422 Inf  -6.518  <.0001
# CidetrakNOW_8in - AldTCP_12in       -2.278 0.425 Inf  -5.366  <.0001
# CidetrakNOW_8in - BiolurePpo        -2.857 0.428 Inf  -6.673  <.0001
# AldTCP_1in - AldTCP_4in             -1.111 0.387 Inf  -2.871  0.0785
# AldTCP_1in - AldTCP_12in            -0.641 0.393 Inf  -1.630  0.7320
# AldTCP_1in - BiolurePpo             -1.220 0.403 Inf  -3.029  0.0503
# AldTCP_4in - AldTCP_12in             0.470 0.376 Inf   1.251  0.9166
# AldTCP_4in - BiolurePpo             -0.109 0.377 Inf  -0.289  1.0000
# AldTCP_12in - BiolurePpo            -0.579 0.377 Inf  -1.536  0.7881
# 
# Results are given on the log (not the response) scale. 
# P value adjustment: tukey method for comparing a family of 8 estimates