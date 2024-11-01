#============================================================================
# script8_y19_nonmd_stats.R
#
#
#============================================================================

## Restart R

### Full factorial model
library(lme4)
library(MASS)
library(car)
library(emmeans)

# Load data
dat <- readRDS("./data/y19_nonmd_trapsums.Rds")
#     NB lure and row both are and ought to be factors

# run model
model <- glmer.nb(Sum ~ lure + (1 | row), data = dat)

summary(model)
# Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
#  Family: Negative Binomial(4.3046)  ( log )
# Formula: Sum ~ lure + (1 | row)
#    Data: dat
# 
# AIC      BIC   logLik deviance   df.resid 
# 486.5    508.1   -233.3    466.5       54 
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -1.4830 -0.6600 -0.2596  0.5087  3.1277 
# 
# Random effects:
#   Groups Name        Variance Std.Dev.
# row    (Intercept) 0.03539  0.1881  
# Number of obs: 64, groups:  row, 8
# 
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)           5.5984     0.1847  30.303  < 2e-16 ***
# lureAldTCP_12in      -0.7717     0.2455  -3.144  0.00167 ** 
# lureAldTCP_4in       -1.1558     0.2487  -4.647 3.36e-06 ***
# lureAldTCP_1in       -1.1100     0.2495  -4.449 8.61e-06 ***
# lureCidetrakNOW_8in  -4.5660     0.3214 -14.206  < 2e-16 ***
# lureCidetrakNOW_4in  -4.3210     0.3063 -14.106  < 2e-16 ***
# lureCidetrakNOW_1in  -4.5253     0.3193 -14.173  < 2e-16 ***
# lureBlankCtrl        -7.6917     1.0294  -7.472 7.89e-14 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#             (Intr) lATCP_12 lATCP_4 lrATCP_1 lCNOW_8 lCNOW_4 lCNOW_1
# lrAldTCP_12 -0.656                                                  
# lrAldTCP_4n -0.654  0.497                                           
# lrAldTCP_1n -0.648  0.489    0.501                                  
# lrCdtrNOW_8 -0.503  0.383    0.385   0.382                          
# lrCdtrNOW_4 -0.526  0.396    0.398   0.398    0.305                 
# lrCdtrNOW_1 -0.506  0.385    0.389   0.381    0.296   0.305         
# lurBlnkCtrl -0.157  0.119    0.119   0.119    0.091   0.095   0.092 

anova_result <- Anova(model, type = "II")
print(anova_result)
# Analysis of Deviance Table (Type II Wald chisquare tests)
# 
# Response: Sum
#       Chisq Df Pr(>Chisq)    
# lure 513.67  7  < 2.2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

emmeans_nb <- emmeans(model, "lure")
pairs(emmeans_nb)
# contrast                          estimate    SE  df z.ratio p.value
# NowBiolure - AldTCP_12in            0.7717 0.245 Inf   3.144  0.0356
# NowBiolure - AldTCP_4in             1.1558 0.249 Inf   4.647  0.0001
# NowBiolure - AldTCP_1in             1.1100 0.249 Inf   4.449  0.0002
# NowBiolure - CidetrakNOW_8in        4.5660 0.321 Inf  14.206  <.0001
# NowBiolure - CidetrakNOW_4in        4.3210 0.306 Inf  14.106  <.0001
# NowBiolure - CidetrakNOW_1in        4.5253 0.319 Inf  14.173  <.0001
# NowBiolure - BlankCtrl              7.6917 1.029 Inf   7.472  <.0001
# AldTCP_12in - AldTCP_4in            0.3841 0.248 Inf   1.550  0.7803
# AldTCP_12in - AldTCP_1in            0.3383 0.250 Inf   1.352  0.8788
# AldTCP_12in - CidetrakNOW_8in       3.7943 0.321 Inf  11.817  <.0001
# AldTCP_12in - CidetrakNOW_4in       3.5493 0.308 Inf  11.541  <.0001
# AldTCP_12in - CidetrakNOW_1in       3.7536 0.319 Inf  11.758  <.0001
# AldTCP_12in - BlankCtrl             6.9200 1.030 Inf   6.722  <.0001
# AldTCP_4in - AldTCP_1in            -0.0458 0.249 Inf  -0.184  1.0000
# AldTCP_4in - CidetrakNOW_8in        3.4102 0.322 Inf  10.598  <.0001
# AldTCP_4in - CidetrakNOW_4in        3.1652 0.308 Inf  10.264  <.0001
# AldTCP_4in - CidetrakNOW_1in        3.3694 0.319 Inf  10.549  <.0001
# AldTCP_4in - BlankCtrl              6.5359 1.030 Inf   6.347  <.0001
# AldTCP_1in - CidetrakNOW_8in        3.4560 0.323 Inf  10.701  <.0001
# AldTCP_1in - CidetrakNOW_4in        3.2110 0.309 Inf  10.406  <.0001
# AldTCP_1in - CidetrakNOW_1in        3.4152 0.322 Inf  10.619  <.0001
# AldTCP_1in - BlankCtrl              6.5817 1.030 Inf   6.390  <.0001
# CidetrakNOW_8in - CidetrakNOW_4in  -0.2450 0.370 Inf  -0.662  0.9979
# CidetrakNOW_8in - CidetrakNOW_1in  -0.0408 0.380 Inf  -0.107  1.0000
# CidetrakNOW_8in - BlankCtrl         3.1257 1.050 Inf   2.977  0.0584
# CidetrakNOW_4in - CidetrakNOW_1in   0.2042 0.369 Inf   0.554  0.9993
# CidetrakNOW_4in - BlankCtrl         3.3707 1.046 Inf   3.223  0.0278
# CidetrakNOW_1in - BlankCtrl         3.1665 1.049 Inf   3.018  0.0519

# Results are given on the log (not the response) scale. 
# P value adjustment: tukey method for comparing a family of 8 estimates 