#============================================================================
# script6_y16_attraction_to_traps.R
# Last revised: 2018-10-29 / 2022-08-03
#
# Examines wing trap counts comparing meso-dispenser segments and monitoring
# lures. Conducted in the Parlier area in October. Provides mean and SE.
# Univariate t-tests used with the two attractants with non-0 trap capture,
# demonstrating that they are significantly greater than 0 but not significantly
# different.
#
# PARTS
# 0. Declare libraries and functions (line 15)
# 1. Import data (line 28)
# 2. Produce summary statistics (line 60)
# 3. Perform t-tests
# 
#============================================================================


#-- 0. Declare libraries and functions--------------------------------------

library(tidyverse)
library(lubridate)
library(FSA)
#library(DescTools)

#-- 1. Import data ---------------------------------------------------------

dat <- readRDS("data/y16_trap_comparison.Rds")
dat
# # A tibble: 24 × 7
#      Rep Position TrtLabel Trt     Int0      Int2      Count
#    <dbl>    <dbl> <chr>    <chr>   <chr>     <chr>     <dbl>
#  1     1        1 a        Phero   07OCT2016 13OCT2016    16
#  2     1        2 c        Ald     07OCT2016 13OCT2016     0
#  3     1        3 b        Ald+TCP 07OCT2016 13OCT2016     6
#  4     2        1 c        Ald     07OCT2016 13OCT2016     0
#  5     2        2 a        Phero   07OCT2016 13OCT2016     4
#  6     2        3 b        Ald+TCP 07OCT2016 13OCT2016     5
#  7     3        1 b        Ald+TCP 07OCT2016 13OCT2016     2
#  8     3        2 a        Phero   07OCT2016 13OCT2016     3
#  9     3        3 c        Ald     07OCT2016 13OCT2016     0
# 10     4        1 b        Ald+TCP 07OCT2016 13OCT2016     1

#    Int0 and Int2 are constants, can be ignored

#-- 2. Produce summary statistics -------------------------------------------

Means <- dat %>% group_by(Trt) %>%
  summarise(nObs = sum(!is.na(Count)),
            mn = mean(Count),
            sem = se(Count)) 

#Means$meansep <- c("a","b","b")
Means
# A tibble: 3 x 5
# Trt      nObs    mn   sem meansep
# <fct>   <int> <dbl> <dbl> <chr>  
# 1 Phero       8  7    1.87  a      
# 2 Ald+TCP     8  2.88 0.718 a      
# 3 Ald         8  0    0     b      

#-- 3. Plot the data --------------------------------------------------------

# Plots Means as a verical bar chart with error bars 
p <- ggplot(Means, aes(y=mn, x=Trt)) +
  geom_col() +
  geom_errorbar(mapping = aes(ymin = mn, ymax = mn + sem), width = 0.2) +
  #geom_text(data = Means, aes(label=meansep, x = Trt, y = mn, hjust= 0, vjust= - 8),  inherit.aes = FALSE) +
  theme_bw() +
  #scale_x_discrete(labels = c("Phero", "Ald+TCP", "Ald" )) +
  ylab("Adults per trap") +
  theme(axis.title.y=element_text(color = "black", size = 9),
        axis.text.y = element_text(color = "black", size = 9),
        axis.title.x = element_blank()
  )

p

ggsave(filename = "./Results/y16_traps_and_attraction.jpg", p,
       width = 2.83, height = 1.9, dpi = 300, units = "in", device='jpg')


#-- 4. Stats -----------------------------------------

# Is the mean of Ald+TCP different from 0? Yes
t.test(dat$Count[dat$Trt == "Ald+TCP"])
# 
# One Sample t-test
# 
# data:  dat$Count[dat$Trt == "Ald+TCP"]
# t = 4.0038, df = 7, p-value = 0.005165
# alternative hypothesis: true mean is not equal to 0
# 95 percent confidence interval:
#   1.177033 4.572967
#   sample estimates:
#   mean of x 
#   2.875 

t.test(dat$Count[dat$Trt == "Phero"])

# One Sample t-test
# 
# data:  dat$Count[dat$Trt == "Phero"]
# t = 3.7417, df = 7, p-value = 0.007247
# alternative hypothesis: true mean is not equal to 0
# 95 percent confidence interval:
#   2.576193 11.423807
# sample estimates:
#   mean of x 
# 7 

# Is the mean of Ald+TCP different from Phero? No
dat2 <- dat %>% 
  filter(Trt != "Ald")

t.test(Count ~ Trt, data = dat2)
# 
#           Welch Two Sample t-test
# 
# data:  Count by Trt
# t = -2.0585, df = 9.0187, p-value = 0.06959
# alternative hypothesis: true difference in means between group Ald+TCP and group Phero is not equal to 0
# 95 percent confidence interval:
#   -8.6567109  0.4067109
# sample estimates:
#   mean in group Ald+TCP   mean in group Phero 
# 2.875                 7.000 

