#============================================================================
# script7_y19_nonmd.R
#
# Read and graph data from an August 2019 experiment comparing NOW trapping
# with sections of MESO (mating disruption) dispensers with Ald only, or
# with Ald and TCP. This is the non-MD conducted at the Mike Woolf I-5
# pistachio orchard.
#
# 1. Read in data, show monitoring intervals (line 21)
# 2. Numerical means and SE in table (line 41)
# 3. Box plot of treatment effects (line 82)
# 4. Stats--Kruskal-Wallis, and... (line 107)
#
#============================================================================

library(tidyverse)
library(lubridate)
library(DescTools)
library(scales)
library(FSA) # for SE function and Dunn Test

#-- 1. Read in data -----------------------------------------

### Read in data file and convert date data types
mwoolf <- read_csv("./data/y19-mike-woolf-meso-lures-data.csv")

# convert from datetime
mwoolf$StartDate <- as.Date(mdy(mwoolf$StartDate))
mwoolf$EndDate <- as.Date(mdy(mwoolf$EndDate))

# view data set
head(mwoolf,3)
# A tibble: 3 x 7
#     row   pos lure        StartDate  EndDate    trap_id Count
#   <dbl> <dbl> <chr>       <date>     <date>       <dbl> <dbl>
# 1     1     1 AldTCP_4in  2019-08-02 2019-08-05      11     9
# 2     1     2 AldTCP_12in 2019-08-02 2019-08-05      12    22
# 3     1     3 BlankCtrl   2019-08-02 2019-08-05      13     0

### Treatment names as factors in desired order
trt_names <- c("BlankCtrl","CidetrakNOW_1in","CidetrakNOW_4in","CidetrakNOW_8in","AldTCP_1in","AldTCP_4in","AldTCP_12in","NowBiolure")   

### Specify factors
mwoolf <- mutate(mwoolf,lure = factor(lure, levels=trt_names))
mwoolf$row <- as.factor(mwoolf$row)

### Examine by monitoring interval
mwoolf %>% 
  group_by(StartDate,EndDate) %>% 
  summarise(nObs = sum(!is.na(Count)))
# A tibble: 4 x 3
#   Groups:   StartDate [4]
#   StartDate  EndDate     nObs
#   <date>     <date>     <int>
# 1 2019-08-02 2019-08-05    63
# 2 2019-08-05 2019-08-13    64
# 3 2019-08-13 2019-08-21    64
# 4 2019-08-21 2019-08-27    64

#-- 2. Numerical means and SE in table --------------------------------------
exp_unit_means <- mwoolf %>%
  group_by(lure,row) %>%
  summarise(Count = mean(Count, na.rm = TRUE))

trt_means <- exp_unit_means %>%
  group_by(lure) %>%
  summarise(nObs = sum(!is.na(Count)),
            mn = mean(Count, na.rm = TRUE),
            sem = se(Count))
trt_means
# A tibble: 8 x 4
# lure             nObs      mn    sem
# <fct>           <int>   <dbl>  <dbl>
# 1 BlankCtrl           8  0.0312 0.0312
# 2 CidetrakNOW_1in     8  0.75   0.341 
# 3 CidetrakNOW_4in     8  0.906  0.194 
# 4 CidetrakNOW_8in     8  0.719  0.173 
# 5 AldTCP_1in          8 23.4    6.56  
# 6 AldTCP_4in          8 22.1    3.87  
# 7 AldTCP_12in         8 31.9    5.34  
# 8 NowBiolure          8 70.9    7.92 

write.csv(trt_means,"./results/y16_nonmd_trt_means_se.csv", row.names = FALSE)

#-- 3. ggplot of adults trapped by treatment (not used) --------

p2 <- ggplot(trt_means, aes(x = lure, y = mn)) +
  geom_col() +
  geom_errorbar(mapping = aes(ymin = mn, ymax = mn + sem)) +
  theme_bw() +
  xlab("") +
  ylab("NOW per trap per week") +
  #scale_y_continuous(trans = log2_trans())+#,
  #                     breaks = trans_breaks("log2", function(x) 2^x),
  #                     labels = trans_format("log2", math_format(2^.x))) +
  theme(axis.text.x = element_text(color = "black", size = 7, angle = 45, hjust = 1),
        axis.text.y = element_text(color = "black", size = 8),
        axis.title.x = element_text(color = "black", size = 9),
        axis.title.y = element_text(color = "black", size = 9),
        legend.title = element_text(color = "black", size = 14),
        legend.text = element_text(color = "black", size = 14))

p2

# ggsave(filename = "y19-mike-woolf-mesos-overall.jpg", p2,
#        path = "./results/",
#        width = 5.83, height = 3.91, dpi = 300, units = "in", device='jpg')

#-- 4. stats ------------------------------------------

# Pools monitoring intervals (sum rather than mean for analysis)
exp_unit_sums <- mwoolf %>%
  group_by(lure,row) %>%
  summarise(Sum = sum(Count, na.rm = TRUE))

# output for use statistical analysis
# write.csv(exp_unit_sums,"./data/y19_nonmd_trapsums.csv", row.names = FALSE)
saveRDS(exp_unit_sums,"./data/y19_nonmd_trapsums.Rds")

# will want descending order for posthoc test
# exp_unit_sums$lure <- factor(exp_unit_sums$lure, levels = rev(exp_unit_sums$lure))
trt_names2 <- c("NowBiolure","AldTCP_12in","AldTCP_4in","AldTCP_1in","CidetrakNOW_8in","CidetrakNOW_4in","CidetrakNOW_1in","BlankCtrl")   

exp_unit_sums <- mutate(exp_unit_sums,lure = factor(lure, levels=trt_names2))
levels(exp_unit_sums$lure)
# [1] "NowBiolure"      "AldTCP_12in"     "AldTCP_4in"      "AldTCP_1in"      "CidetrakNOW_8in" "CidetrakNOW_4in" "CidetrakNOW_1in" "BlankCtrl"      

# Use Desc to obtain Kruskall-Wallis test
Desc(Sum ~ lure, data = exp_unit_sums)

# Kruskal-Wallis rank sum test:
#   Kruskal-Wallis chi-squared = 54.595, df = 7, p-value = 1.793e-09

levels(exp_unit_sums$lure)

# Dunn Test from FSA per https://rcompanion.org/rcompanion/d_06.html
PT = dunnTest(Sum ~ lure,
              data=exp_unit_sums,
              method="bh")    # Can adjust p-values;
# See ?p.adjust for options

PT
# Dunn (1964) Kruskal-Wallis multiple comparison
# p-values adjusted with the Benjamini-Hochberg method.

# Output to Excel because there are 28 comparisons
p_tble <- PT$res
write.csv(p_tble,"./output/dunn_test_nomd.csv", row.names = FALSE)
# This non-param method does not suppor dif between Cidetrak and blank
