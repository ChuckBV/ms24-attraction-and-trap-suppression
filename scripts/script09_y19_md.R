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
#library(scales)
library(FSA) # for SE function and Dunn Test

#-- 1. Prep treatment names and keys ----------------------------------------

### Treatment names from previous expt in desired order
trt_names <- c("BlankCtrl","CidetrakNOW_1in","CidetrakNOW_4in","CidetrakNOW_8in","AldTCP_1in","AldTCP_4in","AldTCP_12in","NowBiolure")   

### Corresponding treatment names in current expt
trt_names2 <- c("BiolurePpo","CidetrakNOW_1in","CidetrakNOW_4in","CidetrakNOW_8in","AldTCP_1in","AldTCP_4in","AldTCP_12in","NowBiolure")

trt_names2 <- trt_names2[c(8,2:7,1)] # desired order of consideration
trt_names2

trt_key <- read_csv("./data/y19_nonmd_trt_key.csv")
trt_key <-select(trt_key,trap_id,lure)

#-- 1. Read in data -----------------------------------------

### Read in data file and convert date data types
vverde <- read_csv("./data/y19-vistaverde-meso-lures-data.csv")
vverde$StartDate <- as.Date(mdy(vverde$StartDate))
vverde$EndDate <- as.Date(mdy(vverde$EndDate))
head(vverde,3)
# A tibble: 3 x 7
#     row   pos lure        StartDate EndDate  trap_id Count
#   <dbl> <dbl> <chr>       <chr>     <chr>      <dbl> <dbl>
# 1     1     1 AldTCP_4in  8/28/2019 9/3/2019      11    47
# 2     1     2 AldTCP_12in 8/28/2019 9/3/2019      12    30
# 3     1     3 BlankCtrl   8/28/2019 9/3/2019      13    34

#  Note that treatments for the second 2019 trial (MD) were mislabelled
#  Fixed line 64

### Merge onto trt_key
dat <- left_join(trt_key,vverde)
head(dat,3)
# A tibble: 3 x 7
# trap_id lure          row   pos StartDate EndDate   Count
#     <dbl> <chr>       <dbl> <dbl> <chr>     <chr>     <dbl>
# 1      11 AldTCP_4in      1     1 8/28/2019 9/3/2019     47
# 2      11 AldTCP_4in      1     1 9/3/2019  9/10/2019    43
# 3      12 AldTCP_12in     1     2 8/28/2019 9/3/2019     30

### Specify factors
dat$lure[dat$lure == "BlankCtrl"] <- "BiolurePpo"
dat <- mutate(dat,lure = factor(lure, levels=trt_names2))
dat$row <- as.factor(dat$row)
head(dat,3)

unique(dat$lure)
# 1] AldTCP_4in      AldTCP_12in     BiolurePpo      CidetrakNOW_4in NowBiolure      CidetrakNOW_1in
# [7] CidetrakNOW_8in AldTCP_1in     
# 8 Levels: NowBiolure CidetrakNOW_1in CidetrakNOW_4in CidetrakNOW_8in AldTCP_1in AldTCP_4in ... BiolurePpo

### How many weeks
dat %>% 
  group_by(EndDate) %>% 
  summarise(nObs = sum(!is.na(Count)))
# EndDate     nObs
# <date>     <int>
# 1 2019-09-03    63
# 2 2019-09-10    63
## Dates are 8/28 to 9/10

#-- 2. Numerical means and SE in table  ---------------------------------------

exp_unit_means <- dat %>%
  group_by(lure,row) %>%
  summarise(Count = mean(Count, na.rm = TRUE))

trt_means <- exp_unit_means %>%
  group_by(lure) %>%
  summarise(nObs = sum(!is.na(Count)),
            mn = mean(Count, na.rm = TRUE),
            sem = se(Count))

write.csv(trt_means,"./output/expt2_means_se.csv", row.names = FALSE)

trt_means
# A tibble: 8 x 4
# lure             nObs      mn    sem
# <fct>           <int>   <dbl>  <dbl>
# 1 NowBiolure          8  0.0625 0.0625a
# 2 CidetrakNOW_1in     8  0.25   0.0945a
# 3 CidetrakNOW_4in     8  1.81   0.619b 
# 4 CidetrakNOW_8in     8  1.5    0.366a 
# 5 AldTCP_1in          8  7.88   2.98c  
# 6 AldTCP_4in          8 23.9    4.95d  
# 7 AldTCP_12in         8 14.4    3.53d  
# 8 BiolurePpo          8 25.3    3.51d 

#-- 3. Box plot of treatment effects  ---------------------------------------

p3 <- ggplot(trt_means, aes(x = lure, y = mn)) +
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

p3

ggsave(filename = "y19-vistaverde-mesos-overall.jpg", p3, 
       path = "./results",
       width = 5.83, height = 3.91, dpi = 300, units = "in", device='jpg')

#-- 4. stats ------------------------------------------

# Pools monitoring intervals (sum rather than mean for analysis)
exp_unit_sums <- dat %>%
  group_by(lure,row) %>%
  summarise(Sum = sum(Count, na.rm = TRUE))

unique(exp_unit_sums$lure)
# [1] NowBiolure      CidetrakNOW_1in CidetrakNOW_4in CidetrakNOW_8in AldTCP_1in      AldTCP_4in     
# [7] AldTCP_12in     BiolurePpo     
# 8 Levels: NowBiolure CidetrakNOW_1in CidetrakNOW_4in CidetrakNOW_8in AldTCP_1in AldTCP_4in ... BiolurePpo

# output experimental unit means for SAS
#write.csv(exp_unit_sums,"./data/dat2.csv", row.names = FALSE)
saveRDS(exp_unit_sums,"./data/y19_md_trapsums.Rds")

# will want descending order for posthoc test
trt_names3 <- trt_names2[c(8,6,7,5,3,4,2,1)]   
exp_unit_sums <- mutate(exp_unit_sums,lure = factor(lure, levels=trt_names3))
levels(exp_unit_sums$lure)
# [1] "BiolurePpo"      "AldTCP_4in"      "AldTCP_12in"     "AldTCP_1in"      "CidetrakNOW_4in" "CidetrakNOW_8in" "CidetrakNOW_1in" "NowBiolure"       

# Use Desc to obtain Kruskall-Wallis test
Desc(Sum ~ lure, data = exp_unit_sums)

# Kruskal-Wallis rank sum test:
#   Kruskal-Wallis chi-squared = 45.295, df = 7, p-value = 1.199e-07

# Dunn Test from FSA per https://rcompanion.org/rcompanion/d_06.html
PT = dunnTest(Sum ~ lure,
              data=exp_unit_sums,
              method="bh")    
# Can adjust p-values;
# See ?p.adjust for options

PT
# Dunn (1964) Kruskal-Wallis multiple comparison
# p-values adjusted with the Benjamini-Hochberg method.

# Output to Excel because there are 28 comparisons
p_tble_md <- PT$res
write.csv(p_tble_md,"./output/dunn_test_md.csv", row.names = FALSE)