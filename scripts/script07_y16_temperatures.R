#============================================================================
# Script11_y16_temperatures.R
# 
# Reads in a CIMIS max-min tempertature file
# CIMIS: https://cimis.water.ca.gov/Default.aspx
#
#============================================================================


#-- 0. Declare libraries and functions --------------------------------------
library(tidyverse)


#-- 1. Import data ----------------------------------------------------------

Temps <- read_csv("./data/y16_cimis105.csv")
Temps$Date <- as.Date(mdy(Temps$Date))

Temps
# # A tibble: 23 × 9
#   StnId StnName   CIMISRegion        Date         Jul MaxAirTempC qc1   MinAirTempC qc2  
#   <dbl> <chr>     <chr>              <date>     <dbl>       <dbl> <chr>       <dbl> <chr>
# 1   105 Westlands San Joaquin Valley 2016-09-21   265        32.7 NA           12.5 NA   
# 2   105 Westlands San Joaquin Valley 2016-09-22   266        24.9 NA            6.8 Y    
# 3   105 Westlands San Joaquin Valley 2016-09-23   267        28.2 NA           10   NA  

#-- 2. Clean data and reshape to tidy format --------------------------------

### Tidy format, from Date-Low-High to Date-TempType-Temp
Temps2 <- Temps %>% 
  rename(High = MaxAirTempC, Low = MinAirTempC) %>% 
  pivot_longer(cols = c(High,Low), 
               names_to ="TempType",
               values_to = "Temp") %>% 
  mutate(Day = as.integer(Date - as.Date("2016-09-21")))

Temps2
# # A tibble: 46 × 10
#   StnId StnName   CIMISRegion        Date         Jul qc1   qc2   TempType  Temp   Day
#   <dbl> <chr>     <chr>              <date>     <dbl> <chr> <chr> <chr>    <dbl> <int>
# 1   105 Westlands San Joaquin Valley 2016-09-21   265 NA    NA    High      32.7     0
# 2   105 Westlands San Joaquin Valley 2016-09-21   265 NA    NA    Low       12.5     0
# 3   105 Westlands San Joaquin Valley 2016-09-22   266 NA    Y     High      24.9     1
# 4   105 Westlands San Joaquin Valley 2016-09-22   266 NA    Y     Low        6.8     1

#-- 3. Plot as Line graph with separate lines for low and high --------------

### Below I seem to have converted the X-axis from date to numeric in order
### to get more precise control of the date format.

Tempfig <-
  ggplot(Temps2, aes(x=Day, y=Temp, colour=TempType, group=TempType)) + 
  geom_line(linewidth=1) +
  
  xlab("") +
  ylab("Degrees Celsius") +
  ggtitle("") +
  expand_limits(y=0) +                        # Expand y range
  scale_x_continuous(breaks = c(0,7,14,21), 
                     labels = c("21 Sep", "28 Sep", "5 Oct", "12 Oct"), 
                     limits = c(0, 21)) +
  
  geom_vline(xintercept = 7, linetype="dashed", color = "black") +
  geom_vline(xintercept = 13, linetype="dashed", color = "black") +
  
  theme(axis.title = element_text(color = "black", size = 7),
        axis.text = element_text(color = "black", size=7),
        plot.title = element_text(color = "black", size=7),
        legend.text = element_text(color = "black", size = 7),
        legend.title = element_blank()
  ) 
Tempfig 

ggsave(filename = "./Results/Fig3.eps", Tempfig,
       width = 2.83, height = 1.87, dpi = 300, units = "in", device='eps')


ggsave(filename = "./Results/Y16_daily_hi_lo.png", Tempfig,
       width = 2.83, height = 1.87, dpi = 300, units = "in", device='png')

#-- 4. Get reportable stats ------------------------------------------------

Temps2 <- Temps2 %>% 
  mutate(interval = case_when(Day < 8 ~ 1,
                              Day < 15 ~ 2,
                              .default = 3))

Temps2 %>% 
  group_by(interval,TempType) %>% 
  summarise(max = max(Temp),
            min = min(Temp),
            avg = mean(Temp))
