all
# A tibble: 84 × 16
#     Rep PlotID TrtID TrtLabel TreceID PerAcre DatePlcd  DateRtrvd Lure     P1    P2    P3    P4 Period Plotsum Blend
#   <dbl>  <dbl> <dbl> <chr>      <dbl>   <dbl> <chr>     <chr>     <chr> <dbl> <dbl> <dbl> <dbl>  <dbl>   <dbl> <chr>
# 1     1    100     7 G              1       0 21-Sep-16 28-Sep-16 L2H      51    48    38    36      1     173 None 
# 2     2    200     7 G              1       0 21-Sep-16 28-Sep-16 L2H      23    44    52    32      1     151 None 
# 3     3    300     7 G              1       0 21-Sep-16 28-Sep-16 L2H      32    49    66    76      1     223 None 

### 6 levels of Blend x Dispenser density, plus control
all %>% 
  group_by(TrtID, TrtLabel, Blend, PerAcre) %>% 
  summarise(nObs = n())
# # Groups:   TrtID, TrtLabel, Blend [7]
#   TrtID TrtLabel Blend   PerAcre  nObs
#   <dbl> <chr>    <chr>     <dbl> <int>
# 1     1 A        Ald+TCP      28    12
# 2     2 B        Ald+TCP      12    12
# 3     3 C        Ald+TCP       7    12
# 4     4 D        Ald          28    12
# 5     5 E        Ald          12    12
# 6     6 F        Ald           7    12
# 7     7 G        None          0    12


### Variable description
# Rep -- Replicate block, 1 to 4 (numerical, but logically a factor)
# PlotID -- 28 levels, 1 to 24 plus 100, 200, 300, 400
# TrtID -- numeric, 1 to 7
# TrtLabel -- "A" "B" "C" "D" "E" "F" "G"
# TreceID -- Trece experimental IDs for dispensers
# PerAcre -- Number of dispensers per acre
# DatePlcd -- Date that clean liners were placed and data collection started
# DateRtrvd -- Date that clean liners were placed and data collection stopped
# Lure -- Attractant used for trapping studies
# Period -- Successive date ranges for periods 1, 2, and 3
# Blend -- Ald, Ald+TCP, None 
