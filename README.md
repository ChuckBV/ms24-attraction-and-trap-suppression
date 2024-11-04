# Attraction and trap suppression by high-rate NOW dispensers with one or two components

***Repository*** github.com/ChuckBV/ms22-attraction-and-trap-suppression

## Overview

***The study***
This manuscript describes experiments on attraction of NOW to one- and 
two-component meso-dispensers in 2019 and crop-destruction trap suppression 
experiments in 2016. 

***Simple overview***
The navel orangeworm is an important pest of high-value crops including almonds, 
pistachios, and walnuts. Monitoring and mating disruption are important tools 
for control of this moth, and improved understanding of its response to pheromone 
composition and concentration has the potential to improve monitoring and 
mating disruption. Experiments with high release rate passive diffusion pheromone 
dispensers revealed that capture of males in pheromone traps was suppressed 
more successfully at an intermediate rate of dispensers per acre when a second 
pheromone component was included. Traps baited with part or all of the 
single-compound meso-dispensers as bait were minimally attractive in the 
absence of mating disruption, thereby confirming earlier research on the 
mechanism of mating disruption for navel orangeworm. Traps baited with the 
two-component meso-dispensers captured similar numbers of navel orangeworm in either 
the presence or absence of mating disruption, indicating that this dispenser could 
also be used to improve monitoring. This study provides scientists and extension 
personnel with further insight into the mechanism of the current widely-used 
mating disruption products for navel orangeworm, and indicates that the two-component 
meso-dispensers could provide improvements in monitoring for navel orangeworm 
compared to the presently-used products.

## Scripts

### Scripts 1-7: 2016 mating disruption test (trap suppression)

***script01_y16_trap_md_ctrl_pct_suppression.R***

Compares mean males/plot in 4 replicate blocks between an untreated control 
and the remaining 6 treatments (2 formulations * 3 dispenser densities) for 
each of three monitoring intervals. Table and Welch t-test. (Table 1)

***script02_y16_trap_suppression_per1_stats.R*** 

Mean trap capture by dispenser density plotted separately for period 1 
(figs 2A and 2B, saved as combined figure)

***script03_y16_plot_per1_males_vs_dens_by_formulation.R*** 

GlMM w nb, initially factorial then separate 1-way ANOVAs for 1- and 2-component 
dispensers (for period 1)

***script03a_y16_plot_variability_per1.R*** 

Re-examines Period 1 using an OLS 2-way ANOVA with rep as a fixed factor,
to get a P value for differences among replicate blocks. Also plots
plot sum of moths by dispenser densities as point and line plots with
separate data points for each rep. 

***script04_y16_trap_suppression_per3_stats.R***

Mean trap capture by dispenser density plotted separately for period 3 
(figs 2A and 2B, saved as combined figure)

***script05_y16_plot_per3_males_vs_dens_by_formulation.R***

GlMM w nb, initially factorial then separate 1-way ANOVAs for 1- and 2-component 
dispensers (for period 3).

***script05a_y16_plot_variability_per3.R*** 

Same as script03a*.R, except examines Period 3 rather than period 1. 
 
***script06_y16_attraction_to_traps.R***

Mean and SE with univariate confidence interval (t-test) for traps with 2.5 cm 
segments of 1-component or 2-component dispenser or pheromone monitoring lure.

***script07_y16_temperatures.R*** 

Provides summary of the daily high and low air temperature during the three 
intervals of the 2016 trap suppression experiment
 
### Scrips 8-11: 2019 attraction comparison using sticky traps

***script08_y19_nomd.R*** 
 
Means and SE for 8 treatments in non-mating disruption orchard. Outputs Table 3.

***script09_y19_md.R***

1-way mixed-model ANOVA (GLMM w nb) for capture in sticky traps in a non-mating 
disruption orchard.

***script10_y19_nonmd_stats.R***

Means and SE for 8 treatments in mating disruption orchard. Outputs Table 4.

***script11_y19_md_stats.R***

1-way mixed-model ANOVA (GLMM w nb) for capture in sticky traps in a mating 
disruption orchard.
 
