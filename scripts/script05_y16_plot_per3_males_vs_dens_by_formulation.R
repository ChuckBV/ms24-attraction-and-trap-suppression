#============================================================================
# script5_y16_plot_per3_males_vs_dens_by_formulation.R
#
# 1. Load data and get mean and SE by group (line 12)
# 2. Create and save plots based on means and SE output (line 40)
#
#============================================================================

library(tidyverse)
library(ggpubr)

#---------------------------------------------------------------------------#
#-- 1. Load data and get mean and SE by group -------------------------------

dat <- readRDS("./data/y16_md_trap_suppression.Rds")

# Get mean and SE by group for Period 3 
md_means <- dat %>% 
  mutate(PerHa = case_when(PerAcre == 0 ~ 0,
                           PerAcre == 7 ~ 17,
                           PerAcre == 12 ~ 30,
                           .default = 69)) %>% 
  filter(Period == 3 & PerHa > 0) %>% 
  group_by(Blend,PerHa) %>% 
  summarise(nObs = n(),
            mn = mean(Plotsum, na.rm = T),
            sem = FSA::se(Plotsum))
md_means
# # A tibble: 6 × 5
# # Groups:   Blend [2]
#   Blend   PerHa  nObs    mn   sem
#   <chr>   <dbl> <int> <dbl> <dbl>
# 1 Ald        17     4  8.75 4.37 
# 2 Ald        30     4  5    3.70 
# 3 Ald        69     4  2.25 0.75 
# 4 Ald+TCP    17     4 12.2  3.30 
# 5 Ald+TCP    30     4  3.75 1.70 
# 6 Ald+TCP    69     4  1    0.707

#---------------------------------------------------------------------------#
#-- 2. Create and save plots based on means and SE output -------------------

# Make density into a factor
md_means$PerHa <- factor(md_means$PerHa, levels = unique(md_means$PerHa))

# Split by formulations and plot separately
Ald <- md_means %>% 
  filter(Blend == "Ald")
Ald <- Ald %>% 
  mutate(means_sep = c("a","a","a"))

Ald_tcp <- md_means %>% 
  filter(Blend == "Ald+TCP")
Ald_tcp <- Ald_tcp %>% 
  mutate(means_sep = c("a","ab","b"))

# Vertical bar chart w SE bar for Ald
p1 <- ggplot(Ald, aes(x = PerHa, y = mn)) +
  geom_col() +
  geom_errorbar(mapping = aes(ymin = mn, ymax = mn + sem), width = 0.2) +
  geom_text(aes(label = means_sep, y = mn + sem), vjust = -0.5) +
  ylim(0,19) +
  theme_bw() +
  xlab("Dispensers per ha") +
  ylab("Males") +
  theme(axis.text.x = element_text(color = "black", size = 10), 
        axis.text.y = element_text(color = "black", size = 10),
        axis.title.x = element_text(color = "black", size = 10),
        axis.title.y = element_text(color = "black", size = 10),
        legend.title = element_text(color = "black", size = 10),
        legend.text = element_text(color = "black", size = 10))

p1

ggsave(filename = "fig3a_trap_suppression_ald.jpg", plot = p1, device = "jpg", path = "./results/", 
       dpi = 300, width = 2.83, height = 1.7, units = "in")

# Vertical bar chart w SE bar for Ald+TCP
p2 <- ggplot(Ald_tcp, aes(x = PerHa, y = mn)) +
  geom_col() +
  geom_errorbar(mapping = aes(ymin = mn, ymax = mn + sem), width = 0.2) +
  geom_text(aes(label = means_sep, y = mn + sem), vjust = -0.5) +
  ylim(0,19) +
  theme_bw() +
  xlab("Dispensers per ha") +
  ylab("Males") +
  theme(axis.text.x = element_text(color = "black", size = 10), 
        axis.text.y = element_text(color = "black", size = 10),
        axis.title.x = element_text(color = "black", size = 10),
        axis.title.y = element_text(color = "black", size = 10),
        legend.title = element_text(color = "black", size = 10),
        legend.text = element_text(color = "black", size = 10))

p2

ggsave(filename = "fig3b_trap_suppression_ald_tcp.jpg", plot = p2, device = "jpg", path = "./results/", 
       dpi = 300, width = 2.83, height = 1.7, units = "in")

# Plot the two plots together
p3 <- ggpubr::ggarrange(p1,p2,
                        ncol = 1, 
                        nrow = 2,
                        labels = c("A)","B)"))
p3

ggsave(filename = "fig3_trap_suppression_both.jpg", plot = p3, device = "jpg", path = "./results/", 
       dpi = 300, width = 2.83, height = 3.4, units = "in")
