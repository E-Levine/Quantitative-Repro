## Code for quantitative repro analysis

## Packages
if (!require("pacman")) {install.packages("pacman")}
pacman::p_load(plyr, tidyverse, MASS,
               readxl,
               broom, FSA, rcompanion, rstatix, 
               ggpubr, scales, wesanderson, fansi,
               install = TRUE) 
#
## Load and clean data ####
#
## Water quality
WQ_raw <- read.csv("Data/WQ.csv", header = T, na.strings = "Z")
glimpse(WQ_raw)
WQ <- WQ_raw %>% mutate(
  #Convert Month, Site, Station to factors
  Month = factor(Month, levels = month.abb),
  Site = factor(Site, levels = c("AB", "CR", "LW", "LX", "SL", "TB")),
  Station = as.factor(Station))
#
#
## Dermo
Dermo_raw <- read.csv("Data/Dermo.csv", header = T, na.strings = "Z")
glimpse(Dermo_raw)
Dermo <- Dermo_raw %>% mutate(
  #Convert Month, Site, Station to factors
  Month = factor(Month, levels = month.abb),
  Site = factor(Site, levels = c("AB", "CR", "LW", "LX", "SL", "TB")),
  Season = as.factor(Season)) %>%
  rename_with(~ gsub("\\.", "_", .x))
#
#
## Repro
Repro_raw <- read.csv("Data/Repro.csv", header = T, na.strings = "Z")
glimpse(Repro_raw)
Repro <- Repro_raw %>% mutate(
  #Convert Month, Site, Station to factors
  Month = factor(Month, levels = month.abb),
  Site = factor(Site, levels = c("AB", "CR", "LW", "LX", "SL", "TB")),
  Season = as.factor(Season),
  Sex = as.factor(Sex),
  Stage = as.factor(Stage))
#
#
## CI
CI_raw <- read.csv("Data/Condition.csv", header = T, na.strings = "Z")
glimpse(CI_raw)
Condition <- CI_raw %>% mutate(
  #Convert Month, Site, Station to factors
  Month = factor(Month, levels = month.abb),
  Site = factor(Site, levels = c("AB", "CR", "LW", "LX", "SL", "TB")),
  Season = as.factor(Season)) %>%
  rename_with(~ gsub("\\.", "_", .x)) %>% # Replace . with _
  rename_with(~ gsub("*__g_", "", .x)) # Remove units (__g_)
#
#

#
#
#
## Formatting ####
#
## Color by site
colors <- c("dodgerblue4", "chocolate", "mediumpurple2", "violetred", 
            "forestgreen", "goldenrod1")
names(colors) <- c("AB", "CR", "LW", "LX", "SL", "TB")
colors
#
# Base plot formatting
Base <- theme_bw() +
  theme(panel.grid = element_blank(), panel.border = element_blank(), panel.background = element_blank(),
        axis.line = element_line(color = "black"),
        axis.title = element_text(size = 14, color = "black", family = "serif"),
        axis.text.x = element_text(size = 13, color = "black", 
                                   margin = unit(c(0.5, 0.5, 0, 0.5), "cm"), family = "serif"),
        axis.text.y = element_text(size = 14, color = "black", 
                                   margin = unit(c(0, 0.5, 0, 0), "cm"), family = "serif"),
        axis.ticks.length = unit(-0.15, "cm"), plot.margin = margin(0.25, 0.5, 0.25, 0.25, "cm"))
# Base legend foramtting
legend_config <- theme(legend.position = "top", 
                       legend.text = element_text(size = 12, color = "black", family = "serif"),
                       legend.title = element_text(size = 13, color = "black", family = "serif")) 
#
#
## WQ ####
summary(WQ_raw); head(WQ_raw)
#
# Temperature
temperature <- aggregate(Temp ~ Month + Site, WQ_raw, mean) #data
# TEST NORMALITY
shapiro.test(WQ_raw$Temp) # not normal
#
# TEST HOMOGENEITY OF VARIANCE
bartlett.test(Temp~Month, data=WQ_raw) # at least one sample has a sig diff variance
#
# STATS
(temp_kt <- kruskal.test(WQ_raw$Temp ~ WQ_raw$Site)) #less than 0.05 therefore there are significant differences between sites
temp_kt %>% tidy() %>% as.data.frame() %>% dplyr::select(method, everything())
pairwise.wilcox.test(WQ_raw$Temp, WQ_raw$Site, p.adjust.method = "bonferroni") 
#
# MEANS
# By Site
(Temp_site <- WQ_raw %>% dplyr::select(Site, Temp) %>%
  group_by(Site) %>% get_summary_stats(show = c("mean", "sd", "se")))
# By Site and Month
Temp_monthly_means <- WQ_raw %>% dplyr::select(Site, Month, Temp) %>%
  group_by(Site, Month) %>% get_summary_stats(show = c("mean", "sd", "se")) %>%
  dplyr::select(-variable)
#
# Letters
TT <- (dunnTest(Temp ~ Site, data = WQ_raw, method = "bh"))$res
(TT_letters <- left_join(Temp_site,
                         cldList(comparison = TT$Comparison, p.value = TT$P.adj, threshold = 0.05) %>%
                           dplyr::select(Group, Letter) %>% rename("Site" = Group)))
#
## Plotting 
# By site
ggplot(WQ_raw, aes(x = Site, y = Temp)) +
  geom_boxplot() +
  labs(y= expression("Average temperature ( " * degree* "C)"), x = "Site") +
  scale_y_continuous(expand = c(0,0), limits=c(0,40), breaks = seq(0,40, by = 10)) +
  annotate("text", x=1:6, y=37, label=TT_letters$Letter, family = "serif", size = 6) +
  Base
# Monthly
temperature %>% 
  ggplot(aes(Month, Temp, color = Site))+
  stat_summary(fun = mean, geom = "point", size = 3.5) +
  stat_summary(fun = mean, geom = "line", size = 1,
               aes(group = Site)) +
  scale_x_discrete(expand = c(0,0.1), limits = month.abb) +
  scale_y_continuous(expand = c(0,0), limits=c(0,35), breaks = seq(0,35, by = 5)) +
  labs(y = expression("Average temperature ( " * degree * "C)"), 
       x = "Month") +
  Base + 
  scale_colour_manual(values = colors)+ 
  legend_config+
  guides(color = guide_legend(nrow = 1))
#
#ggsave(path = "Output/", filename = "Fig2_Monthly_Temp_by_Site.tiff",dpi=1000)
#
#
# Salinity
salinity <- aggregate(Salinity ~ Month + Site, WQ_raw, mean)
# TEST NORMALITY
shapiro.test(WQ_raw$Salinity) # not normal
#
# TEST HOMOGENEITY OF VARIANCE
bartlett.test(Salinity~Month, data=WQ_raw)# at least one sample has a sig diff variance
#
# STATS
(sal_kt <- kruskal.test(WQ_raw$Salinity ~ WQ_raw$Site)) #less than 0.05 therefore there are significant differences between sites
sal_kt %>% tidy() %>% as.data.frame() %>% dplyr::select(method, everything())
pairwise.wilcox.test(WQ_raw$Salinity, WQ_raw$Site, p.adjust.method = "bonferroni") 
#
# MEANS
# By Site
(Sal_sites <- WQ_raw %>% dplyr::select(Site, Salinity) %>%
  group_by(Site) %>% get_summary_stats(show = c("mean", "sd", "se")))
# By Site and Month
Sal_monthly_means <- WQ_raw %>% dplyr::select(Site, Month, Salinity) %>%
  group_by(Site, Month) %>% get_summary_stats(show = c("mean", "sd", "se")) %>%
  dplyr::select(-variable)
#
# Letters
ST <- (dunnTest(Salinity ~ Site, data = WQ_raw, method = "bh"))$res
(ST_letters <- left_join(Sal_sites,
                         cldList(comparison = ST$Comparison, p.value = ST$P.adj, threshold = 0.05) %>%
                           dplyr::select(Group, Letter) %>% rename("Site" = Group)))
#
## Plotting 
# By site
ggplot(WQ_raw, aes(x = Site, y = Salinity)) +
  geom_boxplot() +
  labs(y= "Average salinity", x = "Site") +
  scale_y_continuous(expand = c(0,0), limits=c(0,45), breaks = seq(0,45, by = 10)) +
  annotate("text", x=1:6, y=42, label=ST_letters$Letter, family = "serif", size = 6) +
  Base
# Monthly
salinity %>% 
  ggplot(aes(Month, Salinity, color = Site))+
  stat_summary(fun = mean, geom = "point", size = 3.5) +
  stat_summary(fun = mean, geom = "line", size = 1,
               aes(group = Site)) +
  scale_x_discrete(expand = c(0,0.1), limits = month.abb) +
  scale_y_continuous(expand = c(0,0), limits=c(0,35), breaks = seq(0,35, by = 5)) +
  labs(y= "Average salinity", 
       x = "Month") +
  Base + 
  scale_colour_manual(values = colors)+ 
  legend_config+
  guides(color = guide_legend(nrow = 1))
#
#ggsave(path = "Output/", filename = "Fig3_Monthly_Sal_by_Site.tiff",dpi=1000)
# 
#
## Dermo ####

#
#
## Repro ####

#
#
## Condition Index ####

#
#
## Adding in 2011 SLC data ####
#
## WQ

#
#
## Dermo

#
#
## Repro
SLC_repro_raw <- read_xlsx("Data/SLC_2011_AllData.xlsx", sheet = 1, .name_repair = "universal") 
glimpse(SLC_repro_raw)
SLC_repro <- SLC_repro_raw %>% mutate(
  #Convert Month, Site, Station to factors
  Month = factor(format(as.Date(SLC_raw$Date), "%b"), levels = month.abb),
  Site = factor("SL", levels = c("AB", "CR", "LW", "LX", "SL", "TB"))) %>%
  rename_with(~ gsub("\\.", "_", .x)) %>% # Replace . with _
  rename_with(~ gsub("*__g_", "", .x)) # Remove units (__g_)
#
#
## Condition Index
