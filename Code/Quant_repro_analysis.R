## Code for quantitative repro analysis - original data
# Does not include data from St Lucie 2011 samples

## Packages
if (!require("pacman")) {install.packages("pacman")}
pacman::p_load(plyr, tidyverse, MASS,
               readxl,
               broom, FSA, rcompanion, rstatix, car, 
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
Repro_raw <- read.csv("Data/Repro.csv", header = T)
glimpse(Repro_raw)
Repro <- Repro_raw %>% mutate(
  #Convert Month, Site, Station to factors
  Month = factor(Month, levels = month.abb),
  Site = factor(Site, levels = c("AB", "CR", "LW", "LX", "SL", "TB")),
  Season = as.factor(Season),
  Sex = as.factor(Sex),
  Stage = as.factor(Stage),
  OocyteDiam = as.numeric(OocyteDiam))
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
## Color by site
colors <- c("dodgerblue4", "chocolate", "mediumpurple2", "violetred", 
            "forestgreen", "goldenrod1")
names(colors) <- c("AB", "CR", "LW", "LX", "SL", "TB")
colors
Sites <- c("AB" = "Apalachicola Bay", "CR" = "Caloosahatchee", "LW" = "Lake Worth", "LX" = "Loxahatchee", 
           "SL" = "St. Lucie", "TB" = "Tampa Bay")
#
# Base plot formatting
Base <- theme_bw() +
  theme(panel.grid = element_blank(), panel.border = element_blank(), panel.background = element_blank(), axis.line = element_line(color = "black"),
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
# Base facet formatting
FacetBase <- theme(panel.spacing = unit(-0.1, "cm"), strip.placement = "outside", 
                   strip.text.x.top = element_text(size = 13, family = "serif", face = "bold"), 
                   strip.clip = "off", panel.spacing.y = unit(0.5, "lines"))
#
FacetSpace <- theme(panel.spacing.y = unit(-0.05, "lines"), panel.spacing.x = unit(-0.50, "lines"), 
                    axis.text.x = element_text(angle = 60, hjust = 1, vjust = 1, margin = margin(t = 5)))
#
## WQ ####
summary(WQ); head(WQ)
#
# Temperature
temperature <- aggregate(Temp ~ Month + Site, WQ, mean) #data
# TEST NORMALITY
shapiro.test(WQ$Temp) # not normal
#
# TEST HOMOGENEITY OF VARIANCE
bartlett.test(Temp~Month, data=WQ) # at least one sample has a sig diff variance
#
# STATS
(temp_kt <- kruskal.test(WQ$Temp ~ WQ$Site)) #less than 0.05 therefore there are significant differences between sites
temp_kt %>% tidy() %>% as.data.frame() %>% dplyr::select(method, everything())
pairwise.wilcox.test(WQ$Temp, WQ$Site, p.adjust.method = "bonferroni") 
#
# MEANS
# By Site
(Temp_site <- WQ %>% dplyr::select(Site, Temp) %>%
  group_by(Site) %>% get_summary_stats(show = c("mean", "sd", "se")))
# By Site and Month
Temp_monthly_means <- WQ %>% dplyr::select(Site, Month, Temp) %>%
  group_by(Site, Month) %>% get_summary_stats(show = c("mean", "sd", "se")) %>%
  dplyr::select(-variable)
#
# Letters
TT <- (dunnTest(Temp ~ Site, data = WQ, method = "bh"))$res
(TT_letters <- left_join(Temp_site,
                         cldList(comparison = TT$Comparison, p.value = TT$P.adj, threshold = 0.05) %>%
                           dplyr::select(Group, Letter) %>% rename("Site" = Group)))
#
## Plotting 
# By site
ggplot(WQ, aes(x = Site, y = Temp)) +
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
salinity <- aggregate(Salinity ~ Month + Site, WQ, mean)
# TEST NORMALITY
shapiro.test(WQ$Salinity) # not normal
#
# TEST HOMOGENEITY OF VARIANCE
bartlett.test(Salinity~Month, data=WQ)# at least one sample has a sig diff variance
#
# STATS
(sal_kt <- kruskal.test(WQ$Salinity ~ WQ$Site)) #less than 0.05 therefore there are significant differences between sites
sal_kt %>% tidy() %>% as.data.frame() %>% dplyr::select(method, everything())
pairwise.wilcox.test(WQ$Salinity, WQ$Site, p.adjust.method = "bonferroni") 
#
# MEANS
# By Site
(Sal_sites <- WQ %>% dplyr::select(Site, Salinity) %>%
  group_by(Site) %>% get_summary_stats(show = c("mean", "sd", "se")))
# By Site and Month
Sal_monthly_means <- WQ %>% dplyr::select(Site, Month, Salinity) %>%
  group_by(Site, Month) %>% get_summary_stats(show = c("mean", "sd", "se")) %>%
  dplyr::select(-variable)
#
# Letters
ST <- (dunnTest(Salinity ~ Site, data = WQ, method = "bh"))$res
(ST_letters <- left_join(Sal_sites,
                         cldList(comparison = ST$Comparison, p.value = ST$P.adj, threshold = 0.05) %>%
                           dplyr::select(Group, Letter) %>% rename("Site" = Group)))
#
## Plotting 
# By site
ggplot(WQ, aes(x = Site, y = Salinity)) +
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
summary(Dermo); head(Dermo)
# Prevalence
presencemean <- aggregate(PresenceBoth ~ Month + Site, Dermo, mean)
# STATS
(prevalence_kt <- kruskal.test(Dermo$PresenceBoth ~ Dermo$Site)) #less than 0.05 therefore there are significant differences between sites
prevalence_kt %>% tidy() %>% as.data.frame() %>% dplyr::select(method, everything())
pairwise.wilcox.test(Dermo$PresenceBoth, Dermo$Site, p.adjust.method = "bonferroni") 
#
# MEANS
# By Site
(Prev_site <- Dermo %>% dplyr::select(Site, PresenceBoth) %>%
  group_by(Site) %>% get_summary_stats(show = c("mean", "sd", "se")))
# Letters
PrevT <- (dunnTest(PresenceBoth ~ Site, data = Dermo, method = "bh"))$res
(PrevT_letters <- left_join(Prev_site,
                         cldList(comparison = PrevT$Comparison, p.value = PrevT$P.adj, threshold = 0.05) %>%
                           dplyr::select(Group, Letter) %>% rename("Site" = Group)))
#
ggplot(presencemean, aes(x = Site, y = PresenceBoth)) +
  geom_boxplot(fill = "#999999") +
  labs(y= "Dermo Prevalence", x = "Site") +
  scale_y_continuous(expand = c(0,0), limits=c(0,1.3), breaks = seq(0,1.3, by = 0.5)) +
  annotate("text", x=1:6, y=1.2, label=PrevT_letters$Letter, family = "serif", size = 6) +
  Base
#  
#ggsave(path = "Output/", filename = "Fig4_Dermo_prevalence_by_Site.tiff",dpi=1000)
#
#
# By Month and Site
Prev_monthly_means <- Dermo %>% dplyr::select(Site, Month, PresenceBoth) %>%
  group_by(Site, Month) %>% get_summary_stats(show = c("mean", "sd", "se")) %>%
  dplyr::select(-variable) %>%
  complete(Site, Month, fill = list(mean = NA, sd = NA, se = NA, n = NA))
#
presencemean %>% 
  ggplot(aes(Month, PresenceBoth, fill = Site))+
  geom_bar(aes(x=Month, y=PresenceBoth), stat="identity")+
  scale_x_discrete(limits = month.abb) + 
  scale_y_continuous(expand = c(0,0),limits=c(0,1), breaks = seq(0,1, by = .25)) +
    labs(y= "Prevalence of Dermo", 
         x = "Month")  +
  lemon::facet_rep_wrap(.~Site, labeller = labeller(Site = Sites))+
  scale_fill_manual(values = colors, labels = Sites)+
  Base + theme(legend.position = "none")+
  FacetBase + FacetSpace
#
#ggsave(path = "Output/", filename = "Fig5_Dermo_prevalence_Monthly_Site.tiff",dpi=1000)
#
# Intensity
intenseemean <- aggregate(Intensity ~ Month + Site, Dermo, mean) 
# STATS
(intensity_kt <- kruskal.test(Dermo$Intensity ~ Dermo$Site)) #less than 0.05 therefore there are significant differences between sites
intensity_kt %>% tidy() %>% as.data.frame() %>% dplyr::select(method, everything())
pairwise.wilcox.test(Dermo$Intensity, Dermo$Site, p.adjust.method = "bonferroni") 
#
# MEANS
# By Site
(Inte_site <- Dermo %>% dplyr::select(Site, Intensity) %>%
    group_by(Site) %>% get_summary_stats(show = c("mean", "sd", "se")))
# Letters
InteT <- (dunnTest(Intensity ~ Site, data = Dermo, method = "bh"))$res
(InteT_letters <- left_join(Inte_site,
                            cldList(comparison = InteT$Comparison, p.value = InteT$P.adj, threshold = 0.05) %>%
                              dplyr::select(Group, Letter) %>% rename("Site" = Group)))
#
ggplot(intenseemean, aes(x = Site, y = Intensity)) +
  geom_boxplot(fill = "#999999") +
  labs(y= "Dermo Intensity", x = "Site") +
  scale_y_continuous(expand = c(0,0), limits=c(0,3), breaks = seq(0,3, by = 1)) +
  annotate("text", x=1:6, y=2.70, label=PrevT_letters$Letter, family = "serif", size = 6) +
  Base
#  
#ggsave(path = "Output/", filename = "Fig6_Dermo_intensity_by_Site.tiff",dpi=1000)
#
#
# By Month and Site
Inte_monthly_means <- Dermo %>% dplyr::select(Site, Month, Intensity) %>%
  group_by(Site, Month) %>% get_summary_stats(show = c("mean", "sd", "se")) %>%
  dplyr::select(-variable) %>%
  complete(Site, Month, fill = list(mean = NA, sd = NA, se = NA, n = NA))
#
intenseemean %>% 
  ggplot(aes(Month, Intensity, fill = Site))+
  geom_bar(aes(x=Month, y=Intensity), stat="identity")+
  scale_x_discrete(limits = month.abb) + 
  scale_y_continuous(expand = c(0,0),limits=c(0,5), breaks = seq(0,5, by = 1)) +
  labs(y= "Average Intensity of Infection", 
       x = "Month")  +
  lemon::facet_rep_wrap(.~Site, labeller = labeller(Site = Sites))+
  scale_fill_manual(values = colors, labels = Sites)+
  Base + theme(legend.position = "none")+
  FacetBase + FacetSpace
#
#ggsave(path = "Output/", filename = "Fig7_Dermo_intensity_Monthly_Site.tiff",dpi=1000)
#
#
## Repro ####
summary(Repro); head(Repro)
#
# Monthly proportion male v female all sites:
Repro %>% dplyr::group_by(Month, Sex) %>% 
  summarise(Count = n()) %>%
  pivot_wider(names_from = "Sex", values_from = "Count") %>%
  arrange(factor(Month, levels = month.abb)) %>%
  ungroup() %>%
  # Get total samples for proportions
  mutate(Total = rowSums(dplyr::select(., F, M, Z, H), na.rm = TRUE)) %>%
  # Get proportions
  mutate(F_prop = round(F/Total,2),
         M_prop = round(M/Total,2),
         Z_prop = round(Z/Total,2),
         H_prop = round(H/Total,2))
#
Repro %>%
  ggplot()+
  geom_bar(aes(x=Month, fill=Sex), position="fill", color = "darkgreen") +
  labs(x = "Month",
       y = "Proportion") +
  lemon::facet_rep_wrap(.~Site, labeller = labeller(Site = Sites))+
  scale_x_discrete(limits = month.abb) +
  scale_y_continuous(expand = c(0,0))+
  scale_fill_brewer(palette="Greens")+
  Base +
  legend_config + 
  FacetBase + FacetSpace
#
#ggsave(path = "Output/", filename = "Fig8_Repro_MF_Monthly_Site.tiff",dpi=1000)
#
#
# Monthly proportion per stage all sites:
Repro %>% dplyr::group_by(Month, Stage) %>% 
  summarise(Count = n()) %>%
  pivot_wider(names_from = "Stage", values_from = "Count") %>%
  arrange(factor(Month, levels = month.abb)) %>%
  ungroup() %>%
  # Get total samples for proportions
  mutate(Total = rowSums(dplyr::select(., `1`, `2`, `3`, `4`, `NA`), na.rm = TRUE)) %>%
  # Get proportions
  mutate(Prop_1 = round(`1`/Total,2),
         Prop_2 = round(`2`/Total,2),
         Prop_3 = round(`3`/Total,2),
         Prop_4 = round(`4`/Total,2),
         Prop_Z = round(`NA`/Total, 2))
#
Repro %>% 
  ggplot()+
  geom_bar(mapping=aes(x=Month, fill=Stage),position="fill", color = "darkblue") +
  labs(x = "Month",
       y = "Proportion") +
  scale_x_discrete(limits = month.abb) +
  scale_y_continuous(expand = c(0,0))+
  lemon::facet_rep_wrap(.~Site, labeller = labeller(Site = Sites))+
  scale_fill_brewer(palette="Blues")+
  Base + legend_config +
  FacetBase +  FacetSpace
#
#ggsave(path = "Output/", filename = "Fig9_Repro_Stages_Monthly_Site.tiff",dpi=1000)
#
# Monthly proportion per stage by site:
(Stages_by_site <- Repro %>% dplyr::group_by(Site, Month, Stage) %>% 
  summarise(Count = n()) %>%
  pivot_wider(names_from = "Stage", values_from = "Count") %>%
  ungroup() %>%
  # Get total samples for proportions
  mutate(Total = rowSums(dplyr::select(., `1`, `2`, `3`, `4`, `NA`), na.rm = TRUE)) %>%
  # Get proportions
  mutate(Prop_1 = round(`1`/Total,2),
         Prop_2 = round(`2`/Total,2),
         Prop_3 = round(`3`/Total,2),
         Prop_4 = round(`4`/Total,2),
         Prop_Z = round(`NA`/Total, 2)))
#
#
# Number of Oysters with parasites
par <- with(Repro, table(Parasite, Site))

ggplot(as.data.frame(par), aes(factor(Parasite), Freq, fill = Site)) +
  geom_bar(stat="identity", position=position_dodge()) +
  labs(y= "Number of Oysters with Parasites", x = "Parasites") +
  scale_y_continuous(expand = c(0,0), limits=c(0,140), breaks = seq(0,140, by = 20)) +
  scale_fill_manual(values = colors) +
  Base + 
  legend_config + guides(fill = guide_legend(nrow = 1))
#
#ggsave(path = "Output/", filename = "Fig10_Repro_Parasites_Site.tiff",dpi=1000)
#
#
#
## Gonad Percent
gonadmean <- aggregate(GonadPercent ~ Month + Site, Repro, mean)
#Levene test for non-normal data
leveneTest(Repro$GonadPercent, Repro$Site, center = mean) #variances assumed to not be equal
# STATS
(gonadPct_kt <- kruskal.test(Repro$GonadPercent ~ Repro$Site))
gonadPct_kt %>% tidy() %>% as.data.frame() %>% dplyr::select(method, everything())
dunnTest(GonadPercent ~ Site, data = Repro, method = "bonferroni")
#
# MEANS
# By Site
(GPct_site <- Repro %>% dplyr::select(Site, GonadPercent) %>%
    group_by(Site) %>% get_summary_stats(show = c("mean", "sd", "se")))
# Letters
GPctT <- (dunnTest(GonadPercent ~ Site, data = Repro, method = "bh"))$res
(GPctT_letters <- left_join(GPct_site,
                            cldList(comparison = GPctT$Comparison, p.value = GPctT$P.adj, threshold = 0.05) %>%
                              dplyr::select(Group, Letter) %>% rename("Site" = Group)))
#
ggplot(gonadmean, aes(x = Site, y = GonadPercent)) +
  geom_boxplot(fill = "#999999") +
  labs(y= "Gonad Percent Area", x = "Site") +
  scale_y_continuous(expand = c(0,0), limits=c(0,62), breaks = seq(0,60, by = 20)) +
  annotate("text", x=1:6, y=60, label=GPctT_letters$Letter, family = "serif", size = 6) +
  Base
#  
#ggsave(path = "Output/", filename = "Fig11_Repro_Gonad_Percent_by_Site.tiff",dpi=1000)
#
#
# By Month and Site
GPct_monthly_means <- Repro %>% dplyr::select(Site, Month, GonadPercent) %>%
  group_by(Site, Month) %>% get_summary_stats(show = c("mean", "sd", "se")) %>%
  dplyr::select(-variable) %>%
  complete(Site, Month, fill = list(mean = NA, sd = NA, se = NA, n = NA))
#
GPct_monthly_means %>% 
  ggplot(aes(x=Month, y=mean, colour=Site, group=Site))+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), 
                width = .25, size = 0.75) +
  geom_line(size = 0.75) +
  geom_point(size = 2.5) +
  scale_x_discrete(limits = month.abb) + 
  scale_y_continuous(expand = c(0,0),limits=c(0,70), breaks = seq(0,70, by = 10)) +
  labs(y= "Average Gonad Percent Area", 
       x = "Month")  +
  lemon::facet_rep_wrap(.~Site, labeller = labeller(Site = Sites))+
  scale_color_manual(values = colors, labels = Sites)+
  Base + theme(legend.position = "none")+
  FacetBase + FacetSpace
#
#ggsave(path = "Output/", filename = "Fig12_Repro_Gonad_Percent_Monthly_Site.tiff",dpi=1000)
#
#
# Percent area by stage and site
(gonadstage <- Repro %>% dplyr::select(Site, Stage, GonadPercent) %>%
  group_by(Site, Stage) %>% get_summary_stats(show = c("mean", "sd", "se")) %>%
  dplyr::select(-variable) %>%
  complete(Site, Stage, fill = list(mean = NA, sd = NA, se = NA, n = NA)))
#
gonadstage %>%
ggplot(aes(x=Stage, y=mean, fill=Site)) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se),
                size=0.8,    # Thinner lines
                width=.25,
                position=position_dodge(.9)) +
  labs(y= "Average Gonad Percent Area", x = "Stage") +
  scale_y_continuous(expand= c(0,0), limits=c(0,70), breaks = seq(0,70, by = 10)) +
  scale_fill_manual(values = colors) +
  Base + 
  legend_config + guides(fill = guide_legend(nrow = 1))
#
#ggsave(path = "Output/", filename = "Fig13_Repro_Gonad_Percent_Stage_Site.tiff",dpi=1000)
#
#
## Oocyte by Site
oocytemean <- aggregate(OocyteDiam ~ Month + Site, Repro, mean)
#Levene test for non-normal data
leveneTest(Repro$OocyteDiam, Repro$Site, center = mean) #variances assumed to not be equal
# STATS
(oocyte_kt <- kruskal.test(Repro$OocyteDiam ~ Repro$Site)) #no sig diff
Repro %>% dplyr::select(Site, OocyteDiam) %>%
  group_by(Site) %>% get_summary_stats(show = c("mean", "sd", "se"))
#
oocytemean %>% 
  ggplot(aes(x = Site, y = OocyteDiam)) +
  geom_boxplot(fill = "#999999") +
  labs(y= expression("Oocyte Diameter ( " * mu * "m)"), 
       x = "Site") +
  scale_y_continuous(expand = c(0,0), limits=c(0,40), breaks = seq(0,40, by = 10)) +
  Base
#
#ggsave(path = "Output/", filename = "Fig14_Repro_Oocyte_Diameter_Site.tiff",dpi=1000)
#
Repro %>% dplyr::select(Site, Month, OocyteDiam) %>%
  group_by(Site,Month) %>% get_summary_stats(show = c("mean", "sd", "se")) %>% 
  ggplot(aes(x=Month, y=mean, colour=Site, group=Site)) + 
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), 
                width = .25, size = 0.75) +
  geom_line(size = 0.75) +
  geom_point(size = 2.5) +
  labs(y= expression("Average Oocyte Diameter ( " * mu * "m)"), 
       x = "Month") +
  scale_x_discrete(limits = month.abb) +
  scale_y_continuous(expand = c(0,0), limits=c(0,50), breaks = seq(0,50, by = 10)) +
  lemon::facet_rep_wrap(.~Site, labeller = labeller(Site = Sites))+
  scale_colour_manual(values = colors, labels = Sites)+
  Base + theme(legend.position = "none")+
  FacetBase + FacetSpace
#
#ggsave(path = "Output/", filename = "Fig15_Repro_Oocyte_Diameter_Month_Site.tiff",dpi=1000)
#
#
# Oocyte by stage
OOstagemean <- aggregate(oocytemean ~ Site + Stage, Repro, mean)
#Levene test for non-normal data
leveneTest(Repro$OocyteDiam, Repro$Stage, center = mean) #variances assumed to not be equal
# STATS
(oostage_kt <- kruskal.test(Repro$OocyteDiam ~ Repro$Stage))
oostage_kt %>% tidy() %>% as.data.frame() %>% dplyr::select(method, everything())
dunnTest(OocyteDiam ~ Stage, data = Repro, method = "bonferroni")
#
OocyteStagedata<- Repro %>% dplyr::select(Site, Stage, OocyteDiam) %>%
  group_by(Site, Stage) %>% get_summary_stats(show = c("mean", "sd", "se"))
(dunnTest(OocyteDiam ~ Stage, data = Repro, method = "bh"))$res
#
OocyteStagedata %>%
ggplot(aes(x=Stage, y=mean, fill=Site)) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se),
                size=.8,    # Thinner lines
                width=.25,
                position=position_dodge(.93)) +
  labs(y= expression("Average Oocyte Diameter ( " * mu * "m)"), 
       x = "Stage") +
  scale_y_continuous(expand = c(0,0), limits=c(0,40), breaks = seq(0,40, by = 10)) +
  scale_fill_manual(values = colors) +
  Base + 
  legend_config + guides(fill = guide_legend(nrow = 1))
#
#ggsave(path = "Output/", filename = "Fig16_Repro_Oocyte_Diameter_Stage_Site.tiff",dpi=1000)
#
#
## Condition Index ####
#
## Shell height
# STATS
(CISH_kt <- kruskal.test(Condition$SH_mm ~ Condition$Site)) 
CISH_kt %>% tidy() %>% as.data.frame() %>% dplyr::select(method, everything())
pairwise.wilcox.test(Condition$SH_mm, Condition$Site, p.adjust.method = "bonferroni") 
#
# MEANS
# By Site
(CISH_site <- Condition %>% dplyr::select(Site, SH_mm) %>%
    group_by(Site) %>% get_summary_stats(show = c("mean", "sd", "se")))
# By Site and Month
CICH_monthly_means <- Condition %>% dplyr::select(Site, Month, SH_mm) %>%
  group_by(Site, Month) %>% get_summary_stats(show = c("mean", "sd", "se")) %>%
  dplyr::select(-variable)
#
# Letters
CISHT <- (dunnTest(SH_mm ~ Site, data = Condition, method = "bh"))$res
(CISH_letters <- left_join(CISH_site,
                         cldList(comparison = CISHT$Comparison, p.value = CISHT$P.adj, threshold = 0.05) %>%
                           dplyr::select(Group, Letter) %>% rename("Site" = Group)))
#
## Plotting 
# By site
ggplot(Condition, aes(x = Site, y = SH_mm)) +
  geom_boxplot(fill = "#999999") +
  labs(y= "Average shell height (mm)", x = "Site") +
  scale_y_continuous(expand = c(0,0), limits=c(0,130), breaks = seq(0,120, by = 20)) +
  annotate("text", x=1:6, y=125, label=CISH_letters$Letter, family = "serif", size = 6) +
  Base
#
#ggsave(path = "Output/", filename = "Fig17_CI_ShellHeight_Site.tiff",dpi=1000)
#
#NPM manuscript used mean of monthly means:
CICH_monthly_means %>% 
  ggplot(aes(x = Site, y = mean)) +
  geom_boxplot() +
  labs(y= "Average shell height (mm)", x = "Site") +
  scale_y_continuous(expand = c(0,0), limits=c(0,105), breaks = seq(0,100, by = 20)) +
  annotate("text", x=1:6, y=100, label=CISH_letters$Letter, family = "serif", size = 6) +
  Base
# Monthly
CICH_monthly_means %>% 
  ggplot(aes(Month, mean, color = Site, group = Site))+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), 
                width = .25, size = 0.75) +
  geom_line(size = 0.75) +
  geom_point(size = 2.5) +
  scale_x_discrete(limits = month.abb) +
  scale_y_continuous(expand = c(0,0), limits=c(0,100), breaks = seq(0,100, by = 20)) +
  labs(y= "Average Shell Height (mm)", 
       x = "Month") +
  Base + 
  scale_colour_manual(values = colors)+ 
  legend_config+
  guides(color = guide_legend(nrow = 1))
#
#ggsave(path = "Output/", filename = "Fig18_Condition_ShellHeight_Month_Site.tiff",dpi=1000)
#
#
#
#
# ## Condition Index
# STATS
(CI_kt <- kruskal.test(Condition$Condition_Index ~ Condition$Site)) 
CI_kt %>% tidy() %>% as.data.frame() %>% dplyr::select(method, everything())
pairwise.wilcox.test(Condition$Condition_Index, Condition$Site, p.adjust.method = "bonferroni") 
#
# MEANS
# By Site
(CI_site <- Condition %>% dplyr::select(Site, Condition_Index) %>%
    group_by(Site) %>% get_summary_stats(show = c("mean", "sd", "se")))
# By Site and Month
CI_monthly_means <- Condition %>% dplyr::select(Site, Month, Condition_Index) %>%
  group_by(Site, Month) %>% get_summary_stats(show = c("mean", "sd", "se")) %>%
  dplyr::select(-variable)
#
# Letters
CIT <- (dunnTest(Condition_Index ~ Site, data = Condition, method = "bh"))$res
(CI_letters <- left_join(CI_site,
                           cldList(comparison = CIT$Comparison, p.value = CIT$P.adj, threshold = 0.05) %>%
                             dplyr::select(Group, Letter) %>% rename("Site" = Group)))
#
## Plotting 
# By site
ggplot(Condition, aes(x = Site, y = Condition_Index)) +
  geom_boxplot(fill = "#999999") +
  labs(y= "Condition Index", x = "Site") +
  scale_y_continuous(expand = c(0,0), limits=c(0,12), breaks = seq(0,12, by = 2)) +
  annotate("text", x=1:6, y=11.5, label=CISH_letters$Letter, family = "serif", size = 6) +
  Base
#
#ggsave(path = "Output/", filename = "Fig19_CI_Index_Site.tiff",dpi=1000)
#
#NPM manuscript used mean of monthly means:
CI_monthly_means %>% 
  ggplot(aes(x = Site, y = mean)) +
  geom_boxplot() +
  labs(y= "Condition Index", x = "Site") +
  scale_y_continuous(expand = c(0,0), limits=c(0,7), breaks = seq(0,7, by = 2)) +
  annotate("text", x=1:6, y=6.5, label=CISH_letters$Letter, family = "serif", size = 6) +
  Base
# Monthly
CI_monthly_means %>% 
  ggplot(aes(Month, mean, color = Site, group = Site))+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), 
                width = .25, size = 0.75) +
  geom_line(size = 0.75) +
  geom_point(size = 2.5) +
  scale_x_discrete(limits = month.abb) +
  scale_y_continuous(expand = c(0,0), limits=c(0,7), breaks = seq(0,7, by = 2)) +
  labs(y= "Average condition index", 
       x = "Month") +
  Base + 
  scale_colour_manual(values = colors)+ 
  legend_config+
  guides(color = guide_legend(nrow = 1))
#
#ggsave(path = "Output/", filename = "Fig20_Condition_Index_Month_Site.tiff",dpi=1000)
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
