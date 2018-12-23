library(ggplot2)
library(tidyverse)
library(reshape)


biomass <- read.csv("01a_root_shoot_biomass_analysis/Rep_6_root_and_shoot_biomass.csv", 
                    header = TRUE, sep = ',', stringsAsFactors=FALSE)

#### Convert some terms as factors
########################
# As factor & rename levels
biomass$CO2 <- as.factor(biomass$CO2)
biomass$species <- as.factor(biomass$species)
biomass$date.collected <- as.factor(biomass$date.collected)
levels(biomass$CO2)
## Rename levels
levels(biomass$CO2)=c("ambient","elevated")
levels(biomass$species)=c("Sly","Spe")
##
str(biomass)


#### Reshape to use with ggplot
########################
biomass_melt <- melt(biomass,measure.vars  = c("root_mass_g","shoot_mass_g","root_shoot"))
levels(biomass_melt$variable)=c("root_mass_g","shoot_mass_g","root_shoot_ratio")


#### Set up graphical parameters
########################
dodge <- position_dodge(width = 1)  #### Set up dodge to align violin and boxplot; ## A larger dodge width, the more separation.
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7") #Custom palette


#### Filter
########################
mass_shoot <- biomass_melt %>% filter(variable %in% "shoot_mass_g")

mass_root <- biomass_melt %>% filter(variable %in% "root_mass_g")
mass_root2shootratio <- biomass_melt %>% filter(variable %in% "root_shoot_ratio")

#### Do stats!
########################
root_biomass_aov <- aov(root_mass_g ~ CO2*species, data = biomass)
summary(root_biomass_aov)

shoot_biomass_aov <- aov(shoot_mass_g ~ CO2*species, data = biomass)
summary(shoot_biomass_aov)

RS_biomass_aov <- aov(root_shoot ~ CO2*species, data = biomass)
summary(RS_biomass_aov)



write.table(file = "01a_root_shoot_biomass_analysis/summary_anova_root_biomass_CO2-species.txt",
            signif(summary(root_biomass_aov)[[1]],3),
            sep = "\t",quote = F,col.names = NA)
write.table(file = "01a_root_shoot_biomass_analysis/summary_anova_shoot_biomass_CO2-species.txt",
            signif(summary(shoot_biomass_aov)[[1]],3),
            sep = "\t",quote = F,col.names = NA)
write.table(file = "01a_root_shoot_biomass_analysis/summary_anova_RSratio_biomass_CO2-species.txt",
            signif(summary(RS_biomass_aov)[[1]],3),
            sep = "\t",quote = F,col.names = NA)



#### Make plots
########################
library(ggthemes)
pdf("figure01a_violinPlots_biomass_CO2response_bySpecies_ggplot2_v2.pdf",paper = "a4r")
mass_shoot %>% mutate(Groups=as.factor(paste0(mass_shoot$species,mass_shoot$CO2))) %>%
  ggplot(mass_shoot,mapping = aes(x=Groups,y=value)) + 
  geom_violin(aes(color=paste(CO2)),position=position_dodge(1),trim = F,width=1) + 
  #geom_boxplot(width=.05, position = dodge,outlier.colour=NA) +
  geom_tufteboxplot(width=.05, position = dodge,outlier.colour=NA) +
  ggtitle("Shoot biomass response to eCO2") + ylab("Dry mass (g)") + 
  #facet_grid(.~species,scales = "free_x") + # Same scale 
  #geom_point(aes(y = value), color = "black", size = 2)
  ### Colors: 
  ##### Manual
  scale_fill_manual(values=c("skyblue1", "black")) + 
  scale_color_manual(values=c("skyblue1", "black")) +
  theme_bw() + theme(aspect.ratio = 12/16)  +
  NULL


mass_shoot %>% mutate(Groups=as.factor(paste0(mass_shoot$species,mass_shoot$CO2))) %>%
  ggplot(mass_shoot,mapping = aes(x=CO2,y=value)) + 
  geom_violin(aes(color=paste(CO2)),position=position_dodge(1),trim = F,width=1) + 
  #geom_boxplot(width=.05, position = dodge,outlier.colour=NA) +
  geom_tufteboxplot(aes(color=CO2),width=.05, position = dodge,outlier.colour=NA,size=1) +
  geom_line(data = mass_shoot %>% #distinct(sample.number,.keep_all = T) %>%
              mutate(Groups=as.factor(paste0(mass_shoot$species,mass_shoot$CO2))) %>%
              group_by(CO2,species) %>%
              summarise("mw"=mean(value)),
            aes(y = mw, group = species)) +
  ggtitle("Shoot biomass response to eCO2") + ylab("Dry mass (g)") + 
  facet_grid(.~species,scales = "free_x") + # Same scale
  #facet_grid(.~species,scales = "free_x") + # Same scale 
  #geom_point(aes(y = value), color = "black", size = 2)
  ### Colors: 
  ##### Manual
  scale_fill_manual(values=c("skyblue1", "black")) + 
  scale_color_manual(values=c("skyblue1", "black")) +
  theme_classic() +  #theme(aspect.ratio = 12/16)  +
  theme(aspect.ratio = 1/1) +
  NULL

mass_shoot %>% mutate(Groups=as.factor(paste0(mass_shoot$species,mass_shoot$CO2))) %>%
  ggplot(mass_shoot,mapping = aes(x=CO2,y=value)) + 
  geom_violin(aes(color=paste(CO2)),position=position_dodge(1),trim = F,width=1) + 
  #geom_boxplot(width=.05, position = dodge,outlier.colour=NA) +
  geom_tufteboxplot(aes(color=CO2),width=.05, position = dodge,outlier.colour=NA,size=1) +
  # geom_line(data = mass_shoot %>% #distinct(sample.number,.keep_all = T) %>%
  #             mutate(Groups=as.factor(paste0(mass_shoot$species,mass_shoot$CO2))) %>%
  #             group_by(CO2,species) %>%
  #             summarise("mw"=mean(value)),
  #           aes(y = mw, group = species)) +
  ggtitle("Shoot biomass response to eCO2") + ylab("Dry mass (g)") + 
  facet_grid(.~species,scales = "free_x") + # Same scale
  #facet_grid(.~species,scales = "free_x") + # Same scale 
  #geom_point(aes(y = value), color = "black", size = 2)
  ### Colors: 
  ##### Manual
  scale_fill_manual(values=c("skyblue1", "black")) + 
  scale_color_manual(values=c("skyblue1", "black")) +
  theme_classic() +  #theme(aspect.ratio = 12/16)  +
  theme(aspect.ratio = 1/1) +
  NULL

mass_root %>% mutate(Groups=as.factor(paste0(mass_shoot$species,mass_shoot$CO2))) %>%
  ggplot(mass_root,mapping = aes(x=Groups,y=value)) + 
  geom_violin(aes(color=paste(CO2)),position=position_dodge(1),trim = F,width=1) + 
  #geom_boxplot(width=.05, position = dodge,outlier.colour=NA) +
  geom_tufteboxplot(width=.05, position = dodge,outlier.colour=NA) +
  #facet_grid(.~species,scales = "free_x") + # Same scale 
  ggtitle("Root biomass response to eCO2") + ylab("Dry mass (g)") + 
  #geom_point(aes(y = value), color = "black", size = 2)
  ### Colors: 
  ##### Manual
  scale_fill_manual(values=c("skyblue1", "black")) + 
  scale_color_manual(values=c("skyblue1", "black")) +
  theme_bw() + theme(aspect.ratio = 12/16)  +
  NULL



mass_root %>% mutate(Groups=as.factor(paste0(mass_root$species,mass_root$CO2))) %>%
  ggplot(mass_root,mapping = aes(x=CO2,y=value)) + 
  geom_violin(aes(color=paste(CO2)),position=position_dodge(1),trim = F,width=1) + 
  #geom_boxplot(width=.05, position = dodge,outlier.colour=NA) +
  geom_tufteboxplot(aes(color=CO2),width=.05, position = dodge,outlier.colour=NA,size=1) +
  geom_line(data = mass_root %>% #distinct(sample.number,.keep_all = T) %>%
              mutate(Groups=as.factor(paste0(mass_root$species,mass_root$CO2))) %>%
              group_by(CO2,species) %>%
              summarise("mw"=mean(value)),
            aes(y = mw, group = species)) +
  ggtitle("Root biomass response to eCO2") + ylab("Dry mass (g)") + 
  facet_grid(.~species,scales = "free_x") + # Same scale
  #facet_grid(.~species,scales = "free_x") + # Same scale 
  #geom_point(aes(y = value), color = "black", size = 2)
  ### Colors: 
  ##### Manual
  scale_fill_manual(values=c("skyblue1", "black")) + 
  scale_color_manual(values=c("skyblue1", "black")) +
  theme_classic() +  #theme(aspect.ratio = 12/16)  +
  theme(aspect.ratio = 1/1) +
  NULL

mass_root %>% mutate(Groups=as.factor(paste0(mass_root$species,mass_root$CO2))) %>%
  ggplot(mass_root,mapping = aes(x=CO2,y=value)) + 
  geom_violin(aes(color=paste(CO2)),position=position_dodge(1),trim = F,width=1) + 
  #geom_boxplot(width=.05, position = dodge,outlier.colour=NA) +
  geom_tufteboxplot(aes(color=CO2),width=.05, position = dodge,outlier.colour=NA,size=1) +
  # geom_line(data = mass_root %>% #distinct(sample.number,.keep_all = T) %>%
  #             mutate(Groups=as.factor(paste0(mass_root$species,mass_root$CO2))) %>%
  #             group_by(CO2,species) %>%
  #             summarise("mw"=mean(value)),
  #           aes(y = mw, group = species)) +
  ggtitle("Root biomass response to eCO2") + ylab("Dry mass (g)") + 
  facet_grid(.~species,scales = "free_x") + # Same scale
  #facet_grid(.~species,scales = "free_x") + # Same scale 
  #geom_point(aes(y = value), color = "black", size = 2)
  ### Colors: 
  ##### Manual
  scale_fill_manual(values=c("skyblue1", "black")) + 
  scale_color_manual(values=c("skyblue1", "black")) +
  theme_classic() +  #theme(aspect.ratio = 12/16)  +
  theme(aspect.ratio = 1/1) +
  NULL

mass_root2shootratio %>% mutate(Groups=as.factor(paste0(mass_root2shootratio$species,mass_root2shootratio$CO2))) %>%
  ggplot(mass_root2shootratio,mapping = aes(x=Groups,y=value)) + 
  geom_violin(aes(color=paste(CO2)),position=position_dodge(1),trim = F,width=1) + 
  #geom_boxplot(width=.05, position = dodge,outlier.colour=NA) +
  geom_tufteboxplot(width=.05, position = dodge,outlier.colour=NA) +
  ggtitle("Root/Shoot biomass response to eCO2") + ylab("Dry mass (g)") + 
  #geom_point(aes(y = value), color = "black", size = 2)
  ### Colors: 
  ##### Manual
  scale_fill_manual(values=c("skyblue1", "black")) + 
  scale_color_manual(values=c("skyblue1", "black")) +
  theme_bw() + theme(aspect.ratio = 12/16)  +
  NULL



mass_root2shootratio %>% mutate(Groups=as.factor(paste0(mass_root2shootratio$species,mass_root2shootratio$CO2))) %>%
  ggplot(mass_root2shootratio,mapping = aes(x=CO2,y=value)) + 
  geom_violin(aes(color=paste(CO2)),position=position_dodge(1),trim = F,width=1) + 
  #geom_boxplot(width=.05, position = dodge,outlier.colour=NA) +
  geom_tufteboxplot(aes(color=CO2),width=.05, position = dodge,outlier.colour=NA,size=1) +
  geom_line(data = mass_root2shootratio %>% #distinct(sample.number,.keep_all = T) %>%
              mutate(Groups=as.factor(paste0(mass_root2shootratio$species,mass_root2shootratio$CO2))) %>%
              group_by(CO2,species) %>%
              summarise("mw"=mean(value)),
            aes(y = mw, group = species)) +
  ggtitle("Root/Shoot biomass response to eCO2") + ylab("Dry mass (g)") + 
  facet_grid(.~species,scales = "free_x") + # Same scale
  #facet_grid(.~species,scales = "free_x") + # Same scale 
  #geom_point(aes(y = value), color = "black", size = 2)
  ### Colors: 
  ##### Manual
  scale_fill_manual(values=c("skyblue1", "black")) + 
  scale_color_manual(values=c("skyblue1", "black")) +
  theme_classic() +  #theme(aspect.ratio = 12/16)  +
  theme(aspect.ratio = 1/1) +
  NULL

mass_root2shootratio %>% mutate(Groups=as.factor(paste0(mass_root2shootratio$species,mass_root2shootratio$CO2))) %>%
  ggplot(mass_root2shootratio,mapping = aes(x=CO2,y=value)) + 
  geom_violin(aes(color=paste(CO2)),position=position_dodge(1),trim = F,width=1) + 
  #geom_boxplot(width=.05, position = dodge,outlier.colour=NA) +
  geom_tufteboxplot(aes(color=CO2),width=.05, position = dodge,outlier.colour=NA,size=1) +
  # geom_line(data = mass_root2shootratio %>% #distinct(sample.number,.keep_all = T) %>%
  #             mutate(Groups=as.factor(paste0(mass_root2shootratio$species,mass_root2shootratio$CO2))) %>%
  #             group_by(CO2,species) %>%
  #             summarise("mw"=mean(value)),
  #           aes(y = mw, group = species)) +
  ggtitle("Root/Shoot biomass response to eCO2") + ylab("Dry mass (g)") + 
  facet_grid(.~species,scales = "free_x") + # Same scale
  #facet_grid(.~species,scales = "free_x") + # Same scale 
  #geom_point(aes(y = value), color = "black", size = 2)
  ### Colors: 
  ##### Manual
  scale_fill_manual(values=c("skyblue1", "black")) + 
  scale_color_manual(values=c("skyblue1", "black")) +
  theme_classic() +  #theme(aspect.ratio = 12/16)  +
  theme(aspect.ratio = 1/1) +
  NULL

allBiomass <- ggplot(biomass_melt,mapping = aes(x=species,y=value,color=CO2)) + 
  geom_violin(trim = FALSE,position = dodge) + 
  #geom_boxplot(width=.05, outlier.colour=NA, position = dodge) +
  geom_tufteboxplot(width=.05, outlier.colour=NA, position = dodge) +
  ggtitle("Shoot, Root and R/S biomass response to eCO2") + ylab("Dry mass (g)") + 
  #geom_boxplot(width = 0.1)
  #geom_point(aes(y = value), color = "black", size = 2)
  ### Colors: 
  ##### Manual
  # scale_fill_manual(values=cbPalette) +
  # scale_color_manual(values=cbPalette) +
  ##### brew
  #scale_fill_brewer(palette="Set1") + scale_color_brewer(palette="Dark2") + 
  scale_fill_manual(values=c("skyblue1", "black")) + scale_color_manual(values=c("skyblue1", "black")) + 
  facet_grid(variable~.,scales = "free_y")

# allBiomass + theme_minimal()
# allBiomass + theme_gray()
# allBiomass + theme_gray() + geom_dotplot(binaxis='y', stackdir='center',position=position_dodge(1),dotsize = 0.5)

# https://sebastiansauer.github.io/vis_interaction_effects/
biomass_melt %>% 
  ggplot() +
  aes(x = CO2, y = value, group = variable, color = CO2) +
  scale_color_manual(values=c("skyblue1", "black")) + 
  geom_point( alpha = .5) +
  geom_smooth(method = "lm",color="orange",size=0.8) +
  facet_grid(variable~species,scales = "free_y") +
  ggtitle("Shoot, Root and R/S biomass response to eCO2 (interaction viz)") + ylab("Dry mass (g)")

dev.off()




