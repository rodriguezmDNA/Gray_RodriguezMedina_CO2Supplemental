library(lmerTest)
library(ggplot2)
library(tidyverse)
library(reshape)
library(ggthemes)

files_datasets <- list.files(path = "01b_photosynthesis", pattern="csv",full.names = T)
files_datasets

### Seems that data was gathered over 3 consecutive days
datasets <- lapply(files_datasets,function(x){ 
  read.csv(x,header = TRUE, sep = ',', stringsAsFactors=FALSE)
})
sapply(datasets, dim)
LiCor <- do.call("rbind",datasets)
colnames(LiCor)[4] <- "CO2"

colnames(LiCor)
#View(LiCor)
table(LiCor$tray) #8 trays
table(LiCor$plant) #4 plants (bioreps?)
table(LiCor$date) #3 timepoints
###### Of all the measurements in the table we're rather interested in 2: Photosynthetic rate (Photo) and conductance (Cond)

#### Convert some terms as factors
########################
LiCor$CO2 <- as.factor(LiCor$CO2)
LiCor$species <- as.factor(LiCor$species)
levels(LiCor$species)=c("Sly","Spe")


#### Set up graphical parameters
########################
dodge <- position_dodge(width = 1)  #### Set up dodge to align violin and boxplot; ## A larger dodge width, the more separation.
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7") #Custom palette

#### ANOVA
########################
### With random effects
photo_mm_anova <- anova(lmer(Photo~CO2*species+(1|date),data = LiCor))
write.table(file = "01b_photosynthesis/summary_mixedmodel_anova_Photosynthesis_CO2-species_DayRandom.txt",
            signif(photo_mm_anova,4),
            sep = "\t",quote = F,col.names = NA)

conductance_mm_anova <- anova(lmer(Cond~CO2*species+(1|date),data = LiCor))
write.table(file = "01b_photosynthesis/summary_mixedmodel_anova_StomatalConductance_CO2-species_DayRandom.txt",
            signif(conductance_mm_anova,4),
            sep = "\t",quote = F,col.names = NA)


write.table(file = "01b_photosynthesis/01_LiCorRawData.txt",
            LiCor,
            sep = "\t",quote = F,col.names = NA)


pdf("figure01x_mixedModel-violinPlots_photosyntheticRate-conductance_CO2response_bySpecies_RandomDate_ggplot2.pdf",paper = "a4r")

LiCor %>% 
  ggplot(.,mapping = aes(x=paste(species,CO2),y=Photo)) +
  #scale_fill_manual(values=c("skyblue1", "black")) + 
  scale_color_manual(values=c("#ee9423","#ee23e3","#9423ee",
                              "skyblue1", "black")) +
  geom_violin(position = dodge,trim = F, aes(color=CO2),size=1) + 
  #geom_tufteboxplot(size=2,width=.5, position = dodge,outlier.colour=NA,aes(color=CO2)) +
  geom_boxplot(width=.15, position = dodge,outlier.colour=NA,aes(color=CO2)) +
  geom_jitter(aes(shape=date,color=date), position = position_dodge(width = 0.25),size=1.5) + 
  
  facet_grid(.~species,scales = "free_x") + # Same scale
  theme(aspect.ratio = 1/1) +
  ggtitle("Photosynthetic rate by species in response to eCO2") +  #ylab("Photosynthetic C Assimilation") + 
  ylab(expression(paste("Photosynthetic C Assimilation μmol", " ", CO[2], " ", m^-2, " ", s^-1, sep=""))) +
  xlab("Species and treatment") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


LiCor %>% #distinct(sample.number,.keep_all = T) %>%
  ggplot(.,mapping = aes(x=paste(species,CO2),y=Photo)) +
  scale_fill_manual(values=c("skyblue1", "black")) + scale_color_manual(values=c("skyblue1", "black")) +
  geom_violin(position = dodge,trim = F, aes(color=CO2),size=1) + 
  #geom_jitter(aes(shape=date,color=date), position = position_dodge(width = 0.25)) + 
  #geom_boxplot(width=.05, position = dodge,outlier.colour=NA) +
  geom_tufteboxplot(size=2,width=.5, position = dodge,outlier.colour=NA,aes(color=CO2)) +
  facet_grid(.~species,scales = "free_x") + # Same scale
  theme(aspect.ratio = 1/1) +
  ggtitle("Photosynthetic rate by species in response to eCO2") +  #ylab("Photosynthetic C Assimilation") + 
  ylab(expression(paste("Photosynthetic C Assimilation μmol", " ", CO[2], " ", m^-2, " ", s^-1, sep=""))) +
  xlab("Species and treatment") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_line(data = LiCor %>% #distinct(sample.number,.keep_all = T) %>% 
              group_by(CO2,species) %>% summarise("mw"=mean(Photo)), 
             aes(y = mw, group = species),alpha=1)


####### Conductance
LiCor %>% 
  ggplot(.,mapping = aes(x=paste(species,CO2),y=Cond)) +
  #scale_fill_manual(values=c("skyblue1", "black")) + 
  scale_color_manual(values=c("#ee9423","#ee23e3","#9423ee",
                              "skyblue1", "black")) +
  geom_violin(position = dodge,trim = F, aes(color=CO2),size=1) + 
  #geom_tufteboxplot(size=2,width=.5, position = dodge,outlier.colour=NA,aes(color=CO2)) +
  geom_boxplot(width=.15, position = dodge,outlier.colour=NA,aes(color=CO2)) +
  geom_jitter(aes(shape=date,color=date), position = position_dodge(width = 0.25),size=1.5) + 
  
  facet_grid(.~species,scales = "free_x") + # Same scale
  theme(aspect.ratio = 1/1) +
  ggtitle("Stomatal Conductance rate by species in response to eCO2") + 
  ylab(expression(paste("Stomatal Conductance mmol", " ", " ", m^-2, " ", s^-1, sep=""))) +
  xlab("Species and treatment") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


LiCor %>% #distinct(sample.number,.keep_all = T) %>%
  ggplot(.,mapping = aes(x=paste(species,CO2),y=Cond)) +
  scale_fill_manual(values=c("skyblue1", "black")) + scale_color_manual(values=c("skyblue1", "black")) +
  geom_violin(position = dodge,trim = F, aes(color=CO2),size=1) + 
  #geom_jitter(aes(shape=date,color=date), position = position_dodge(width = 0.25)) + 
  #geom_boxplot(width=.05, position = dodge,outlier.colour=NA) +
  geom_tufteboxplot(size=2,width=.5, position = dodge,outlier.colour=NA,aes(color=CO2)) +
  facet_grid(.~species,scales = "free_x") + # Same scale
  theme(aspect.ratio = 1/1) +
  ggtitle("Stomatal Conductance rate by species in response to eCO2") + 
  ylab(expression(paste("Stomatal Conductance mmol", " ", " ", m^-2, " ", s^-1, sep=""))) +
  xlab("Species and treatment") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_line(data = LiCor %>% #distinct(sample.number,.keep_all = T) %>% 
              group_by(CO2,species) %>% summarise("mw"=mean(Cond)), 
            aes(y = mw, group = species),alpha=1)
dev.off()
