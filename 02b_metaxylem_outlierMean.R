library(ggplot2)
library(tidyverse)
library(reshape)
library(agricolae)
library(ggthemes)

metaxylem_diam <- read.csv("02b_analysis_metaxylem/Reps_2thru5_metaxylem_diameter.csv", 
                           header = TRUE, sep = ',', stringsAsFactors=FALSE)
head(metaxylem_diam)
str(metaxylem_diam)


idx <- which (metaxylem_diam$Species %in% "M82" & 
                metaxylem_diam$Day %in% "10" & 
                metaxylem_diam$CO2 %in% "elevated")# & diam$image %in% "C" & diam$Rep %in% 3)

### Replace outlier by the mean of the group - outlier 
metaxylem_diam[idx[2],"Area"] <- mean(metaxylem_diam[idx[-2],"Area"])
metaxylem_diam[idx,]



# qplot(y = Area,x=Species,color=CO2, data = diam,geom = "violin",facets = .~Day,trim=F)
# qplot(y = Area,x=Species,color=CO2, data = diam,geom = "boxplot",facets = .~Day,width=0.5)

metaxylem_diam_melt <- melt(metaxylem_diam,measure.vars  = c("metaxylem.diam","Area","Angle"))
levels(metaxylem_diam_melt$variable) #=c("root_mass_g","shoot_mass_g","root_shoot_ratio")


#### Convert some terms as factors
########################
metaxylem_diam$CO2 <- as.factor(metaxylem_diam$CO2)
metaxylem_diam$Species <- as.factor(metaxylem_diam$Species)
levels(metaxylem_diam$Species)=c("Sly","Spe")
metaxylem_diam$Day <- as.factor(metaxylem_diam$Day)

#### Set up graphical parameters
########################
dodge <- position_dodge(width = 1)  #### Set up dodge to align violin and boxplot; ## A larger dodge width, the more separation.
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7") #Custom palette


#############################################

#### ANOVA
########################
## Anovas for diameter, area and angle


### Diameter
diameter_CO2_Species_Day_response <- aov(metaxylem.diam ~ Species*CO2*Day,data = metaxylem_diam)
summary(diameter_CO2_Species_Day_response)
write.table(file = "metaxylem-diameter_CO2-Species-Day_response_fixedOutlier.txt",
            signif(summary(diameter_CO2_Species_Day_response)[[1]],4),
            sep = "\t",quote = F,col.names = NA)

# diameter_CO2_Species_response <- aov(metaxylem.diam ~ Species*CO2,data = metaxylem_diam)
# summary(diameter_CO2_Species_response)
# write.table(file = "02b_analysis_metaxylem/metaxylem-diameter_CO2-Species_response.txt",
#             summary(diameter_CO2_Species_response)[[1]],
#             sep = "\t",quote = F,col.names = NA)

### Area
area_CO2_Species_Day_response <- aov(Area ~ Species*CO2*Day,data = metaxylem_diam)
summary(area_CO2_Species_Day_response)
write.table(file = "metaxylem-area_CO2-Species-Day_response_fixedOutlier.txt",
            signif(summary(area_CO2_Species_Day_response)[[1]],4),
            sep = "\t",quote = F,col.names = NA)

# area_CO2_Species_response <- aov(Area ~ Species*CO2,data = metaxylem_diam)
# summary(area_CO2_Species_response)
# write.table(file = "02b_analysis_metaxylem/metaxylem-area_CO2-Species_response.txt",summary(area_CO2_Species_response)[[1]],
#             sep = "\t",quote = F,col.names = NA)

### Angle
# angle_CO2_Species_Day_response <- aov(Angle ~ Species*CO2*Day,data = metaxylem_diam)
# summary(angle_CO2_Species_Day_response)
# write.table(file = "02b_analysis_metaxylem/metaxylem-angle_CO2-Species-Day_response.txt",summary(angle_CO2_Species_Day_response)[[1]],
#             sep = "\t",quote = F,col.names = NA)
# 
# angle_CO2_Species_response <- aov(Angle ~ Species*CO2,data = metaxylem_diam)
# summary(angle_CO2_Species_response)
# write.table(file = "02b_analysis_metaxylem/metaxylem-angle_CO2-Species_response.txt",summary(angle_CO2_Species_response)[[1]],
#             sep = "\t",quote = F,col.names = NA)


#### ggplots
########################

#pdf("figure02b_metaxylem_CO2response_bySpecies-Day_ggplot2.pdf",paper = "a4r")
pdf("figure02b_metaxylem_CO2response_bySpecies-Day_ggplot2_Tukey_fixedOutlier.pdf", width = 10,height = 12)

####### Metaxylem diameter
### Add groups to the data
metaxylem_diam$Groups <- paste(metaxylem_diam$Species,metaxylem_diam$CO2,metaxylem_diam$Day,sep="_")
metaxylem_diam$Groups <- factor(metaxylem_diam$Groups,levels = unique(gtools::mixedsort(metaxylem_diam$Groups)))


########## Extra data for plots
### Summarize diameter for line plot
meanDiam <- metaxylem_diam %>% distinct(sample.number,.keep_all = T) %>% group_by(Groups) %>% 
  summarise("meanDiameter"=mean(metaxylem.diam))

### Summarize for Tukey 
maxDiam = metaxylem_diam %>% group_by(Groups) %>% summarize(MaxDiameter=max(metaxylem.diam))

### Do tukey tests
hsd_diameter=HSD.test(aov(metaxylem.diam~Groups,data=metaxylem_diam), "Groups", group=T)
hsd_diameter$groups <- hsd_diameter$groups[levels(metaxylem_diam$Groups),]
tukeyDiameter <- data.frame(rownames(hsd_diameter$groups),
                            do.call("rbind",strsplit(rownames(hsd_diameter$groups),"_")),
                            hsd_diameter$groups,stringsAsFactors = T)
colnames(tukeyDiameter) <- c("Groups","Species","CO2","Day","median_diameter","groups")

tukeyDiameter <- left_join(tukeyDiameter,meanDiam,by="Groups")
head(tukeyDiameter)

plotGG <- ggplot(metaxylem_diam,aes(x=Groups,y=metaxylem.diam)) +
  scale_fill_manual(values=c("skyblue1", "black")) + scale_color_manual(values=c("skyblue1", "black")) +
  geom_violin(aes(color=paste(CO2)),position=position_dodge(1),trim = F,width=1) + 
  #geom_boxplot(aes(color=paste(CO2)),position=position_dodge(1),width=0.09,outlier.shape = NA) + 
  geom_tufteboxplot(position=position_dodge(1),outlier.colour="transparent", width = 1,color="black") + 
  #geom_dotplot(aes(fill=CO2),binaxis='y', stackdir='center',position=position_dodge(1),dotsize = 0.5)  + #Looks cleaner than with
  #geom_jitter(aes(color=paste(CO2)),width = 0.05,size=0.75) + 
  geom_line(data=tukeyDiameter, aes(x=Groups, y=median_diameter,group = paste(CO2,Species),color=CO2),alpha=0.5) +
  #geom_text(data=tukeyDiameter, aes(x=Groups, y=(maxDiam$MaxDiameter)*1.45,label=groups,group=NULL),vjust=0,color="black") +
  facet_grid(.~Species,scales = "free_x",as.table = T) +
  theme(aspect.ratio = 12/16) + 
  coord_cartesian(ylim = c(min(metaxylem_diam$metaxylem.diam)*.6, max(metaxylem_diam$metaxylem.diam)*1.5)) +
  #theme_tufte(base_size = 15) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("metaxylem diameter change by species in response to eCO2 over time") + 
  ylab(expression(paste("metaxylem diameter (",mu,"m)")))

plotGG + theme_classic() + theme(aspect.ratio = 12/16) 
plotGG + theme_classic() + theme(aspect.ratio = 12/16) + 
  geom_text(data=tukeyDiameter, aes(x=Groups, y=(maxDiam$MaxDiameter)*1.45,label=groups,group=NULL),vjust=0,color="black") 

plotGG + theme_classic() + theme(aspect.ratio = 12/16) + 
  geom_text(data=tukeyDiameter, aes(x=Groups, y=max(maxDiam$MaxDiameter)*1.45,label=groups,group=NULL),vjust=0,color="black") 

####### Area
#################################################


### Summarize diameter for line plot
meanArea <- metaxylem_diam %>% distinct(sample.number,.keep_all = T) %>% group_by(Groups) %>% summarise("meanArea"=mean(Area))

### Summarize for Tukey 
maxArea = metaxylem_diam %>% group_by(Groups) %>% summarize(MaxArea=max(Area))

### Do tukey tests
hsd_area=HSD.test(aov(Area~Groups,data=metaxylem_diam), "Groups", group=T)
hsd_area$groups <- hsd_area$groups[levels(metaxylem_diam$Groups),]
tukeyArea <- data.frame(rownames(hsd_area$groups),
                        do.call("rbind",strsplit(rownames(hsd_area$groups),"_")),
                        hsd_area$groups,stringsAsFactors = T)
colnames(tukeyArea) <- c("Groups","Species","CO2","Day","median_Area","groups")

tukeyArea <- left_join(tukeyArea,meanArea,by="Groups")
head(tukeyArea)

plotGG <- ggplot(metaxylem_diam,aes(x=Groups,y=Area)) +
  scale_fill_manual(values=c("skyblue1", "black")) + scale_color_manual(values=c("skyblue1", "black")) +
  geom_violin(aes(color=paste(CO2)),position=position_dodge(1),trim = F,width=1) + 
  #geom_boxplot(aes(color=paste(CO2)),position=position_dodge(1),width=0.09,outlier.shape = NA) + 
  geom_tufteboxplot(position=position_dodge(1),outlier.colour="transparent", width = 1,color="black") + 
  #geom_dotplot(aes(fill=CO2),binaxis='y', stackdir='center',position=position_dodge(1),dotsize = 0.5)  + #Looks cleaner than with
  #geom_jitter(aes(color=paste(CO2)),width = 0.05,size=0.75) + 
  geom_line(data=tukeyArea, aes(x=Groups, y=median_Area,group = paste(CO2,Species),color=CO2),alpha=0.5) +
  #geom_text(data=tukeyArea, aes(x=Groups, y=(maxArea$MaxArea)*1.45,label=groups,group=NULL),vjust=0,color="black") +
  facet_grid(.~Species,scales = "free_x",as.table = T) +
  theme(aspect.ratio = 12/16) + 
  #coord_cartesian(ylim = c(min(metaxylem_diam$metaxylem.diam)*.6, max(metaxylem_diam$metaxylem.diam)*1.5)) +
  #theme_tufte(base_size = 15) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("metaxylem area change by species in response to eCO2 over time") + 
  ylab(expression(paste("metaxylem area change in response to eCO2 (",mu,"m"^"2",")")))


plotGG + theme_classic() + theme(aspect.ratio = 12/16) 
plotGG + theme_classic() + theme(aspect.ratio = 12/16) + 
  geom_text(data=tukeyArea, aes(x=Groups, y=(maxArea$MaxArea)*1.45,label=groups,group=NULL),vjust=0,color="black")
plotGG + theme_classic() + theme(aspect.ratio = 12/16) + 
  geom_text(data=tukeyArea, aes(x=Groups, y=max(maxArea$MaxArea)*1.45,label=groups,group=NULL),vjust=0,color="black")

par(mfrow=c(2,2))
plot(hsd_area,las=2,main="HSD_Area")
plot(hsd_diameter,las=2,main="HSD_Diameter")
par(mfrow=c(2,2))
dev.off()











