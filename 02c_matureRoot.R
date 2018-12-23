library(ggplot2)
library(tidyverse)
library(reshape)
library(agricolae)
library(ggthemes)

#subset data to only include xylem vessels larger than 20 um in diameter (Rincon et al 2003)
matureXylem <- read.csv("02c_matureRoot_analysis/xylem_vessel_size_v2.csv", header = TRUE, sep = ',', stringsAsFactors=FALSE)


# subset data to only include xylem vessels larger than 20 um in diameter (Rincon et al 2003)
matureXylem_filterDiam <- matureXylem %>% filter(diameter >= 20)
write.table(matureXylem_filterDiam,file = "02c_matureRoot_analysis/xylem_vessel_size_v2_FilteredByDiameter_atLeast20.txt",
            quote = F,sep = "\t",row.names = F)

#### Set up graphical parameters
########################
dodge <- position_dodge(width = 1)  #### Set up dodge to align violin and boxplot; ## A larger dodge width, the more separation.
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7") #Custom palette


#### Convert some terms as factors
########################
head(matureXylem_filterDiam)

matureXylem_filterDiam$CO2 <- as.factor(matureXylem_filterDiam$CO2)
matureXylem_filterDiam$tissue <- as.factor(matureXylem_filterDiam$tissue)
matureXylem_filterDiam$tissue <- relevel(matureXylem_filterDiam$tissue,ref = "primary")

## Rename species
matureXylem_filterDiam$Species <- as.factor(matureXylem_filterDiam$Species)
levels(matureXylem_filterDiam$Species)=c("Sly","Spe")

### Remove bad samples
matureXylem_filterDiam <- matureXylem_filterDiam %>% filter(notes == "")
dim(matureXylem_filterDiam)

### Preprocess data
########################

### Group by sample/vessel number
#Find the number of vessels for each image by getting the max of vessel_no
matureXylem_filterDiam <- matureXylem_filterDiam %>% 
  select(sample.number,vessel_no) %>% 
  group_by(sample.number) %>% summarise("MaxVessel"=max(vessel_no)) %>% left_join(matureXylem_filterDiam,.,by = "sample.number")

head(matureXylem_filterDiam)
###### summary of Area
matureXylem_filterDiam_byGroup <- group_by(matureXylem_filterDiam, sample.number)
matureXylem_filterDiam_byGroup_Areasummary <- summarise(matureXylem_filterDiam_byGroup,
                                                        avg_area = mean(Area, na.rm = TRUE),
                                                        max_area = max(Area, na.rm = TRUE),
                                                        min_area = min(Area, na.rm = TRUE),
                                                        sum_area = sum(Area, na.rm = TRUE))

matureXylem_filterDiam <- matureXylem_filterDiam_byGroup_Areasummary %>% left_join(matureXylem_filterDiam,.,by = "sample.number")
########################################################################

####################
########## Factors to test: Area, vessel number, avg_area and MaxVessel of primary/lateral root

##### Average area#############################################
matureXylem_filterDiam_primary_avg_area <- matureXylem_filterDiam %>% 
  filter(tissue %in% "primary") %>%  #Only primary root
  #Remove duplicated avg_area values (since we're going with the average, leaving unfiltered would affect the anova)
  distinct(sample.number,.keep_all = T)
aovResult <- matureXylem_filterDiam_primary_avg_area %>% aov(avg_area ~ CO2*Species, data = .) %>% summary()
write.table(file = "02c_1_mature-xylem_primaryRoot_avg_area_CO2-Species_response.txt",
            signif(aovResult[[1]],3),
            sep = "\t",quote = F,col.names = NA)

matureXylem_filterDiam_lateral_avg_area <- matureXylem_filterDiam %>% 
  filter(tissue %in% "lateral") %>%  #Only primary root
  #Remove duplicated avg_area values (since we're going with the average, leaving unfiltered would affect the anova)
  distinct(sample.number,.keep_all = T)

aovResult <- matureXylem_filterDiam_lateral_avg_area %>% aov(avg_area ~ CO2*Species, data = .) %>% summary()
write.table(file = "02c_2_mature-xylem_lateralRoot_avg_area_CO2-Species_response.txt",
            signif(aovResult[[1]],3),
            sep = "\t",quote = F,col.names = NA)

aovResult <- matureXylem_filterDiam %>% distinct(avg_area,.keep_all = T) %>% aov(avg_area ~ CO2*Species*tissue, data = .) %>% summary()
# write.table(file = "02c_matureRoot_analysis/mature-xylem_3way_avg_area_CO2-Species_response.txt",
#             signif(aovResult[[1]],3),
#             sep = "\t",quote = F,col.names = NA)


##### Number of vessels
#############################################
aovResult <- matureXylem_filterDiam %>% distinct(sample.number,.keep_all = T) %>% 
  aov(MaxVessel ~ CO2*Species*tissue, data = .) %>% summary()
write.table(file = "02c_3_mature-xylem_3way_MaxVesselNumber_CO2-Species_response.txt",
            signif(aovResult[[1]],3),
            sep = "\t",quote = F,col.names = NA)

# aovResult <- matureXylem_filterDiam %>% #distinct(sample.number,.keep_all = T) %>%
#   aov(Area ~ CO2*Species*tissue, data = .) %>% summary()
# write.table(file = "02c_matureRoot_analysis/mature-xylem_Area_3way_MaxVesselNumber_CO2-Species_response.txt",
#             aovResult[[1]],
#             sep = "\t",quote = F,col.names = NA)


#pdf("figure02c_matureMetaxylem_CO2response_bySpecies-Tissue_ggplot2.pdf",paper = "a4r")
pdf("figure02c_matureMetaxylem_CO2response_bySpecies-Tissue_ggplot2.pdf", width = 10,height = 12)
####### Metaxylem diameter
### Add groups to the data
matureXylem_filterDiam$Groups <- paste(matureXylem_filterDiam$Species,matureXylem_filterDiam$CO2,matureXylem_filterDiam$tissue,
                                       sep="_")
matureXylem_filterDiam$Groups <- factor(matureXylem_filterDiam$Groups,
                                        #levels = unique(gtools::mixedsort(matureXylem_filterDiam$Groups)))
                                        levels = unique(matureXylem_filterDiam$Groups))


########## Extra data for plots
### Summarize diameter for line plot
meanAvgArea <- matureXylem_filterDiam %>% distinct(sample.number,.keep_all = T) %>% group_by(Groups) %>% 
  summarise("meanAvgArea"=mean(avg_area))

### Summarize for Tukey 
maxAvgArea = matureXylem_filterDiam %>% group_by(Groups) %>% summarize(maxAvgArea=max(avg_area))

### Do tukey tests

aovFit <- matureXylem_filterDiam %>% distinct(sample.number,.keep_all = T) %>% group_by(Groups) %>% aov(avg_area~Groups,data=.)
hsd_avgarea=HSD.test(aovFit, "Groups", group=T)
hsd_avgarea$groups <- hsd_avgarea$groups[levels(matureXylem_filterDiam$Groups),]
tukeyAvgArea <- data.frame(rownames(hsd_avgarea$groups),
                           do.call("rbind",strsplit(rownames(hsd_avgarea$groups),"_")),
                           hsd_avgarea$groups,stringsAsFactors = T)
colnames(tukeyAvgArea) <- c("Groups","Species","CO2","Tissue","mean_avg_area","groups")

tukeyAvgArea <- left_join(tukeyAvgArea,meanAvgArea,by="Groups")
head(tukeyAvgArea)



plotGG <- matureXylem_filterDiam %>% distinct(sample.number,.keep_all = T) %>%
  ggplot(.,mapping = aes(x=Groups,y=avg_area)) +
  scale_fill_manual(values=c("skyblue1", "black")) + scale_color_manual(values=c("skyblue1", "black")) +
  geom_violin(aes(color=paste(CO2)),position=position_dodge(1),trim = T,width=1) + 
  #geom_boxplot(aes(color=paste(CO2)),position=position_dodge(1),width=0.09,outlier.shape = NA) + 
  geom_tufteboxplot(position=position_dodge(1),outlier.colour="transparent", width = 1,color="black") + 
  #geom_dotplot(aes(fill=CO2),binaxis='y', stackdir='center',position=position_dodge(1),dotsize = 0.5)  + #Looks cleaner than with
  #geom_jitter(aes(color=paste(CO2)),width = 0.05,size=0.75) + 
  #geom_line(data=tukeyAvgArea, aes(x=Groups, y=mean_avg_area,group = paste(CO2,Species),color=CO2),alpha=0.5) +
  #geom_text(data=tukeyAvgArea, aes(x=Groups, y=(maxAvgArea$maxAvgArea)*1.45,label=groups,group=NULL),vjust=0,color="black") +
  facet_grid(.~Species,scales = "free_x",as.table = T) +
  theme(aspect.ratio = 12/16) +
  #coord_cartesian(ylim = c(min(metaxylem_diam$metaxylem.diam)*.6, max(metaxylem_diam$metaxylem.diam)*1.5)) +
  #theme_tufte(base_size = 15) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("metaxylem average area change by species in response to eCO2 over time") + 
  ylab(expression(paste("metaxylem average area change in response to eCO2 (",mu,"m"^"2",")"))) 

plotGG + theme_bw() + theme(aspect.ratio = 12/16) 
plotGG + theme_bw() + theme(aspect.ratio = 12/16) + 
  geom_text(data=tukeyAvgArea, aes(x=Groups, y=(maxAvgArea$maxAvgArea)*1.45,label=groups,group=NULL),vjust=0,color="black")
plotGG + theme_bw() + theme(aspect.ratio = 12/16) + 
  geom_text(data=tukeyAvgArea, aes(x=Groups, y=max(maxAvgArea$maxAvgArea)*1.45,label=groups,group=NULL),vjust=0,color="black")

### Summarize max number of vessels for line plot
meanMaxVessel <- matureXylem_filterDiam %>% 
  distinct(sample.number,.keep_all = T) %>% 
  group_by(Groups) %>% 
  summarise("meanMaxVessel"=mean(MaxVessel))

### Summarize for Tukey 
absoluteMaxVessel = matureXylem_filterDiam %>% group_by(Groups) %>% summarize(absoluteMaxVessel=max(MaxVessel))

### Do tukey tests
aovFit <- matureXylem_filterDiam %>% distinct(sample.number,.keep_all = T) %>% group_by(Groups) %>% aov(MaxVessel~Groups,data=.)
hsd_MaxVessel=HSD.test(aovFit, "Groups", group=T)
hsd_MaxVessel$groups <- hsd_MaxVessel$groups[levels(matureXylem_filterDiam$Groups),]
tukeyMaxVessel <- data.frame(rownames(hsd_MaxVessel$groups),
                             do.call("rbind",strsplit(rownames(hsd_MaxVessel$groups),"_")),
                             hsd_MaxVessel$groups,stringsAsFactors = T)
colnames(tukeyMaxVessel) <- c("Groups","Species","CO2","Tissue","mean_MaxVessel","groups")

tukeyMaxVessel <- left_join(tukeyMaxVessel,meanMaxVessel,by="Groups")
head(tukeyMaxVessel)


plotGG <- matureXylem_filterDiam %>% distinct(sample.number,.keep_all = T) %>%
  ggplot(.,mapping = aes(x=Groups,y=MaxVessel)) +
  scale_fill_manual(values=c("skyblue1", "black")) + scale_color_manual(values=c("skyblue1", "black")) +
  geom_violin(aes(color=paste(CO2)),position=position_dodge(1),trim = T,width=1) + 
  #geom_boxplot(aes(color=paste(CO2)),position=position_dodge(1),width=0.09,outlier.shape = NA) + 
  geom_tufteboxplot(position=position_dodge(1),outlier.colour="transparent", width = 1,color="black") + 
  #geom_dotplot(aes(fill=CO2),binaxis='y', stackdir='center',position=position_dodge(1),dotsize = 0.5)  + #Looks cleaner than with
  #geom_jitter(aes(color=paste(CO2)),width = 0.05,size=0.75) + 
  #geom_line(data=tukeyAvgArea, aes(x=Groups, y=mean_avg_area,group = paste(CO2,Species),color=CO2),alpha=0.5) +
  #geom_text(data=tukeyMaxVessel, aes(x=Groups, y=(absoluteMaxVessel$absoluteMaxVessel)*1.45,label=groups,group=NULL),vjust=0,color="black") +
  facet_grid(.~Species,scales = "free_x",as.table = T) +
  theme(aspect.ratio = 12/16) +
  #coord_cartesian(ylim = c(min(metaxylem_diam$metaxylem.diam)*.6, max(metaxylem_diam$metaxylem.diam)*1.5)) +
  #theme_tufte(base_size = 15) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("metaxylem max vessel change by species in response to eCO2 over time") + 
  ylab(expression(paste("Maximum number of vessels change in response to eCO2","")))


plotGG + theme_bw() + theme(aspect.ratio = 12/16) 
plotGG + theme_bw() + theme(aspect.ratio = 12/16) + 
  geom_text(data=tukeyMaxVessel, aes(x=Groups, y=(absoluteMaxVessel$absoluteMaxVessel)*1.45,label=groups,group=NULL),vjust=0,color="black")
plotGG + theme_classic() + theme(aspect.ratio = 12/16) + 
  geom_text(data=tukeyMaxVessel, aes(x=Groups, y=max(absoluteMaxVessel$absoluteMaxVessel)*1.45,label=groups,group=NULL),vjust=0,color="black")

par(mfrow=c(2,2))
plot(hsd_avgarea,las=2,main="HSD_AverageArea")
plot(hsd_MaxVessel,las=2,main="HSD_MaxVesselNumber")
par(mfrow=c(2,2))
dev.off()
