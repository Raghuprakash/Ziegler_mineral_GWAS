
setwd("~/Ziegler_mineral_GWAS/data")
Leaf_nutt <-read.csv("Leaf_nutrient_data.csv",header=T,na.strings=NA)
head(Leaf_nutt)
str(Leaf_nutt)

Leaf_nutt$PlotID <-as.factor(Leaf_nutt$PlotID)

library("ggplot2")
#install.packages("ggpval")
library("ggpval")
attach(Leaf_nutt)

Dry_Weight<- Leaf_nutt$DW
Treatment <- Leaf_nutt$Trt
Chlorophyll<-Leaf_nutt$CHL

p1 <- ggplot(Leaf_nutt,aes(x=Dry_Weight, colour=Treatment)) + geom_density() +

#add_pval(test, pairs =c(3,2), test='wilcox.test')
#str(test)

   ylab("Density") +
   xlab("Dry_Weight(g)")+
 # ggtitle("Density plot of dryweight\n under N treatment") +
 # theme(plot.title = element_text(hjust = 0.5)) +
#theme(plot.title=element_text(size=12, face="bold"),
  theme(axis.title.y=element_text(size = 10, vjust=+0.2, face="bold"),
      axis.title.x=element_text(size = 10, vjust=-0.2, face="bold"),
      axis.text.y=element_text(size = 10, face="bold"),
      axis.text.x=element_text(size = 10,face="bold"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank())

#ggplot(Leaf_nutt,aes(x=Dry_Weight, fill=Chlorophyll)) +geom_density(alpha=.3)  

  # alternative, a little easier
  
#install.packages("gridExtra") 
#library(gridExtra)

p2 <- ggplot(Leaf_nutt,aes(x=CHL, colour=Treatment)) + geom_density() +
  ylab("Density") +
  xlab("Chlorophyll (umol/m2)")+
  #ggtitle("Density plot of chlorophyll\n under N treatment") +
  #theme(plot.title = element_text(hjust = 0.5))+
  #theme(plot.title=element_text(size=12, face="bold"),
  theme(axis.title.y=element_text(size = 10, vjust=+0.2, face="bold"),
        axis.title.x=element_text(size = 10, vjust=-0.2, face="bold"),
        axis.text.y=element_text(size = 10, face="bold"),
        axis.text.x=element_text(size = 10,face="bold"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

#p3 <- grid.arrange(p1, p2, nrow=1)
p3 <- ggplot(Leaf_nutt,aes(x=FW, colour=Treatment)) + geom_density() +
  ylab("Density") +
  xlab("Fresh_Weight(g)")+
  #ggtitle("Density plot of fresh weight\n under N treatment") +
  #theme(plot.title = element_text(hjust = 0.5))+
  #theme(plot.title=element_text(size=12, face="bold"),
  theme(axis.title.y=element_text(size = 10, vjust=+0.2, face="bold"),
        axis.title.x=element_text(size = 10, vjust=-0.2, face="bold"),
        axis.text.y=element_text(size = 10, face="bold"),
        axis.text.x=element_text(size = 10,face="bold"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


p4 <- ggplot(Leaf_nutt,aes(x=LA, colour=Treatment)) + geom_density() +
  ylab("Density") +
  xlab("Leaf_Area(cm2)")+
  #ggtitle("Density plot of leaf area\n under N treatment") +
  #theme(plot.title = element_text(hjust = 0.5))+
  #theme(plot.title=element_text(size=12, face="bold"),
  theme(axis.title.y=element_text(size = 10, vjust=+0.2, face="bold"),
        axis.title.x=element_text(size = 10, vjust=-0.2, face="bold"),
        axis.text.y=element_text(size = 10, face="bold"),
        axis.text.x=element_text(size = 10,face="bold"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

#p3 <- grid.arrange(p1, p2, nrow=1)
panel1 <- grid.arrange(p1,
             p2,
             p3,
             p4,
             top = textGrob("Phenotypic_data_CRRI-2018",
                            gp=gpar(fontsize=16,fontface="bold")),
             nrow=2)

p5 <- ggplot(Leaf_nutt,aes(x=N, colour=Treatment)) + geom_density() +
  ylab("Density") +
  xlab("Nitrogen(%)")+
  #ggtitle("Density plot of nitrogen\n under N treatment") +
  #theme(plot.title = element_text(hjust = 0.5))+
  #theme(plot.title=element_text(size=12, face="bold"),
  theme(axis.title.y=element_text(size = 10, vjust=+0.2, face="bold"),
       axis.title.x=element_text(size = 10, vjust=-0.2, face="bold"),
       axis.text.y=element_text(size = 10, face="bold"),
        axis.text.x=element_text(size = 10,face="bold"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

p5
p6 <- ggplot(Leaf_nutt,aes(x=Fe, colour=Treatment)) + geom_density() +
  ylab("Density") +
  xlab("Iron (ppm)")+
  #ggtitle("Density plot of nitrogen\n under N treatment") +
  #theme(plot.title = element_text(hjust = 0.5))+
  #theme(plot.title=element_text(size=12, face="bold"),
  theme(axis.title.y=element_text(size = 10, vjust=+0.2, face="bold"),
        axis.title.x=element_text(size = 10, vjust=-0.2, face="bold"),
        axis.text.y=element_text(size = 10, face="bold"),
        axis.text.x=element_text(size = 10,face="bold"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

p7 <- ggplot(Leaf_nutt,aes(x=Zn, colour=Treatment)) + geom_density() +
  ylab("Density") +
  xlab("Zinc (ppm)")+
  #ggtitle("Density plot of nitrogen\n under N treatment") +
  #theme(plot.title = element_text(hjust = 0.5))+
  #theme(plot.title=element_text(size=12, face="bold"),
  theme(axis.title.y=element_text(size = 10, vjust=+0.2, face="bold"),
        axis.title.x=element_text(size = 10, vjust=-0.2, face="bold"),
        axis.text.y=element_text(size = 10, face="bold"),
        axis.text.x=element_text(size = 10,face="bold"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

p8 <- ggplot(Leaf_nutt,aes(x=LA, colour=Treatment)) + geom_density() +
  ylab("Density") +
  xlab("Copper (ppm)")+
  #ggtitle("Density plot of nitrogen\n under N treatment") +
  #theme(plot.title = element_text(hjust = 0.5))+
  #theme(plot.title=element_text(size=12, face="bold"),
  theme(axis.title.y=element_text(size = 10, vjust=+0.2, face="bold"),
        axis.title.x=element_text(size = 10, vjust=-0.2, face="bold"),
        axis.text.y=element_text(size = 10, face="bold"),
        axis.text.x=element_text(size = 10,face="bold"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

#library(grid)

panel2 <- grid.arrange(p5,
             p6,
             p7,
             p8,
             top = textGrob("Leaf Mineral Concentrations",
                            gp=gpar(fontsize=16,fontface="bold")),
             nrow=2)

grid.arrange(panel1,panel2,nrow=2)
install.packages("grid")


