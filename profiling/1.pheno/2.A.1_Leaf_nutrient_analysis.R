
setwd("~/Ziegler_mineral_GWAS/data")
Leaf_nutt <-read.csv("Leaf_nutrient_data.csv",header=T,na.strings=NA)
head(Leaf_nutt)
str(Leaf_nutt)

Leaf_nutt$PlotID <-as.factor(Leaf_nutt$PlotID)
