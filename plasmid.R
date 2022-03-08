setwd("C:/Users/Admin/Desktop/CNR/Daphnia/dnase/HGT Daphnia_Bacteri/")

#degradation of plasmid and amplicon by daphnia
plasmid<-read.delim("plasmid_degradation.txt")
attach(plasmid)

plot(plasmidG~as.factor(start), pch=21, bg="blue", col="blue", cex=2)

abline(lmG)
s2<-start*start
lmG<-lm(log(plasmidG)~log(start+1))

summary(lmG)

plot(plasmidG,plasmidT)
cor.test(plasmidG,plasmidT)


plot(log(plasmidT)~start,pch=21, bg="blue", col="blue", cex=2)
abline(lmT, col="blue")
lmT<-lm(log(plasmidT)~start) 
#plot(lmT)
summary(lmT)

plot(ampliconG~start)
abline(lmAG, col="blue")
lmAG<-lm(log(ampliconG)~alive) 
plot(lmAG)
summary(lmAG)



plot(ampliconT~start)
abline(lmAT, col="blue")
lmAT<-lm(log(ampliconT)~start) 
summary(lmAT)

#Transformation

trans<-read.csv("transformation.csv", header = T)
colnames(trans)<-c("treatment", "1ng", "2ng")
library("car")
library("reshape2")
library("ggplot2")
library("emmeans")
mtra<-melt(trans)
head(mtra)

ggplot(mtra, aes(x=treatment, y=value,  fill=variable)) +
  geom_boxplot(size=0.75, alpha=0.5)  + 
  ylab("")+
  scale_fill_manual(values=c("#8c8c8c", "#ffffff"), name="plasmid concentration")+
  theme(axis.line= element_line(color="black"),legend.position = "bottom", legend.text=element_text(size=14), legend.title = element_text(size=14), axis.text.x = element_text(size = 10),axis.text.y = element_text(size = 10),
        axis.title.y = element_text(size = 14), axis.title.x = element_text(size = 12),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())
 

lmTra2<-lm(log(mtra$value+1)~mtra$treatment+mtra$variable)
Anova(lmTra2)
pairs(emmeans(lmTra2, ~ treatment+variable))

