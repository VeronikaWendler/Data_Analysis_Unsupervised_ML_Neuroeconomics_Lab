
#Dimensions data


# instalation of the package to perform the analysis
install.packages("emmeans") 
install.packages("ggpubr")
install.packages("ggplot2")
install.packages("tidyverse")
install.packages("plotly")
install.packages("colorspace")
install.packages("ggsignif")
install.packages("gt")
install.packages("dplyr")
install.packages("broom")
install.packages("sjPlot")
install.packages("sjstats")
install.packages("xtable")
install.packages("dplyr")
install.packages("tidyr")
install.packages("sjPlot")
install.packages("brms")
install.packages("glmmTMB")
install.packages("MASS")
install.packages("gplots")
install.packages("car")
install.packages("stats")
install.packages("lattice")

#libs 

library("emmeans") 
library("ggpubr")
library("ggplot2")
library("tidyverse")
library("plotly")
library("colorspace")
library("ggsignif")
library("gt")
library("dplyr")
library("broom")
library(sjPlot)
library(sjstats)
library("xtable")
library("dplyr")
library("tidyr")
library("knitr")
library("sjPlot")
library("brms")
library("glmmTMB")
library("MASS")
library("gplots")
library(car)
library(stats)
library(lattice)





#rm(list = ls(all.names = TRUE))


.libPaths("C:/Users/Neuroeconomics Lab/Documents/R")
# read the csv file
my_data <- read.csv("C:/Users/Neuroeconomics Lab/Documents/R/Dime_Trans_M10_NEW.csv", head = TRUE, sep=",")
my_data
my_dataSub <- my_data[ , c("D1S1T",	"SelfRT",	"DIMT", "subIDT")]


# the factor are already defined but R needs to know them so we tell him ^^

my_data$grp <- factor(my_data$SelfR, levels = c(1,2,3), labels = c("S1","S2", "S3"))
my_data$param <- factor(my_data$DIM, levels = c(1,2,3,4), labels = c("DIM1","DIM2","DIM3","DIM4"))


res.aov1 <- aov(D1S1T ~ grp * param, data = my_data)
summary (res.aov1)
#ggboxplot(my_data,x = "param", y = "D1S1", color = "grp" )

#post hoc testing 


TukeyHSD(res.aov1, which = "grp")
TukeyHSD(res.aov1, which = "param")

emmeans(res.aov1, list(pairwise ~ grp), adjust = "tukey")
emmeans(res.aov1, list(pairwise ~param), adjust = "tukey")
emmeans(res.aov1, list(pairwise ~param*grp), adjust = "tukey")

#or

# Post-hoc tests
grp_emm <- emmeans(res.aov1, pairwise ~ grp | param, adjust = "tukey")
param_emm <- emmeans(res.aov1, pairwise ~ param | grp, adjust = "tukey")

grp_tbl <- as.data.frame(summary(grp_emm)$contrasts)
grp_tbl$comparison <- rownames(grp_tbl)
grp_tbl <- grp_tbl[, c("comparison", "estimate", "SE", "df", "t.ratio", "p.value")]

param_tbl <- as.data.frame(summary(param_emm)$contrasts)
param_tbl$comparison <- rownames(param_tbl)
param_tbl <- param_tbl[, c("comparison", "estimate", "SE", "df", "t.ratio", "p.value")]

print(grp_tbl)
print(param_tbl)




# Create summary table using broom to convert summary output to tidy format
# Create scientific table with gt
# Use broom to convert summary output to tidy format
tidy_anova <- tidy(res.aov1)

table_anova <- gt(tidy_anova) %>%
  tab_header(title = "ANOVA table") %>%
  tab_footnote("Note: p < .05 is considered significant") %>%
  tab_options(column_labels.font.size = 14, table.font.size = 12)
print(table_anova)




# broom to convert summary output to tidy format
tidy_anova <- tidy(res.aov1)

#table with gt
table_anova <- gt(tidy_anova) %>%
  tab_header(title = "ANOVA table") %>%
  tab_footnote("Note: p < .05 is considered significant") %>%
  tab_options(
    column_labels.font.size = 14,
    table.font.size = 12)%>%
print(table_anova)



#using lmer

install.packages("lme4")
install.packages("emmeans")
install.packages("lmerTest")
install.packages("Matrix")
library("Matrix")
library(lme4)
library(emmeans)
library(lmerTest)


fit1 = lmer( formula = D1S1T ~ param* grp+(1|subIDT), data = my_data)
print(fit1)
summary(fit1)

#post hoc of lmer *
emmeans(fit1, list(pairwise ~param*grp), adjust = "tukey")
emm_options(lmerTest.limit = 763, lmer.df = "satterthwaite")
tab_model(fit1, p.val = "satterthwaite")
summary(fit1)


# additive lmer +
fitA = lmer( formula = D1S1T ~ param + grp+(1|subIDT), data = my_data)
print(fitA)
summary(fitA)

#post hoc of lmer
emmeans(fitA, list(pairwise ~param+grp), adjust = "tukey")
emm_options(lmerTest.limit = 763, lmer.df = "satterthwaite")
tab_model(fitA, p.val = "satterthwaite")
summary(fitA)



# possible visualization options:
#library("ggpubr")
#ggline(my_data, x = "grp", y = "D1S1", 
       #add = c("mean_se", "jitter"), 
       #order = c("ctrl", "trt1", "trt2"),
       #ylab = "RawData", xlab = "SelfRank")

#Box plot
boxplot(D1S1T ~ grp*param, data = my_data,
        xlab = "Dimension and Self Rank", ylab = "D1S1=RawData")
#plotmeans
library("gplots")
plotmeans(D1S1 ~ grp, data = my_data, frame = FALSE,
          xlab = "Self Rank", ylab = "D1S1 = RawData",
          main="Mean Plot with 95% CI") 



# 1. Homogeneity of variances
plot(res.aov1, 1)
qqmath(fit1, id=0.05)
library(car)
leveneTest(D1S1 ~ param, data = my_data)
fligner.test(D1S1 ~ param, data = my_data)
shapiro.test(my_data$D1S1)
hist(my_data$D1S1, col='steelblue')
ggqqplot(my_data$D1S1)
# 2. Normality
plot(res.aov1, 2)

ggbetweenstats(warpbreaks,wool, breaks, outlier.tagging = TRUE)

qqmath(~log(D1S1T) | grp*param, data = my_data, xlab = "Ordered trials", ylab = "log(RT)")
qqmath(~log(D1S1T) | grp*param, data = my_data22, xlab = "Ordered trials", ylab = "log(RT)")
qqmath(~log(D1S1T) | grp*param, data = my_data22, xlab = "Ordered trials", ylab = "log(RT)", ylim = c(-6, 2))


#rep <- rep[which(abs(rep$sd) < 2.5), ]  # exclude outlier trials
#my_data2 <- my_data[which(abs(my_data$D1S1 < -3.86803))]

summary(my_data)

install.packages("ggstatsplot")
library(ggstatsplot)

lol <- boxplot(D1S1T~grp*param, data=my_data, notch=TRUE,
               col=(c("gold","darkgreen")),
               main="Dimensions Self_Rank", xlab="S1D1 - S3D4, all combos in order")$out


#ggplot(my_data,aes(y=D1S1, x=SelfR, fill=DIM))+
  #stat_summary(fun.y="mean", geom="bar",position="dodge")+
  #stat_summary(fun.data = mean_se, geom = "errorbar", position="dodge",width=.8)


#### der coole PLOT #####

ggboxplot(my_data, x= "DIM", y = "D1S1", color = "SelfR", main= "Self in Dimensions in RawData")
stat_summary(fun = mean, geom = "box", position= "dodge", color = "black")+ stat_summary(fun = mean, geom = "point")


ggboxplot(D1S1T~grp,y= my_data$param, data= my_data, main= "Self in Dimensions in RawData", notch = TRUE)
stat_summary(fun = mean, geom = "box", position= "dodge", color = "black")+ stat_summary(fun = mean, geom = "point")
  


library(dplyr)
library(lme4)


# Exclude outliers from DIM, SelfR, and D1S1
my_data22 <- my_data %>% 
  filter(DIMT >= quantile(DIMT, 0.05) & DIMT <= quantile(DIMT, 0.95),
         SelfRT >= quantile(SelfRT, 0.05) & SelfRT <= quantile(SelfRT, 0.95),
         D1S1T >= quantile(D1S1T, 0.05) & D1S1T <= quantile(D1S1T, 0.95))

fit2 = lmer( formula = D1S1 ~ param* grp+(1|subID), data = my_data22)
print(fit2)
summary(fit2)

model <- lmer(formula, data=my_data22)

summary(model)

#post hoc of lmer model

emmeans(fit2, list(pairwise ~param*grp), adjust = "tukey")
emm_options(lmerTest.limit = 763, lmer.df = "satterthwaite")
tab_model(fit2, p.val = "satterthwaite")
summary(fit2)

# 1. Homogeneity of variances
plot(res.aov1, 1)
qqmath(fit1, id=0.05)
library(car)
leveneTest(D1S1 ~ param, data = my_data)
fligner.test(D1S1 ~ param, data = my_data)
shapiro.test(my_data22$D1S1)
hist(my_data22$D1S1, col='steelblue')
ggqqplot(my_data$D1S1)
# 2. Normality
plot(res.aov1, 2)







####################################################################################################################
############### Dimnesions and questionnaires ############################################################################################
####################################################################################################################




#rm(list = ls(all.names = TRUE))


.libPaths("C:/Users/Neuroeconomics Lab/Documents/R")
# read the csv file
my_data <- read.csv("C:/Users/Neuroeconomics Lab/Documents/R/Dime_03_01.csv", head = TRUE, sep=",")
my_data
my_dataSub <- my_data[ , c("D1S1T",	"SelfRT",	"DIMT", "subIDT", "STAI_S_F",
                           "STAI_t_F",	"STAI_total_F",	"IRI_total_F",	"IRI_PT_F",	
                           "IRI_FS_F",	"IRI_EC_F",	"IRI_PD_F", "Move", "SDO_F","RSE_F",
                           "Mach_F",	"Machn_A_F",	"Mach_Ctrl_F",	"Mach_S_F",	"Mach_D_F",	"M_C_F",
                           "INCOM_F",	"SVO_self_F",	"SVO_other_F",	"SNS_n_F",	"SNS_d_F",	"SNS_e_F",
                           "Svo_6_F",	"RSE_r_F",	"desir_F",	"WTC_F",	"CmpCoop_F",	"gender", "vGen")]


# grp = Self ranks ; param = Dimensions

my_data$grp <- factor(my_data$SelfR, levels = c(1,2,3), labels = c("S1","S2", "S3"))
my_data$param <- factor(my_data$DIM, levels = c(1,2,3,4), labels = c("DIM1","DIM2","DIM3","DIM4"))


# subset Move: Loosing or gaining a rank 

move1 <- subset(my_data, Move=='1')
move0 <- subset(my_data, Move=='0')


# Dimensions subsetting 

DIM1 <- subset(my_data, DIMT =='1')
DIM2 <- subset(my_data, DIMT =='2')
DIM3 <- subset(my_data, DIMT =='3')
DIM4 <- subset(my_data, DIMT =='4')



################################   my_data   ########################################################################

move <- factor(my_data$Move, levels = c(0, 1), labels = c("MoveDown", "MoveUp"))
Stai_SF <- factor(my_data$STAI_S_F, levels = c(0, 1), labels = c("Low", "High"))
STAI_TF <- factor(my_data$STAI_t_F, levels = c(0, 1), labels = c("Low", "High"))
STAI_totalF <- factor(my_data$STAI_total_F, levels = c(0, 1), labels = c("Low", "High"))
IRI_PTF <- factor(my_data$IRI_PT_F, levels = c(0, 1), labels = c("Low", "High"))
IRI_FSF <- factor(my_data$IRI_FS_F, levels = c(0, 1), labels = c("Low", "High"))
IRI_ECF <- factor(my_data$IRI_EC_F, levels = c(0, 1), labels = c("Low", "High"))
iRI_PDF <- factor(my_data$IRI_PD_F, levels = c(0, 1), labels = c("Low", "High"))
Move <- factor(my_data$Move, levels = c(0, 1), labels = c("Low", "High"))
SDO_F <- factor(my_data$SDO_F, levels = c(0, 1), labels = c("Low", "High"))
RSE_F <- factor(my_data$RSE_F, levels = c(0, 1), labels = c("Low", "High"))
#Mach_F <- factor(my_data$Mach_F, levels = c(0, 1), labels = c("Low", "High"))
Machn_A_F <- factor(my_data$Machn_A_F, levels = c(0, 1), labels = c("Low", "High"))
Mach_Ctrl_F <- factor(my_data$Mach_Ctrl_F, levels = c(0, 1), labels = c("Low", "High"))
Mach_S_F <- factor(my_data$Mach_S_F, levels = c(0, 1), labels = c("Low", "High"))
Mach_D_F <- factor(my_data$Mach_D_F, levels = c(0, 1), labels = c("Low", "High"))
M_C_F <- factor(my_data$M_C_F, levels = c(0, 1), labels = c("Low", "High"))
INCOM_F <- factor(my_data$INCOM_F, levels = c(0, 1), labels = c("Low", "High"))
SVO_self_F <- factor(my_data$SVO_self_F, levels = c(0, 1), labels = c("Low", "High"))
SVO_other_F <- factor(my_data$SVO_other_F, levels = c(0, 1), labels = c("Low", "High"))
SNS_n_F <- factor(my_data$SNS_n_F, levels = c(0, 1), labels = c("Low", "High"))
SNS_d_F <- factor(my_data$SNS_d_F, levels = c(0, 1), labels = c("Low", "High"))
SNS_e_F <- factor(my_data$SNS_e_F, levels = c(0, 1), labels = c("Low", "High"))
Svo_6_F <- factor(my_data$Svo_6_F, levels = c(0, 1), labels = c("Low", "High"))
RSE_r_F <- factor(my_data$RSE_r_F, levels = c(0, 1), labels = c("Low", "High"))
desir_F <- factor(my_data$desir_F, levels = c(0, 1), labels = c("Low", "High"))
WTC_F <- factor(my_data$WTC_F, levels = c(0, 1), labels = c("Low", "High"))
CmpCoop_F <- factor(my_data$CmpCoop_F, levels = c(0, 1), labels = c("Low", "High"))
gender <- factor(my_data$gender, levels = c(0, 1), labels = c("Low", "High"))


################################   MOVE 1   ########################################################################


moveM1 <- factor(move1$Move, levels = c(0, 1), labels = c("MoveDown", "MoveUp"))
Stai_SFM1 <- factor(move1$STAI_S_F, levels = c(0, 1), labels = c("Low", "High"))
STAI_TFM1 <- factor(move1$STAI_t_F, levels = c(0, 1), labels = c("Low", "High"))
STAI_totalFM1 <- factor(move1$STAI_total_F, levels = c(0, 1), labels = c("Low", "High"))
IRI_PTFM1 <- factor(move1$IRI_PT_F, levels = c(0, 1), labels = c("Low", "High"))
IRI_FSFM1 <- factor(move1$IRI_FS_F, levels = c(0, 1), labels = c("Low", "High"))
IRI_ECFM1 <- factor(move1$IRI_EC_F, levels = c(0, 1), labels = c("Low", "High"))
iRI_PDFM1 <- factor(move1$IRI_PD_F, levels = c(0, 1), labels = c("Low", "High"))
MoveM1 <- factor(move1$Move, levels = c(0, 1), labels = c("Low", "High"))
SDO_FM1 <- factor(move1$SDO_F, levels = c(0, 1), labels = c("Low", "High"))
RSE_FM1 <- factor(move1$RSE_F, levels = c(0, 1), labels = c("Low", "High"))
#Mach_FM1- factor(move1$Mach_F, levels = c(0, 1), labels = c("Low", "High"))
Machn_A_FM1 <- factor(move1$Machn_A_F, levels = c(0, 1), labels = c("Low", "High"))
Mach_Ctrl_FM1 <- factor(move1$Mach_Ctrl_F, levels = c(0, 1), labels = c("Low", "High"))
Mach_S_FM1 <- factor(move1$Mach_S_F, levels = c(0, 1), labels = c("Low", "High"))
Mach_D_FM1 <- factor(move1$Mach_D_F, levels = c(0, 1), labels = c("Low", "High"))
M_C_FM1 <- factor(move1$M_C_F, levels = c(0, 1), labels = c("Low", "High"))
INCOM_FM1 <- factor(move1$INCOM_F, levels = c(0, 1), labels = c("Low", "High"))
SVO_self_FM1 <- factor(move1$SVO_self_F, levels = c(0, 1), labels = c("Low", "High"))
SVO_other_FM1 <- factor(move1$SVO_other_F, levels = c(0, 1), labels = c("Low", "High"))
SNS_n_FM1 <- factor(move1$SNS_n_F, levels = c(0, 1), labels = c("Low", "High"))
SNS_d_FM1 <- factor(move1$SNS_d_F, levels = c(0, 1), labels = c("Low", "High"))
SNS_e_FM1 <- factor(move1$SNS_e_F, levels = c(0, 1), labels = c("Low", "High"))
Svo_6_FM1 <- factor(move1$Svo_6_F, levels = c(0, 1), labels = c("Low", "High"))
RSE_r_FM1 <- factor(move1$RSE_r_F, levels = c(0, 1), labels = c("Low", "High"))
desir_FM1 <- factor(move1$desir_F, levels = c(0, 1), labels = c("Low", "High"))
WTC_FM1 <- factor(move1$WTC_F, levels = c(0, 1), labels = c("Low", "High"))
CmpCoop_FM1 <- factor(move1$CmpCoop_F, levels = c(0, 1), labels = c("Low", "High"))
genderM1 <- factor(move1$gender, levels = c(0, 1), labels = c("Low", "High"))


################################   MOVE 0   ########################################################################

move <- factor(move0$Move, levels = c(0, 1), labels = c("MoveDown", "MoveUp"))
Stai_SF0 <- factor(move0$STAI_S_F, levels = c(0, 1), labels = c("Low", "High"))
STAI_TF0 <- factor(move0$STAI_t_F, levels = c(0, 1), labels = c("Low", "High"))
STAI_totalF0 <- factor(move0$STAI_total_F, levels = c(0, 1), labels = c("Low", "High"))
IRI_PTF0 <- factor(move0$IRI_PT_F, levels = c(0, 1), labels = c("Low", "High"))
IRI_FSF0 <- factor(move0$IRI_FS_F, levels = c(0, 1), labels = c("Low", "High"))
IRI_ECF0 <- factor(move0$IRI_EC_F, levels = c(0, 1), labels = c("Low", "High"))
iRI_PDF0 <- factor(move0$IRI_PD_F, levels = c(0, 1), labels = c("Low", "High"))
Move0 <- factor(move0$Move, levels = c(0, 1), labels = c("Low", "High"))
SDO_F0 <- factor(move0$SDO_F, levels = c(0, 1), labels = c("Low", "High"))
RSE_F0 <- factor(move0$RSE_F, levels = c(0, 1), labels = c("Low", "High"))
#Mach_F0 <- factor(move0$Mach_F, levels = c(0, 1), labels = c("Low", "High"))
Machn_A_F0 <- factor(move0$Machn_A_F, levels = c(0, 1), labels = c("Low", "High"))
Mach_Ctrl_F0 <- factor(move0$Mach_Ctrl_F, levels = c(0, 1), labels = c("Low", "High"))
Mach_S_F0 <- factor(move0$Mach_S_F, levels = c(0, 1), labels = c("Low", "High"))
Mach_D_F0 <- factor(move0$Mach_D_F, levels = c(0, 1), labels = c("Low", "High"))
M_C_F0 <- factor(move0$M_C_F, levels = c(0, 1), labels = c("Low", "High"))
INCOM_F0 <- factor(move0$INCOM_F, levels = c(0, 1), labels = c("Low", "High"))
SVO_self_F0 <- factor(move0$SVO_self_F, levels = c(0, 1), labels = c("Low", "High"))
SVO_other_F0 <- factor(move0$SVO_other_F, levels = c(0, 1), labels = c("Low", "High"))
SNS_n_F0 <- factor(move0$SNS_n_F, levels = c(0, 1), labels = c("Low", "High"))
SNS_d_F0 <- factor(move0$SNS_d_F, levels = c(0, 1), labels = c("Low", "High"))
SNS_e_F0 <- factor(move0$SNS_e_F, levels = c(0, 1), labels = c("Low", "High"))
desir_F0 <- factor(move0$desir_F, levels = c(0, 1), labels = c("Low", "High"))
WTC_F0 <- factor(move0$WTC_F, levels = c(0, 1), labels = c("Low", "High"))
CmpCoop_F0 <- factor(move0$CmpCoop_F, levels = c(0, 1), labels = c("Low", "High"))
gender0 <- factor(move0$gender, levels = c(0, 1), labels = c("Low", "High"))
RSE_r_F0 <- factor(move0$RSE_r_F, levels = c(0, 1), labels = c("Low", "High"))


################################   DIM1  ########################################################################


move1 <- factor(DIM1$Move, levels = c(0, 1), labels = c("MoveDown", "MoveUp"))
Stai_SF1 <- factor(DIM1$STAI_S_F, levels = c(0, 1), labels = c("Low", "High"))
STAI_TF1 <- factor(DIM1$STAI_t_F, levels = c(0, 1), labels = c("Low", "High"))
STAI_totalF1 <- factor(DIM1$STAI_total_F, levels = c(0, 1), labels = c("Low", "High"))
IRI_PTF1 <- factor(DIM1$IRI_PT_F, levels = c(0, 1), labels = c("Low", "High"))
IRI_FSF1 <- factor(DIM1$IRI_FS_F, levels = c(0, 1), labels = c("Low", "High"))
IRI_ECF1 <- factor(DIM1$IRI_EC_F, levels = c(0, 1), labels = c("Low", "High"))
iRI_PDF1 <- factor(DIM1$IRI_PD_F, levels = c(0, 1), labels = c("Low", "High"))
Move1<- factor(DIM1$Move, levels = c(0, 1), labels = c("Low", "High"))
SDO_F1 <- factor(DIM1$SDO_F, levels = c(0, 1), labels = c("Low", "High"))
RSE_F1 <- factor(DIM1$RSE_F, levels = c(0, 1), labels = c("Low", "High"))
#Mach_F1 <- factor(DIM1$Mach_F, levels = c(0, 1), labels = c("Low", "High"))
Machn_A_F1 <- factor(DIM1$Machn_A_F, levels = c(0, 1), labels = c("Low", "High"))
Mach_Ctrl_F1 <- factor(DIM1$Mach_Ctrl_F, levels = c(0, 1), labels = c("Low", "High"))
Mach_S_F1 <- factor(DIM1$Mach_S_F, levels = c(0, 1), labels = c("Low", "High"))
Mach_D_F1 <- factor(DIM1$Mach_D_F, levels = c(0, 1), labels = c("Low", "High"))
M_C_F1 <- factor(DIM1$M_C_F, levels = c(0, 1), labels = c("Low", "High"))
INCOM_F1 <- factor(DIM1$INCOM_F, levels = c(0, 1), labels = c("Low", "High"))
SVO_self_F1 <- factor(DIM1$SVO_self_F, levels = c(0, 1), labels = c("Low", "High"))
SVO_other_F1 <- factor(DIM1$SVO_other_F, levels = c(0, 1), labels = c("Low", "High"))
SNS_n_F1 <- factor(DIM1$SNS_n_F, levels = c(0, 1), labels = c("Low", "High"))
SNS_d_F1 <- factor(DIM1$SNS_d_F, levels = c(0, 1), labels = c("Low", "High"))
SNS_e_F1 <- factor(DIM1$SNS_e_F, levels = c(0, 1), labels = c("Low", "High"))
Svo_6_F1 <- factor(DIM1$Svo_6_F, levels = c(0, 1), labels = c("Low", "High"))
RSE_r_F1 <- factor(DIM1$RSE_r_F, levels = c(0, 1), labels = c("Low", "High"))
desir_F1 <- factor(DIM1$desir_F, levels = c(0, 1), labels = c("Low", "High"))
WTC_F1 <- factor(DIM1$WTC_F, levels = c(0, 1), labels = c("Low", "High"))
CmpCoop_F1 <- factor(DIM1$CmpCoop_F, levels = c(0, 1), labels = c("Low", "High"))
gender1 <- factor(DIM1$gender, levels = c(0, 1), labels = c("Low", "High"))



################################   DIM2  ########################################################################

move2 <- factor(DIM2$Move, levels = c(0, 1), labels = c("MoveDown", "MoveUp"))
Stai_SF2 <- factor(DIM2$STAI_S_F, levels = c(0, 1), labels = c("Low", "High"))
STAI_TF2 <- factor(DIM2$STAI_t_F, levels = c(0, 1), labels = c("Low", "High"))
STAI_totalF2 <- factor(DIM2$STAI_total_F, levels = c(0, 1), labels = c("Low", "High"))
IRI_PTF2 <- factor(DIM2$IRI_PT_F, levels = c(0, 1), labels = c("Low", "High"))
IRI_FSF2 <- factor(DIM2$IRI_FS_F, levels = c(0, 1), labels = c("Low", "High"))
IRI_ECF2 <- factor(DIM2$IRI_EC_F, levels = c(0, 1), labels = c("Low", "High"))
iRI_PDF2 <- factor(DIM2$IRI_PD_F, levels = c(0, 1), labels = c("Low", "High"))
Move2 <- factor(DIM2$Move, levels = c(0, 1), labels = c("Low", "High"))
SDO_F2 <- factor(DIM2$SDO_F, levels = c(0, 1), labels = c("Low", "High"))
RSE_F2 <- factor(DIM2$RSE_F, levels = c(0, 1), labels = c("Low", "High"))
#Mach_F2 <- factor(DIM2$Mach_F, levels = c(0, 1), labels = c("Low", "High"))
Machn_A_F2 <- factor(DIM2$Machn_A_F, levels = c(0, 1), labels = c("Low", "High"))
Mach_Ctrl_F2 <- factor(DIM2$Mach_Ctrl_F, levels = c(0, 1), labels = c("Low", "High"))
Mach_S_F2 <- factor(DIM2$Mach_S_F, levels = c(0, 1), labels = c("Low", "High"))
Mach_D_F2 <- factor(DIM2$Mach_D_F, levels = c(0, 1), labels = c("Low", "High"))
M_C_F2 <- factor(DIM2$M_C_F, levels = c(0, 1), labels = c("Low", "High"))
INCOM_F2 <- factor(DIM2$INCOM_F, levels = c(0, 1), labels = c("Low", "High"))
SVO_self_F2 <- factor(DIM2$SVO_self_F, levels = c(0, 1), labels = c("Low", "High"))
SVO_other_F2 <- factor(DIM2$SVO_other_F, levels = c(0, 1), labels = c("Low", "High"))
SNS_n_F2 <- factor(DIM2$SNS_n_F, levels = c(0, 1), labels = c("Low", "High"))
SNS_d_F2 <- factor(DIM2$SNS_d_F, levels = c(0, 1), labels = c("Low", "High"))
SNS_e_F2 <- factor(DIM2$SNS_e_F, levels = c(0, 1), labels = c("Low", "High"))
Svo_6_F2 <- factor(DIM2$Svo_6_F, levels = c(0, 1), labels = c("Low", "High"))
RSE_r_F2 <- factor(DIM2$RSE_r_F, levels = c(0, 1), labels = c("Low", "High"))
desir_F2 <- factor(DIM2$desir_F, levels = c(0, 1), labels = c("Low", "High"))
WTC_F2 <- factor(DIM2$WTC_F, levels = c(0, 1), labels = c("Low", "High"))
CmpCoop_F2 <- factor(DIM2$CmpCoop_F, levels = c(0, 1), labels = c("Low", "High"))
gender2 <- factor(DIM2$gender, levels = c(0, 1), labels = c("Low", "High"))

################################   DIM3  ########################################################################

move3 <- factor(DIM3$Move, levels = c(0, 1), labels = c("MoveDown", "MoveUp"))
Stai_SF3 <- factor(DIM3$STAI_S_F, levels = c(0, 1), labels = c("Low", "High"))
STAI_TF3 <- factor(DIM3$STAI_t_F, levels = c(0, 1), labels = c("Low", "High"))
STAI_totalF3 <- factor(DIM3$STAI_total_F, levels = c(0, 1), labels = c("Low", "High"))
IRI_PTF3 <- factor(DIM3$IRI_PT_F, levels = c(0, 1), labels = c("Low", "High"))
IRI_FSF3 <- factor(DIM3$IRI_FS_F, levels = c(0, 1), labels = c("Low", "High"))
IRI_ECF3 <- factor(DIM3$IRI_EC_F, levels = c(0, 1), labels = c("Low", "High"))
iRI_PDF3 <- factor(DIM3$IRI_PD_F, levels = c(0, 1), labels = c("Low", "High"))
Move3 <- factor(DIM3$Move, levels = c(0, 1), labels = c("Low", "High"))
SDO_F3 <- factor(DIM3$SDO_F, levels = c(0, 1), labels = c("Low", "High"))
RSE_F3 <- factor(DIM3$RSE_F, levels = c(0, 1), labels = c("Low", "High"))
#Mach_F3 <- factor(DIM3$Mach_F, levels = c(0, 1), labels = c("Low", "High"))
Machn_A_F3 <- factor(DIM3$Machn_A_F, levels = c(0, 1), labels = c("Low", "High"))
Mach_Ctrl_F3 <- factor(DIM3$Mach_Ctrl_F, levels = c(0, 1), labels = c("Low", "High"))
Mach_S_F3 <- factor(DIM3$Mach_S_F, levels = c(0, 1), labels = c("Low", "High"))
Mach_D_F3 <- factor(DIM3$Mach_D_F, levels = c(0, 1), labels = c("Low", "High"))
M_C_F3 <- factor(DIM3$M_C_F, levels = c(0, 1), labels = c("Low", "High"))
INCOM_F3<- factor(DIM3$INCOM_F, levels = c(0, 1), labels = c("Low", "High"))
SVO_self_F3 <- factor(DIM3$SVO_self_F, levels = c(0, 1), labels = c("Low", "High"))
SVO_other_F3 <- factor(DIM3$SVO_other_F, levels = c(0, 1), labels = c("Low", "High"))
SNS_n_F3 <- factor(DIM3$SNS_n_F, levels = c(0, 1), labels = c("Low", "High"))
SNS_d_F3 <- factor(DIM3$SNS_d_F, levels = c(0, 1), labels = c("Low", "High"))
SNS_e_F3 <- factor(DIM3$SNS_e_F, levels = c(0, 1), labels = c("Low", "High"))
Svo_6_F3 <- factor(DIM3$Svo_6_F, levels = c(0, 1), labels = c("Low", "High"))
RSE_r_F3 <- factor(DIM3$RSE_r_F, levels = c(0, 1), labels = c("Low", "High"))
desir_F3 <- factor(DIM3$desir_F, levels = c(0, 1), labels = c("Low", "High"))
WTC_F3 <- factor(DIM3$WTC_F, levels = c(0, 1), labels = c("Low", "High"))
CmpCoop_F3 <- factor(DIM3$CmpCoop_F, levels = c(0, 1), labels = c("Low", "High"))
gender3 <- factor(DIM3$gender, levels = c(0, 1), labels = c("Low", "High"))

################################   DIM4  ########################################################################

move4 <- factor(DIM4$Move, levels = c(0, 1), labels = c("MoveDown", "MoveUp"))
Stai_SF4 <- factor(DIM4$STAI_S_F, levels = c(0, 1), labels = c("Low", "High"))
STAI_TF4 <- factor(DIM4$STAI_t_F, levels = c(0, 1), labels = c("Low", "High"))
STAI_totalF4 <- factor(DIM4$STAI_total_F, levels = c(0, 1), labels = c("Low", "High"))
IRI_PTF4 <- factor(DIM4$IRI_PT_F, levels = c(0, 1), labels = c("Low", "High"))
IRI_FSF4 <- factor(DIM4$IRI_FS_F, levels = c(0, 1), labels = c("Low", "High"))
IRI_ECF4 <- factor(DIM4$IRI_EC_F, levels = c(0, 1), labels = c("Low", "High"))
iRI_PDF4 <- factor(DIM4$IRI_PD_F, levels = c(0, 1), labels = c("Low", "High"))
Move4 <- factor(DIM4$Move, levels = c(0, 1), labels = c("Low", "High"))
SDO_F4 <- factor(DIM4$SDO_F, levels = c(0, 1), labels = c("Low", "High"))
RSE_F4 <- factor(DIM4$RSE_F, levels = c(0, 1), labels = c("Low", "High"))
#Mach_F4 <- factor(DIM4$Mach_F, levels = c(0, 1), labels = c("Low", "High"))
Machn_A_F4 <- factor(DIM4$Machn_A_F, levels = c(0, 1), labels = c("Low", "High"))
Mach_Ctrl_F4 <- factor(DIM4$Mach_Ctrl_F, levels = c(0, 1), labels = c("Low", "High"))
Mach_S_F4 <- factor(DIM4$Mach_S_F, levels = c(0, 1), labels = c("Low", "High"))
Mach_D_F4<- factor(DIM4$Mach_D_F, levels = c(0, 1), labels = c("Low", "High"))
M_C_F4 <- factor(DIM4$M_C_F, levels = c(0, 1), labels = c("Low", "High"))
INCOM_F4 <- factor(DIM4$INCOM_F, levels = c(0, 1), labels = c("Low", "High"))
SVO_self_F4 <- factor(DIM4$SVO_self_F, levels = c(0, 1), labels = c("Low", "High"))
SVO_other_F4 <- factor(DIM4$SVO_other_F, levels = c(0, 1), labels = c("Low", "High"))
SNS_n_F4 <- factor(DIM4$SNS_n_F, levels = c(0, 1), labels = c("Low", "High"))
SNS_d_F4 <- factor(DIM4$SNS_d_F, levels = c(0, 1), labels = c("Low", "High"))
SNS_e_F4 <- factor(DIM4$SNS_e_F, levels = c(0, 1), labels = c("Low", "High"))
Svo_6_F4 <- factor(DIM4$Svo_6_F, levels = c(0, 1), labels = c("Low", "High"))
RSE_r_F4 <- factor(DIM4$RSE_r_F, levels = c(0, 1), labels = c("Low", "High"))
desir_F4 <- factor(DIM4$desir_F, levels = c(0, 1), labels = c("Low", "High"))
WTC_F4 <- factor(DIM4$WTC_F, levels = c(0, 1), labels = c("Low", "High"))
CmpCoop_F4 <- factor(DIM4$CmpCoop_F, levels = c(0, 1), labels = c("Low", "High"))
gender4 <- factor(DIM4$gender, levels = c(0, 1), labels = c("Low", "High"))


################################   ANOVA my_data  ########################################################################

res.aov1 <- aov(D1S1T ~ grp , data = my_data)
summary (res.aov1)

################################   ANOVA DIM1  ########################################################################

res.aov11_I <- aov(D1S1T ~ grp*IRI_ECF1, data = DIM1)
summary (res.aov11_I)

res.aov11_MC <- aov(D1S1T ~ grp*Mach_Ctrl_F1, data = DIM1)
summary (res.aov11_MC)
res.aov11_Ma <- aov(D1S1T ~ grp*Mach_S_F1, data = DIM1)
summary (res.aov11_Ma)
res.aov11_MD <- aov(D1S1T ~ grp*Mach_D_F1, data = DIM1)
summary (res.aov11_MD)
res.aov11_IN <- aov(D1S1T ~ grp*INCOM_F1, data = DIM1)
summary (res.aov11_IN)
res.aov11_SVS <- aov(D1S1T ~ grp*SVO_self_F1, data = DIM1)
summary (res.aov11_SVS)


################################   ANOVA DIM2  ########################################################################

res.aov12_I <- aov(D1S1T ~ grp*IRI_ECF2, data = DIM2)
summary (res.aov12_I)

res.aov12_MC <- aov(D1S1T ~ grp*Mach_Ctrl_F2, data = DIM2)
summary (res.aov12_MC)
res.aov12_Ma <- aov(D1S1T ~ grp*Mach_S_F2, data = DIM2)
summary (res.aov12_Ma)
res.aov12_MD <- aov(D1S1T ~ grp*Mach_D_F2, data = DIM2)
summary (res.aov12_MD)
res.aov12_IN <- aov(D1S1T ~ grp*INCOM_F2, data = DIM2)
summary (res.aov12_IN)
res.aov12_SVS <- aov(D1S1T ~ grp*SVO_self_F2, data = DIM2)
summary (res.aov12_SVS)


################################   ANOVA DIM3  ########################################################################

res.aov13_I <- aov(D1S1T ~ grp+IRI_ECF3, data = DIM3)
summary (res.aov13_I)

res.aov13_MC <- aov(D1S1T ~ grp+Mach_Ctrl_F3, data = DIM3)
summary (res.aov13_MC)
res.aov13_Ma <- aov(D1S1T ~ grp+Mach_S_F3, data = DIM3)
summary (res.aov13_Ma)
res.aov13_MD <- aov(D1S1T ~ grp+Mach_D_F3, data = DIM3)
summary (res.aov13_MD)
res.aov13_IN <- aov(D1S1T ~ grp+INCOM_F3, data = DIM3)
summary (res.aov13_IN)
res.aov13_SVS <- aov(D1S1T ~ grp+SVO_self_F3, data = DIM3)
summary (res.aov13_SVS)

################################   ANOVA DIM4  ########################################################################

res.aov14_I <- aov(D1S1T ~ grp*IRI_ECF4, data = DIM4)
summary (res.aov14_I)

res.aov14_MC <- aov(D1S1T ~ grp*Mach_Ctrl_F4, data = DIM4)
summary (res.aov14_MC)
res.aov14_Ma <- aov(D1S1T ~ grp*Mach_S_F4, data = DIM4)
summary (res.aov14_Ma)
res.aov14_MD <- aov(D1S1T ~ grp*Mach_D_F4, data = DIM4)
summary (res.aov14_MD)
res.aov14_IN <- aov(D1S1T ~ grp*INCOM_F4, data = DIM4)
summary (res.aov14_IN)
res.aov14_SVS <- aov(D1S1T ~ grp*SVO_self_F4, data = DIM4)
summary (res.aov14_SVS)


#ggboxplot(my_data, x = "param", y = "D1S1", color = "grp" )

#post hoc testing x SELF RANK

TukeyHSD(res.aov11_I, which = "grp")
TukeyHSD(res.aov11_MC, which = "grp")
TukeyHSD(res.aov11_Ma, which = "grp")
TukeyHSD(res.aov11_MD, which = "grp")
TukeyHSD(res.aov11_IN, which = "grp")
TukeyHSD(res.aov11_SVS, which = "grp")

TukeyHSD(res.aov12_I, which = "grp")
TukeyHSD(res.aov12_MC, which = "grp")
TukeyHSD(res.aov12_Ma, which = "grp")
TukeyHSD(res.aov12_MD, which = "grp")
TukeyHSD(res.aov12_IN, which = "grp")
TukeyHSD(res.aov12_SVS, which = "grp")

TukeyHSD(res.aov13_I, which = "grp")
TukeyHSD(res.aov13_MC, which = "grp")
TukeyHSD(res.aov13_Ma, which = "grp")
TukeyHSD(res.aov13_MD, which = "grp")
TukeyHSD(res.aov13_IN, which = "grp")
TukeyHSD(res.aov13_SVS, which = "grp")

TukeyHSD(res.aov14_I, which = "grp")
TukeyHSD(res.aov14_MC, which = "grp")
TukeyHSD(res.aov14_Ma, which = "grp")
TukeyHSD(res.aov14_MD, which = "grp")
TukeyHSD(res.aov14_IN, which = "grp")
TukeyHSD(res.aov14_SVS, which = "grp")

#post hoc testing x QUESTIONNAIRE 

emmeans(res.aov11, list(pairwise ~ grp), adjust = "tukey")
emmeans(res.aov12, list(pairwise ~ grp), adjust = "tukey")
emmeans(res.aov13, list(pairwise ~ grp), adjust = "tukey")
emmeans(res.aov14, list(pairwise ~ grp), adjust = "tukey")


################# QUEST IRI ECF
emmeans(res.aov11_I, list(pairwise ~grp*IRI_ECF1), adjust = "tukey")

emmeans(res.aov12_I, list(pairwise ~grp*IRI_ECF2), adjust = "tukey")

emmeans(res.aov13_I, list(pairwise ~grp*IRI_ECF3), adjust = "tukey")

emmeans(res.aov14_I, list(pairwise ~grp*IRI_ECF4), adjust = "tukey")


################# QUEST Mach Ctrl

emmeans(res.aov11_MC, list(pairwise ~grp*Mach_Ctrl_F1), adjust = "tukey")

emmeans(res.aov12_MC, list(pairwise ~grp*Mach_Ctrl_F2), adjust = "tukey")

emmeans(res.aov13_MC, list(pairwise ~grp*Mach_Ctrl_F3), adjust = "tukey")

emmeans(res.aov14_MC, list(pairwise ~grp*Mach_Ctrl_F4), adjust = "tukey")


################# QUEST Mach SF

emmeans(res.aov11_Ma, list(pairwise ~grp*Mach_S_F1), adjust = "tukey")

emmeans(res.aov12_Ma, list(pairwise ~grp*Mach_S_F2), adjust = "tukey")

emmeans(res.aov13_Ma, list(pairwise ~grp*Mach_S_F3), adjust = "tukey")

emmeans(res.aov14_Ma, list(pairwise ~grp*Mach_S_F4), adjust = "tukey")


################# QUEST Mach DF

emmeans(res.aov11_MD, list(pairwise ~grp*Mach_D_F1), adjust = "tukey")

emmeans(res.aov12_MD, list(pairwise ~grp*Mach_D_F2), adjust = "tukey")

emmeans(res.aov13_MD, list(pairwise ~grp*Mach_D_F3), adjust = "tukey")

emmeans(res.aov14_MD, list(pairwise ~grp*Mach_D_F4), adjust = "tukey")


################# QUEST INCOME

emmeans(res.aov11_IN, list(pairwise ~grp*INCOM_F1), adjust = "tukey")

emmeans(res.aov12_IN, list(pairwise ~grp*INCOM_F2), adjust = "tukey")

emmeans(res.aov13_IN, list(pairwise ~grp*INCOM_F3), adjust = "tukey")

emmeans(res.aov14_IN, list(pairwise ~grp*INCOM_F4), adjust = "tukey")


################# QUEST SVO_self_F

emmeans(res.aov11_SVS, list(pairwise ~grp*SVO_self_F1), adjust = "tukey")

emmeans(res.aov12_SVS, list(pairwise ~grp*SVO_self_F2), adjust = "tukey")

emmeans(res.aov13_SVS, list(pairwise ~grp*SVO_self_F3), adjust = "tukey")

emmeans(res.aov14_SVS, list(pairwise ~grp*SVO_self_F4), adjust = "tukey")



#or

# Post-hoc tests
grp_emm <- emmeans(res.aov1, pairwise ~ grp | param, adjust = "tukey")
param_emm <- emmeans(res.aov1, pairwise ~ param | grp, adjust = "tukey")

grp_emm <- emmeans(res.aov2, pairwise ~ grp | param, adjust = "tukey")
param_emm <- emmeans(res.aov2, pairwise ~ param | grp, adjust = "tukey")

grp_emm <- emmeans(res.aov3, pairwise ~ grp | param, adjust = "tukey")
param_emm <- emmeans(res.aov3, pairwise ~ param | grp, adjust = "tukey")


grp_tbl <- as.data.frame(summary(grp_emm)$contrasts)
grp_tbl$comparison <- rownames(grp_tbl)
grp_tbl <- grp_tbl[, c("comparison", "estimate", "SE", "df", "t.ratio", "p.value")]

param_tbl <- as.data.frame(summary(param_emm)$contrasts)
param_tbl$comparison <- rownames(param_tbl)
param_tbl <- param_tbl[, c("comparison", "estimate", "SE", "df", "t.ratio", "p.value")]

print(grp_tbl)
print(param_tbl)




# Create summary table using broom to convert summary output to tidy format
# Create scientific table with gt
# Use broom to convert summary output to tidy format
tidy_anova <- tidy(res.aov1)

table_anova <- gt(tidy_anova) %>%
  tab_header(title = "ANOVA table") %>%
  tab_footnote("Note: p < .05 is considered significant") %>%
  tab_options(column_labels.font.size = 14, table.font.size = 12)
print(table_anova)




# broom to convert summary output to tidy format
tidy_anova <- tidy(res.aov1)

#table with gt
table_anova <- gt(tidy_anova) %>%
  tab_header(title = "ANOVA table") %>%
  tab_footnote("Note: p < .05 is considered significant") %>%
  tab_options(
    column_labels.font.size = 14,
    table.font.size = 12)%>%
  print(table_anova)


########################## lmer #######################################################
#using lmer

install.packages("lme4")
install.packages("emmeans")
install.packages("lmerTest")
install.packages("Matrix")
library("Matrix")
library(lme4)
library(emmeans)
library(lmerTest)


# additive lmer + fitA  IRI
fitA = lmer( formula = D1S1T ~ grp+IRI_ECF1+(1|subIDT), data = DIM1)
print(fitA)
summary(fitA)

# multi lmer +
fitA = lmer( formula = D1S1T ~  grp*IRI_ECF1+(1|subIDT), data = DIM1)
print(fitA)
summary(fitA)



# additive lmer + fitB  MACH CTRL
fitB = lmer( formula = D1S1T ~grp+Mach_Ctrl_F4+(1|subIDT), data = DIM4)
print(fitB)
summary(fitB)

# multi lmer +
fitB = lmer( formula = D1S1T ~grp*Mach_Ctrl_F4+(1|subIDT), data = DIM4)
print(fitB)
summary(fitB)



# additive lmer + fitC  MACH FS
fitC = lmer( formula = D1S1T ~ grp+Mach_S_F3+(1|subIDT), data = DIM3)
print(fitC)
summary(fitC)

# multi lmer +
fitC = lmer( formula = D1S1T ~ grp*Mach_S_F4+(1|subIDT), data = DIM4)
print(fitC)
summary(fitC)



# additive lmer + fitC  MACH DF
fitC = lmer( formula = D1S1T ~ grp+Mach_D_F4+(1|subIDT), data = DIM4)
print(fitC)
summary(fitC)

# multi lmer +
fitC = lmer( formula = D1S1T ~ grp*Mach_D_F4+(1|subIDT), data = DIM4)
print(fitC)
summary(fitC)


# additive lmer + fitC  INCOM
fitC = lmer( formula = D1S1T ~ grp+INCOM_F4+(1|subIDT), data = DIM4)
print(fitC)
summary(fitC)

# multi lmer +
fitC = lmer( formula = D1S1T ~ grp*INCOM_F4+(1|subIDT), data = DIM4)
print(fitC)
summary(fitC)


# additive lmer + fitC  SVO_self_F
fitC = lmer( formula = D1S1T ~ grp+SVO_self_F3+(1|subIDT), data = DIM3)
print(fitC)
summary(fitC)

# multi lmer +
fitC = lmer( formula = D1S1T ~ grp*SVO_self_F4+(1|subIDT), data = DIM4)
print(fitC)
summary(fitC)


# additive lmer + fitC  SVO_other_F4
fitC = lmer( formula = D1S1T ~ grp+SVO_other_F1+(1|subIDT), data = DIM1)
print(fitC)
summary(fitC)

# multi lmer +
fitC = lmer( formula = D1S1T ~ grp*SVO_other_F1+(1|subIDT), data = DIM1)
print(fitC)
summary(fitC)


# additive lmer + fitC  SNS_n_F4
fitC = lmer( formula = D1S1T ~ grp+SNS_n_F3+(1|subIDT), data = DIM4)
print(fitC)
summary(fitC)

# multi lmer +
fitC = lmer( formula = D1S1T ~ grp*SNS_n_F4+(1|subIDT), data = DIM4)
print(fitC)
summary(fitC)


# additive lmer + fitC  desir_F1
fitC = lmer( formula = D1S1T ~ grp+desir_F4 +(1|subIDT), data = DIM4)
print(fitC)
summary(fitC)

# multi lmer +
fitC = lmer( formula = D1S1T ~ grp*desir_F4+(1|subIDT), data = DIM4)
print(fitC)
summary(fitC)


# additive lmer + fitC  WTC_F
fitC = lmer( formula = D1S1T ~ grp+WTC_F4 +(1|subIDT), data = DIM4)
print(fitC)
summary(fitC)

# multi lmer +
fitC = lmer( formula = D1S1T ~ grp*WTC_F4+(1|subIDT), data = DIM4)
print(fitC)
summary(fitC)


#CmpCoop_F4
# additive lmer + fitC  CmpCoop_F
fitC = lmer( formula = D1S1T ~ grp+CmpCoop_F4 +(1|subIDT), data = DIM4)
print(fitC)
summary(fitC)

# multi lmer +
fitC = lmer( formula = D1S1T ~ grp*CmpCoop_F4+(1|subIDT), data = DIM4)
print(fitC)
summary(fitC)


# additive lmer + fitC  CmpCoop_F
fitC = lmer( formula = D1S1T ~ grp+RSE_r_F4 +(1|subIDT), data = DIM4)
print(fitC)
summary(fitC)

# multi lmer +
fitC = lmer( formula = D1S1T ~ grp*RSE_r_F4+(1|subIDT), data = DIM4)
print(fitC)
summary(fitC)


# additive lmer + fitC  gender4
fitC = lmer( formula = D1S1T ~ grp+gender4 +(1|subIDT), data = DIM4)
print(fitC)
summary(fitC)

# multi lmer +
fitC = lmer( formula = D1S1T ~ grp*gender4+(1|subIDT), data = DIM4)
print(fitC)
summary(fitC)


# additive lmer + fitC  gender4
fitC = lmer( formula = D1S1T ~ grp+RSE_F4+(1|subIDT), data = DIM4)
print(fitC)
summary(fitC)

# multi lmer +
fitC = lmer( formula = D1S1T ~ grp*RSE_F4+(1|subIDT), data = DIM4)
print(fitC)
summary(fitC)


# additive lmer + fitC  SDO_F1
fitC = lmer( formula = D1S1T ~ grp+SDO_F4+(1|subIDT), data = DIM4)
print(fitC)
summary(fitC)

# multi lmer +
fitC = lmer( formula = D1S1T ~ grp*SDO_F4+(1|subIDT), data = DIM4)
print(fitC)
summary(fitC)


# additive lmer + fitC  SDO_F1
fitC = lmer( formula = D1S1T ~ grp+move4+(1|subIDT), data = DIM4)
print(fitC)
summary(fitC)

# multi lmer +
fitC = lmer( formula = D1S1T ~ grp*move4+(1|subIDT), data = DIM4)
print(fitC)
summary(fitC)

#post hoc of lmer fitA
emmeans(fitA, list(pairwise ~ grp+IRI_ECF1), adjust = "tukey")
emm_options(lmerTest.limit = 763, lmer.df = "satterthwaite")
tab_model(fitA, p.val = "satterthwaite")
summary(fitA)

emmeans(fitA, list(pairwise ~grp*IRI_ECF1), adjust = "tukey")
emm_options(lmerTest.limit = 763, lmer.df = "satterthwaite")
tab_model(fitA, p.val = "satterthwaite")
summary(fitA)


#post hoc of lmer fitB
emmeans(fitB, list(pairwise ~grp+Mach_Ctrl_F4), adjust = "tukey")
emm_options(lmerTest.limit = 763, lmer.df = "satterthwaite")
tab_model(fitB, p.val = "satterthwaite")
summary(fitB)

emmeans(fitB, list(pairwise ~ grp*Mach_Ctrl_F4), adjust = "tukey")
emm_options(lmerTest.limit = 763, lmer.df = "satterthwaite")
tab_model(fitB, p.val = "satterthwaite")
summary(fitB)


#post hoc of lmer fitC
emmeans(fitC, list(pairwise ~grp+Mach_S_F4), adjust = "tukey")
emm_options(lmerTest.limit = 763, lmer.df = "satterthwaite")
tab_model(fitC, p.val = "satterthwaite")
summary(fitC)

emmeans(fitC, list(pairwise ~grp*Mach_S_F4), adjust = "tukey")
emm_options(lmerTest.limit = 763, lmer.df = "satterthwaite")
tab_model(fitC, p.val = "satterthwaite")
summary(fitC)


#post hoc of lmer fitC
emmeans(fitC, list(pairwise ~grp+Mach_D_F4), adjust = "tukey")
emm_options(lmerTest.limit = 763, lmer.df = "satterthwaite")
tab_model(fitC, p.val = "satterthwaite")
summary(fitC)

#post hoc of lmer INCOM
emmeans(fitC, list(pairwise ~grp*INCOM_F4), adjust = "tukey")
emm_options(lmerTest.limit = 763, lmer.df = "satterthwaite")
tab_model(fitC, p.val = "satterthwaite")
summary(fitC)

emmeans(fitC, list(pairwise ~grp+INCOM_F4), adjust = "tukey")
emm_options(lmerTest.limit = 763, lmer.df = "satterthwaite")
tab_model(fitC, p.val = "satterthwaite")
summary(fitC)


#post hoc of lmer SVO Self
emmeans(fitC, list(pairwise ~grp*SVO_self_F4), adjust = "tukey")
emm_options(lmerTest.limit = 763, lmer.df = "satterthwaite")
tab_model(fitC, p.val = "satterthwaite")
summary(fitC)

emmeans(fitC, list(pairwise ~grp+SVO_self_F3), adjust = "tukey")
emm_options(lmerTest.limit = 763, lmer.df = "satterthwaite")
tab_model(fitC, p.val = "satterthwaite")
summary(fitC)

#post hoc of lmer SVO other
emmeans(fitC, list(pairwise ~grp*SVO_other_F2), adjust = "tukey")
emm_options(lmerTest.limit = 763, lmer.df = "satterthwaite")
tab_model(fitC, p.val = "satterthwaite")
summary(fitC)

emmeans(fitC, list(pairwise ~grp+SVO_other_F2), adjust = "tukey")
emm_options(lmerTest.limit = 763, lmer.df = "satterthwaite")
tab_model(fitC, p.val = "satterthwaite")
summary(fitC)

#post hoc of lmer SNS_N
emmeans(fitC, list(pairwise ~grp*SNS_n_F4), adjust = "tukey")
emm_options(lmerTest.limit = 763, lmer.df = "satterthwaite")
tab_model(fitC, p.val = "satterthwaite")
summary(fitC)

emmeans(fitC, list(pairwise ~grp+SNS_n_F4), adjust = "tukey")
emm_options(lmerTest.limit = 763, lmer.df = "satterthwaite")
tab_model(fitC, p.val = "satterthwaite")
summary(fitC)


#post hoc of lmer SNS_N
emmeans(fitC, list(pairwise ~grp*desir_F4), adjust = "tukey")
emm_options(lmerTest.limit = 763, lmer.df = "satterthwaite")
tab_model(fitC, p.val = "satterthwaite")
summary(fitC)

emmeans(fitC, list(pairwise ~grp+desir_F4), adjust = "tukey")
emm_options(lmerTest.limit = 763, lmer.df = "satterthwaite")
tab_model(fitC, p.val = "satterthwaite")
summary(fitC)

#post hoc of lmer WTC_F
emmeans(fitC, list(pairwise ~grp*WTC_F4), adjust = "tukey")
emm_options(lmerTest.limit = 763, lmer.df = "satterthwaite")
tab_model(fitC, p.val = "satterthwaite")
summary(fitC)

emmeans(fitC, list(pairwise ~grp+WTC_F4), adjust = "tukey")
emm_options(lmerTest.limit = 763, lmer.df = "satterthwaite")
tab_model(fitC, p.val = "satterthwaite")
summary(fitC)


#post hoc of lmer CmpCoop_F
emmeans(fitC, list(pairwise ~grp*RSE_r_F4), adjust = "tukey")
emm_options(lmerTest.limit = 763, lmer.df = "satterthwaite")
tab_model(fitC, p.val = "satterthwaite")
summary(fitC)

emmeans(fitC, list(pairwise ~grp+RSE_r_F4), adjust = "tukey")
emm_options(lmerTest.limit = 763, lmer.df = "satterthwaite")
tab_model(fitC, p.val = "satterthwaite")
summary(fitC)


#post hoc of lmer gender1
emmeans(fitC, list(pairwise ~grp*gender4), adjust = "tukey")
emm_options(lmerTest.limit = 763, lmer.df = "satterthwaite")
tab_model(fitC, p.val = "satterthwaite")
summary(fitC)

emmeans(fitC, list(pairwise ~grp+gender4), adjust = "tukey")
emm_options(lmerTest.limit = 763, lmer.df = "satterthwaite")
tab_model(fitC, p.val = "satterthwaite")
summary(fitC)


#post hoc of lmer RSE_F4
emmeans(fitC, list(pairwise ~grp*RSE_F4), adjust = "tukey")
emm_options(lmerTest.limit = 763, lmer.df = "satterthwaite")
tab_model(fitC, p.val = "satterthwaite")
summary(fitC)

emmeans(fitC, list(pairwise ~grp+RSE_F4), adjust = "tukey")
emm_options(lmerTest.limit = 763, lmer.df = "satterthwaite")
tab_model(fitC, p.val = "satterthwaite")
summary(fitC)

#post hoc of lmer SDO_F1
emmeans(fitC, list(pairwise ~grp*SDO_F4), adjust = "tukey")
emm_options(lmerTest.limit = 763, lmer.df = "satterthwaite")
tab_model(fitC, p.val = "satterthwaite")
summary(fitC)

emmeans(fitC, list(pairwise ~grp+SDO_F4), adjust = "tukey")
emm_options(lmerTest.limit = 763, lmer.df = "satterthwaite")
tab_model(fitC, p.val = "satterthwaite")
summary(fitC)

#post hoc of lmer move
emmeans(fitC, list(pairwise ~grp*move4), adjust = "tukey")
emm_options(lmerTest.limit = 763, lmer.df = "satterthwaite")
tab_model(fitC, p.val = "satterthwaite")
summary(fitC)

emmeans(fitC, list(pairwise ~grp+move4), adjust = "tukey")
emm_options(lmerTest.limit = 763, lmer.df = "satterthwaite")
tab_model(fitC, p.val = "satterthwaite")
summary(fitC)


# GLM for frequency of generous choices (s1, s2, s3) ~ DIM1T ratings 


# multi lmer vGen Dim 

fitE = lmer( formula = vGen ~ grp+(1|subIDT), data = my_data)
print(fitE)
summary(fitE)


# additive lmer + fitC  SDO_F1
fitE = lmer( formula = vGen ~ grp+(1|subIDT), data = my_data)
print(fitE)
summary(fitE)


#post hoc test 
emmeans(fitE, list(pairwise ~grp), adjust = "tukey")
emm_options(lmerTest.limit = 763, lmer.df = "satterthwaite")
tab_model(fitE, p.val = "satterthwaite")
summary(fitE)



# Multivariate multiple regression 

FIT = lm(cbind(vGen, D1S1T) ~ grp +(1|subIDT), data = my_data)
summary(FIT)

emmeans(FIT, pairwise ~ grp,data = my_data)
emm_options(lmerTest.limit = 763, lmer.df = "satterthwaite")
tab_model(FIT, p.val = "satterthwaite")
summary(FIT)












# possible visualization options:
#library("ggpubr")
#ggline(my_data, x = "grp", y = "D1S1", 
#add = c("mean_se", "jitter"), 
#order = c("ctrl", "trt1", "trt2"),
#ylab = "RawData", xlab = "SelfRank")

#Box plot
boxplot(D1S1T ~ grp*param*Mach_Ctrl_F1, data = move1,
        xlab = "Dimension and Self Rank", ylab = "D1S1=RawData")
#plotmeans
library("gplots")
plotmeans(D1S1T ~ Mach_Ctrl_F1, data = move1, frame = FALSE,
          xlab = "Self Rank", ylab = "D1S1 = RawData",
          main="Mean Plot with 95% CI") 



# 1. Homogeneity of variances
plot(res.aov1, 1)
qqmath(fit1, id=0.05)
library(car)
leveneTest(D1S1 ~ param, data = my_data)
fligner.test(D1S1 ~ param, data = my_data)
shapiro.test(my_data$D1S1)
hist(my_data$D1S1, col='steelblue')
ggqqplot(my_data$D1S1)
# 2. Normality
plot(res.aov1, 2)

ggbetweenstats(warpbreaks,wool, breaks, outlier.tagging = TRUE)

qqmath(~log(D1S1T) | grp*param, data = my_data, xlab = "Ordered trials", ylab = "log(RT)")
qqmath(~log(D1S1T) | grp*param, data = my_data22, xlab = "Ordered trials", ylab = "log(RT)")
qqmath(~log(D1S1T) | grp*param, data = my_data22, xlab = "Ordered trials", ylab = "log(RT)", ylim = c(-6, 2))


#rep <- rep[which(abs(rep$sd) < 2.5), ]  # exclude outlier trials
#my_data2 <- my_data[which(abs(my_data$D1S1 < -3.86803))]

summary(my_data)

install.packages("ggstatsplot")
library(ggstatsplot)

lol <- boxplot(D1S1T~param*Mach_Ctrl_F1, data=move1, notch=TRUE,
               col=(c("gold","darkgreen")),
               main="Dimensions Self_Rank", xlab="S1D1 - S3D4, all combos in order")$out


#ggplot(my_data,aes(y=D1S1, x=SelfR, fill=DIM))+
#stat_summary(fun.y="mean", geom="bar",position="dodge")+
#stat_summary(fun.data = mean_se, geom = "errorbar", position="dodge",width=.8)


#### der coole PLOT #####

ggboxplot(move1, x= "DIM", y = "D1S1", color = "SelfR", main= "Self in Dimensions in RawData")
stat_summary(fun = mean, geom = "box", position= "dodge", color = "black")+ stat_summary(fun = mean, geom = "point")


ggboxplot(D1S1T~grp,y= my_data$param, data= my_data, main= "Self in Dimensions in RawData", notch = TRUE)
stat_summary(fun = mean, geom = "box", position= "dodge", color = "black")+ stat_summary(fun = mean, geom = "point")



library(dplyr)
library(lme4)


# Exclude outliers from DIM, SelfR, and D1S1
my_data22 <- my_data %>% 
  filter(DIMT >= quantile(DIMT, 0.05) & DIMT <= quantile(DIMT, 0.95),
         SelfRT >= quantile(SelfRT, 0.05) & SelfRT <= quantile(SelfRT, 0.95),
         D1S1T >= quantile(D1S1T, 0.05) & D1S1T <= quantile(D1S1T, 0.95))

fit2 = lmer( formula = D1S1 ~ param* grp+(1|subID), data = my_data22)
print(fit2)
summary(fit2)

model <- lmer(formula, data=my_data22)

summary(model)

#post hoc of lmer model

emmeans(fit2, list(pairwise ~param*grp), adjust = "tukey")
emm_options(lmerTest.limit = 763, lmer.df = "satterthwaite")
tab_model(fit2, p.val = "satterthwaite")
summary(fit2)

# 1. Homogeneity of variances
plot(res.aov1, 1)
qqmath(fit1, id=0.05)
library(car)
leveneTest(D1S1 ~ param, data = my_data)
fligner.test(D1S1 ~ param, data = my_data)
shapiro.test(my_data22$D1S1)
hist(my_data22$D1S1, col='steelblue')
ggqqplot(my_data$D1S1)
# 2. Normality
plot(res.aov1, 2)





############################### NOT USED #####################################################################

res.aov2 <- aov(D1S1T ~ grp, data = move0)
summary (res.aov2)

res.aov3 <- aov(D1S1T ~ grp, data = move1)
summary (res.aov3)

res.aov21 <- aov(D1S1T ~ grp*IRI_ECF1, data = move0)
summary (res.aov21)

res.aov31 <- aov(D1S1T ~ grp*IRI_ECFM1, data = move1)
summary (res.aov31)

res.aov22 <- aov(D1S1T ~ grp*IRI_ECF2, data = move0)
summary (res.aov22)

res.aov32 <- aov(D1S1T ~ grp*IRI_ECF2, data = move1)
summary (res.aov32)

res.aov23 <- aov(D1S1T ~ grp*IRI_ECF3, data = move0)
summary (res.aov23)

res.aov33 <- aov(D1S1T ~ grp*IRI_ECF3, data = move1)
summary (res.aov33)

res.aov24 <- aov(D1S1T ~ grp*IRI_ECF4, data = move0)
summary (res.aov24)

res.aov34 <- aov(D1S1T ~ grp*IRI_ECF4, data = move1)
summary (res.aov34)



emmeans(res.aov21, list(pairwise ~ grp), adjust = "tukey")
emmeans(res.aov23, list(pairwise ~ grp), adjust = "tukey")
emmeans(res.aov24, list(pairwise ~ grp), adjust = "tukey")
emmeans(res.aov31, list(pairwise ~ grp), adjust = "tukey")
emmeans(res.aov32, list(pairwise ~ grp), adjust = "tukey")
emmeans(res.aov33, list(pairwise ~ grp), adjust = "tukey")
emmeans(res.aov34, list(pairwise ~ grp), adjust = "tukey")


TukeyHSD(res.aov2, which = "grp")
TukeyHSD(res.aov21, which = "grp")
TukeyHSD(res.aov22, which = "grp")
TukeyHSD(res.aov23, which = "grp")
TukeyHSD(res.aov24, which = "grp")


TukeyHSD(res.aov3, which = "grp")
TukeyHSD(res.aov31, which = "grp")
TukeyHSD(res.aov32, which = "grp")
TukeyHSD(res.aov33, which = "grp")
TukeyHSD(res.aov34, which = "grp")


emmeans(res.aov2, list(pairwise ~grp*IRI_ECF1), adjust = "tukey")
emmeans(res.aov22, list(pairwise ~grp*IRI_ECF2), adjust = "tukey")
emmeans(res.aov23, list(pairwise ~grp*IRI_ECF3), adjust = "tukey")
emmeans(res.aov24, list(pairwise ~grp*IRI_ECF4), adjust = "tukey")

emmeans(res.aov31, list(pairwise ~grp*IRI_ECF1), adjust = "tukey")
emmeans(res.aov32, list(pairwise ~grp*IRI_ECF2), adjust = "tukey")
emmeans(res.aov33, list(pairwise ~grp*IRI_ECF3), adjust = "tukey")
emmeans(res.aov34, list(pairwise ~grp*IRI_ECF4), adjust = "tukey")



emmeans(res.aov2, list(pairwise ~ grp), adjust = "tukey")
emmeans(res.aov2, list(pairwise ~param*grp), adjust = "tukey")

emmeans(res.aov3, list(pairwise ~ grp), adjust = "tukey")
emmeans(res.aov3, list(pairwise ~param*grp), adjust = "tukey")



#################################### NEW     DIMENSION                       ################################################################
#################################### NEW     CODE                 ################################################################
#################################### NEW                ################################################################
#################################### NEW        ################################################################
#################################### NEW ################################################################
install.packages("jtools")

library("emmeans") 
library("ggpubr")
library("ggplot2")
library("tidyverse")
library("plotly")
library("colorspace")
library("ggsignif")
library("gt")
library("dplyr")
library("broom")
library(sjPlot)
library(sjstats)
library("xtable")
library("dplyr")
library("tidyr")
library("knitr")
library("sjPlot")
library("brms")
library("glmmTMB")
library("MASS")
library("gplots")
library(car)
library(stats)
library(lattice)
library(jtools)

#rm(list = ls(all.names = TRUE))
# read the csv file
my_data <- read.csv("C:/Users/Neuroeconomics Lab/Documents/R/Dime_08_23.csv", head = TRUE, sep=",")
my_data
my_dataSub <- my_data[ , c("subID",	"D1",	"D2",	"D3",	"D4",	"Fqgen", "Move","SelfRank", "Dimension",	"DimIndex")]

# group level are the self ranks, this means the rank of the individual in the hierarchy,
#s1 = BEGINNER, s2 = INTERMEDIATE, s3 = EXPERT

grp <- factor(my_data$SelfRank, levels = c(1, 2, 3), labels = c("S1", "S2", "S3"))
Dim <- factor(my_data$DimIndex, levels = c(1, 2, 3, 4), labels = c("Dim1", "Dim2", "Dim3", "Dim4"))


move1 <- subset(my_data, Move == '1' )
move0 <- subset(my_data, Move == '0' )
grp1 <- factor(move1$SelfRank, levels = c(1, 2, 3), labels = c("S1", "S2", "S3"))
grp0 <- factor(move0$SelfRank, levels = c(1, 2, 3), labels = c("S1", "S2", "S3"))
Dim1 <- factor(move1$DimIndex, levels = c(1, 2, 3, 4), labels = c("Dim1", "Dim2", "Dim3", "Dim4"))
Dim0 <- factor(move0$DimIndex, levels = c(1, 2, 3, 4), labels = c("Dim1", "Dim2", "Dim3", "Dim4"))


# multi lmer for move 0, and post hoc tukey with estimated means
fitA = lmer(formula = Fqgen ~ grp0 * D1 * D2 * D3 * D4 +(1|subID), data = move0)
summary(fitA)


emmeans(fitA, list(pairwise ~ grp0 * D1 * D2 * D3 * D4), adjust = "tukey")
emm_options(lmerTest.limit = 763, lmer.df = "satterthwaite")
tab_model(fitA, p.val = "satterthwaite")
summary(fitA)

# multi lmer for move 1, and post hoc tukey with estimated means
fitA = lmer(formula = Fqgen ~ grp1 * D1 * D2 * D3 * D4 +(1|subID), data = move1)
summary(fitA)

emmeans(fitA, list(pairwise ~grp1 * D1 * D2 * D3 * D4), adjust = "tukey")
emm_options(lmerTest.limit = 763, lmer.df = "satterthwaite")
tab_model(fitA, p.val = "satterthwaite")
summary(fitA)

# Visualization attempts 

install.packages("ggfortify")
library(ggfortify)

ggplot(move0, aes(x = D1, y = Fqgen))+ 
  geom_point()
ggplot(move0, aes(x = D2, y = Fqgen))+ 
  geom_point()
ggplot(move0, aes(x = D3, y = Fqgen))+ 
  geom_point()
ggplot(move0, aes(x = D4, y = Fqgen))+ 
  geom_point()


library(plotly)

big_plot <- ggplot(my_data,
                   aes(x = Dimension, y = Fqgen, 
                       color = grp, shape = Dim))+
  geom_point()

ggplotly(big_plot)

# plot for interaction 
big_plot0 <- ggplot(my_data,
                   aes(x = Dimension, y = Fqgen, 
                       color = grp, shape = Dim))+
  geom_point()
ggplotly(big_plot)


# plot for interaction MOVE 0  
big_plot0 <- ggplot(move0,
                    aes(x = Dimension, y = Fqgen, 
                        color = grp0, shape = Dim0))+
  geom_point() 
ggplotly(big_plot0)
ggplot(move0, aes(x = grp0, y = Fqgen)) + geom_boxplot()

install.packages(c("rgl", "car"))
library("rgl")
library("car")

# VERY COOL PLOT 
scatter3d(x=Dimension1, y= Dimension2, z = Dimension3)

Dimension1 <- my_data$D1
Dimension2 <- my_data$D2
Dimension3 <- my_data$D3
Dimension4 <- my_data$D4



# Forest plot move 0 
# Load required

install.packages("forestplot")
library(lme4)
library(ggplot2)
library(forestplot)

# Load required packages
library(lme4)
library(ggplot2)

# Fit the linear mixed-effects model
fitA <- lmer(formula = Fqgen ~ grp0 * D1 * D2 + (1 | subID), data = move0)

# Predict the response variable using the model
move0$Fqgen_pred <- predict(fitA)

# IMPORTANT INTERACTION MOVE 0 
# fit model with 3-way-interaction
fitA <- lmer(formula = Fqgen ~ grp0 * D1 * D2 + (1 | subID), data = move0)
emmeans(fitA, list(pairwise ~grp0 * D1 * D2), adjust = "tukey")
emm_options(lmerTest.limit = 763, lmer.df = "satterthwaite")
tab_model(fitA, p.val = "satterthwaite")
summary(fitA)
summary(fitA)

# select only levels 30, 50 and 70 from continuous variable Barthel-Index
library(sjPlot)
library(sjmisc)
library(ggplot2)
theme_set(theme_sjplot())

# fit model with interaction

plot_model(fitA, type = "pred", terms = c("grp0", "Dimension"))



# 3D Scatter Plot 
library(plotly)

library(plotly)

data <- read.csv("https://raw.githubusercontent.com/plotly/datasets/master/gapminderDataFiveYear.csv")

data_2007 <- data[which(data$year == 2007),]
data_2007 <- data_2007[order(data_2007$continent, data_2007$country),]
data_2007$size <- data_2007$pop
colors <- c('#4AC6B7', '#1972A4', '#965F8A', '#FF7070', '#C61951')

fig <- plot_ly(data_2007, x = ~gdpPercap, y = ~lifeExp, z = ~pop, color = ~continent, size = ~size, colors = colors,
               marker = list(symbol = 'circle', sizemode = 'diameter'), sizes = c(5, 150),
               text = ~paste('Country:', country, '<br>Life Expectancy:', lifeExp, '<br>GDP:', gdpPercap,
                             '<br>Pop.:', pop))
fig <- fig %>% layout(title = 'Life Expectancy v. Per Capita GDP, 2007',
                      scene = list(xaxis = list(title = 'GDP per capita (2000 dollars)',
                                                gridcolor = 'rgb(255, 255, 255)',
                                                range = c(2.003297660701705, 5.191505530708712),
                                                type = 'log',
                                                zerolinewidth = 1,
                                                ticklen = 5,
                                                gridwidth = 2),
                                   yaxis = list(title = 'Life Expectancy (years)',
                                                gridcolor = 'rgb(255, 255, 255)',
                                                range = c(36.12621671352166, 91.72921793264332),
                                                zerolinewidth = 1,
                                                ticklen = 5,
                                                gridwith = 2),
                                   zaxis = list(title = 'Population',
                                                gridcolor = 'rgb(255, 255, 255)',
                                                type = 'log',
                                                zerolinewidth = 1,
                                                ticklen = 5,
                                                gridwith = 2)),
                      paper_bgcolor = 'rgb(243, 243, 243)',
                      plot_bgcolor = 'rgb(243, 243, 243)')

fig

install.packages("scatter3D")
install.packages("sp")
install.packages("sf")
install.packages("rgl")
install.packages("plot3Drgl")
install.packages("plotly.js")
library(plot3Drgl)
library(rgl)
library(sp)
library(sf)
library(plotly.js)



plot3d( 
  x=my_data$D1, y=my_data$D2, z=my_data$D3, 
  col = my_data$Fqgen, 
  type = 's', 
  radius = .1,
  xlab="D1", ylab="D2", zlab="D3")

rglwidget()







X <- subset(my_data, select = c("D1","D2","D3","D4" ))

prin_comp <- prcomp(X, rank. = 4)

components <- prin_comp[["x"]]
components <- data.frame(components)
components$PC2 <- -components$PC2
components$PC3 <- -components$PC3
components = cbind(components, c(my_data$D1,my_data$D2,my_data$D3,my_data$D4 ))

tot_explained_variance_ratio <- summary(prin_comp)[["importance"]]['Proportion of Variance',]
tot_explained_variance_ratio <- 100 * sum(tot_explained_variance_ratio)

tit = 'Total Explained Variance = 99.48'

fig <- plot_ly(components, x = ~PC1, y = ~PC2, z = ~PC3, color = ~c(my_data$D1,my_data$D2,my_data$D3,my_data$D4 ), colors = c('#636EFA','#EF553B','#00CC96') )
  

fig <- fig %>%
  layout(
    title = tit,
    scene = list(bgcolor = "#e5ecf6")
  )

fig



#################################### END     DIMENSION                       ################################################################
#################################### END     CODE                ################################################################
#################################### END                ################################################################
#################################### END        ################################################################
#################################### END ################################################################












#################### NEW Data Set with all the moves inculded, Move = how an individual gains or loses a rank in the hierarchy #############################################################################################

#rm(list = ls(all.names = TRUE))

# read the csv file
my_data <- read.csv("C:/Users/Neuroeconomics Lab/Documents/R/Dime_allMoves_08_23.csv", head = TRUE, sep=",")
my_data
my_dataSub <- my_data[ , c("Move",	"subID","D1",	"D2",	"D3",	"D4",	"Fqgen",	"SelfRank")]

move1 <- subset(my_data, Move == '1' )
move0 <- subset(my_data, Move == '0' )
move2 <- subset(my_data, Move == '2' )
move3 <- subset(my_data, Move == '3' )




grp <- factor(my_data$SelfRank, levels = c(0, 1, 3), labels = c("S1", "S2", "S3"))
grp1 <- factor(move1$SelfRank, levels = c(0, 1, 3), labels = c("S1", "S2", "S3"))
grp0 <- factor(move0$SelfRank, levels = c(0, 1, 3), labels = c("S1", "S2", "S3"))
grp2 <- factor(move2$SelfRank, levels = c(0, 1, 3), labels = c("S1", "S2", "S3"))
grp3 <- factor(move3$SelfRank, levels = c(0, 1, 3), labels = c("S1", "S2", "S3"))

# multi lmer for move 0 
fitA = lmer(formula = Fqgen ~ grp0 * D1 * D2 * D3 * D4 +(1|subID), data = move0)
summary(fitA)

emmeans(fitA, list(pairwise ~grp0 * D1 * D2 * D3 * D4), adjust = "tukey")
emm_options(lmerTest.limit = 763, lmer.df = "satterthwaite")
tab_model(fitA, p.val = "satterthwaite")
summary(fitA)

# multi lmer for move 1
fitA = lmer(formula = Fqgen ~ grp1 * D1 * D2 * D3 * D4 +(1|subID), data = move1)
summary(fitA)

emmeans(fitA, list(pairwise ~grp1 * D1 * D2 * D3 * D4), adjust = "tukey")
emm_options(lmerTest.limit = 763, lmer.df = "satterthwaite")
tab_model(fitA, p.val = "satterthwaite")
summary(fitA)

# multi lmer for move 2
fitA = lmer(formula = Fqgen ~ grp2 * D1 * D2 * D3 * D4 +(1|subID), data = move2)
summary(fitA)

emmeans(fitA, list(pairwise ~grp2 * D1 * D2 * D3 * D4), adjust = "tukey")
emm_options(lmerTest.limit = 763, lmer.df = "satterthwaite")
tab_model(fitA, p.val = "satterthwaite")
summary(fitA)


# multi lmer for move 3
fitA = lmer(formula = Fqgen ~ grp3 * D1 * D2 * D3 * D4 +(1|subID), data = move3)
summary(fitA)

emmeans(fitA, list(pairwise ~grp3 * D1 * D2 * D3 * D4), adjust = "tukey")
emm_options(lmerTest.limit = 763, lmer.df = "satterthwaite")
tab_model(fitA, p.val = "satterthwaite")
summary(fitA)


# different model

# multi lmer for move 0 
fitA = lmer(formula = Fqgen ~ D1 * D2 * D3 * D4 +(1|subID), data = move2)
summary(fitA)

emmeans(fitA, list(pairwise ~ D1 * D2 * D3 * D4), adjust = "tukey")
emm_options(lmerTest.limit = 763, lmer.df = "satterthwaite")
tab_model(fitA, p.val = "satterthwaite")
summary(fitA)

# multi lmer for move 1
fitA = lmer(formula = Fqgen ~ grp1 + D1 + D2 + D3 + D4 +(1|subID), data = move1)
summary(fitA)

emmeans(fitA, list(pairwise ~grp1 + D1 + D2 + D3 + D4), adjust = "tukey")
emm_options(lmerTest.limit = 763, lmer.df = "satterthwaite")
tab_model(fitA, p.val = "satterthwaite")
summary(fitA)



# multi lmer for move 3
fitA = lmer(formula = Fqgen ~ grp3 + D1 + D2 + D3 + D4 +(1|subID), data = move3)
summary(fitA)

emmeans(fitA, list(pairwise ~grp3 + D1 + D2 + D3 + D4), adjust = "tukey")
emm_options(lmerTest.limit = 763, lmer.df = "satterthwaite")
tab_model(fitA, p.val = "satterthwaite")
summary(fitA)




