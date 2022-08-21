"R code for project"
"Author: Chris Ni"
"Start Date: 10/08/2021"
save.image()
library(dplyr)
library(tidyverse)
#1 Data Preparation
#1.1 Setting working directory
setwd("C:/Users/nm061/OneDrive/Actuarial Data Analytics/ACST8095/Project Due 20210910")
#1.2 Importing Data
cancer <- read.csv("2_R code/Cancer.csv")
"Transform dataset into tibble"
cancer <- as_tibble(cancer)
head(cancer)
#1.3 Overview on Data
head(cancer)
names(cancer)
str(cancer)
summary(cancer)
#1.4 Data Cleaning Summary
"A. Rename Variable"
cancer_2 <- rename(cancer,PersonID="ï..PersonID")
"B. Transform string under Race to integer for following regression"
cancer_2["Race"][cancer_2["Race"] == "1 non-indigenous citizen"] <- 1
cancer_2["Race"][cancer_2["Race"] == "2 indigenous citizen"] <- 2
cancer_2["Race"][cancer_2["Race"] == "3 other"] <- 3
unique(cancer_2$Race)
"C. Smoker--99 shown under Smoker which should be data error"
cancer_2["Smoker"][cancer_2["Smoker"] == 99] <- "not clear"
"D. Age--need to exclud ages under 18"
summary(cancer_2$Age)
hist(cancer_2$Age,xlim=c(18,90),ylim = c(0,300),xlab = "Age",ylab = "Count",main = "Frequency Distribution of Sample Age")
cancer_3 <- cancer_2[which(cancer_2$Age>=18),]
cancer_train <- cancer_3 %>% filter(Dataset=='train')
"E. Data Transformation"
cancer_3$Gender <- factor(cancer_3$Gender)
cancer_3$MHscore <- factor(cancer_3$MHscore)
cancer_3$Totalothermorbidity <- factor(cancer_3$Totalothermorbidity)
cancer_3$Insurance <- factor(cancer_3$Insurance)
cancer_3$Employment <- factor(cancer_3$Employment)
cancer_3$Race <- factor(cancer_3$Race)
cancer_3$Smoker <- factor(cancer_3$Smoker)
cancer_3$Emphysema <- factor(cancer_3$Emphysema)
cancer_3$Stroke <- factor(cancer_3$Stroke)
cancer_3$Coronary <- factor(cancer_3$Coronary)
cancer_3$Cholesterol <- factor(cancer_3$Cholesterol)
cancer_3$Diabetes <- factor(cancer_3$Diabetes)
cancer_3$Asthma <- factor(cancer_3$Asthma)
cancer_3$HighBP <- factor(cancer_3$HighBP)
#2 Data Check
#2.1 Overall Check
"Missing Value"
install.packages("naniar")
install.packages("simputation")
library(naniar)
library(simputation)
miss_var_summary(cancer_3)
"Duplicates"
nrow(distinct(cancer_3))
"Train & Test"
nrow(cancer_3 %>% filter(Dataset=="train"))
nrow(cancer_3 %>% filter(Dataset=="test"))
#2.2 Variable Names Check
names(cancer_2)
#2.3 Dichotomous Variables Check
"Gender--Result is fine"
unique(cancer_3$Gender)
prop.table(table(cancer_3$Gender))
"Employment--Result is fine"
unique(cancer_3$Employment)
prop.table(table(cancer_3$Employment))
"Insurance--Result is fine"
unique(cancer_3$Insurance)
prop.table(table(cancer_3$Insurance))
"Smoker--99 shown under Smoker which should be data error"
cancer_2["Smoker"][cancer_2["Smoker"] == 99] <- "not clear"
unique(cancer_3$Smoker)
prop.table(table(cancer_3$Smoker))
"Emphysema--Result is fine"
unique(cancer_3$Emphysema)
prop.table(table(cancer_3$Emphysema))
"Stroke--Result is fine"
unique(cancer_3$Stroke)
prop.table(table(cancer_3$Stroke))
"Coronary--Result is fine"
unique(cancer_3$Coronary)
prop.table(table(cancer_3$Coronary))
"Cholesterol--Result is fine"
unique(cancer_3$Cholesterol)
prop.table(table(cancer_3$Cholesterol))
"Diabetes--Result is fine"
unique(cancer_3$Diabetes)
prop.table(table(cancer_3$Diabetes))
"Asthma--Result is fine"
unique(cancer_3$Asthma)
prop.table(table(cancer_3$Asthma))
"HighBP--Result is fine"
unique(cancer_3$HighBP)
prop.table(table(cancer_3$HighBP))
#2.4 Numerical Variables Check
"Expenditure"
summary(cancer_3$Expenditure)
cancer_3 %>%
  ggplot(aes(Expenditure/1000)) + geom_boxplot(fill="light blue",color="dark blue") +
  labs(
    title = "Figure 2.1 Boxplot of Expenditure",
    subtitle = "Adults (age 18 and over)",
    caption = "Data from survey in 2020.",
    x = "Expenditure($thousand)",
  ) +
  geom_vline(aes(xintercept=25),color="red",linetype="dashed",size=1) +
  geom_vline(aes(xintercept=125),color="red",linetype="dashed",size=1) +
  scale_x_continuous(breaks = seq(0, 250, 25), lim = c(0, 250))
nrow(cancer_3 %>% filter(Expenditure<=25000))
nrow(cancer_3 %>% filter(Expenditure>25000 & Expenditure<=125000))
nrow(cancer_3 %>% filter(Expenditure>125000))
nrow(cancer_3 %>% filter(Expenditure<=25000))/nrow(cancer_3)
nrow(cancer_3 %>% filter(Expenditure>25000 & Expenditure<=125000))/nrow(cancer_3)
nrow(cancer_3 %>% filter(Expenditure>125000))/nrow(cancer_3)
line.data.expenditure <- data.frame(xintercept=c(median(cancer_3$Expenditure)/1000,mean(cancer_3$Expenditure)/1000),lines=c("Median","Mean")
                                    ,color=c("red","purple"))
cancer_3 %>%
  ggplot(aes(x=Expenditure/1000)) + geom_histogram(color="dark blue",fill="light blue") +
  labs(
    title = "Figure 1.1 Histogram of Expenditure",
    subtitle = "Adults (age 18 and over)",
    caption = "Data from survey in 2020.",
    x = "Expenditure($thousand)"
  ) +
  geom_vline(aes(xintercept=xintercept,color=lines),line.data.expenditure,linetype="dashed",size=1) +
  scale_color_manual(values = line.data.expenditure$color)
"Income"
summary(cancer_3$Income)
cancer_3 %>%
  ggplot(aes(Income/1000)) + geom_boxplot(fill="light blue",color="dark blue") +
  labs(
    title = "Figure 2.2 Boxplot of Income",
    subtitle = "Adults (age 18 and over)",
    caption = "Data from survey in 2020.",
    x = "Income($thousand)",
  ) +
  geom_vline(aes(xintercept=100),color="red",linetype="dashed",size=1) +
  geom_vline(aes(xintercept=250),color="red",linetype="dashed",size=1) +
  scale_x_continuous(breaks = seq(0, 400, 50), lim = c(0, 400))
nrow(cancer_3 %>% filter(Income<=100000))
nrow(cancer_3 %>% filter(Income>100000 & Income<=250000))
nrow(cancer_3 %>% filter(Income>250000 & Income<=400000))
nrow(cancer_3 %>% filter(Income<=100000))/nrow(cancer_3)
nrow(cancer_3 %>% filter(Income>100000 & Income<=250000))/nrow(cancer_3)
nrow(cancer_3 %>% filter(Income>250000 & Income<=400000))/nrow(cancer_3)
line.data.income <- data.frame(xintercept=c(median(cancer_3$Income)/1000,mean(cancer_3$Income)/1000),lines=c("Median","Mean"),color=c("red","purple"))
cancer_3 %>%
  ggplot(aes(x=Income/1000)) + geom_histogram(color="dark blue",fill="light blue") +
  labs(
    title = "Figure 1.2 Histogram of Income",
    subtitle = "Adults (age 18 and over)",
    caption = "Data from survey in 2020.",
    x = "Income($thousand)"
  ) +
  geom_vline(aes(xintercept=xintercept,color=lines),line.data.income,linetype="dashed",size=1) +
  scale_color_manual(values = line.data.income$color) +
  xlim(0,300)
"Age"
summary(cancer_3$Age)
cancer_low_age <- cancer_3 %>% filter(Age<=19)
view(cancer_low_age)
cancer_3 %>%
  ggplot(aes(Age)) + geom_boxplot(fill="light blue",color="dark blue") +
  labs(
    title = "Figure 2.3 Boxplot of Age",
    subtitle = "Adults (age 18 and over)",
    caption = "Data from survey in 2020.",
    x = "Age",
  ) +
  geom_vline(aes(xintercept=22),color="red",linetype="dashed",size=1) +
  scale_x_continuous(breaks = seq(18, 88, 5), lim = c(18, 88))
line.data.Age <- data.frame(xintercept=c(median(cancer_3$Age),mean(cancer_3$Age)),lines=c("Median","Mean"),color=c("red","purple"))
cancer_3 %>%
  ggplot(aes(x=Age)) + geom_histogram(color="dark blue",fill="light blue") +
  labs(
    title = "Figure 1.3 Histogram of Age",
    subtitle = "Adults (age 18 and over)",
    caption = "Data from survey in 2020.",
    x = "Age"
  ) +
  geom_vline(aes(xintercept=xintercept,color=lines),line.data.Age,linetype="dashed",size=1) +
  scale_color_manual(values = line.data.Age$color) +
  scale_x_continuous(breaks = seq(18, 85, 5), lim = c(18, 85))
"BMI"
summary(cancer_3$BMI)
cancer_3 %>%
  ggplot(aes(BMI)) + geom_boxplot(fill="light blue",color="dark blue") +
  labs(
    title = "Figure 2.4 Boxplot of BMI",
    subtitle = "Adults (age 18 and over)",
    caption = "Data from survey in 2020.",
    x = "BMI",
  ) +
  geom_vline(aes(xintercept=17),color="red",linetype="dashed",size=1) +
  geom_vline(aes(xintercept=49),color="red",linetype="dashed",size=1) +
  scale_x_continuous(breaks = seq(0, 80, 5), lim = c(0, 80))
nrow(cancer_3 %>% filter(BMI<=40))
nrow(cancer_3 %>% filter(BMI>40))
line.data.BMI <- data.frame(xintercept=c(median(cancer_3$BMI),mean(cancer_3$BMI)),lines=c("Median","Mean"),color=c("red","purple"))
cancer_3 %>%
  ggplot(aes(x=BMI)) + geom_histogram(color="dark blue",fill="light blue") +
  labs(
    title = "Figure 1.4 Histogram of BMI",
    subtitle = "Adults (age 18 and over)",
    caption = "Data from survey in 2020.",
    x = "BMI"
  ) +
  geom_vline(aes(xintercept=xintercept,color=lines),line.data.BMI,linetype="dashed",size=1) +
  scale_color_manual(values = line.data.BMI$color) +
  xlim(0,80)
nrow(cancer_3 %>% filter(BMI<=50))
nrow(cancer_3 %>% filter(BMI>50))
#2.5 Ordinal variables Check
"Race--Result is fine"
unique(cancer_3$Race)
prop.table(table(cancer_3$Race))
"MHscore(1-4)--Result is fine"
unique(cancer_3$MHscore)
prop.table(table(cancer_3$MHscore))
cancer_3 %>%
  ggplot(aes(MHscore)) + geom_bar(color="dark blue",fill="light blue",width = 0.5) +
  labs(
    title = "Figure 1.5 Barplot of MHscore",
    subtitle = "Adults (age 18 and over)",
    caption = "Data from survey in 2020.",
    x = "MHscore"
  ) + ylim(0,800)
"Totalothermorbidity(0-7)--Result is fine"
unique(cancer_3$Totalothermorbidity)
prop.table(table(cancer_3$Totalothermorbidity))
cancer_3 %>%
  ggplot(aes(Totalothermorbidity)) + geom_bar(color="dark blue",fill="light blue",width = 0.5) +
  labs(
    title = "Figure 1.6 Barplot of Totalothermorbidity",
    subtitle = "Adults (age 18 and over)",
    caption = "Data from survey in 2020.",
    x = "Totalothermorbidity"
  )
#3 Relationship
#Part1 Dichotomous Variables
"1.1 Check Insurance Status vs Expenditure"
cancer_3 %>% filter(Expenditure<=25000) %>%
  ggplot(aes(Insurance,Expenditure/1000,fill=Insurance)) + geom_boxplot(color="dark blue") +
  labs(
    title = "Figure 3.1 Boxplot of Expenditure vs Private Insurance",
    subtitle = "Adults (age 18 and over) & Low Expenditure Bracket",
    caption = "Data from survey in 2020.",
    x = "Private Insurance",
    y = "Expenditure($thousand)"
  )+ theme(legend.position="right")+
  scale_fill_manual("Insurance or Not",labels = c("No", "Yes"),values=c("light blue","pink"))
cancer_3 %>% filter(Expenditure>25000 & Expenditure<=125000) %>%
  ggplot(aes(Insurance,Expenditure/1000,fill=Insurance)) + geom_boxplot(color="dark blue") +
  labs(
    title = "Figure 3.2 Boxplot of Expenditure vs Private Insurance",
    subtitle = "Adults (age 18 and over) & High Expenditure Bracket",
    caption = "Data from survey in 2020.",
    x = "Private Insurance",
    y = "Expenditure($thousand)"
  )+ theme(legend.position="right")+
  scale_fill_manual("Insurance or Not",labels = c("No", "Yes"),values=c("light blue","pink"))
"Further breakdown based on expenditure and insurance"
cancer_NonInsured_low_exp <- cancer_3 %>% filter(Insurance==0 & Expenditure<=25000)
cancer_NonInsured_high_exp <- cancer_3 %>% filter(Insurance==0 & Expenditure>25000 & Expenditure<125000)
cancer_Insured_low_exp <- cancer_3 %>% filter(Insurance==1 & Expenditure<=25000)
cancer_Insured_high_exp <- cancer_3 %>% filter(Insurance==1 & Expenditure>25000 & Expenditure<125000)
"1.2 Disease Variables"
"Data Transformation"
DF_NonInsured_Low <- select(cancer_NonInsured_low_exp, Expenditure, Emphysema, Stroke, Coronary, Cholesterol, Diabetes,Asthma, HighBP)
DF_NonInsured_High <- select(cancer_NonInsured_high_exp, Expenditure, Emphysema, Stroke, Coronary, Cholesterol, Diabetes,Asthma, HighBP)
DF_Insured_Low <- select(cancer_Insured_low_exp, Expenditure, Emphysema, Stroke, Coronary, Cholesterol, Diabetes,Asthma, HighBP)
DF_Insured_High <- select(cancer_Insured_high_exp, Expenditure, Emphysema, Stroke, Coronary, Cholesterol, Diabetes,Asthma, HighBP)
DF_NonInsured_Low %>%
  gather(variable, value, -Expenditure) %>%
  ggplot(aes(factor(value), Expenditure/1000, fill = factor(value))) +
  geom_boxplot(color="dark blue") +
  facet_wrap(~variable, scales = "free_x", nrow = 1, strip.position = "bottom") +
  theme(panel.spacing = unit(0, "lines"),
        panel.border = element_rect(fill = NA),
        strip.background = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "none",
        strip.placement = "outside") +
  labs(
    title = "Figure 3.3 Boxplot of Expenditure vs Disease",
    subtitle = "Adults (age 18 and over) without Private Insurance & Low Expenditure Bracket",
    caption = "Data from survey in 2020.",
    x = "Disease",
    y = "Expenditure($thousand)"
  )+ theme(legend.position="right")+
  scale_fill_manual("Disease or Not",labels = c("No", "Yes"),values=c("light blue","pink")) +
  geom_hline(aes(yintercept=1),color="red",linetype="dashed",size=1) +
  geom_hline(aes(yintercept=15),color="red",linetype="dashed",size=1)
DF_Insured_Low %>%
  gather(variable, value, -Expenditure) %>%
  ggplot(aes(factor(value), Expenditure/1000, fill = factor(value))) +
  geom_boxplot(color="dark blue") +
  facet_wrap(~variable, scales = "free_x", nrow = 1, strip.position = "bottom") +
  theme(panel.spacing = unit(0, "lines"),
        panel.border = element_rect(fill = NA),
        strip.background = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "none",
        strip.placement = "outside") +
  labs(
    title = "Figure 3.4 Boxplot of Expenditure vs Disease",
    subtitle = "Adults (age 18 and over) with Private Insurance & Low Expenditure Bracket",
    caption = "Data from survey in 2020.",
    x = "Disease",
    y = "Expenditure($thousand)"
  )+ theme(legend.position="right")+
  scale_fill_manual("Disease or Not",labels = c("No", "Yes"),values=c("light blue","pink")) +
  geom_hline(aes(yintercept=0),color="red",linetype="dashed",size=1) +
  geom_hline(aes(yintercept=6),color="red",linetype="dashed",size=1)
DF_NonInsured_High %>%
  gather(variable, value, -Expenditure) %>%
  ggplot(aes(factor(value), Expenditure/1000, fill = factor(value))) +
  geom_boxplot(color="dark blue") +
  facet_wrap(~variable, scales = "free_x", nrow = 1, strip.position = "bottom") +
  theme(panel.spacing = unit(0, "lines"),
        panel.border = element_rect(fill = NA),
        strip.background = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "none",
        strip.placement = "outside") +
  labs(
    title = "Figure 3.5 Boxplot of Expenditure vs Disease",
    subtitle = "Adults (age 18 and over) without Private Insurance & High Expenditure Bracket",
    caption = "Data from survey in 2020.",
    x = "Disease",
    y = "Expenditure($thousand)"
  )+ theme(legend.position="right")+
  scale_fill_manual("Disease or Not",labels = c("No", "Yes"),values=c("light blue","pink")) +
  geom_hline(aes(yintercept=30),color="red",linetype="dashed",size=1) +
  geom_hline(aes(yintercept=70),color="red",linetype="dashed",size=1)
DF_Insured_High %>%
  gather(variable, value, -Expenditure) %>%
  ggplot(aes(factor(value), Expenditure/1000, fill = factor(value))) +
  geom_boxplot(color="dark blue") +
  facet_wrap(~variable, scales = "free_x", nrow = 1, strip.position = "bottom") +
  theme(panel.spacing = unit(0, "lines"),
        panel.border = element_rect(fill = NA),
        strip.background = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "none",
        strip.placement = "outside") +
  labs(
    title = "Figure 3.6 Boxplot of Expenditure vs Disease",
    subtitle = "Adults (age 18 and over) with Private Insurance & High Expenditure Bracket",
    caption = "Data from survey in 2020.",
    x = "Disease",
    y = "Expenditure($thousand)"
  )+ theme(legend.position="right")+
  scale_fill_manual("Disease or Not",labels = c("No", "Yes"),values=c("light blue","pink")) +
  geom_hline(aes(yintercept=10),color="red",linetype="dashed",size=1) +
  geom_hline(aes(yintercept=60),color="red",linetype="dashed",size=1)
"1.3 Gender and Race"
cancer_4$Race <- factor(cancer_4$Race, levels = c("1", "2","3"),
                        labels = c("Indigenous", "Non-Indigenous","Other"))
cancer_4 %>% filter(Expenditure<=25000 & Insurance=="No Private Insurance") %>%
  ggplot(aes(Gender,Expenditure/1000,fill=Gender)) + geom_boxplot(color="dark blue") +
  labs(
    title = "Figure 3.7 Boxplot of Expenditure vs Gender under Different Race",
    subtitle = "Adults (age 18 and over) without Private Insurance & Low Expenditure Bracket",
    caption = "Data from survey in 2020.",
    x = "Gender",
    y = "Expenditure($thousand)"
  )+ theme(legend.position="right") +
  scale_fill_manual("Male or Female",labels = c("Male", "Female"),values=c("light blue","pink")) +
  facet_wrap(~Race) +
  geom_hline(aes(yintercept=1),color="red",linetype="dashed",size=1) +
  geom_hline(aes(yintercept=11),color="red",linetype="dashed",size=1)
cancer_4 %>% filter(Expenditure<=25000 & Insurance=="Private Insurance") %>%
  ggplot(aes(Gender,Expenditure/1000,fill=Gender)) + geom_boxplot(color="dark blue") +
  labs(
    title = "Figure 3.8 Boxplot of Expenditure vs Gender under Different Race",
    subtitle = "Adults (age 18 and over) with Private Insurance & Low Expenditure Bracket",
    caption = "Data from survey in 2020.",
    x = "Gender",
    y = "Expenditure($thousand)"
  )+ theme(legend.position="right") +
  scale_fill_manual("Male or Female",labels = c("Male", "Female"),values=c("light blue","pink")) +
  facet_wrap(~Race) +
  geom_hline(aes(yintercept=0),color="red",linetype="dashed",size=1) +
  geom_hline(aes(yintercept=5),color="red",linetype="dashed",size=1)
cancer_4 %>% filter(Expenditure>25000 & Expenditure<125000 & Insurance=="No Private Insurance") %>%
  ggplot(aes(Gender,Expenditure/1000,fill=Gender)) + geom_boxplot(color="dark blue") +
  labs(
    title = "Figure 3.9 Boxplot of Expenditure vs Gender under Different Race",
    subtitle = "Adults (age 18 and over) without Private Insurance & High Expenditure Bracket",
    caption = "Data from survey in 2020.",
    x = "Gender",
    y = "Expenditure($thousand)"
  )+ theme(legend.position="right") +
  scale_fill_manual("Male or Female",labels = c("Male", "Female"),values=c("light blue","pink")) +
  facet_wrap(~Race) + scale_y_continuous(breaks = seq(25, 125, 25), lim = c(25, 125)) +
  geom_hline(aes(yintercept=25),color="red",linetype="dashed",size=1) +
  geom_hline(aes(yintercept=60),color="red",linetype="dashed",size=1)
cancer_4 %>% filter(Expenditure>25000 & Expenditure<125000 & Insurance=="Private Insurance") %>%
  ggplot(aes(Gender,Expenditure/1000,fill=Gender)) + geom_boxplot(color="dark blue") +
  labs(
    title = "Figure 3.10 Boxplot of Expenditure vs Gender under Different Race",
    subtitle = "Adults (age 18 and over) with Private Insurance & High Expenditure Bracket",
    caption = "Data from survey in 2020.",
    x = "Gender",
    y = "Expenditure($thousand)"
  )+ theme(legend.position="right") +
  scale_fill_manual("Male or Female",labels = c("Male", "Female"),values=c("light blue","pink")) +
  facet_wrap(~Race) +scale_y_continuous(breaks = seq(25, 125, 25), lim = c(25, 125))
"1.4 Employment"
cancer_3 %>% filter(Expenditure<=25000) %>%
  ggplot(aes(Employment,Expenditure/1000,fill=Employment)) + geom_boxplot(color="dark blue") +
  labs(
    title = "Figure 3.11 Boxplot of Expenditure vs Employment",
    subtitle = "Adults (age 18 and over) & Low Expenditure Bracket",
    caption = "Data from survey in 2020.",
    x = "Employment",
    y = "Expenditure($thousand)"
  )+ theme(legend.position="right")+
  scale_fill_manual("Employed or Not",labels = c("No", "Yes"),values=c("light blue","pink"))
cancer_3 %>% filter(Expenditure>25000 & Expenditure<=125000) %>%
  ggplot(aes(Employment,Expenditure/1000,fill=Employment)) + geom_boxplot(color="dark blue") +
  labs(
    title = "Figure 3.12 Boxplot of Expenditure vs Employment",
    subtitle = "Adults (age 18 and over) & High Expenditure Bracket",
    caption = "Data from survey in 2020.",
    x = "Employment",
    y = "Expenditure($thousand)"
  )+ theme(legend.position="right")+
  scale_fill_manual("Employed or Not",labels = c("No", "Yes"),values=c("light blue","pink"))
"1.5 Smoker"
cancer_3 %>% filter(Expenditure<=25000 & Smoker!="not clear") %>%
  ggplot(aes(Smoker,Expenditure/1000,fill=Smoker)) + geom_boxplot(color="dark blue") +
  labs(
    title = "Figure 3.13 Boxplot of Expenditure vs Smoker",
    subtitle = "Adults (age 18 and over) & Low Expenditure Bracket",
    caption = "Data from survey in 2020.",
    x = "Smoker",
    y = "Expenditure($thousand)"
  )+ theme(legend.position="right")+
  scale_fill_manual("Smoker or Not",labels = c("No", "Yes"),values=c("light blue","pink"))
cancer_3 %>% filter(Expenditure>25000 & Expenditure<=125000 & Smoker!="not clear") %>%
  ggplot(aes(Smoker,Expenditure/1000,fill=Smoker)) + geom_boxplot(color="dark blue") +
  labs(
    title = "Figure 3.14 Boxplot of Expenditure vs Smoker",
    subtitle = "Adults (age 18 and over) & High Expenditure Bracket",
    caption = "Data from survey in 2020.",
    x = "Smoker",
    y = "Expenditure($thousand)"
  )+ theme(legend.position="right")+
  scale_fill_manual("Smoker or Not",labels = c("No", "Yes"),values=c("light blue","pink"))
#Part2 Ordinal Variables
"2.1 MHscore"
cancer_4 <- cancer_3
cancer_4$Insurance <- factor(cancer_4$Insurance, levels = c("0", "1"),
                             labels = c("No Private Insurance", "Private Insurance"))
cancer_4 %>% filter(Expenditure<=25000) %>%
  ggplot(aes(MHscore,Expenditure/1000)) + geom_boxplot(fill="light blue",color="dark blue") +
  labs(
    title = "Figure 4.1 Boxplot of Expenditure vs MHscore",
    subtitle = "Adults (age 18 and over) Low Expenditure Bracket",
    caption = "Data from survey in 2020.",
    x = "MHscore",
    y = "Expenditure($thousand)"
  )+ theme(legend.position="none") +facet_grid(~ Insurance)
cancer_4 %>% filter(Expenditure>25000 & Expenditure<=125000) %>%
  ggplot(aes(MHscore,Expenditure/1000)) + geom_boxplot(fill="light blue",color="dark blue") +
  labs(
    title = "Figure 4.2 Boxplot of Expenditure vs MHscore",
    subtitle = "Adults (age 18 and over) High Expenditure Bracket",
    caption = "Data from survey in 2020.",
    x = "MHscore",
    y = "Expenditure($thousand)"
  )+ theme(legend.position="none") +facet_grid(~ Insurance) +
  scale_y_continuous(breaks = seq(25, 125, 25), lim = c(25, 125))
"2.2 Totalothermorbidity"
cancer_4 %>% filter(Expenditure<=25000) %>%
  ggplot(aes(Totalothermorbidity,Expenditure/1000)) + geom_boxplot(fill="light blue",color="dark blue") +
  labs(
    title = "Figure 4.3 Boxplot of Expenditure vs Total Other Morbidity",
    subtitle = "Adults (age 18 and over) Low Expenditure Bracket",
    caption = "Data from survey in 2020.",
    x = "Totalothermorbidity",
    y = "Expenditure($thousand)"
  )+ theme(legend.position="none") +facet_wrap(~ Insurance,ncol = 1)
cancer_4 %>% filter(Expenditure>25000 & Expenditure<=125000) %>%
  ggplot(aes(Totalothermorbidity,Expenditure/1000)) + geom_boxplot(fill="light blue",color="dark blue") +
  labs(
    title = "Figure 4.4 Boxplot of Expenditure vs Total Other Morbidity",
    subtitle = "Adults (age 18 and over) High Expenditure Bracket",
    caption = "Data from survey in 2020.",
    x = "Totalothermorbidity",
    y = "Expenditure($thousand)"
  )+ theme(legend.position="none") +facet_wrap(~ Insurance,ncol = 1) +
  scale_y_continuous(breaks = seq(25, 125, 25), lim = c(25, 125))
#Part3 Significance linear relationship between target variable and other numeric variable
cancer_numeric <- cancer_3 %>% filter(Expenditure<=15000 & Insurance==0 & Totalothermorbidity==0)
cancer_numeric_male <- cancer_3 %>% filter(Expenditure<=15000 & Insurance==0 & Totalothermorbidity==0 & Gender==0)
cancer_numeric_female <- cancer_3 %>% filter(Expenditure<=25000 & Insurance==0 & Totalothermorbidity==0 & Gender==1 & Age!=24)
numeric.set <- select(cancer_numeric,Expenditure, Income, Age, BMI)
cor.test(numeric.set$Expenditure,log(numeric.set$Income),method = "pearson")
cor.test(numeric.set$Expenditure,numeric.set$Age,method = "pearson")
cor.test(numeric.set$Expenditure,numeric.set$BMI,method = "pearson")
“3.1 Low Expenditure Bracket”
cancer_numeric %>%
  ggplot(aes(Income/1000,Expenditure/1000,colour=Gender)) + geom_point() +
  labs(
    title = "Figure 5.1 Scatter Plot of Expenditure vs Income",
    subtitle = "Adults (age 18 and over) Low Expenditure Bracket",
    caption = "Data from survey in 2020.",
    x = "Income",
    y = "Expenditure($thousand)"
  ) + scale_x_log10() +
  scale_color_manual(labels = c("Male", "Female"),values = c("blue", "deep pink")) +
  geom_smooth(method=lm,se=FALSE)
cancer_numeric %>%
  ggplot(aes(Age,Expenditure/1000,color=Gender)) + geom_point() +
  labs(
    title = "Figure 5.2 Scatter Plot of Expenditure vs Age",
    subtitle = "Adults (age 18 and over) Low Expenditure Bracket",
    caption = "Data from survey in 2020.",
    x = "Age",
    y = "Expenditure($thousand)"
  ) + scale_color_manual(labels = c("Male", "Female"),values = c("blue", "deep pink")) +
  geom_smooth(method=lm,se=FALSE)
cancer_numeric %>%
  ggplot(aes(BMI,Expenditure/1000,color=Gender)) + geom_point() +
  labs(
    title = "Figure 5.3 Scatter Plot of Expenditure vs BMI",
    subtitle = "Adults (age 18 and over) Low Expenditure Bracket",
    caption = "Data from survey in 2020.",
    x = "BMI",
    y = "Expenditure($thousand)"
  ) + scale_color_manual(labels = c("Male", "Female"),values = c("blue", "deep pink")) +
  geom_smooth(method=lm,se=FALSE)
“3.2 High Expenditure Bracket”
cancer_numeric_high <- cancer_3 %>% filter(Expenditure>15000 & Expenditure<=125000 & Insurance==0 & Totalothermorbidity==0)
numeric.set.high <- select(cancer_numeric_high,Expenditure, Income, Age, BMI)
cor.test(numeric.set.high$Expenditure,log10(numeric.set.high$Income),method = "pearson")
cor.test(numeric.set.high$Expenditure,numeric.set.high$Age,method = "pearson")
cor.test(numeric.set.high$Expenditure,numeric.set.high$BMI,method = "pearson")
cancer_numeric_high %>%
  ggplot(aes(Income/1000,Expenditure/1000,colour=Gender)) + geom_point() +
  labs(
    title = "Figure 5.4 Scatter Plot of Expenditure vs Income",
    subtitle = "Adults (age 18 and over) High Expenditure Bracket",
    caption = "Data from survey in 2020.",
    x = "Income",
    y = "Expenditure($thousand)"
  ) + scale_x_log10() +
  scale_color_manual(labels = c("Male", "Female"),values = c("blue", "deep pink")) +
  geom_smooth(method=lm,se=FALSE) +
  scale_y_continuous(breaks = seq(15, 75, 20), lim = c(15, 75))
cancer_numeric_high %>%
  ggplot(aes(Age,Expenditure/1000,color=Gender)) + geom_point() +
  labs(
    title = "Figure 5.5 Scatter Plot of Expenditure vs Age",
    subtitle = "Adults (age 18 and over) High Expenditure Bracket",
    caption = "Data from survey in 2020.",
    x = "Age",
    y = "Expenditure($thousand)"
  ) + scale_color_manual(labels = c("Male", "Female"),values = c("blue", "deep pink")) +
  geom_smooth(method=lm,se=FALSE)+
  scale_y_continuous(breaks = seq(15, 75, 20), lim = c(15, 75))
cancer_numeric_high %>%
  ggplot(aes(BMI,Expenditure/1000,color=Gender)) + geom_point() +
  labs(
    title = "Figure 5.6 Scatter Plot of Expenditure vs BMI",
    subtitle = "Adults (age 18 and over) High Expenditure Bracket",
    caption = "Data from survey in 2020.",
    x = "BMI",
    y = "Expenditure($thousand)"
  ) + scale_color_manual(labels = c("Male", "Female"),values = c("blue", "deep pink")) +
  geom_smooth(method=lm,se=FALSE) +
  scale_y_continuous(breaks = seq(15, 75, 20), lim = c(15, 75))
#4 Linear Regression Modelling
#4.1 Modelling Fit & Summary cancer_train <- cancer_3 %>% filter(Dataset=="train")
cancer_train_clean <- cancer_train %>% filter(Expenditure<=125000 &
                                                Income<=250000 &
                                                BMI<75 &
                                                BMI>10 &
                                                Smoker!="not clear")
cancer_train_clean$MHscore <-as.numeric(cancer_train_clean$MHscore)
cancer_train_clean$Totalothermorbidity <-as.numeric(cancer_train_clean$Totalothermorbidity)
cancer_model <- lm(Expenditure ~ Income + Age + BMI + Gender +
                     + Race + Employment + Insurance + MHscore +
                     Smoker + Emphysema + Stroke + Coronary +
                     Cholesterol + Diabetes + Asthma + HighBP + Totalothermorbidity
                   ,data = cancer_train_clean)
cancer_model <- lm(Expenditure ~ Income + Age + BMI + Gender +
                     + Race + Employment + Insurance + MHscore +
                     Smoker + Emphysema + Stroke + Coronary +
                     Cholesterol + Diabetes + Asthma + HighBP
                   ,data = cancer_train_clean)
summary(lm(Expenditure ~ Income + Age + BMI + Gender +
             + Race + Employment + Insurance + MHscore +
             Smoker + Emphysema + Stroke + Coronary +
             Cholesterol + Diabetes + Asthma + HighBP + Totalothermorbidity
           ,data = cancer_train_clean))
summary(lm(Expenditure ~ Income + Age + BMI + Gender +
             + Race + Employment + Insurance + MHscore +
             Smoker + Emphysema + Stroke + Coronary +
             Cholesterol + Diabetes + Asthma + HighBP
           ,data = cancer_train_clean))
#4.2 Testing
cancer_test <- cancer_3 %>% filter(Dataset=="test" & Smoker!="not clear")
cancer_test$MHscore <- as.numeric(cancer_test$MHscore)
cancer_test_Predict <- predict(cancer_model,cancer_test)
cancer_test_Complete <- cancer_test %>% mutate(Expected.Expenditure=cancer_test_Predict,
                                               AE = Expenditure/Expected.Expenditure)
summary(cancer_test_Complete$AE)
nrow(cancer_test_Complete %>% filter(Expected.Expenditure<=0))
cancer_test_Complete %>% filter(Expenditure<=150000) %>%
  ggplot(aes(Expected.Expenditure/1000,Expenditure/1000)) + geom_point(color="dark blue") +
  labs(
    title = "Figure 6.1 Scatter Plot of Actual vs Expected Expenditure",
    subtitle = "Adults (age 18 and over)",
    caption = "Data from survey in 2020.",
    x = "Expected Expenditure",
    y = "Actual Expenditure"
  ) + geom_abline(intercept = 0,
                  slope = 1,
                  color = "red",
                  linetype="dashed",
                  size = 1)+
  scale_x_continuous(breaks = seq(0, 150, 50), lim = c(0, 150))
scale_y_continuous(breaks = seq(0, 150, 50), lim = c(0, 150))
#5 Complimentary Analysis
#5.1 Employment vs Income
cancer_3 %>% filter(Income<=100000) %>%
  ggplot(aes(Employment,Income/1000,fill=Employment)) + geom_boxplot(color="dark blue") +
  labs(
    title = "Figure 7.1 Boxplot of Income vs Employment",
    subtitle = "Adults (age 18 and over) & Low Income Bracket",
    caption = "Data from survey in 2020.",
    x = "Employment",
    y = "Income($thousand)"
  )+ theme(legend.position="right")+
  scale_fill_manual("Employed or Not",labels = c("No", "Yes"),values=c("light blue","pink"))
cancer_3 %>% filter(Income>100000 & Income<250000) %>%
  ggplot(aes(Employment,Income/1000,fill=Employment)) + geom_boxplot(color="dark blue") +
  labs(
    title = "Figure 7.2 Boxplot of Income vs Employment",
    subtitle = "Adults (age 18 and over) & High Income Bracket",
    caption = "Data from survey in 2020.",
    x = "Employment",
    y = "Income($thousand)"
  )+ theme(legend.position="right")+
  scale_fill_manual("Employed or Not",labels = c("No", "Yes"),values=c("light blue","pink"))
#5.2 Totalothermorbidities vs Other Numeric Variables
cor.test(cancer_train_clean$Totalothermorbidity,cancer_train_clean$Income)
cor.test(cancer_train_clean$Totalothermorbidity,cancer_train_clean$Age)
cor.test(cancer_train_clean$Totalothermorbidity,cancer_train_clean$BMI)
cancer_train_clean %>%
  ggplot(aes(Totalothermorbidity,Income/1000)) + geom_boxplot(color="dark blue",fill="light blue") +
  labs(
    title = "Figure 7.3 Boxplot of Income vs Totalothermorbidities",
    subtitle = "Adults (age 18 and over)",
    caption = "Data from survey in 2020.",
    x = "totalothermorbidities",
    y = "Income($thousand)"
  )+ theme(legend.position="right")
cancer_train_clean %>%
  ggplot(aes(Totalothermorbidity,BMI)) + geom_boxplot(color="dark blue",fill="light blue") +
  labs(
    title = "Figure 7.4 Boxplot of BMI vs Totalothermorbidities",
    subtitle = "Adults (age 18 and over)",
    caption = "Data from survey in 2020.",
    x = "totalothermorbidities",
    y = "BMI"
  )+ theme(legend.position="right")
cancer_train_clean %>%
  ggplot(aes(Totalothermorbidity,Age)) + geom_boxplot(color="dark blue",fill="light blue") +
  labs(
    title = "Figure 7.5 Boxplot of Age vs Totalothermorbidities",
    subtitle = "Adults (age 18 and over)",
    caption = "Data from survey in 2020.",
    x = "totalothermorbidities",
    y = "Age"
  )+ theme(legend.position="right")
