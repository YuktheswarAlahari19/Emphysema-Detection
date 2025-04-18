library(readxl)
library(data.table)
library(tidyverse)
library(dplyr)
library(plyr)
library(ggthemes)
library(ggridges)

empdata <- read_csv("/media/yuk/HOLLOW/SharedRoot/machine/MainProjects/EmphysemaDetection/Main/X1.csv")

view(empdata)
dim(empdata)

attach(empdata)

empdata %>%  
  select(COPDSEVERITY) %>% 
  count(COPDSEVERITY) %>% 
  view()

unique(empdata$COPDSEVERITY)

empdata$COPDSEVERITY <- factor(
  empdata$COPDSEVERITY,
  levels = c("MILD", "MODERATE", "SEVERE", "VERY SEVERE"),
  ordered = TRUE
)

levels(empdata$COPDSEVERITY)

nominal_data <- c("gender", "smoking","Diabetes","muscular","hypertension","AtrialFib","IHD")
view(nominal_data)

for (item in nominal_data){
  if(item %in% colnames(empdata)){
    empdata[[item]] <- factor(empdata[[item]])
    print(levels(empdata[[item]]))
  }
}



empdata$AGEquartiles <- factor(
  empdata$AGEquartiles,
  levels = c(1, 2, 3, 4),
  labels = c("Q1", "Q2", "Q3", "Q4"),
  ordered = TRUE
)

levels(empdata$AGEquartiles)
view(empdata)


# To see what age Groups are having Weak Lungs 

ggplot(empdata , aes(x = AGEquartiles , y = Lung_Efficiency_Index , fill  = COPDSEVERITY)) +
  geom_col(position = "dodge")+
  labs(
    title = "Lung Efficiency Index by Age Quartile and COPD Severity",
    subtitle = "Comparison of Lung Function across Age Groups and Disease Stages",
    x = "Age Quartiles (Q1: 35–50, Q2: 51–60, Q3: 61–70, Q4: 71–85)",
    y = "Lung Efficiency Index (FEV1 / FVC %)",
    fill = "COPD Severity"
  ) +
  theme_fivethirtyeight()+
  theme(axis.title = element_text())


# Which Genders Gets Highly Effected by COPD causing Anxiety & Depression

ggplot(empdata , aes(x = AGE, y = HAD, fill = gender)) +
  geom_boxplot() +
  labs(
    title = "Distribution of HAD Scores by Gender Across Age",
    subtitle = "Boxplot Showing Anxiety and Depression Scores by Gender and Age",
    x = "Age",
    y = "HAD Score (Hospital Anxiety and Depression Scale)",
    fill = "Gender (0-Female 1-Male)"
  )+
  theme_fivethirtyeight() +
  theme(axis.title = element_text())


# Comparing which Age Group smokes Majorly 



ggplot(empdata , aes(x = AGE, y = HAD, fill = gender)) +
  geom_boxplot() +
  labs(
    title = "Distribution of HAD Scores by Gender Across Age",
    subtitle = "Boxplot Showing Anxiety and Depression Scores by Gender and Age",
    x = "Age",
    y = "HAD Score (Hospital Anxiety and Depression Scale)",
    fill = "Gender (0-Female 1-Male)"
  )+
  theme_fivethirtyeight() +
  theme(axis.title = element_text())



#Comparing MW best to Lung efficiency index with COPDSEVERITY

ggplot(empdata, aes(x = Lung_Efficiency_Index, y = MWT1Best, color = COPDSEVERITY)) +
  geom_jitter(width = 0.2, alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title    = "6MWT Distance vs. Lung Efficiency by COPD Severity",
    subtitle = "Scatter plot with linear trend lines for each stage",
    x        = "Lung Efficiency Index ",
    y        = "Best 6MWT Distance (meters)",
    color    = "COPD Severity"
  ) +
  theme_fivethirtyeight() +
  theme(axis.title = element_text())





