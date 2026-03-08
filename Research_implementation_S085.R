
# STUDENT STRESS DATASET ANALYSIS

# 1 Load Libraries

library(tidyverse)
library(ggplot2)
library(dplyr)
library(psych)
library(readr)

# 2 Import Dataset

data <- read.csv("student_stress.csv")

head(data)
tail(data)
View(data)

# 3 Dataset Information

str(data)
summary(data)
dim(data)
colnames(data)

# 4 Rename Columns (Fix Long Names)

colnames(data) <- c(
  "Gender",
  "Age",
  "Stress_Level",
  "Rapid_Heartbeat",
  "Anxiety",
  "Sleep_Problems",
  "Anxiety_Level",
  "Headaches",
  "Irritation",
  "Concentration_Problems",
  "Low_Mood",
  "Health_Issues",
  "Loneliness",
  "Academic_Workload",
  "Peer_Competition",
  "Relationship_Stress",
  "Professor_Difficulty",
  "Work_Environment",
  "Lack_of_Leisure_Time",
  "Home_Environment",
  "Low_Academic_Confidence",
  "Subject_Confidence",
  "Activity_Conflict",
  "Class_Attendance",
  "Weight_Change",
  "Stress_Type"
)

# 5 Check Missing Values

colSums(is.na(data))

# 6 Remove Missing Values

data <- na.omit(data)

# 7 Convert Categorical Variables

data$Gender <- as.factor(data$Gender)
data$Stress_Type <- as.factor(data$Stress_Type)

# 8 Descriptive Statistics

mean(data$Age)
median(data$Age)
sd(data$Age)

table(data$Gender)
table(data$Stress_Level)

describe(data)

# 9 Graph 1 Stress Distribution

ggplot(data, aes(x = Stress_Level)) +
  geom_bar(fill = "steelblue") +
  labs(
    title = "Stress Level Distribution",
    x = "Stress Level",
    y = "Number of Students"
  )

# 10 Graph 2 Age Distribution

ggplot(data, aes(x = Age)) +
  geom_histogram(binwidth = 2, fill = "orange", color = "black") +
  labs(
    title = "Age Distribution of Students",
    x = "Age",
    y = "Frequency"
  )

# 11 Graph 3 Stress by Gender

ggplot(data, aes(x = Gender, fill = Stress_Level)) +
  geom_bar(position = "dodge") +
  labs(
    title = "Stress Level by Gender",
    x = "Gender",
    y = "Count"
  )

# 12 Graph 4 Sleep Problems vs Stress

ggplot(data, aes(x = Sleep_Problems, fill = Stress_Level)) +
  geom_bar(position = "dodge") +
  labs(
    title = "Sleep Problems vs Stress",
    x = "Sleep Problems",
    y = "Count"
  )

# 13 Graph 5 Stress Type Pie Chart

stress_counts <- table(data$Stress_Type)

pie(
  stress_counts,
  main = "Type of Stress Experienced",
  col = c("lightblue","pink","lightgreen")
)

# 14 Hypothesis Testing

# Stress vs Sleep Problems
table1 <- table(data$Stress_Level, data$Sleep_Problems)
chisq.test(table1)

# Stress vs Academic Workload
table2 <- table(data$Stress_Level, data$Academic_Workload)
chisq.test(table2)

# 15 Correlation Analysis

numeric_data <- data[sapply(data, is.numeric)]

cor(numeric_data)

# 16 Save Clean Dataset

write.csv(data,"cleaned_student_stress.csv")
