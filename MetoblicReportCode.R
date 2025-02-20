library(readxl)
library(dplyr)
library(lme4)

# Data pre-processing
df_orig <- read_excel("~/TAMU/stat692/Project 1.xlsx")
df <- na.omit(df_orig) # delete missing data
df <- df %>%
  group_by(record_id) %>%
  filter(all(c(1, 2) %in% Time) ) %>%
  ungroup()

## Set the variables as factor
df$School_ID <- factor(df$School_ID)
df$student_sex <- factor(df$student_sex)
df$student_grade <- factor(df$student_grade)
df$Time <- factor(df$Time)

# Normality Test
shapiro.test(df$MetS_Z_Score) # Shows good normality.

# Linear Mixed-effect Model (LMM)
model <- lmer(MetS_Z_Score ~ Time + student_sex + student_grade + (1 | School_ID), data = df)
summary(model)

# Wilcoxon Signed-Rank Test
library(tidyr)
df_wilcox <- df %>% pivot_wider(names_from = Time, values_from = MetS_Z_Score)
df_wilcox$diff <- df_wilcox$post - df_wilcox$pre
df_wilcox$diff2 <- abs(df_wilcox$diff)
df_wilcox$rank <- rank(df_wilcox$diff2)
wilcox.test(df_wilcox$pre, df_wide$post, paired = TRUE)
summary(df_wilcox)
