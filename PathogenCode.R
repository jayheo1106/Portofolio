library(dplyr)
library(ggplot2)
library(stringr)
library(broom)
library(DescTools)
library(epitools)
library(sf)

# data pre processing
df1 <- read.csv("Project2-1.csv")
df2 <- read.csv("Project2-2.csv")

# clean the df2 data
df2$Village.Name <- str_to_upper(str_extract(df2$Village.Name, "(?<=\\().+?(?=\\))"))
df2 <- df2[,-2]

# clean the df1 data
df1 <- df1 %>% 
  mutate(age_group = cut(Age, breaks = c(-Inf, 2, 5, Inf), labels = c("<2 years", "3-5 years", "≥5 years"))) %>% # make age group
  select(-Age) %>%
  rename(Age=age_group) %>%
  left_join(df2 %>% select(Village.Name, Latitude, Longitude) %>% # add the lon, lat info from df2
              rename(Village = Village.Name), by = "Village") %>%
  filter(Sex %in% c("M", "F")) %>% # remove missing sex
  filter(!is.na(Latitude), !is.na(Longitude)) # remove missing village in village info and age


# clustering the region
df1_sf <- df1 %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)
set.seed(692) 
n_clusters <- 3 
kmeans_result <- kmeans(st_coordinates(df1_sf), centers = n_clusters) # K-means clustering
df1$Region <- kmeans_result$cluster
df1 <- df1 %>%
  mutate(Region = case_when(
    Region == 1 ~ "Mayo Kebbi Est",
    Region == 2 ~ "Moyen Chari",
    Region == 3 ~ "Chari Bagurmi",
  )) %>%
  select(Age, Sex, Region, Village, Latitude, Longitude, 
         M.haemocanis, C.M..haematoparvum, C.M.turicensis,
         Babesia.spp., H..canis, A..platys, E..canis)

ggplot() +
  geom_sf(data = df1_sf, aes(color = as.factor(df1$Region)), size = 3) +
  theme_minimal() +
  labs(title = "Spatial Clustering of Villages", color = "Cluster Region") +
  theme(legend.position = "bottom")

# factorize
df1$Age <- as.factor(df1$Age)
df1$Sex <- as.factor(df1$Sex)
df1$Region <- as.factor(df1$Region)
head(df1)

unique(df1$Village[df1$Region == "Mayo Kebbi Est"])
unique(df1$Village[df1$Region == "Moyen Chari"])
unique(df1$Village[df1$Region == "Chari Bagurmi"])

# missing value in pathogen check
response_vars <- c("M.haemocanis", "C.M..haematoparvum", "C.M.turicensis", 
                   "Babesia.spp.", "H..canis", "A..platys", "E..canis")
which(is.na(df1[,7])); which(is.na(df1[,8])); which(is.na(df1[,9])); which(is.na(df1[,10]))
which(is.na(df1[,11])); # "H..canis"
which(is.na(df1[,12])); # "A..platys"
which(is.na(df1[,13]))

summarize_infection <- function(data, pathogen, group_var) {
  data %>%
    filter(!is.na(get(pathogen))) %>% 
    group_by(!!sym(group_var)) %>%
    summarise(
      n = n(),  
      total_infected = sum(get(pathogen)),
      proportion = BinomCI(total_infected, n, conf.level = 0.95, method = "wald")[1],
      ci_lower = BinomCI(total_infected, n, conf.level = 0.95, method = "wald")[2],
      ci_upper = BinomCI(total_infected, n, conf.level = 0.95, method = "wald")[3] 
    )
}

create_summary_dataframe <- function(data, pathogen) {
  summary_df <- data.frame(Group = character(),
                           n = integer(),
                           Total_infected = integer(),
                           Proportion = numeric(),
                           CI_Lower = numeric(),
                           CI_Upper = numeric(),
                           stringsAsFactors = FALSE)
  
  # summary of age_group
  age_summary <- summarize_infection(data, pathogen, "Age") %>%
    mutate(Group = Age) %>%
    select(Group, n, total_infected, proportion, ci_lower, ci_upper)
  
  # summary of sex
  sex_summary <- summarize_infection(data, pathogen, "Sex") %>%
    mutate(Group = Sex) %>%
    select(Group, n, total_infected, proportion, ci_lower, ci_upper)
  
  # summary of region
  region_summary <- summarize_infection(data, pathogen, "Region") %>%
    mutate(Group = Region) %>%
    select(Group, n, total_infected, proportion, ci_lower, ci_upper) 
  
  summary_df <- bind_rows(age_summary, sex_summary, region_summary)
  summary_df <- summary_df %>%
    mutate(across(c(proportion, ci_lower, ci_upper), ~ round(., 4)))
  colnames(summary_df) <- c("Group", "n", "Total_infected", "Proportion", "CI_Lower", "CI_Upper")
  return(summary_df)
}

summary_1 <- create_summary_dataframe(df1, "M.haemocanis")
summary_2 <- create_summary_dataframe(df1, "C.M..haematoparvum")
summary_3 <- create_summary_dataframe(df1, "C.M.turicensis")
summary_4 <- create_summary_dataframe(df1, "Babesia.spp.")
summary_5 <- create_summary_dataframe(df1, "H..canis")
summary_6 <- create_summary_dataframe(df1, "A..platys")
summary_7 <- create_summary_dataframe(df1, "E..canis")


# function create a proportion plot by age, sex, region
create_proportion_plot <- function(data, pathogen_name) {
  data$Group <- factor(data$Group, 
                       levels = c("<2 years", "3-5 years", "≥5 years", "F", "M", 
                                  "Mayo Kebbi Est", "Chari Bagurmi", "Moyen Chari"))
  data$Category <- case_when(
    data$Group %in% c("<2 years", "3-5 years", "≥5 years") ~ "Age",
    data$Group %in% c("F", "M") ~ "Sex",
    data$Group %in% c("Mayo Kebbi Est", "Chari Bagurmi", "Moyen Chari") ~ "Region"
  )
  data$Category <- factor(data$Category, 
                          levels = c("Age", "Sex", "Region"))
  plot <- ggplot(data, aes(x = Group, y = Proportion, fill = Category)) +
    geom_bar(stat = "identity", position = "dodge", width = 0.7) +
    geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), 
                  position = position_dodge(width = 0.7), 
                  width = 0.2) +
    labs(title = paste("Proportion of", pathogen_name, "by Group"),
         x = "Group",
         y = "Proportion of Infection") +
    scale_fill_manual(values = c("lightblue", "lightgreen", "lightcoral")) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    facet_wrap(~ Category, scales = "free_x")
  
  return(plot)
}

create_proportion_plot(summary_1, "M.haemocanis")
create_proportion_plot(summary_2, "C.M..haematoparvum") 
create_proportion_plot(summary_3, "C.M.turicensis")
create_proportion_plot(summary_4, "Babesia.spp.") 
create_proportion_plot(summary_5, "H..canis")
create_proportion_plot(summary_6, "A..platys") 
create_proportion_plot(summary_7, "E..canis")

# Chi-square test function
perform_chi_squared_test <- function(data, group_var, response_var) {
  # Remove rows with NA values in the specified group or response variable
  filtered_data <- data[!is.na(data[[group_var]]) & !is.na(data[[response_var]]), ]
  
  # Create a contingency table
  contingency_table <- table(filtered_data[[group_var]], filtered_data[[response_var]])
  
  # Perform the chi-squared test only if the contingency table has more than one level for both variables
  if (nrow(contingency_table) > 1 && ncol(contingency_table) > 1) {
    chi_squared_result <- chisq.test(contingency_table)
    print(paste("Group:", response_var, ", ",group_var))
    print(chi_squared_result)
  } else {
    print(paste("Group:", group_var, "- Not enough data for Chi-square test"))
  }
}

response_vars <- c("M.haemocanis", "C.M..haematoparvum", "C.M.turicensis", 
                   "Babesia.spp.", "H..canis", "A..platys", "E..canis")
for (response in response_vars) {
  perform_chi_squared_test(df1, "Age", response)
  perform_chi_squared_test(df1, "Sex", response)
  perform_chi_squared_test(df1, "Region", response)
}


response_vars <- c("M.haemocanis", "C.M..haematoparvum", "C.M.turicensis", 
                   "Babesia.spp.", "H..canis", "A..platys", "E..canis")

create_coefficient_summary <- function(model) {
  model_summary <- summary(model)
  coefficients <- as.data.frame(model_summary$coefficients)
  coefficients <- tibble::rownames_to_column(coefficients, "Variable")
  colnames(coefficients) <- c("Variable", "Estimate", "Standard Error", "z-value", "p-value")
  coefficients$Significance <- cut(coefficients$`p-value`,
                                   breaks = c(-Inf, 0.001, 0.01, 0.05, 0.1, Inf),
                                   labels = c("***", "**", "*", ".", " "),
                                   right = FALSE)
  return(coefficients)
}


# 1. "M.haemocanis",
model_full <- glm(M.haemocanis ~ Age + Sex + Age:Sex, data = df1, family = "binomial")
model_final <- step(model_full, direction = "backward", trace = 0)
coeff_summary <- create_coefficient_summary(model_final)
coeff_summary 

# 2. "C.M..haematoparvum"
model_full <- glm(C.M..haematoparvum ~ Age + Sex + Region + Age:Sex + Age:Region + Sex:Region,
                  data = df1, family = "binomial")
model_final <- step(model_full, direction = "backward", trace = 0)
coeff_summary <- create_coefficient_summary(model_final)
coeff_summary 

# 3. "C.M.turicensis"
model_full <- glm(C.M.turicensis ~ Age + Sex + Age:Sex,
                  data = df1, family = "binomial")
model_final <- step(model_full, direction = "backward", trace = 0)
coeff_summary <- create_coefficient_summary(model_final)
coeff_summary 

# 4. "Babesia.spp."
model_full <- glm(Babesia.spp. ~ Region,
                  data = df1, family = "binomial")
model_final <- step(model_full, direction = "backward", trace = 0)
coeff_summary <- create_coefficient_summary(model_final)
coeff_summary  

# 5. "H..canis"
model_full <- glm(H..canis ~ Age + Region + Age:Region,
                  data = df1, family = "binomial")
model_final <- step(model_full, direction = "backward", trace = 0)
coeff_summary <- create_coefficient_summary(model_final)
coeff_summary  

# 6. "A..platys"
model_full <- glm(A..platys ~ Age + Region + Age:Region,
                  data = df1, family = "binomial")
model_final <- step(model_full, direction = "backward", trace = 0)
coeff_summary <- create_coefficient_summary(model_final)
coeff_summary 

# 7. "E..canis"
model_full <- glm(E..canis ~ Region,
                  data = df1, family = "binomial")
model_final <- step(model_full, direction = "backward", trace = 0)
coeff_summary <- create_coefficient_summary(model_final)
coeff_summary 

perform_backward_elimination <- function(data, response_vars) {
  results <- list() 
  for (response_var in response_vars) {
    formula_full <- as.formula(paste(response_var, "~ age_group + Sex + Region + age_group:Sex + age_group:Region + Sex:Region + age_group:Sex:Region"))
    model_full <- glm(formula_full, data = data, family = "binomial")
    model_final <- step(model_full, direction = "backward", trace = 0)
    results[[response_var]] <- summary(model_final)
  }
  return(results)  
}

Reference

Schrader, W. (2006). *Biostatistics Lecture 12: Logistic Regression*. https://www.unm.edu/~schrader/biostat/bio2/Spr06/lec12.pdf

Suleiman, M. H., & Tarek, A. (2023). *Study of the prevalence of tick-borne pathogens in dogs in selected regions of Sudan*. Figure 1. https://www.researchgate.net/figure/Study-regions-sites-and-approximate-numbers-of-dogs-enrolled-in-each-village-for-the-FBZ_fig1_358923283






