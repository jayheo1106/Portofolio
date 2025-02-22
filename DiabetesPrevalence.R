# csv file 
file <- "PLACES__Local_Data_for_Better_Health__County_Data_2024_release_20241112.csv"
data <- read.csv(file, sep = ",", , stringsAsFactors = FALSE, colClasses = c(LocationID = "character")) 

# data pre-processing
data <- data %>% 
  mutate(    # lon, lat column 
    lon = as.numeric(sub("POINT \\(([^ ]+) .+", "\\1", Geolocation)),
    lat = as.numeric(sub("POINT \\([^ ]+ ([^ ]+)\\)", "\\1", Geolocation))
  ) %>%
  filter(!is.na(lon) & !is.na(lat)) %>%  # remove missing values in the lon and lat columns
  filter(StateAbbr == "TX", 
         Measure == "Diagnosed diabetes among adults", 
         Data_Value_Type == "Age-adjusted prevalence") %>%
  select(Year, StateAbbr, StateDesc, LocationName, Category, Measure, 
         Data_Value_Unit, Data_Value, TotalPopulation, TotalPop18plus, lon, lat)

# census data
census_api_key("bfb94392ac894cf3164deb75b562c804bfd9f4ea", install = TRUE, overwrite = TRUE)

state_abbr <- "TX"
state_name <- "Texas"  

variables <- c(
  poverty = "S1701_C03_001",            # Poverty rate
  unemployment = "S2301_C04_001",       # Unemployment rate
  per_capita_income = "B19301_001",     # Per capita income
  
  # Age groups
  t_population = "S0101_C01_001",  # Total population 
  page_15_19 = "S0101_C02_005",    # Percentage of Age 15-19
  page_20_24 = "S0101_C02_006",    # Percentage of Age 20-24
  page_25_29 = "S0101_C02_007",    # Percentage of Age 25-29
  page_30_34 = "S0101_C02_008",    # Percentage of Age 30-34
  page_35_39 = "S0101_C02_009",    # Percentage of Age 35-39
  page_40_44 = "S0101_C02_010",    # Percentage of Age 40-44
  page_45_49 = "S0101_C02_011",    # Percentage of Age 45-49
  page_50_54 = "S0101_C02_012",    # Percentage of Age 50-54
  page_55_59 = "S0101_C02_013",    # Percentage of Age 55-59
  page_60_64 = "S0101_C02_014",    # Percentage of Age 60-64
  page_65_69 = "S0101_C02_015",    # Percentage of Age 65-69
  page_70_74 = "S0101_C02_016",    # Percentage of Age 70-74
  page_75_79 = "S0101_C02_017",    # Percentage of Age 75-79
  page_80_84 = "S0101_C02_018",    # Percentage of Age 80-84
  page_85_plus = "S0101_C02_019",   # Percentage of Age 85+
  
  # Race/Ethnicity
  white = "B03002_003",            # White alone
  hispanic = "B03002_012",         # Hispanic or Latino
  asian = "B02001_005",            # Asian alone
  black = "B03002_004",                # Black or African American alone
  e_total = "B01003_001",
  
  # Health insurance
  perc_health_insurance = "S2701_C03_001" # Percentage with health insurance)
  census_data <- get_acs(
    geography = "county", 
    variables = variables, 
    state = state_abbr, 
    year = 2022, 
    survey = "acs5",
    geometry = FALSE)
  census_data_wide <- census_data %>%
    select(NAME, variable, estimate) %>%
    pivot_wider(
      names_from = variable,  # The variable names will become column names
      values_from = estimate   # The estimates will be the values in the new columns
    ) %>%
    mutate(
      # Clean up county names for merging
      NAME = gsub(",? (County|Texas)", "", NAME),
      # Calculate racial/ethnic percentages
      perc_white = (white / e_total) * 100,  # Percentage White
      perc_black = (black / e_total) * 100,  # Percentage Black
      perc_asian = (asian / e_total) * 100,  # Percentage Asian
      perc_hispanic = (hispanic / e_total) * 100,  # Percentage Hispanic
    ) %>%
    rename(LocationName = NAME)  
  merged_data <- left_join(data,census_data_wide,  by = "LocationName")
  merged_data <- merged_data %>%
    mutate(
      p_age_15_29 = page_15_19 + page_20_24 + page_25_29,
      p_age_30_39 = page_30_34 + page_35_39,
      p_age_40_64 = page_40_44 + page_45_49 + page_50_54 + page_55_59 + page_60_64,
      p_age_65_plus = page_65_69 + page_70_74 + page_75_79 + page_80_84 + page_85_plus
    ) %>%
    select(-starts_with("page_")) %>% 
    select(everything(), p_age_15_29, p_age_30_39, p_age_40_64, p_age_65_plus) %>%
    select(-asian, -white, -black, -hispanic) %>%
    mutate(
      across(
        c(Data_Value, poverty, unemployment, perc_health_insurance, perc_white, perc_black, perc_asian, perc_hispanic, 
          p_age_15_29, p_age_30_39, p_age_40_64, p_age_65_plus), 
        ~ round(. / 100, 4))
    ) %>%
    mutate(
      per_capita_income = round(log(per_capita_income + 1), 3)
    )
  
  
  # sf object
  data_sf <- st_as_sf(merged_data, coords = c("lon", "lat"), crs = 4326, remove = FALSE)
  
  # plot Prevalence of Diagnosed Diabetes in texas
  state_map <- map("county", regions = "texas", plot = FALSE, fill = TRUE) %>%
    st_as_sf(crs = 4326)
  ggplot() +
    # State and county boundaries (you may need a shapefile for Texas boundaries)
    geom_sf(data = state_map, fill = "white", color = "black", size = 0.2) +
    # Plotting diabetes prevalence data
    geom_sf(data = data_sf, aes(color = Data_Value), size = 3, alpha = 0.8) +
    scale_color_gradient(low = "lightblue", high = "darkblue", name = "Prevalence") +
    labs(
      title = paste("Diagnosed Diabetes Among Adults in", state_name, "(County Level)"),
      x = "Longitude",
      y = "Latitude"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.4, size = 14, face = "bold"),
      legend.title = element_text(size = 12),
      legend.text = element_text(size = 10))
  
  # DBSCAN clustering
  ## clustering data and variables
  set.seed(321)  
  clustering_data <- merged_data %>% 
    select(lon, lat, Data_Value) %>% 
    na.omit()  
  clustering_matrix <- as.matrix(scale(clustering_data))
  within_cluster_dispersion <- function(data, eps, minPts) {
    db_result <- dbscan(data, eps = eps, minPts = minPts)
    #calculate the sum of within-cluster dispersions for all clusters
    wkd <- 0
    for (cluster_id in unique(db_result$cluster)) {
      if (cluster_id != 0) {  
        cluster_points <- data[db_result$cluster == cluster_id, ]
        cluster_center <- colMeans(cluster_points)
        wkd <- wkd + sum(rowSums((cluster_points - cluster_center)^2))
      }
    }
    return(wkd)
  }
  
  # Set minPts and calculate k-distances (use k = minPts - 1)
  minPts <- 4  # You can adjust this based on guidelines above
  k <- minPts - 1
  
  # Calculate k-distances for each point (using clustering_matrix from previous code)
  k_distances <- kNNdist(clustering_matrix, k = k)
  
  # Plot the sorted k-distances
  plot(sort(k_distances), type = "l", 
       main = paste("k-Distance Plot (k =", k, ")"),
       xlab = "Points sorted by distance", 
       ylab = paste(k, "-NN Distance"))
  abline(h = 0.7, col = "red", lty = 2)  # Optional line to indicate estimated eps
  dbscan_result <- dbscan(clustering_matrix, eps = 0.47, minPts = 4)
  data_sf$cluster <- dbscan_result$cluster
  
  # plot with dbscan result
  ggplot() +
    # State and county boundaries 
    geom_sf(data = state_map, fill = "white", color = "black", size = 0.2) +
    scale_color_viridis_d(option = "plasma", na.translate = FALSE) +
    # Plot clusters with distinct colors
    geom_sf(data = data_sf, aes(color = as.factor(cluster)), size = 3, alpha = 0.8) +
    labs(
      title = paste("DBSCAN Clusters of Diagnosed Diabetes Prevalence in", state_name),
      x = "Longitude",
      y = "Latitude"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.4, size = 14, face = "bold"),
      legend.title = element_text(size = 12),
      legend.text = element_text(size = 10))
  
  ## INLA
  library(INLA)
  library(ggregplot)
  library(beepr)
  phen <- c("lon", "lat", "cluster") # Base columns with spatial information we'll need
  resp <- "Data_Value" # Response variable
  covar <- c("per_capita_income", "poverty", "unemployment", "perc_health_insurance",
             "perc_white", "perc_black", "perc_asian", "perc_hispanic",
             "p_age_15_29", "p_age_30_39", "p_age_40_64", "p_age_65_plus")
  TestHosts <- na.omit(data_sf[, c(phen, resp, covar)]) # Getting rid of NA's, picking adults
  
  # multi-collinearity 
  library(car)
  model <- lm(Data_Value ~ 
                per_capita_income + poverty + unemployment + perc_health_insurance + 
                perc_white + perc_black + perc_asian + perc_hispanic + 
                p_age_15_29 + p_age_30_39 + p_age_40_64 + p_age_65_plus, 
              data = data_sf)
  vif(model)
  
  ## drop the high multi-collinearity (perc_white, perc_black, perc_hispanic)
  covar <- c("per_capita_income", "poverty", "unemployment", "perc_health_insurance",
             "perc_asian", 
             "p_age_15_29", "p_age_30_39", "p_age_40_64", "p_age_65_plus")
  
  
  # covariate selection 
  HostModelSel <- INLAModelSel(resp, covar, "cluster", "iid", "beta", TestHosts)
  Finalcovar <- HostModelSel$Removed[[length(HostModelSel$Removed)]]
  
  # Setting up a mesh
  Locations = cbind(TestHosts$lon, TestHosts$lat) # using the sampling locations 
  MeshA <- inla.mesh.2d(jitter(Locations), max.edge = c(20, 40))
  MeshB <- inla.mesh.2d(Locations, max.edge = c(20, 40))
  MeshC <- inla.mesh.2d(Locations, max.edge = c(10, 20))
  Mesh <- MeshB # pick MeshB
  plot(MeshA)
  points(Locations, col = "red", pch = 2)
  plot(MeshB)
  points(Locations, col = "red", pch = 2)
  plot(MeshC)
  points(Locations, col = "red", pch = 2)
  
  # Making the A matrix
  HostsA <- inla.spde.make.A(Mesh, loc = Locations) # Making A matrix
  Hosts.spde = inla.spde2.pcmatern(mesh = Mesh, prior.range = c(10, 0.5), prior.sigma = c(.5, .5)) # Making SPDE
  w.Host <- inla.spde.make.index('w', n.spde = Hosts.spde$n.spde) # making the w
  # Making the model matrix
  X0 <- model.matrix(as.formula(paste0(" ~ -1 + ", paste(Finalcovar, collapse = " + "))), data = TestHosts) 
  # make the model matrix using the final model selection formula without a response variable.
  X <- as.data.frame(X0) 
  
  # Making the stack
  N <- nrow(TestHosts)
  StackHost <- inla.stack(
    data = list(y = TestHosts$Data_Value), # specify the response variable
    # data = list(y = TestHosts[,resp]),
    A = list(1, 1, 1, HostsA), # Vector of Multiplication factors for random and fixed effects              
    effects = list(
      Intercept = rep(1, N), # specify the manual intercept!
      X = X, # attach the model matrix
      cluster = TestHosts$cluster,
      w = w.Host)) # Leave
  
  f1 <- as.formula(paste0("y ~ -1 + Intercept + ", paste0(colnames(X), collapse = " + ")))
  f2 <- as.formula(paste0("y ~ -1 + Intercept + ", paste0(colnames(X), collapse = " + "), " +  f(cluster, model = 'iid')"))
  f3 <- as.formula(paste0("y ~ -1 + Intercept + ", paste0(colnames(X), collapse = " + "), " +  f(cluster, model = 'iid') + f(w, model = Hosts.spde)"))
  
  # models
  IM1 <- inla(f1, # Base model (no random effects)
              family = "beta",
              data = inla.stack.data(StackHost),
              control.compute = list(dic = TRUE),
              control.predictor = list(A = inla.stack.A(StackHost)))
  IM2 <- inla(f2, # f1 + cluster random effects
              family = "beta",
              data = inla.stack.data(StackHost),
              control.compute = list(dic = TRUE),
              control.predictor = list(A = inla.stack.A(StackHost)))
  IM3 <- inla(f3, # f2 + SPDE random effect 
              family = "beta",
              data = inla.stack.data(StackHost),
              control.compute = list(dic = TRUE),
              control.predictor = list(A = inla.stack.A(StackHost)))
  SpatialHostList <- list(IM1, IM2, IM3)
  
  ggField(IM3, Mesh, Groups = 1) +
    scale_fill_brewer(palette = "Blues") 
  
  INLADICFig(SpatialHostList, ModelNames = c("Base", "IID", "SPDE"))
  
  summary(IM3)
  IM3$summary.fixed
  Efxplot(list(IM3))
  
  # data visulaiziaton
  ## bar chart with confidence interval
  data <- data.frame(
    Characteristic = c(
      "Total", "Men", "Women", 
      "White, Non-Hispanic", "Black, Non-Hispanic", "Asian, Non-Hispanic", "Hispanic", 
      "Less than high school", "High school", "More than high school"
    ),
    Diagnosed = c(10.1, 11.6, 8.8, 8.9, 12.4, 11.1, 13.0, 14.2, 11.9, 8.6),
    Lower_CI = c(9.2, 10.3, 7.6, 7.6, 10.8, 9.7, 11.4, 12.3, 9.3, 7.8),
    Upper_CI = c(11.0, 13.0, 10.2, 10.4, 14.1, 12.6, 14.9, 16.2, 15.2, 9.5),
    Category = c(
      "Total", "Sex", "Sex", 
      "Race", "Race", "Race", "Race", 
      "Education", "Education", "Education"))
  plot_diagnosed_diabetes <- function(data, category_name) {
    ggplot(data %>% filter(Category == category_name), aes(x = Characteristic, y = Diagnosed, fill = Characteristic)) +
      geom_bar(stat = "identity", show.legend = FALSE, width = 0.7) +  # Bar plot
      geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI), width = 0.2, color = "black") +  # Add error bars
      coord_flip() +  # Flip coordinates for better readability
      labs(
        title = paste("Diagnosed Diabetes by", category_name),
        x = category_name,
        y = "Percentage (%)"
      ) +
      theme_minimal(base_size = 14) +
      scale_fill_brewer(palette = "Set3")  # Add a visually distinct color palette}
    plot_diagnosed_diabetes(data, "Sex")
    plot_diagnosed_diabetes(data, "Race")
    plot_diagnosed_diabetes(data, "Education")
    
    
    # Input the data with counts
    data <- data.frame(
      Characteristic = c(
        "Men", "Women", 
        "White, Non-Hispanic", "Black, Non-Hispanic", "Asian, Non-Hispanic", "Hispanic", 
        "Less than high school", "High school", "More than high school"),
      Diagnosed = c(116, 88, 89, 124, 111, 130, 142, 119, 86),  # Counts of diagnosed cases
      Total = c(1000, 1000, 1000, 1000, 1000, 1000, 1000, 1000, 1000),  # Total population size
      Category = c(
        "Sex", "Sex", 
        "Race", "Race", "Race", "Race", 
        "Education", "Education", "Education"))
    perform_chi_square <- function(data, category) {
      subset <- data[data$Category == category, ]
      ## Create a contingency table (Diagnosed vs. Not Diagnosed)
      contingency_table <- matrix(
        c(subset$Diagnosed, subset$Total - subset$Diagnosed), 
        ncol = 2, 
        byrow = FALSE,
        dimnames = list(subset$Characteristic, c("Diagnosed", "Not Diagnosed")))
      ## Perform Chi-Square Test
      chi_sq <- chisq.test(contingency_table)
      return(list(
        "Category" = category,
        "Chi-Square Statistic" = chi_sq$statistic,
        "p-Value" = chi_sq$p.value,
        "Degrees of Freedom" = chi_sq$parameter))}
    ## Perform Chi-Square Test for each category
    results <- lapply(unique(data$Category), function(cat) perform_chi_square(data, cat))
    chi_square_results <- do.call(rbind, lapply(results, as.data.frame))