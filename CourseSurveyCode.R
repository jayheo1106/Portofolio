library(dplyr)
load("surveyoutsynth.Rdata")
surveyoutsynth <- as.data.frame(surveyoutsynth)
partial <- which(rowSums(is.na(surveyoutsynth)) != 12)
surveyoutsynth[partial,][is.na(surveyoutsynth[partial,])] <- 0
data <- as.data.frame(ifelse(surveyoutsynth[,1:12]>0.5, 1, 0))
data <- cbind(data, surveyoutsynth$SP22Class) 
colnames(data)[13] <- "SP22Class"

# note that 3201 not here because 3202 is a prereq for 3410
data_3202 <- data %>% filter(SP22Class == "STAT3202")
data_3302 <- data %>% filter(SP22Class == "STAT3302")
data_4202 <- data %>% filter(SP22Class == "STAT4202")

Group <- c("STAT3202", "STAT3302", "STAT4202")
N_h <- c(149, 173, 130)
N <- sum(N_h)
div_N <- round(N_h/N,3)
n_h <- c(nrow(data_3202), nrow(data_3302), nrow(data_4202))
r_h <- c(sum(!is.na(data_3202$STAT_3410_Summer_22)), 
         sum(!is.na(data_3302$STAT_3410_Summer_22)), 
         sum(!is.na(data_4202$STAT_3410_Summer_22))) 
num_yes <- c(sum(data_3202$STAT_3410_Summer_22, na.rm=TRUE),
             sum(data_3302$STAT_3410_Summer_22, na.rm=TRUE),
             sum(data_4202$STAT_3410_Summer_22, na.rm=TRUE))
y_h <- round(num_yes/r_h, 3)
s2 <- c(round(var(data_3202$STAT_3410_Summer_22, na.rm=TRUE),3),
        round(var(data_3302$STAT_3410_Summer_22, na.rm=TRUE),3),
        round(var(data_4202$STAT_3410_Summer_22, na.rm=TRUE),3))

table <- cbind(Group, N_h, div_N, n_h, r_h, num_yes, y_h, s2); as.data.frame(table)
y_str <- sum(div_N * y_h); y_str
var_y_str <- sum(div_N^2 * s2 / n_h * (1-(n_h/N_h)), na.rm = TRUE); var_y_str
CI <- cbind(y_str-1.96*var_y_str, y_str+1.96*var_y_str); CI
569*CI

