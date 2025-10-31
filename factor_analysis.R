library(ggplot2)
library(ggmap)
library(tidyr)
library(dplyr)
library(psych) 
library(leaflet)

# Read data
data <- read.csv('/Users/ingridamerica/Documents/harvest_weather_joined.csv')

# Select numeric variables for factor analysis (exclude IDs, categorical vars)
numeric_data <- data %>% 
  select(where(is.numeric)) %>% 
  select(-c(DoY_x, Year_x,Year_y, date_int,Date.1, DoY_y, Rep, Sample, PCPN_mm_d, Grain_yield_kg_ha))  # Remove non-relevant numeric columns
numeric_data <- na.omit(numeric_data)
#Standardize data (mean=0, sd=1)
scaled_data <- scale(numeric_data)

#Correlation matrix (visual inspection)
cor_matrix <- cor(scaled_data)
dev.new(width=10, height=8)
corrplot::corrplot(cor_matrix, method = "number")

library(ggplot2)
ggplot(data, aes(x = T_max_C, y = Grain_yield_g_m2, color = Treatment)) +
  geom_point() +
  labs(title = "Yield vs. Max Temperature by Treatment",
       x = "Max Temperature (°C)", y = "Grain Yield (g/m²)")

# For AGB (Biomass)
ggplot(data, aes(x = T_max_C, y = AGB_g_m2, color = Treatment)) +
  geom_smooth(method = "lm") +
  labs(title = "Treatment × Temperature Interaction on Biomass")

#Does treatment effects depend on temperature ? 
Y <- cbind(data$Grain_yield_g_m2, 
           data$AGB_g_m2, 
           data$Harvest_index)
data$Treatment = as.factor(data$Treatment)
data$Crop = as.factor(data$Crop)

# Include T_max_C and T_min_C as covariates
obj <- manova(Y ~ Treatment*Crop +T_max_C + Sol_Rad_MJ_m2_d + RH_f+Wind_spd_m_s  , data = data)

# Check significance of Treatment AFTER controlling for temperature
summary(obj, test = "Wilks")

#Factor analysis 
library(psych)
library(GPArotation)

efa_vars <- data[, c("Grain_yield_g_m2","Stover_g_m2","Harvest_index","AGB_g_m2", "T_max_C", "Percent_H2O",
                     "RH_f", "Wind_spd_m_s")]
scaled_data = scale(efa_vars)
KMO(efa_vars)
cortest.bartlett(cor(efa_vars), n = nrow(efa_vars))

fa.parallel(efa_vars, fa = "fa", n.iter = 100)

#number of factors 
obj=princomp(efa_vars, cor=TRUE)
obj$loadings
dev.new(width=10, height=8)
plot(obj, type="lines", main="Scree Plot")

#Run EFA with 2 factors 
efa_results = factanal(efa_vars, factors = 2 , rotation = "varimax" )
efa_results = factanal(efa_vars, factors = 2 , rotation = "promax" )
L = efa_results$loadings[, 1:2]
Psi= diag(efa_results$uniquenesses)
1-efa_results$uniquenesses
print(efa_results, digits = 2 , cutoff = 0.3)

#EFA different for treatments 
efa_scores <- factanal(x = efa_vars, factors = 2, rotation = "promax", scores = "regression")
scores_df <- as.data.frame(efa_scores$scores)
scores_df$Treatment <- data$Treatment
aggregate(. ~ Treatment, data = scores_df, mean)

#visualizing the mean each year 
means = tapply(data$Grain_yield_g_m2, list(data$Year_x, data$Treatment), mean)
print(means)

