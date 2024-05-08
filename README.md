---
title: "Water Potability Prediction Study"
author: "Joshua Yepes"
date: "2024-04-18"
output: github_document
---


# Introduction

Access to safe drinking water is essential for human health and well-being. However, ensuring water safety remains a significant challenge in many regions worldwide. Contamination of water sources by pollutants, pathogens, and other hazardous substances poses a threat to public health, leading to adverse health effects.

In this study, the aim is to investigate the predictive capability of water quality metrics in determining the potability of water. By analyzing a dataset containing information on water quality parameters collected from approximately 4,000 bodies of water, the goal is to seek to explore this dataset and develop predictive models that can classify water as safe or unsafe for consumption.

## Hypothesis

### Null Hypothesis (H0)
There is no significant relationship between water quality metrics and water potability, the predictive models developed will not outperform random chance in determining whether water is safe for consumption.

### Alternative Hypothesis (H1)
There is a significant relationship between water quality metrics and water potability, the predictive models developed will demonstrate accuracy higher than random chance in determining whether water is safe for consumption.

## Objectives

The primary objectives of this study are as follows:

1. To identify key water quality metrics that significantly influence water potability and understand their relationships.

2. To test hypothesis regarding the relationship between water quality metrics and water potability, providing insights into the feasibility of using predictive models for water safety assessment.




## Data Collection and Processing



### Libaries Imported
```{r, message=FALSE, warning=FALSE}
library(tidyverse)
library(readr)
library(summarytools)
library(dplyr)
library(ggplot2)
library(corrplot)
library(gridExtra)
```

### About the Dataset

The water_potability.csv file contains water quality metrics for 3276 different water bodies.

Key Variables:

1.pH Value

2.Hardness

3.Solids

4.Chloramines

6.Conductivity

7.Organic Carbon

8.Trihalomethanes

9.Turbidity

10.Potability: The target variable indicating whether water is safe for drinking (1) or not (0), serving as the primary focus of analysis.

#### Load the data
```{r,message=FALSE, warning=FALSE}
df <- read_csv("water_potability.csv")
View(df)
```
#### View Summary of data to get an understanding
```{r, message=FALSE, warning=FALSE}
numeric_summary <- summary(df)
numeric_summary
```
#### Data Wrangling
```{r, message=FALSE, warning=FALSE}
#Cleaning:  Handled any missing values, so new result is 2011 obs. instead of 3726 obs.

df_cleanData <- df %>%
  filter(!is.na(ph) & !is.na(Hardness) & !is.na(Solids) & !is.na(Chloramines) & 
         !is.na(Sulfate) & !is.na(Conductivity) & !is.na(Organic_carbon) & 
         !is.na(Trihalomethanes) & !is.na(Turbidity) & !is.na(Potability))

# I want to see how much water is Potable
df_potableOnly <- df_cleanData %>%
  filter(Potability == 1)

# I want to see how much water is not Potable
df_nonPotableOnly <- df_cleanData %>%
  filter(Potability == 0)

```



# Exploratory Data Analysis (EDA)

- For the remaining of the paper, I am only going to investigate 4 metrics of water:
- pH 
- Hardness
- Chloramines
- Trihalomethanes. 
- After doing research, these 4 variables are the ones I am most interested in and believe can effect potability the most since they have do a lot of what makes water clean in the first place. So I would like to research further into them.


## pH:
-Reflects acidity or alkalinity, critical for assessing suitability for consumption. High or low pH levels can indicate potential health risks.

```{r, message=FALSE, warning=FALSE}
library(dplyr)
library(ggplot2)


filtered_dataPH <- df_cleanData %>%
  filter(ph >= 6.5 & ph <= 8.5) %>%
 select(ph, Potability)


potable_count <- sum(filtered_dataPH$Potability == 1)
non_potable_count <- sum(filtered_dataPH$Potability == 0)

plot_data <- data.frame(
  Potability = c("Potable", "Non-Potable"),
  Count = c(potable_count, non_potable_count)
)

ggplot(plot_data, aes(x = Potability, y = Count, fill = Potability)) +
  geom_bar(stat = "identity") +
  labs(title = "Potability of Water Samples within Healthy pH Range from 6.5 to 8.5",
       x = "Potability",
       y = "Count") +
  theme_minimal()




#### Count of Potable and Non-Potable Water
library(ggplot2)

# Create a dataframe with counts
data_counts <- data.frame(
  Category = c("Potable", "Non-Potable"),
  Count = c(nrow(df_potableOnly), nrow(df_nonPotableOnly))
)

# Create a bar plot
ggplot(data_counts, aes(x = Category, y = Count, fill = Category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Count), vjust = -0.5, color = "black", size = 3) +  # Add text labels for count
  labs(title = "Count of Potable and Non-Potable Water",
       x = "Potability",
       y = "Count") +
  theme_minimal()

```

![1chart](https://github.com/jydataman/Exploratory_-data-_analysis-_Water_Potability/assets/141103262/1472b2aa-869d-488c-bde0-c25787253e1d)

![2chart](https://github.com/jydataman/Exploratory_-data-_analysis-_Water_Potability/assets/141103262/9c3a9ccf-bfe5-4637-9b99-6d0881f7a52f)





**Observation:** Supposedly drinking and healthy pH levels are from 6.5 to 8.5. And although I did not expect my column of 981 rows to show that every single one is potable, I didn't think that given the fact that Ph levels are somewhat healthy I thought it would be more potable. Turns out its only 427/981 potable from healthy ph levels. But at the same time The total Potable water in the entire data set is 811/2011. Therefore, 427 bodies of water with healthy pH levels from 6.5-8.5 turn out to be 52% of the entire Potable waters to begin with.



## Hardness:
-Indicates mineral concentration, impacting taste and health implications. Certain minerals can be beneficial or harmful in excessive amounts.

```{r, message=FALSE, warning=FALSE}
library(ggplot2)
library(dplyr)

# Plot 1: Non-Healthy Hardness
plot_non_healthy <- df_cleanData %>%
  filter(Hardness < 60 | Hardness > 120) %>%
  group_by(Potability) %>%
  summarise(Count = n()) %>%
  ggplot(aes(x = Potability, y = Count, fill = factor(Potability))) +  
  geom_bar(stat = "identity") +
  geom_text(aes(label = Count), vjust = -0.5, size = 3) + 
  labs(title = "Non-Healthy Hardness",
       x = "Potability",
       y = "Count",
       fill = "Potability") + 
  theme_minimal()



# Plot 2: Healthy Hardness
plot_healthy <- df_cleanData %>%
  filter(Hardness >= 60 & Hardness <= 120) %>%
  group_by(Potability) %>%
  summarise(Count = n()) %>%
  ggplot(aes(x = Potability, y = Count, fill = factor(Potability))) + 
  geom_bar(stat = "identity") +
  geom_text(aes(label = Count), vjust = -0.5, size = 3) + 
  labs(title = "Healthy Hardness",
       x = "Potability",
       y = "Count",
       fill = "Potability") +  
  theme_minimal()

# Arrange the plots side by side
library(gridExtra)
grid.arrange(plot_non_healthy, plot_healthy, ncol = 2, top = "Potability Comparison")  

```
![3chart](https://github.com/jydataman/Exploratory_-data-_analysis-_Water_Potability/assets/141103262/4418f658-78a6-470d-bf8a-4270d91d951a)



**Observation:** In the assessment of water quality, I identified 1,980 bodies of water with hardness levels falling outside the bounds of healthy standards. Conversely, only 31 bodies of water met the criteria for optimal hardness levels. Despite expectations that all water samples meeting the healthy hardness standards would be potable, the findings revealed a variation. Surprisingly, only 17 out of the 31 samples with healthy hardness levels were deemed potable. This contrast highlights an intriguing observation: while 55% of water with healthy hardness levels is potable, the potability rate drops to 40% for water with non-healthy hardness levels. This 15% decrease difference shows that healthy hardness is hard to tell and may be more complex. It it hard to judge 31 samples vs 1,980 samples. And given there was only a 15% decrease.



## Chloramines:
-Used for water disinfection, their presence requires monitoring to ensure safety without adverse effects.
```{r, message=FALSE, warning=FALSE}

library(ggplot2)
library(dplyr)

# Filtered data for potable and non-potable samples within the specified range
filtered_potable <- df_cleanData %>%
  filter(Potability == 1, Chloramines <= 4.5) %>%
  select(Chloramines)
  
filtered_non_potable <- df_cleanData %>%
  filter(Potability == 0, Chloramines <= 4.5) %>%
  select(Chloramines)

count_potable <- nrow(filtered_potable)
count_non_potable <- nrow(filtered_non_potable)

# Create side-by-side box plots for potable and non-potable samples
ggplot() +
  geom_boxplot(data = filtered_potable, aes(x = Chloramines, y = "Potable"), fill = "lightblue", color = "blue", alpha = 0.5, position = position_dodge(width = 0.8)) +
  geom_boxplot(data = filtered_non_potable, aes(x = Chloramines, y = "Non-Potable"), fill = "lightpink", color = "red", alpha = 0.5, position = position_dodge(width = 0.8)) +
  geom_text(aes(x = 2, y = max(filtered_potable$Chloramines), label = paste()), 
            vjust = 1, hjust = 0.5, color = "blue") +
  geom_text(aes(x = 3, y = min(filtered_non_potable$Chloramines), label = paste()), 
            vjust = -0.5, hjust = 0.5, color = "red") +
  labs(title = "Distribution of Healthy Chloramine Concentrations",
       x = "Chloramine Concentration",
       y = "Potability") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
```

![4hcart](https://github.com/jydataman/Exploratory_-data-_analysis-_Water_Potability/assets/141103262/49ae2856-8d8f-40ec-b517-adc80a3e6a69)


**Observation:** The healthy amounts of Chloramines is up to 4.5. So I expected potability to be higher since I filtered out only showed the healthy values under 4.5. The Values were Really close. 52%  of healthy chloramines is potable.


```{r, message=FALSE, warning=FALSE}
library(ggplot2)
library(dplyr)

filtered_unhealthy_potable <- df_cleanData %>%
  filter(Potability == 1, Chloramines > 4.6)

filtered_unhealthy_non_potable <- df_cleanData %>%
  filter(Potability == 0, Chloramines > 4.6)

ggplot() +
  geom_boxplot(data = filtered_unhealthy_potable, aes(x = Chloramines, y = "Potable"), fill = "lightblue", color = "blue", alpha = 0.5, position = position_dodge(width = 0.8)) +
  geom_boxplot(data = filtered_unhealthy_non_potable, aes(x = Chloramines, y = "Non-Potable"), fill = "lightpink", color = "red", alpha = 0.5, position = position_dodge(width = 0.8)) +
  geom_text(aes(x = 2, y = max(filtered_unhealthy_potable$Chloramines), label = paste()), 
            vjust = 1, hjust = 0.5, color = "blue") +
  geom_text(aes(x = 3, y = min(filtered_unhealthy_non_potable$Chloramines), label = paste()), 
            vjust = -0.5, hjust = 0.5, color = "red") +
  labs(title = "Distribution of Unhealthy Chloramine Concentrations",
       x = "Chloramine Concentration",
       y = "Potability") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

```

![5chart](https://github.com/jydataman/Exploratory_-data-_analysis-_Water_Potability/assets/141103262/eb4a66f0-55a9-40d9-8b30-cd1b966a5569)


**Observation:** The Unhealthy amounts of Chloramines is past 4.5. So I expected Potability to be lower since I filtered out to only show the  values above 4.5.It its 40%  of non-healthy chloramines is potable.


## Trihalomethanes:
-Byproducts of water chlorination, posing health risks if present in elevated concentrations. A general guidelines would be 80-100.


```{r,message=FALSE, warning=FALSE}
library(ggplot2)
library(dplyr)

onlyTmh <- df_cleanData %>%
  mutate(Potability = as.factor(Potability)) %>%
  select(Trihalomethanes, Potability)

ggplot(onlyTmh, aes(x = Potability, y = Trihalomethanes, fill = Potability)) +
  geom_bar(stat = "identity") +
  labs(x = "Potable/Non-potable", y = "Trihalomethanes Levels") +
  scale_fill_manual(values = c("green", "red"), labels = c("Potable", "Non-potable")) +
  
  geom_hline(yintercept = 80, linetype = "dashed", color = "blue") +
  geom_hline(yintercept = 100, linetype = "dashed", color = "red") +
  
  annotate("text", x = 1, y = 80, label = "80 µg/L", vjust = -1, color = "blue") +
  annotate("text", x = 1, y = 100, label = "100 µg/L", vjust = -1, color = "red") +
  
  scale_y_continuous(limits = c(0, 124)) +
  
  theme_minimal()
```
![6chart](https://github.com/jydataman/Exploratory_-data-_analysis-_Water_Potability/assets/141103262/8fbb8aad-f1bb-4321-9a10-eb46ef56a369)



**Observation:** I only considered Trihalomethanes levels under 100 because levels over 100 are considered unhealthy. Therefore, for levels under 100, we observed a higher probability of water being potable. We also noticed that levels within the healthy range of 80-100 are aligned with potable water.


## Insights about EDA

### pH

- **Conclusion**: pH remains a crucial factor in assessing water quality, as water with healthy pH levels is over half of the entire potable water dataset.

### Hardness

- **Complexity**: While hardness indicates mineral concentration and can impact taste and health implications, its influence on potability is not straightforward.
- **Conclusion**: Factors beyond hardness, such as other contaminants, may also play a crucial role in determining water potability. Or perhaps hardness is not a huge factor in potability, considering there was only 31 "healthy" hardness samples to begin with.

### Chloramines


- **Conclusion**:Chloramines are important for disinfection, but they may not be the big factor of water potability but rather indicate "clean water." With this said though, I do believe having healthy levels of Chloramine is important for potable water as although the data is close, it still showed 12% more potable for the healthy levels of Chloramine,

### Trihalomethanes


- **Conclusion**: While Trihalomethanes are important for disinfection, their levels within the healthy range may not necessarily indicate water safety but rather "clean water." However as with Chloramine, although the data is close, it still does show healthy levels of Trihalomethane being more potable.



# Methods

## Model Development-Linear Model
```{r, message=FALSE, warning=FALSE}
# Load required libraries
library(ggplot2)
library(dplyr)

# Check if df_cleanData exists and has the required columns
if (!exists("df_cleanData")) {
  stop("df_cleanData does not exist.")
} else if (!all(c("ph", "Hardness", "Chloramines", "Trihalomethanes", "Potability") %in% colnames(df_cleanData))) {
  stop("df_cleanData is missing required columns.")
}

# Create selected_data
selected_data <- df_cleanData %>%
  select(ph, Hardness, Chloramines, Trihalomethanes, Potability)

# Check if selected_data is created properly
if (!exists("selected_data")) {
  stop("selected_data not created.")
}

# Create a separate linear model for each predictor variable
model_ph <- lm(Potability ~ ph, data = selected_data)
model_Hardness <- lm(Potability ~ Hardness, data = selected_data)
model_Chloramines <- lm(Potability ~ Chloramines, data = selected_data)
model_Trihalomethanes <- lm(Potability ~ Trihalomethanes, data = selected_data)

get_summary <- function(model) {
  summary_data <- summary(model)
  r <- cor(selected_data$Potability, fitted(model))
  p_value <- summary_data$coefficients[2, 4]
  return(list(r = r, p_value = p_value))
}

# Get summary statistics for each model
summary_ph <- get_summary(model_ph)
summary_Hardness <- get_summary(model_Hardness)
summary_Chloramines <- get_summary(model_Chloramines)
summary_Trihalomethanes <- get_summary(model_Trihalomethanes)

# ph
plot_ph <- ggplot(selected_data, aes(x = ph, y = Potability)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "ph", y = "Potability") +
  annotate("text", x = 4, y = 0.8, label = paste("r =", round(summary_ph$r, 3), "\n", "p-value =", round(summary_ph$p_value, 3)), parse = TRUE) +
  ggtitle("Linear Model: Potability ~ ph")

# Hardness
plot_Hardness <- ggplot(selected_data, aes(x = Hardness, y = Potability)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Hardness", y = "Potability") +
  annotate("text", x = 60, y = 0.8, label = paste("r =", round(summary_Hardness$r, 3), "\n", "p-value =", round(summary_Hardness$p_value, 3)), parse = TRUE) +
  ggtitle("Linear Model: Potability ~ Hardness")

# Chloramines
plot_Chloramines <- ggplot(selected_data, aes(x = Chloramines, y = Potability)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Chloramines", y = "Potability") +
  annotate("text", x = 0, y = 0.8, label = paste("r =", round(summary_Chloramines$r, 3), "\n", "p-value =", round(summary_Chloramines$p_value, 3)), parse = TRUE) +
  ggtitle("Linear Model: Potability ~ Chloramines")

# Trihalomethanes
plot_Trihalomethanes <- ggplot(selected_data, aes(x = Trihalomethanes, y = Potability)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Trihalomethanes", y = "Potability") +
  annotate("text", x = 80, y = 0.8, label = paste("r =", round(summary_Trihalomethanes$r, 3), "\n", "p-value =", round(summary_Trihalomethanes$p_value, 3)), parse = TRUE) +
  ggtitle("Linear Model: Potability ~ Trihalomethanes")


print(plot_ph)
print(plot_Hardness)
print(plot_Chloramines)
print(plot_Trihalomethanes)

```
![7chart1](https://github.com/jydataman/Exploratory_-data-_analysis-_Water_Potability/assets/141103262/ed645de7-8998-49e6-8561-556bd80ac61c)

![8chart](https://github.com/jydataman/Exploratory_-data-_analysis-_Water_Potability/assets/141103262/6901a055-655d-456a-a311-66542087551a)

![9chart](https://github.com/jydataman/Exploratory_-data-_analysis-_Water_Potability/assets/141103262/8a32f83a-6b57-47a1-be69-f38bf3881a83)


![10chart](https://github.com/jydataman/Exploratory_-data-_analysis-_Water_Potability/assets/141103262/8ca03ff3-fbdf-480e-9b3c-89d55f05de50)


# Results


### Checking with Predecitve Model

```{r, message=FALSE, warning=FALSE}

predict_model <- lm(Potability ~ ph + Hardness + Chloramines + Trihalomethanes, data = selected_data)


summary(predict_model)

perfectly_healthy <- data.frame(ph = 7.5, Hardness = 80, Chloramines = 3.0, Trihalomethanes = 90)
average_values <- data.frame(ph = 7, Hardness = 196, Chloramines = 7.0, Trihalomethanes = 67)
regular_healthy <- data.frame(ph = 6.8, Hardness = 150, Chloramines = 4.0, Trihalomethanes =100)
regular_unhealthy <- data.frame(ph = 4 , Hardness = 250, Chloramines = 8.0, Trihalomethanes = 30)
absurdly_super_unhealthy <- data.frame(ph = 14, Hardness = 300, Chloramines = 10.0, Trihalomethanes = 120)


test_data <- rbind(perfectly_healthy, average_values, regular_healthy, regular_unhealthy, absurdly_super_unhealthy)
test_data$Potability <- c("Perfectly Healthy", "Average of All Values", "Regular Healthy", "Regular Unhealthy", "Absurdly Super Unhealthy")

prediction <- predict(predict_model, newdata = test_data)

prediction_result <- cbind(test_data, Potability_Prediction = prediction)

# Print prediction result using kable
knitr::kable(prediction_result, caption = "To see how well it can predict potability for different scenarios. 1 means can predict 0 means cannot predict")
```

![11chart](https://github.com/jydataman/Exploratory_-data-_analysis-_Water_Potability/assets/141103262/2ab4eff5-7553-466b-b1ed-dcca619e88b2)



### Checking with Inverse Predecitve Model
```{r, message=FALSE, warning=FALSE}
generate_synthetic_data <- function(n, potability_data) {
  
  ph <- sample(potability_data$ph, n, replace = TRUE)
  Hardness <- sample(potability_data$Hardness, n, replace = TRUE)
  Chloramines <- sample(potability_data$Chloramines, n, replace = TRUE)
  Trihalomethanes <- sample(potability_data$Trihalomethanes, n, replace = TRUE)
  
  # Create dataframe
  data <- data.frame(ph, Hardness, Chloramines, Trihalomethanes)
  
  # Add Potability column
  data$Potability <- rep(1, n)
  
  return(data)
}

# Generate synthetic data with known Potability (1 for potable)
synthetic_data_potable <- generate_synthetic_data(100, selected_data[selected_data$Potability == 1, ])

head(synthetic_data_potable)
```

![12chart](https://github.com/jydataman/Exploratory_-data-_analysis-_Water_Potability/assets/141103262/bb0b2a84-ca4e-4c57-8964-5634b7b1e64a)


### Overview of the dataset: Mean and Median to further investigate
```{r,message=FALSE, warning=FALSE}

mean_values_potable <- colMeans(selected_data[selected_data$Potability == 1, c("ph", "Hardness", "Chloramines", "Trihalomethanes")])
mean_values_not_potable <- colMeans(selected_data[selected_data$Potability == 0, c("ph", "Hardness", "Chloramines", "Trihalomethanes")])

median_values_potable <- apply(selected_data[selected_data$Potability == 1, c("ph", "Hardness", "Chloramines", "Trihalomethanes")], 2, median)
median_values_not_potable <- apply(selected_data[selected_data$Potability == 0, c("ph", "Hardness", "Chloramines", "Trihalomethanes")], 2, median)

cat("Mean values for potable water:\n")
print(mean_values_potable)
cat("\nMean values for not potable water:\n")
print(mean_values_not_potable)

cat("\nMedian values for potable water:\n")
print(median_values_potable)
cat("\nMedian values for not potable water:\n")
print(median_values_not_potable)
```

![13chart](https://github.com/jydataman/Exploratory_-data-_analysis-_Water_Potability/assets/141103262/cf4f6c99-722a-40d3-bce6-80ad510468dc)


# Analysis

While there is more correlation with some variables compared to others, the difference isn't significant according to the linear model. To further illustrate this point, a predictive model was created. The predictive model demonstrates the variance among the four water variables and showcases how closely the model can predict potability across different drastic points. Which showed that the numbers are quite close to each other showing no true significance. Additionally, to validate the findings, the prediction model was inverted to identify which potable samples have healthy amounts of the variables, revealing that the parameters tested for are more or less with par with the mean of those values, which means that there is not true significance as well.

# Discussion

Potable water which means water safe for consumption, is important. However, in my research, identifying what makes potable water proved to be challenging I initially attempted to narrow it down to four significant variables that contribute to water safety. My assumption was that if water is deemed safe overall, it should be safe to drink. However, this assumption was challenged as I looked more into the data.

The data and methods of testing showed time after time that the safety of water isn't determined by a single prominent factor. In fact in the looking at the mean and median for potable vs not potable the values were really close, which cemented this point. I think a combination of variables and scenarios determine water potability. Because despite analyzing a large dataset, finding a clear definition of potable water was hard to find. Perhaps additional investigation into other variables that I didn't consider initially could have provided more clarity or a different data set that focused on a more specific region.

One aspect that stood out among the complexity was pH levels.

* It appeared that pH plays a crucial role in determining water safety.
* However, the issue of water hardness proved to be more complex, requiring different data and analysis.
* Similarly, while chloramine contributes to water cleanliness, its use for drinking water remains uncertain, as does that of Trihalomethane. 

Therefore, it's crucial to know the difference between clean water and water safe for drinking. While water meeting certain cleanliness standards is essential, ensuring its safety for consumption involves a different process. This complexity is important to state and makes individualized research and data gathering more important for this topic, instead of a "know all guide" into water safety.

# Conclusion

The research although proved my hypothesis to be wrong, it highlighted other things. For starters, water around the world in general isn't safe for drinking. And water data should be used on your location and then checked with an expert.

With that thought in mind, this prompts a consideration that the water we commonly consume might undergo heavy filtration.Consequently, rather than solely focusing on water content, exploring water filter could be more important, especially as it advances. And to conclude, although this analysis underscores the importance of the four variables in defining clean water, further data collection on additional factors is necessary to gain a comprehensive understanding. 

# References: MLA

BBC Future. "Is Filtered Water Healthier Than Tap Water?" BBC Future, 17 Apr. 2024, www.bbc.com/future/article/20240417-is-filtered-water-healthier-than-tap-water.

Healthline. "What Is the Healthiest Water to Drink?" Healthline, www.healthline.com/nutrition/what-is-the-healthiest-water-to-drink#drinking-water.


United States Geological Survey. "Hardness of Water." Water Science School - USGS, www.usgs.gov/special-topics/water-science-school/science/hardness-water#:~:text=The%20simple%20definition%20of%20water,minerals%2C%20largely%20calcium%20and%20magnesium.

Centers for Disease Control and Prevention. "Water Disinfection: Chloramines." Healthy Water - CDC, www.cdc.gov/healthywater/drinking/public/water_disinfection.html#:~:text=Chloramines%20are%20a%20group%20of,are%20still%20safe%20to%20drink.

Santa Clara Valley Water District. "Trihalomethanes (THMs)." Valley Water, www.valleywater.org/your-water/water-quality/protecting-your-water/trihalomethanes-thms#:~:text=Trihalomethanes%20(THMs)%20are%20a%20byproduct,common%20of%20which%20are%20THMs.

Aditya Kadiwal. "Water Potability Dataset." Kaggle, www.kaggle.com/datasets/adityakadiwal/water-potability.

**Libraries Used:**
Wickham, H., & Bryan, J. (2019). tidyverse: Easily Install and Load the 'Tidyverse' (Version 1.2.1). R Foundation for Statistical Computing, Vienna, Austria. [Online]. Available: https://tidyverse.org/

Wickham, H., Hester, J., & Francois, R. (2018). readr: Read Rectangular Text Data (Version 1.3.1). R Foundation for Statistical Computing, Vienna, Austria. [Online]. Available: https://cran.r-project.org/web/packages/readr/index.html

Larmarange, J. (2021). summarytools: Tools to Quickly and Neatly Summarize Data (Version 0.9.8). R Foundation for Statistical Computing, Vienna, Austria. [Online]. Available: https://cran.r-project.org/web/packages/summarytools/index.html

Wickham, H., Francois, R., Henry, L., & Müller, K. (2021). dplyr: A Grammar of Data Manipulation (Version 1.0.7). R Foundation for Statistical Computing, Vienna, Austria. [Online]. Available: https://cran.r-project.org/web/packages/dplyr/index.html

Wickham, H. (2016). ggplot2: Elegant Graphics for Data Analysis (Version 3.3.5). R Foundation for Statistical Computing, Vienna, Austria. [Online]. Available: https://cran.r-project.org/web/packages/ggplot2/index.html

Wei, T., & Simko, V. (2017). corrplot: Visualization of a Correlation Matrix (Version 0.84). R Foundation for Statistical Computing, Vienna, Austria. [Online]. Available: https://cran.r-project.org/web/packages/corrplot/index.html

Auguie, B. (2017). gridExtra: Miscellaneous Functions for "Grid" Graphics (Version 2.3). R Foundation for Statistical Computing, Vienna, Austria. [Online]. Available: https://cran.r-project.org/web/packages/gridExtra/index.html










