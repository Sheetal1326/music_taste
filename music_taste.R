#INSTALLING PACKAGES

install.packages("janitor")
install.packages("DataExplorer")
install.packages("tidyverse")
install.packages("caret")
install.packages("varImp")
library(janitor)
library(DataExplorer)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(rstatix)
library(reshape2)
library(gplots)
library(caret)  # for one-hot encoding
library(varImp)
library(corrplot)
library(car)

#DATA CLEAN

#Reading the data
music<- read.csv("C:/Users/91879/Downloads/mxmh_survey_results.csv")
head(music)
df <- clean_names(music)
introduce(df)
#replace all the empty values with NA
df <- replace(df, df=='',NA)
#Finding missing values
plot_missing(df) #from the plot we can see that there are many null values in "BPM"
#BPM has many null values so we are replacing it with the median value for each fav_genre
df1 <- df %>%
  group_by(fav_genre) %>%
  mutate(bpm = ifelse(is.na(bpm), median(bpm, na.rm = TRUE), bpm))
#Since the number of NA values isn't very high we are going to drop them
df_drop_NA <- df1 %>% drop_na()
plot_missing(df_drop_NA) #we can from this plot that there are no more null values in the data set
#Now let's check for outliers in the data
options(repr.plot.width = 12, repr.plot.height = 8)
df_drop_NA %>%
  dplyr::select(!timestamp) %>%
  plot_boxplot(
    by="music_effects",
    geom_boxplot_args = list(fill = "violet"),
    theme_config = list(
      axis.text.x = element_text(hjust = 1, face = "bold"),
      axis.text = element_text(size = 12, colour = "brown"),
      axis.title = element_text(size = 12, colour = "black"),
      axis.line = element_line(colour = "black", linewidth = .5),
      plot.title = element_text(hjust = 0.5),
      panel.background = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      strip.text = element_text(size = 16),
      strip.background = element_rect(fill = "orange")
    )
  )
#finding Q1, Q3, and interquartile range for values in column bpm
Q1B <- quantile(df_drop_NA$bpm, .25)
Q3B<- quantile(df_drop_NA$bpm, .75)
IQRB <- IQR(df_drop_NA$bpm)
#let's only keep rows in dataframe that have values within 1.5*IQR of Q1 and Q3
df_no_bpm_outlier <- subset(df_drop_NA, df_drop_NA$bpm > (Q1B - 1.5*IQRB) & df_drop_NA$bpm < (Q3B + 1.5*IQRB))
#similarly finding Q1, Q3, and interquartile range for values in column hours_per_day
Q1H <- quantile(df_drop_NA$hours_per_day, .25)
Q3H <- quantile(df_drop_NA$hours_per_day, .75)
IQRH <- IQR(df_drop_NA$hours_per_day)
#Let's only keep rows in dataframe that have values within 1.5*IQR of Q1 and Q3
music_clean <-subset(df_no_bpm_outlier, df_no_bpm_outlier$hours_per_day > (Q1H - 1.5*IQRH) & df_no_bpm_outlier$hours_per_day < (Q3H + 1.5*IQRH))
dim(music)
dim(music_clean) #the dimensions reduce from 736 X 33  to  655 X 33
#plotting graph after cleaning up all the outliers
music_clean %>%
  dplyr::select(!timestamp) %>%
  drop_na() %>%
  plot_boxplot(
    by="music_effects",
    geom_boxplot_args = list(fill = "green"),
    theme_config = list(
      axis.text.x = element_text(hjust = 1, face = "bold"),
      axis.text = element_text(size = 12, colour = "black"),
      axis.title = element_text(size = 12, colour = "black"),
      axis.line = element_line(colour = "black", linewidth = .5),
      plot.title = element_text(hjust = 0.5),
      panel.background = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      strip.text = element_text(size = 16),
      strip.background = element_rect(fill = "purple")
    )
  )
savehistory()


#EDA

#______________________________________________________________________
df <- music_clean
#Respondents' background
g <- ggplot(df, aes(age)) + geom_histogram(fill = "mediumaquamarine", color = "black")

p <- plotly::ggplotly(g)
p <- plotly::config(p, staticPlot = FALSE)
p

summary(df$age)

#Streaming services
s_colors <- c("lightgreen", "red", "blue", "violetred", "gold", "brown")

services <- as.data.frame(table(df$primary_streaming_service))
services <- services[-1, ]
p <- plotly::plot_ly(services, labels = ~Var1, values = ~Freq, type = "pie", colors = s_colors) %>%
  plotly::layout(title = "Streaming services by popularity")
p

#most popular music streaming service
service_counts <- table(df$primary_streaming_service) / nrow(df)
service_counts
s_ages <- df %>%
  group_by(primary_streaming_service) %>%
  summarize(median_age = median(age))

s_ages <- s_ages %>%
  filter(primary_streaming_service != "")

print(s_ages)

# Print the interactive plot to find mode of hours listened
p<- plotly::plot_ly(df, x = ~`hours_per_day`, type = "histogram", histnorm = "density", 
                    autobinx = TRUE, marker = list(color = 'slateblue')) %>%
  plotly::layout(title = "Distribution of Hours per day",
                 xaxis = list(title = "Hours per day"),
                 yaxis = list(title = "Density"),
                 bargap = 0.05, # Adjust the gap between bars
                 barmode = "overlay") 
p



#MODEL BUILDING

# Convert categorical variables to factors
df$instrumentalist <- as.factor(df$instrumentalist)
df$composer <- as.factor(df$composer)
df$while_working <- as.factor(df$while_working)
df$exploratory <- as.factor(df$exploratory)
df$foreign_languages <- as.factor(df$foreign_languages)
df$instrumentalist <- as.numeric(df$instrumentalist)
df$composer <- as.numeric(df$composer)
df$while_working <- as.numeric(df$while_working)
df$exploratory <- as.numeric(df$exploratory)
df$foreign_languages <- as.numeric(df$foreign_languages)

selected_vars <- c("age", "hours_per_day", "while_working", "instrumentalist", "composer", "exploratory", "foreign_languages")

# Subset the dataframe to include only the selected variables
selected_df <- df[, selected_vars]

# Compute the correlation matrix
corr_matrix <- cor(selected_df)
corr_matrix_rounded <- round(corr_matrix, 2)
# Create a heatmap
gplots::heatmap.2(corr_matrix_rounded, Colv = NA, Rowv = NA, 
          col = colorRampPalette(c("blue", "green"))(25), 
          scale = "none", margins = c(5, 10), 
          labRow = selected_vars, cexRow = 0.8, cexCol = 0.8, 
          main = "Heatmap of Selected Variables", 
          xlab = "Variables", ylab = "Variables",
          cellnote = corr_matrix_rounded, notecol = "white", 
          notecex = 0.6, trace = "none")

par(mfrow = c(2, 2)) # Set up a 2x2 grid of plots
barplot(table(df$anxiety), xlab = "Value", ylab = "Frequency", main = "Anxiety", col = "red")
barplot(table(df$depression), xlab = "Value", ylab = "Frequency", main = "Depression", col = "green")
barplot(table(df$insomnia), xlab = "Value", ylab = "Frequency", main = "Insomnia", col = "purple")
barplot(table(df$ocd), xlab = "Value", ylab = "Frequency", main = "OCD",col = "blue")
par(mfrow = c(1, 1))

#relation between hours listened and disorder
anxiety_extreme <- mean(df$hours_per_day[df$anxiety > 8])
depression_extreme <- mean(df$hours_per_day[df$depression > 8])
insomnia_extreme <- mean(df$hours_per_day[df$insomnia > 8])
ocd_extreme <- mean(df$hours_per_day[df$ocd > 8])
extreme_means <- c(anxiety_extreme, depression_extreme, insomnia_extreme, ocd_extreme)
disorder_names <- c("Anxiety", "Depression", "Insomnia", "OCD")

bar_plot_plotly <- plotly::plot_ly(x = extreme_means, y = disorder_names, type = "bar", orientation = "h",
                                   text = paste("Disorder:", disorder_names, "<br>",
                                                "Avg hours listened:", round(extreme_means, 2)),
                                   hoverinfo = "text",
                                   marker = list(color = c("lightpink", "cornflowerblue", "darkmagenta", "orange"))) %>%
  plotly::layout(xaxis = list(title = "Avg hours listened", range = c(0, 4)),
                 yaxis = list(title = "Disorder"),
                 title = "Hours listened for individuals with extreme MH rankings")

# Display the plot
bar_plot_plotly

anxiety_extreme <- mean(df$hours_per_day[df$anxiety < 3])
depression_extreme <- mean(df$hours_per_day[df$depression < 3])
insomnia_extreme <- mean(df$hours_per_day[df$insomnia < 3])
ocd_extreme <- mean(df$hours_per_day[df$ocd < 3])
extreme_means <- c(anxiety_extreme, depression_extreme, insomnia_extreme, ocd_extreme)

bar_plot_plotly <- plotly::plot_ly(x = extreme_means, y = disorder_names, type = "bar", orientation = "h",
                           text = paste("Disorder:", disorder_names, "<br>",
                                        "Avg hours listened:", round(extreme_means, 2)),
                           hoverinfo = "text",
                           marker = list(color = c("lightpink", "cornflowerblue", "darkmagenta", "orange"))) %>%
  plotly::layout(xaxis = list(title = "Avg hours listened",range = c(0, 4)),
         yaxis = list(title = "Disorder"),
         title = "Hours listened for individuals with low MH rankings")

# Display the plot
bar_plot_plotly

# Create a heatmap for disorders
corr_matrix <- cor(df[, tolower(disorder_names)])
corr_matrix_rounded <- round(corr_matrix, 2)

# Create a heatmap with rounded correlation values displayed
gplots::heatmap.2(corr_matrix_rounded, Colv = NA, Rowv = NA, 
          col = colorRampPalette(c("white", "darkred"))(25), 
          scale = "none", margins = c(5, 10), 
          labRow = disorder_names, cexRow = 0.8, cexCol = 0.8, 
          main = "Heatmap of Mental disorders", 
          xlab = "Variables", ylab = "Variables",
          cellnote = corr_matrix_rounded, notecol = "black", 
          notecex = 0.6, trace = "none")

# Get unique effects and their counts
df <- df %>% filter(music_effects != "")
effects <- df$music_effects%>% unique()
effects_counts <- df$music_effects %>% table()
effects_counts <- effects_counts[effects_counts > 0]
effects <- effects[effects_counts > 0]

# Create an interactive pie chart
pie_chart <- plotly::plot_ly(labels = names(effects_counts),
                             values = effects_counts,
                             type = "pie") %>%
  plotly::layout(title = "Effects of Music on Mental Health",
                 xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                 yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
pie_chart

summary_table <- df %>%
  group_by(fav_genre, music_effects) %>%
  summarise(count = n()) %>%
  mutate(percent = count / sum(count) * 100) %>%
  arrange(fav_genre, desc(music_effects))

# Plot the stacked bar chart
plotly::plot_ly(summary_table, x = ~fav_genre, y = ~percent, color = ~music_effects, type = "bar") %>%
  plotly::layout(title = "Effect of Music by Favorite Genre",
                 xaxis = list(title = "Favorite Genre"),
                 yaxis = list(title = "Percentage"),
                 barmode = "stack") %>%
  plotly::config(displayModeBar = TRUE) 

#music_clean <- df 
#______________________________________________________________________

music_clean<-data.frame(music_clean)


# Select relevant features
data <- music_clean %>%
  select(hours_per_day, while_working, 
         instrumentalist, composer, 
         fav_genre, exploratory, foreign_languages, bpm,
         frequency_classical:frequency_video_game_music,  # frequency of each genre
         anxiety, depression,insomnia, ocd, music_effects)


data$while_working <- as.numeric(factor(music_clean$while_working, levels = c("No", "Yes")))
data$instrumentalist <- as.numeric(factor(music_clean$instrumentalist, levels = c("No", "Yes")))
data$composer <- as.numeric(factor(music_clean$composer, levels = c("No", "Yes")))
#data$fav_genre <- as.numeric(factor(music_data$fav_genre, levels = unique(music_data$fav_genre)))
data$fav_genre <- as.numeric(factor(music_clean$fav_genre, levels = c("Classical","Country","EDM","Folk","Gospel","Hip hop", "Jazz","K pop","Latin","Lofi","Metal","Pop","R&B","Rap","Rock","Video game music",NA)))
data$exploratory <- as.numeric(factor(music_clean$exploratory, levels = c("No", "Yes")))
data$foreign_languages <- as.numeric(factor(music_clean$foreign_languages, levels = c("No", "Yes")))
data$frequency_classical <- as.numeric(factor(music_clean$frequency_classical, levels = c("Never", "Rarely", "Sometimes", "Very frequently")))
data$frequency_country <- as.numeric(factor(music_clean$frequency_country, levels = c("Never", "Rarely", "Sometimes", "Very frequently")))
data$frequency_edm <- as.numeric(factor(music_clean$frequency_edm, levels = c("Never", "Rarely", "Sometimes", "Very frequently")))
data$frequency_folk <- as.numeric(factor(music_clean$frequency_folk, levels = c("Never", "Rarely", "Sometimes", "Very frequently")))
data$frequency_gospel <- as.numeric(factor(music_clean$frequency_gospel, levels = c("Never", "Rarely", "Sometimes", "Very frequently")))
data$frequency_hip_hop <- as.numeric(factor(music_clean$frequency_hip_hop, levels = c("Never", "Rarely", "Sometimes", "Very frequently")))
data$frequency_jazz <- as.numeric(factor(music_clean$frequency_jazz, levels = c("Never", "Rarely", "Sometimes", "Very frequently")))
data$frequency_k_pop <- as.numeric(factor(music_clean$frequency_k_pop, levels = c("Never", "Rarely", "Sometimes", "Very frequently")))
data$frequency_latin <- as.numeric(factor(music_clean$frequency_latin, levels = c("Never", "Rarely", "Sometimes", "Very frequently")))
data$frequency_lofi <- as.numeric(factor(music_clean$frequency_lofi, levels = c("Never", "Rarely", "Sometimes", "Very frequently")))
data$frequency_metal <- as.numeric(factor(music_clean$frequency_metal, levels = c("Never", "Rarely", "Sometimes", "Very frequently")))
data$frequency_pop <- as.numeric(factor(music_clean$frequency_pop, levels = c("Never", "Rarely", "Sometimes", "Very frequently")))
data$frequency_r_b <- as.numeric(factor(music_clean$frequency_r_b, levels = c("Never", "Rarely", "Sometimes", "Very frequently")))
data$frequency_rap <- as.numeric(factor(music_clean$frequency_rap, levels = c("Never", "Rarely", "Sometimes", "Very frequently")))
data$frequency_rock <- as.numeric(factor(music_clean$frequency_rock, levels = c("Never", "Rarely", "Sometimes", "Very frequently")))
data$frequency_video_game_music <- as.numeric(factor(music_clean$frequency_video_game_music, levels = c("Never", "Rarely", "Sometimes", "Very frequently")))
data$music_effects <- as.numeric(factor(music_clean$music_effects, levels = c("Improve", "No effect", "Worsen")))



# Split data into training and testing sets
set.seed(123)  # For reproducibility
training_index <- sample(1:nrow(data), size = 0.8 * nrow(data))
training_data <- data[training_index, ]
testing_data <- data[-training_index, ]

# Build separate models for anxiety and depression (linear regression example)
anxiety_model <- lm(anxiety ~ ., data = training_data)
depression_model <- lm(depression ~ ., data = training_data)
insomnia_model <- lm(insomnia ~ ., data = training_data)
ocd_model <- lm(ocd ~ ., data = training_data)

# Summary of the regression model for Anxiety
summary(anxiety_model)

# Summary of the regression model for Depression
summary(depression_model)

# Summary of the regression model for Insomnia
summary(insomnia_model)

# Summary of the regression model for OCD
summary(ocd_model)


# Evaluate model performance on the testing set (example for anxiety)
predictions <- predict(anxiety_model, newdata = testing_data)
rmse <- sqrt(mean((testing_data$anxiety - predictions)^2))
cat("RMSE for Anxiety Model:", rmse, "\n")

# Evaluate model performance for depression 
predictions <- predict(depression_model, newdata = testing_data)
rmse <- sqrt(mean((testing_data$depression - predictions)^2))
cat("RMSE for Depression Model:", rmse, "\n")

# Evaluate model performance for insomnia
predictions <- predict(insomnia_model, newdata = testing_data)
rmse <- sqrt(mean((testing_data$insomnia - predictions)^2))
cat("RMSE for Insomnia Model:", rmse, "\n")

# Evaluate model performance for ocd
predictions <- predict(ocd_model, newdata = testing_data)
rmse <- sqrt(mean((testing_data$ocd - predictions)^2))
cat("RMSE for OCD Model:", rmse, "\n")

# Set up a 2x2 plotting layout
par(mfrow = c(2, 2))

# Plot Actual vs. Predicted Values for each model separately
plot(testing_data$anxiety, predictions, xlab = "Actual", ylab = "Predicted", main = "Actual vs. Predicted (Anxiety)")
abline(0, 1, col = "red")

# Repeat for each model
plot(testing_data$depression, predictions, xlab = "Actual", ylab = "Predicted", main = "Actual vs. Predicted (Depression)")
abline(0, 1, col = "blue")

plot(testing_data$insomnia, predictions, xlab = "Actual", ylab = "Predicted", main = "Actual vs. Predicted (Insomnia)")
abline(0, 1, col = "green")

plot(testing_data$ocd, predictions, xlab = "Actual", ylab = "Predicted", main = "Actual vs. Predicted (OCD)")
abline(0, 1, col = "orange")



par(mfrow = c(2, 2))  # Set up a 2x2 plotting layout

# Plot Residuals vs. Fitted for all four models
plot(depression_model, which = 1, main = "Residuals vs. Fitted (Depression Model)")
plot(anxiety_model, which = 1, main = "Residuals vs. Fitted (Anxiety Model)")
plot(insomnia_model, which = 1, main = "Residuals vs. Fitted (Insomnia Model)")
plot(ocd_model, which = 1, main = "Residuals vs. Fitted (OCD Model)")


# Set up a 2x2 plotting layout
par(mfrow = c(2, 2))

# Plot Normal Q-Q Plot for Anxiety Model
plot(anxiety_model, which = 2, main = "Normal Q-Q Plot (Anxiety)")
plot(depression_model, which = 2, main = "Normal Q-Q Plot (Depression)")
plot(insomnia_model, which = 2, main = "Normal Q-Q Plot (Insomnia)")
plot(ocd_model, which = 2, main = "Normal Q-Q Plot (OCD)")




# Set up a 2x2 plotting layout
par(mfrow = c(2, 2))

# Plot Scale-Location plot for Anxiety Model
plot(anxiety_model, which = 3, main = "Scale-Location Plot (Anxiety)")
plot(depression_model, which = 3, main = "Scale-Location Plot (Depression)")
plot(insomnia_model, which = 3, main = "Scale-Location Plot (Insomnia)")
plot(ocd_model, which = 3, main = "Scale-Location Plot (OCD)")



# Set up a 2x2 plotting layout
par(mfrow = c(2, 2))

# Plot Cook's distance plot for Anxiety Model
plot(anxiety_model, which = 4, main = "Cook's Distance Plot (Anxiety)")
plot(depression_model, which = 4, main = "Cook's Distance Plot (Depression)")
plot(insomnia_model, which = 4, main = "Cook's Distance Plot (Insomnia)")
plot(ocd_model, which = 4, main = "Cook's Distance Plot (OCD)")



# Set up a 2x2 plotting layout
par(mfrow = c(2, 2))

# Plot leverage plot for Anxiety Model
plot(anxiety_model, which = 5, main = "Leverage Plot (Anxiety)")
plot(depression_model, which = 5, main = "Leverage Plot (Depression)")
plot(insomnia_model, which = 5, main = "Leverage Plot (Insomnia)")
plot(ocd_model, which = 5, main = "Leverage Plot (OCD)")



# Set up a 2x2 plotting layout
par(mfrow = c(2, 2))

# Plot Cook's distance plot for Anxiety Model
plot(anxiety_model, which = 6, main = "Cook's Distance Plot (Anxiety)")
plot(depression_model, which = 6, main = "Cook's Distance Plot (Depression)")
plot(insomnia_model, which = 6, main = "Cook's Distance Plot (Insomnia)")
plot(ocd_model, which = 6, main = "Cook's Distance Plot (OCD)")

# Set up a 2x2 plotting layout
par(mfrow = c(2, 2))

# Partial Regression Plot for hours_per_day in Anxiety Model
car::avPlots(anxiety_model, terms = "hours_per_day", main = "Partial Regression Plot (Anxiety)")
car::avPlots(depression_model, terms = "hours_per_day", main = "Partial Regression Plot (Depression)")
car::avPlots(insomnia_model, terms = "hours_per_day", main = "Partial Regression Plot (Insomnia)")
car::avPlots(ocd_model, terms = "hours_per_day", main = "Partial Regression Plot (OCD)")




# Set up a 2x2 plotting layout
par(mfrow = c(2, 2))

# Interaction Plot for Anxiety Model
interaction.plot(testing_data$while_working, testing_data$instrumentalist, predictions, 
                 xlab = "While Working", ylab = "Predictions", main = "Interaction Plot (Anxiety)")

# Repeat for the other three models
interaction.plot(testing_data$while_working, testing_data$instrumentalist, predictions, 
                 xlab = "While Working", ylab = "Predictions", main = "Interaction Plot (Depression)")

interaction.plot(testing_data$while_working, testing_data$instrumentalist, predictions, 
                 xlab = "While Working", ylab = "Predictions", main = "Interaction Plot (Insomnia)")

interaction.plot(testing_data$while_working, testing_data$instrumentalist, predictions, 
                 xlab = "While Working", ylab = "Predictions", main = "Interaction Plot (OCD)")

corr_matrix <- cor(training_data)
corrplot(corr_matrix, method = "color")




