# Load necessary libraries
library(ggplot2)
library(dplyr)

# Load the dataset
movie_data <- read.csv("C:\\Users\\Ghouse\\A169\\Dataset\\IMDbmovies.csv"
)


movie_data$avg_vote <- as.numeric(movie_data$avg_vote)
movie_data$duration <- as.numeric(movie_data$duration)

# Check for missing values and handle them
movie_data <- na.omit(movie_data)

# Remove outliers 
movie_data <- movie_data %>%
  filter(duration <= 400)

# Histogram of avg_vote with normal curve
ggplot(movie_data, aes(x = avg_vote)) +
  geom_histogram(aes(y = ..count.., fill = ..count..), bins = 30, color = "black") + 
  scale_fill_gradient(low = "green", high = "red") + 
  scale_x_continuous(breaks = seq(1, 10, 1), limits = c(1, 10)) + 
  scale_y_continuous(breaks = seq(0, 1800, 200), limits = c(0, 1800)) + 
  stat_function(fun = function(x) {
    # Scale the normal distribution curve to match the frequency counts
    dnorm(x, mean = mean(movie_data$avg_vote, na.rm = TRUE), 
          sd = sd(movie_data$avg_vote, na.rm = TRUE)) * nrow(movie_data) * (10 - 1) / 30
  }, color = "blue", linewidth = 1) + 
  labs(title = "Histogram of Average Vote with Normal Curve Overlay",
       x = "Average Votes", 
       y = "Frequency (count)") + 
  theme_minimal() +
  theme(
    axis.line = element_line(color = "black"), 
    axis.ticks = element_line(color = "black"), 
    axis.ticks.length = unit(0.2, "cm"),        
    plot.title = element_text(size = 12, hjust = 0.5, face = "bold", color = "gray30"), 
    panel.border = element_rect(color = "black", fill = NA, size = 1) 
  )

# Scatter Plot 
ggplot(movie_data, aes(x = duration, y = avg_vote)) +
  geom_point(color = "blue", alpha = 0.5) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  scale_x_continuous(limits = c(0, 400), breaks = seq(0, 400, 50)) + # Limit x-axis to 400
  scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, 1)) +    # Limit y-axis to 0-10
  labs(title = "Scatter Plot of Duration vs. Average Vote with Trendline",
       x = "Movie Duration",
       y = "Average Votes") +
  theme_minimal() +
  theme(
    axis.line = element_line(color = "black"), 
    axis.ticks = element_line(color = "black"), 
    axis.ticks.length = unit(0.2, "cm"),        
    plot.title = element_text(size = 12, hjust = 0.5, face = "bold", color = "gray30"),
    panel.border = element_rect(color = "black", fill = NA, size = 1) 
  )

