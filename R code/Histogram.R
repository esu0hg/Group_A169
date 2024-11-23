# Load necessary libraries
library(ggplot2)

# Load the dataset
movie_data <- read.csv("C:/Users/ghouse/IMDb movies.csv")

# Ensure the columns are numeric
movie_data$avg_vote <- as.numeric(movie_data$avg_vote)
movie_data$duration <- as.numeric(movie_data$duration)

# Check for missing values and handle them if needed
movie_data <- na.omit(movie_data)

# Histogram of avg_vote with a normal curve overlay
ggplot(movie_data, aes(x = avg_vote)) +
  geom_histogram(aes(y = ..density..), bins = 30, color = "black", fill = "lightblue") +
  stat_function(fun = dnorm, args = list(mean = mean(movie_data$avg_vote, na.rm = TRUE), 
                                         sd = sd(movie_data$avg_vote, na.rm = TRUE)), 
                color = "red", linewidth  = 1) +
  labs(title = "Histogram of avg_vote with Normal Curve Overlay",
       x = "Average Votes (avg_vote)", 
       y = "Density") +
  theme_minimal()

# Scatter Plot with trendline for duration vs. avg_vote
ggplot(movie_data, aes(x = duration, y = avg_vote)) +
  geom_point(color = "blue", alpha = 0.5) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Scatter Plot of Duration vs. avg_vote with Trendline",
       x = "Movie Duration (minutes)",
       y = "Average Votes (avg_vote)") +
  theme_minimal()


