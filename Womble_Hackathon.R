# Chart 1
# This dataset generation code was provided with assistance from ChatGPT
# Set seed for reproducibility
set.seed(123)

# Generate a dataset
df <- data.frame(
  ID = 1:100,  # IDs from 1 to 100
  Category = sample(c("A", "B", "C"), 100, replace = TRUE),  # Randomly assign categories
  Value = round(runif(100, min = 10, max = 100), 2),  # Random values between 10 and 100
  Date = sample(seq(as.Date('2020-01-01'), as.Date('2021-01-01'), by="day"), 100, replace = TRUE)  # Random dates
)

# View the first few rows of the dataset
head(df)

install.packages("ggplot2")
library(ggplot2)

df_summary <- aggregate(Value ~ Category, df, mean)

ggplot(df_summary, aes(x = Category, y = Value)) +
  geom_bar(stat = "identity", fill = "lightblue", color = "black")
  theme_minimal() +
  labs(title = "Average Value by Category", x = "Category", Y = "Value")
  geom_text(aes(label = round(Value, 2)), vjust = -0.5)
  
# Chart 2
  
  # Chatgpt was used to assist with the coding
  # Load necessary libraries
  library(ggplot2)
  library(gridExtra)
  
  # Reshape the Anscombe data for ggplot2 (long format)
  anscombe_long <- data.frame(
    x = c(anscombe$x1, anscombe$x2, anscombe$x3, anscombe$x4),
    y = c(anscombe$y1, anscombe$y2, anscombe$y3, anscombe$y4),
    group = rep(c("Set 1", "Set 2", "Set 3", "Set 4"), each = nrow(anscombe))
  )
  
  # Create a new plotting function for bar charts
  gg_anscombe_bar <- function(data, group_label) {
    ggplot(data, aes(x = as.factor(x), y = y)) +  # Convert x to factor for bar chart
      geom_bar(stat = "identity", aes(fill = group_label), color = "black") +  # Create bars
      labs(title = paste("Bar Chart: ", group_label), x = "x", y = "Sum of y") +
      theme_minimal(base_family = "serif") +  # Set font to serif
      theme(plot.title = element_text(hjust = 0.5, size = 14))  # Center the title
  }
  
  # Bar Plot 1
  bp1 <- gg_anscombe_bar(anscombe_long[anscombe_long$group == "Set 1", ], "Set 1") +
    scale_fill_manual(values = "red")
  
  # Bar Plot 2
  bp2 <- gg_anscombe_bar(anscombe_long[anscombe_long$group == "Set 2", ], "Set 2") +
    scale_fill_manual(values = "darkgreen")
  
  # Bar Plot 3
  bp3 <- gg_anscombe_bar(anscombe_long[anscombe_long$group == "Set 3", ], "Set 3") +
    scale_fill_manual(values = "orange")
  
  # Bar Plot 4
  bp4 <- gg_anscombe_bar(anscombe_long[anscombe_long$group == "Set 4", ], "Set 4") +
    scale_fill_manual(values = "blue")
  
  # Arrange the four bar charts into a grid
  grid.arrange(bp1, bp2, bp3, bp4, nrow = 2)
  
# Chart 3

  library(ggplot2)
  
  # Summarize the dataset
  # We'll use 'mtcars' dataset and create a bar chart of car models vs their horsepower (hp)
  data <- mtcars
  data$car_model <- rownames(data)
  
  # Create a horizontal bar chart
  ggplot(data, aes(x = hp, y = reorder(car_model, hp))) +
    geom_bar(stat = "identity", fill = "lightcoral") +
    labs(title = "Horsepower of Car Models in mtcars Dataset",
         x = "Horsepower (hp)", y = "Car Models") +
    theme_minimal()
  
  # Citation: This code was generated using ChatGPT.
  
# Chart 4
  
  data <- mtcars
  data$transmission <- ifelse(data$am == 0, 'Automatic', 'Manual')
  
  # Summarize data: Average mpg by number of cylinders and transmission type
  data_summary <- aggregate(mpg ~ cyl + transmission, data, mean)
  
  # Create a grouped column chart
  ggplot(data_summary, aes(x = factor(cyl), y = mpg, fill = transmission)) +
    geom_bar(position = "dodge", stat = "identity") +
    labs(title = "Average MPG by Cylinder and Transmission Type (mtcars Dataset)", 
         x = "Number of Cylinders", y = "Miles per Gallon (MPG)") +
    theme_minimal() +
    scale_fill_manual(values = c("Automatic" = "lightblue", "Manual" = "lightcoral"))
  
  # Citation: This code was generated using ChatGPT.