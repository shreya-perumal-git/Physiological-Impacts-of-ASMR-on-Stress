library(ggplot2)


# Defining the Heart Rate Data
# Note: Data from tables were turned into a list for eah column 
# to simplify the R code

# Distraction Level 1
level1 <- data.frame(
  Baseline = c(79, 114.333, 52, 78.333, 91.667, 102.333, 97.333, 75.333, 68.667, 76.333),
  ASMR1 = c(70, 104, 49, 78, 86, 104, 94, 73, 69, 74),
  ASMR2 = c(74, 103, 51, 70, 81, 101, 91, 70, 67, 71),
  Distraction = c(79, 104, 49, 76, 85, 115, 106, 81, 78, 81),
  Post1 = c(89, 103, 49, 75, 85, 102, 92, 69, 64, 72),
  Post2 = c(89, 102, 52, 78, 86, 102, 90, 70, 63, 71),
  DistractionLevel = "1"
)

# Distraction Level 2
level2 <- data.frame(
  Baseline = c(73.667, 62, 67, 83, 76.667, 107.667, 64, 107.333, 68.333, 109),
  ASMR1 = c(72, 64, 65, 88, 80, 104, 61, 102, 64, 104),
  ASMR2 = c(74, 64, 65, 81, 74, 102, 60, 100, 60, 100),
  Distraction = c(72, 67, 65, 96, 81, 117, 76, 118, 77, 118),
  Post1 = c(NA, 68, 65, 86, 83, 99, 55, 96, 59, 100),
  Post2 = c(73, NA, 68, NA, 80, 100, 55, 97, 60, 98),
  DistractionLevel = "2"
)

# Distraction Level 3
level3 <- data.frame(
  Baseline = c(79.333, 80.333, 88.333, 90.333, 72.667, 83.667, 78.333, 88.667, 73, 76.667),
  ASMR1 = c(NA, 80, 89, 88, 78, 75, 70, 86, 70, 74),
  ASMR2 = c(80, 80, 81, 85, 69, 73, 70, 84, 68, 71),
  Distraction = c(90, 79, 94, 96, 79, 99, 96, 112, 92, 99),
  Post1 = c(81, 79, 81, 91, 70, 73, 69, 85, 68, 73),
  Post2 = c(81, 80, 90, 91, 73, 74, 69, 86, 69, 74),
  DistractionLevel = "3"
)

# Combine all levels in one single variable
all_data <- rbind(level1, level2, level3)

# Reshape to long format using base R's reshape function
long_data <- reshape(all_data, 
                     varying = c("Baseline", "ASMR1", "ASMR2", "Distraction", "Post1", "Post2"),
                     v.names = "HR", 
                     timevar = "Phase", 
                     times = c("Baseline", "ASMR1", "ASMR2", "Distraction", "Post1", "Post2"),
                     direction = "long")

# Convert Distraction Level to factor
long_data$DistractionLevel <- factor(long_data$DistractionLevel)

# Set the order of phases and rename them
long_data$Phase <- factor(long_data$Phase, 
                          levels = c("Baseline", "ASMR1", "ASMR2", "Distraction", "Post1", "Post2"),
                          labels = c("Baseline (0s)", "ASMR (30s)", "ASMR (60s)", "Distraction (90s)", "ASMR Post (120s)", "ASMR Post (150s)"))

# Calculate the mean HR for each Phase and Distraction Level
avg_hr <- aggregate(HR ~ DistractionLevel + Phase, data = long_data, FUN = mean, na.rm = TRUE)

# Plot the Interaction Plot using ggplot
p <- ggplot(avg_hr, aes(x = Phase, y = HR, color = DistractionLevel, group = DistractionLevel)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  labs(
    title = "Interaction Plot: HR across Phases and Distraction Levels",
    x = "Phase", y = "Heart Rate (HR)", color = "Distraction Level"
  ) +
  theme_minimal(base_size = 14)

#print(p) # When uncommented will produce the interaction plot

# Q-Q plot for Systolic BP by Phase and Distraction Level
# When the code below is uncommented it will produce the QQ plots
#qqnorm(long_data$HR, main = "Q-Q Plot: Heart Rate")
#qqline(long_data$HR, col = "green")

# Generates the ANOVA tables
aov_HR <- aov(HR ~ DistractionLevel * Phase, data = long_data)
summary(aov_HR)

# Conducts the Shapiro Test
shapiro.test(residuals(aov_HR))

# Generates the Multiple Regression 
model <- lm(HR ~ Phase + DistractionLevel, data = long_data)
summary(model)

