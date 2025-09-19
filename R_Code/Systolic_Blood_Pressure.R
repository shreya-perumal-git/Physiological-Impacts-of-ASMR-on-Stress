library(ggplot2)

#Defining the Systolic Blood Pressure Data

# Distraction Level 1
Systolic_L1 <- data.frame(
  BP_Systolic_Baseline = c(70.5, 81.66666667, 78.33333333, 79.33333333, 82.33333333, 87.33333333, 87, 76.66666667, 74, 79.66666667),
  ASMR_BP_1_Systolic = c(78, 74, 78, 73, 77, 91, 81, 75, 70, 70),
  ASMR_BP_2_Systolic = c(70, 74, 74, 76, 81, 94, 85, 72, 74, 76),
  Distraction_BP_Systolic = c(70, 76, 75, 72, 80, 94, 93, 82, 81, 84),
  Post_BP_1_Systolic = c(68, 77, 75, 77, 77, 92, 85, 71, 74, 74),
  Post_BP_2_Systolic = c(70, 72, 76, 74, 78, 93, 89, 72, 69, 71),
  DistractionLevel = "1"
)

# Distraction Level 2
Systolic_L2 <- data.frame(
  BP_Systolic_Baseline = c(75, 89.33333333, 85.33333333, 73, 75, 87.66666667, 69.66666667, 87, 72.66666667, 89.66666667),
  ASMR_BP_1_Systolic = c(82, 86, 85, 73, 78, 87, 74, 88, 73, 91),
  ASMR_BP_2_Systolic = c(78, 89, 85, 75, 73, 85, 66, 85, 68, 93),
  Distraction_BP_Systolic = c(76, 86, 86, 74, 77, 89, 83, 92, 84, 94),
  Post_BP_1_Systolic = c(79, 85, 82, 76, 78, 80, 68, 84, 69, 90),
  Post_BP_2_Systolic = c(79, 85, 79, 76, 79, 92, 69, 86, 73, 87),
  DistractionLevel = "2"
)

# Distraction Level 3
Systolic_L3 <- data.frame(
  BP_Systolic_Baseline = c(77.66666667, 86, 82.66666667, 82.66666667, 76.66666667, 80.33333333, 75.66666667, 80.66666667, 73.66666667, 77.66666667),
  ASMR_BP_1_Systolic = c(71, 80, 81, 81, 75, 77, 72, 83, 77, 74),
  ASMR_BP_2_Systolic = c(80, 82, 80, 80, 71, 74, 77, 81, 67, 76),
  Distraction_BP_Systolic = c(76, 78, 83, 84, 74, 83, 87, 92, 82, 82),
  Post_BP_1_Systolic = c(72, 75, 79, 82, 73, 78, 70, 83, 70, 72),
  Post_BP_2_Systolic = c(72, 78, 77, 82, 75, 74, 71, 79, 70, 78),
  DistractionLevel = "3"
)

# Combines all data into one variable
systolic_data = rbind(Systolic_L1, Systolic_L2, Systolic_L3)

# Reshape systolic data
sys_long_data <- reshape(systolic_data, 
                         varying = c("BP_Systolic_Baseline", "ASMR_BP_1_Systolic", "ASMR_BP_2_Systolic", 
                                     "Distraction_BP_Systolic", "Post_BP_1_Systolic", "Post_BP_2_Systolic"),
                         v.names = "BP_Systolic", 
                         timevar = "Phase", 
                         times = c("BP_Systolic_Baseline", "ASMR_BP_1_Systolic", "ASMR_BP_2_Systolic", 
                                   "Distraction_BP_Systolic", "Post_BP_1_Systolic", "Post_BP_2_Systolic"),
                         direction = "long")


# Convert Distraction Level to a factor
sys_long_data$DistractionLevel <- factor(sys_long_data$DistractionLevel)

# Set the order of phases and rename them for clarity
sys_long_data$Phase <- factor(sys_long_data$Phase, 
                              levels = c("BP_Systolic_Baseline", "ASMR_BP_1_Systolic", "ASMR_BP_2_Systolic", 
                                         "Distraction_BP_Systolic", "Post_BP_1_Systolic", "Post_BP_2_Systolic"),
                              labels = c("Baseline (0s)", "ASMR (30s)", "ASMR (60s)", 
                                         "Distraction (90s)", "ASMR Post (120s)", "ASMR Post (150s)"))

# Calculate the mean Systolic BP for each Phase and Distraction Level
avg_systolic <- aggregate(BP_Systolic ~ DistractionLevel + Phase, data = sys_long_data, FUN = mean, na.rm = TRUE)

# Generate the Interaction plots using ggplot
p <- ggplot(avg_systolic, aes(x = Phase, y = BP_Systolic, color = DistractionLevel, group = DistractionLevel)) +
  geom_line(linewidth = 1.2) +  # Use linewidth instead of size, this was as a result of an error
  geom_point(size = 2) +
  labs(
    title = "Interaction Plot: Systolic BP across Phases and Distraction Levels",
    x = "Phase", y = "Systolic BP", color = "Distraction Level"
  ) +
  theme_minimal(base_size = 14)

# When uncommented will print the Interaction plot
#print(p)

# Q-Q plot for Systolic BP by Phase and Distraction Level
# When the code below is uncommmented it will produce the plot
#qqnorm(sys_long_data$BP_Systolic, main = "Q-Q Plot: Systolic BP")
#qqline(sys_long_data$BP_Systolic, col = "red")

# Generate the ANOVA tables
aov_sys <- aov(BP_Systolic ~ DistractionLevel * Phase, data = sys_long_data)
summary(aov_sys)

# Conduct a Shapiro Test
shapiro.test(residuals(aov_sys))

# Generates the Multiple Regression
model <- lm(BP_Systolic ~ Phase + DistractionLevel, data = sys_long_data)
summary(model)
