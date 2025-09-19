library(ggplot2)

# Define the Diastolic data

# Distration Level 1
Diastolic_L1 <- data.frame(
  BP_Diastolic_Baseline = c(100.5, 131, 107.6666667, 116, 126, 138, 135, 121.6666667, 116, 122),
  ASMR_BP_1_Diastolic = c(97, 108, 105, 114, 118, 145, 131, 113, 114, 113),
  ASMR_BP_2_Diastolic = c(99, 110, 102, 113, 117, 141, 136, 113, 116, 114),
  Distraction_BP_Diastolic = c(100, 128, 99, 113, 118, 141, 142, 126, 125, 129),
  Post_BP_1_Diastolic = c(102, 114, 99, 114, 122, 140, 139, 113, 102, 115),
  Post_BP_2_Diastolic = c(106, 113, 99, 117, 119, 140, 139, 119, 102, 113),
  DistractionLevel = "1"
)

# Distraction Level 2
Diastolic_L2 <- data.frame(
  BP_Diastolic_Baseline = c(115.6666667, 136.6666667, 130, 106.3333333, 116.6666667, 140.3333333, 108.6666667, 143, 111, 144),
  ASMR_BP_1_Diastolic = c(114, 129, 130, 99, 121, 141, 109, 142, 102, 142),
  ASMR_BP_2_Diastolic = c(115, 133, 127, 102, 112, 140, 109, 141, 106, 141),
  Distraction_BP_Diastolic = c(107, 131, 126, 103, 121, 142, 122, 146, 122, 149),
  Post_BP_1_Diastolic = c(108, 131, 123, 104, 120, 132, 106, 136, 104, 140),
  Post_BP_2_Diastolic = c(108, 131, 124, 104, 122, 140, 109, 130, 102, 136),
  DistractionLevel = "2"
)

# DIstraction Level 3
Diastolic_L3 <- data.frame(
  BP_Diastolic_Baseline = c(107, 129, 129.3333333, 130.3333333, 115.3333333, 118, 118.6666667, 122.3333333, 111.6666667, 115.3333333),
  ASMR_BP_1_Diastolic = c(106, 119, 128, 122, 112, 115, 112, 123, 114, 113),
  ASMR_BP_2_Diastolic = c(115, 128, 119, 117, 112, 118, 112, 117, 113, 116),
  Distraction_BP_Diastolic = c(107, 131, 131, 129, 113, 128, 131, 142, 127, 133),
  Post_BP_1_Diastolic = c(107, 137, 111, 127, 117, 114, 108, 119, 111, 117),
  Post_BP_2_Diastolic = c(107, 132, 123, 128, 112, 117, 111, 117, 111, 113),
  DistractionLevel = "3"
)

# Combine all diastolic data
diastolic_data = rbind(Diastolic_L1, Diastolic_L2, Diastolic_L3)

# Reshape the data
sys_long_data <- reshape(diastolic_data, 
                         varying = c("BP_Diastolic_Baseline", "ASMR_BP_1_Diastolic", "ASMR_BP_2_Diastolic", 
                                     "Distraction_BP_Diastolic", "Post_BP_1_Diastolic", "Post_BP_2_Diastolic"),
                         v.names = "BP_Diastolic", 
                         timevar = "Phase", 
                         times = c("BP_Diastolic_Baseline", "ASMR_BP_1_Diastolic", "ASMR_BP_2_Diastolic", 
                                   "Distraction_BP_Diastolic", "Post_BP_1_Diastolic", "Post_BP_2_Diastolic"),
                         direction = "long")


# Convert Distraction Level to a factor
sys_long_data$DistractionLevel <- factor(sys_long_data$DistractionLevel)

# Set the order of phases and rename them for clarity
sys_long_data$Phase <- factor(sys_long_data$Phase, 
                              levels = c("BP_Diastolic_Baseline", "ASMR_BP_1_Diastolic", "ASMR_BP_2_Diastolic", 
                                         "Distraction_BP_Diastolic", "Post_BP_1_Diastolic", "Post_BP_2_Diastolic"),
                              labels = c("Baseline (0s)", "ASMR (30s)", "ASMR (60s)", 
                                         "Distraction (90s)", "ASMR Post (120s)", "ASMR Post (150s)"))


# Calculate the mean Diastolic BP for each Phase and Distraction Level
avg_bp <- aggregate(BP_Diastolic ~ DistractionLevel + Phase, data = sys_long_data, FUN = mean, na.rm = TRUE)

# Generate Interaction Plots using ggplot
p <- ggplot(avg_bp, aes(x = Phase, y = BP_Diastolic, color = DistractionLevel, group = DistractionLevel)) +
  geom_line(linewidth = 1.2) +  # Use linewidth instead of size, as a result of error
  geom_point(size = 2) +
  labs(
    title = "Interaction Plot: Diastolic BP across Phases and Distraction Levels",
    x = "Phase", y = "Diastolic BP", color = "Distraction Level"
  ) +
  theme_minimal(base_size = 14)

#print(p) # When uncommented will produce the interaction plot

# Q-Q plot for Diastolic BP by Phase and Distraction Level
# When the lines below are uncommented it will produce QQ plots
#qqnorm(sys_long_data$BP_Diastolic, main = "Q-Q Plot: Diastolic BP")
#qqline(sys_long_data$BP_Diastolic, col = "blue")

# Produce the ANOVA tables
aov_dia <- aov(BP_Diastolic ~ DistractionLevel * Phase, data = sys_long_data)
summary(aov_dia)

# Conduct the Shapiro Test
shapiro.test(residuals(aov_dia))

# Generate the multiple regression
model <- lm(BP_Diastolic ~ Phase + DistractionLevel, data = sys_long_data)
summary(model)

