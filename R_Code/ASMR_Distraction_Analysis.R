
library(tidyverse)

data <- read_csv("Fixified_All_Levels.csv")

asmr_data <- data %>%
  filter(Time_Seconds %in% c(90, 120, 150)) %>%
  select(Participant, Time_Seconds, HR, BP_Systolic, BP_Diastolic) %>%
  pivot_wider(names_from = Time_Seconds, values_from = c(HR, BP_Systolic, BP_Diastolic)) %>%
  mutate(
    HR_Rate_90_120 = (HR_120 - HR_90) / 30,
    HR_Rate_120_150 = (HR_150 - HR_120) / 30,
    SBP_Rate_90_120 = (BP_Systolic_120 - BP_Systolic_90) / 30,
    SBP_Rate_120_150 = (BP_Systolic_150 - BP_Systolic_120) / 30,
    DBP_Rate_90_120 = (BP_Diastolic_120 - BP_Diastolic_90) / 30,
    DBP_Rate_120_150 = (BP_Diastolic_150 - BP_Diastolic_120) / 30
  )

plot_asmr_rate <- function(df, var1, var2, label, file_name) {
  df_clean <- df %>% filter(!is.na(.data[[var1]]) & !is.na(.data[[var2]]))
  
  combined_vals <- c(df_clean[[var1]], df_clean[[var2]])
  interval <- factor(rep(c("90-120", "120-150"), each = nrow(df_clean)))
  test_data <- data.frame(Rate = combined_vals, Interval = interval)
  
  t_result <- t.test(df_clean[[var1]], df_clean[[var2]])
  aov_result <- summary(aov(Rate ~ Interval, data = test_data))[[1]]["Pr(>F)"][[1]][1]
  
  ggplot(test_data, aes(x = Rate, fill = Interval)) +
    geom_histogram(alpha = 0.6, bins = 10, position = "identity") +
    labs(
      title = paste0(label, " - ASMR Rate of Change\n",
                     "T-test p: ", round(t_result$p.value, 4), 
                     " , ANOVA p: ", round(aov_result, 4)),
      x = "Rate of Change per Second", y = "Frequency", fill = "Interval"
    ) +
    theme_minimal()
  
  ggsave(file_name, width = 10, height = 6)
}

plot_asmr_rate(asmr_data, "HR_Rate_90_120", "HR_Rate_120_150", "Heart Rate", "Figure_12_HR_Delta.png")
plot_asmr_rate(asmr_data, "SBP_Rate_90_120", "SBP_Rate_120_150", "Systolic BP", "Figure_13_SBP_Delta.png")
plot_asmr_rate(asmr_data, "DBP_Rate_90_120", "DBP_Rate_120_150", "Diastolic BP", "Figure_14_DBP_Delta.png")
