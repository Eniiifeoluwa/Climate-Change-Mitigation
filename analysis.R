# Load necessary packages
library(readxl)
library(dplyr)
library(agricolae)


duplicated(climate_data)

climate_data = read_excel("climate_data.xlsx")
factor_converter <- function(x) {
  if (!is.data.frame(x)) stop("Input must be a dataframe")
  
  for (column in seq_len(ncol(x))) {
    if (!is.factor(x[[column]]) & length(unique(x[[column]])) < 4) {
      x[[column]] <- as.factor(x[[column]])
    }
  }
  return(x)  
}
climate_data<- na.omit(climate_data)
write.csv(climate_data, "new_climate_data.csv")
climate_data<- factor_converter(climate_data)


# Ensure proper factor types
climate_data$Gender <- as.factor(climate_data$Gender)
climate_data$Education <- as.factor(climate_data$Education)
climate_data$`Law Compliance` <- as.factor(climate_data$`Law Compliance`)




# -------------------- 1. CHI-SQUARE TEST ---------------------
chi_result <- chisq.test(table(climate_data$Gender, climate_data$`Law Compliance`))
chi_df <- data.frame(
  Test = "Chi-Square Test",
  Statistic = chi_result$statistic,
  P_Value = chi_result$p.value,
  Decision = ifelse(chi_result$p.value < 0.05, "Reject Null", "Do Not Reject Null")
)



# -------------------- 2. WILCOXON TEST ----------------------
# Convert to ordered factor and numeric
climate_data$`Initiatives Participation Level` <- factor(climate_data$`Initiatives Participation Level`,
                                                         levels = c("Minimal Involvement", "Occasional Involvement", "Active Involvement"),
                                                         ordered = TRUE)

wilcox_result <- wilcox.test(as.numeric(climate_data$`Initiatives Participation Level`) ~ climate_data$Gender)
wilcox_df <- data.frame(
  Test = "Wilcoxon Test",
  Statistic = wilcox_result$statistic,
  P_Value = wilcox_result$p.value,
  Decision = ifelse(wilcox_result$p.value < 0.05, "Reject Null", "Do Not Reject Null")
)




# -------------------- 3. T-TEST ----------------------------
# Convert Policies Effectiveness to ordered numeric
climate_data$`Policies Effectiveness` <- as.numeric(factor(climate_data$`Policies Effectiveness`,
                                                           levels = c("Very ineffective","Ineffective", "Neutral", "Effective", "Very effective")))

# Subset data where Gender has exactly 2 levels
gender_levels <- levels(climate_data$Gender)
if (length(gender_levels) == 2) {
  ttest_result <- t.test(`Policies Effectiveness` ~ Gender, data = climate_data)
  ttest_df <- data.frame(
    Test = "T-Test",
    Statistic = ttest_result$statistic,
    P_Value = ttest_result$p.value,
    Decision = ifelse(ttest_result$p.value < 0.05, "Reject Null", "Do Not Reject Null")
  )
} else {
  ttest_df <- data.frame(
    Test = "T-Test",
    Statistic = NA,
    P_Value = NA,
    Decision = "Not enough groups for comparison"
  )
}




# -------------------- 4. DUNCAN MULTIPLE RANGE TEST -------------------------
# Clean Education levels with fewer than 2 observations
edu_data <- na.omit(climate_data[, c("Policies Effectiveness", "Education")])
edu_data <- edu_data %>% group_by(Education) %>% filter(n() >= 2) %>% ungroup()
edu_data$Education <- factor(edu_data$Education)

# Run ANOVA and Duncan
anova_model <- aov(`Policies Effectiveness` ~ Education, data = edu_data)
duncan_result <- duncan.test(anova_model, "Education")





# Convert result to dataframe
duncan_df <- duncan_result$groups
duncan_df$Group <- rownames(duncan_df)
duncan_df$Test <- "Duncan Multiple Range"
duncan_df <- duncan_df[, c("Test", "Group", "Policies Effectiveness", "groups")]
colnames(duncan_df) <- c("Test", "Group", "Mean", "Group_Letter")

mannwhitney_result <- wilcox.test(as.numeric(climate_data$`Initiatives Participation Level`) ~ climate_data$Gender)
mannwhitney_df <- data.frame(
  Test = "Mann-Whitney U Test",
  Statistic = mannwhitney_result$statistic,
  P_Value = mannwhitney_result$p.value,
  Decision = ifelse(mannwhitney_result$p.value < 0.05, "Reject Null", "Do Not Reject Null")
)

# -------------------- COMBINE ALL --------------------------
# Combine test summaries
test_results <- bind_rows(chi_df, wilcox_df, ttest_df, mannwhitney_df)

# Print each
print("=== Summary of Tests ===")
print(test_results)

print("=== Duncan Group Comparison ===")
print(duncan_df)

View(test_results)
View(duncan_df)

View(test_results)

wilcox_result
