# Load necessary libraries
library(readxl)
library(dplyr)
library(agricolae)

# Load the dataset
climate_data <- read_excel("climate_data.xlsx")

# Convert relevaread.csv()# Convert relevant columns to factor/numeric
climate_data$Gender <- as.factor(climate_data$Gender)
climate_data$Education <- as.factor(climate_data$Education)
climate_data$`Climate Change Mitigation Policy` <- as.factor(climate_data$`Climate Change Mitigation Policy`)
climate_data$`Policies Effectiveness` <- as.numeric(factor(
  climate_data$`Policies Effectiveness`,
  levels = c("Very ineffective", "Ineffective", "Neutral", "Effective", "Very effective"),
  ordered = TRUE
))
climate_data$`Initiatives Participation Level` <- factor(
  climate_data$`Initiatives Participation Level`,
  levels = c("Minimal Involvement", "Occasional Involvement", "Active Involvement"),
  ordered = TRUE
)

# -------------------- 1. MANN-WHITNEY U TEST -----------------------
mannwhitney_result <- wilcox.test(as.numeric(climate_data$`Initiatives Participation Level`) ~ climate_data$Gender)
mannwhitney_df <- data.frame(
  Test = "Mann-Whitney U Test",
  Statistic = mannwhitney_result$statistic,
  P_Value = mannwhitney_result$p.value,
  Decision = ifelse(mannwhitney_result$p.value < 0.05, "Reject Null", "Do Not Reject Null")
)

# -------------------- 2. WILCOXON SIGNED-RANK TEST ------------------
# Example: Paired test between Policy Awareness and Law Compliance (both yes/no responses encoded as 1/0)
# You can replace with two suitable ordinal/numeric variables
climate_data$Awareness <- ifelse(climate_data$`Government Program Awareness` == "Yes", 1, 0)
climate_data$Compliance <- ifelse(climate_data$`Law Compliance` == "Yes", 1, 0)

wilcox_signed_result <- wilcox.test(climate_data$Awareness, climate_data$Compliance, paired = TRUE)
wilcox_signed_df <- data.frame(
  Test = "Wilcoxon Signed-Rank Test",
  Statistic = wilcox_signed_result$statistic,
  P_Value = wilcox_signed_result$p.value,
  Decision = ifelse(wilcox_signed_result$p.value < 0.05, "Reject Null", "Do Not Reject Null")
)

# -------------------- 3. T-TEST -------------------------------------
# Check gender levels
gender_levels <- levels(climate_data$Gender)
if (length(gender_levels) == 2) {
  ttest_result <- t.test(`Policies Effectiveness` ~ Gender, data = climate_data)
  ttest_df <- data.frame(
    Test = "T-Test (Policies Effectiveness by Gender)",
    Statistic = ttest_result$statistic,
    P_Value = ttest_result$p.value,
    Decision = ifelse(ttest_result$p.value < 0.05, "Reject Null", "Do Not Reject Null")
  )
} else {
  ttest_df <- data.frame(
    Test = "T-Test",
    Statistic = NA,
    P_Value = NA,
    Decision = "Not enough groups"
  )
}

# -------------------- 4. DUNCAN MULTIPLE RANGE TEST -----------------
# Remove groups in Education with <2 observations
edu_data <- climate_data %>% 
  filter(!is.na(`Policies Effectiveness`)) %>%
  group_by(Education) %>%
  filter(n() >= 2) %>%
  ungroup()
edu_data$Education <- factor(edu_data$Education)

# Run ANOVA and Duncan test
anova_model <- aov(`Policies Effectiveness` ~ Education, data = edu_data)
duncan_result <- duncan.test(anova_model, "Education")

# Format Duncan results
duncan_df <- duncan_result$groups
duncan_df$Group <- rownames(duncan_df)
duncan_df$Test <- "Duncan Multiple Range"
duncan_df <- duncan_df[, c("Test", "Group", "Policies Effectiveness", "groups")]
colnames(duncan_df) <- c("Test", "Group", "Mean", "Group_Letter")

# -------------------- 5. SUMMARY OUTPUT ----------------------------
# Combine the test summaries
summary_tests <- bind_rows(
  mannwhitney_df,
  wilcox_signed_df,
  ttest_df, kruskal_df
)
View(summary_tests)
# View results
print("===== Summary of Statistical Tests =====")
print(summary_tests)

print("===== Duncan Group Differences (Education vs Policy Effectiveness) =====")
print(duncan_df)
View(summary_tests)


wilcox_signed_result


# Convert variables to appropriate types
climate_data$`Age Group` <- as.factor(climate_data$`Age Group`)
climate_data$`Initiatives Participation Level` <- factor(
  climate_data$`Initiatives Participation Level`,
  levels = c("Minimal Involvement", "Occasional Involvement", "Active Involvement"),
  ordered = TRUE
)

# Perform Kruskal-Wallis Test
kruskal_result <- kruskal.test(as.numeric(climate_data$`Initiatives Participation Level`) ~ climate_data$`Age Group`)

# Create a summary dataframe
kruskal_df <- data.frame(
  Test = "Kruskal-Wallis Test",
  Statistic = kruskal_result$statistic,
  P_Value = kruskal_result$p.value,
  Decision = ifelse(kruskal_result$p.value < 0.05, "Reject Null", "Do Not Reject Null")
)

# View result
print(kruskal_df)
