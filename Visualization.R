library(readxl)
library(dplyr)
library(ggplot2)
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
climate_data<- factor_converter(climate_data)


climate_data$`Age Group`
count(climate_data, Gender)

ggplot(data = climate_data)+
  geom_bar(mapping = aes( x = Gender, fill = `Age Group`),
           position = 'dodge', )+
  #geom_text(mapping = (aes(x = Gender, y= `Age Group`, label = `Age Group` )))+
  labs(title = "Gender Vs Age Group")+
  theme(plot.title = element_text(hjust = 0.5))
  theme_classic()


ggplot(data = climate_data)+
  geom_bar(mapping = aes( x = Occupation, fill = Occupation))+
  labs(title = "COUNTS OF OCCUPATION")+
  #theme_dark()+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5, face = 'bold'))



ggplot(climate_data, aes(x = Gender, fill = `Law Compliance`)) +
  geom_bar(position = 'dodge') +
  labs(title = "Gender vs Law Compliance Behavior",
       x = "Gender",
       y = "Proportion",
       fill = "Law Compliance") +
  theme_minimal()

# 2. Wilcoxon Rank-Sum Test: Participation Level by Gender (Box Plot)
ggplot(climate_data, aes(x = Gender, fill = `Initiatives Participation Level`)) +
  geom_bar(position = 'dodge') +
  labs(title = "Participation Level by Gender",
       x = "Gender",
       y = "Participation Level") +
  theme_classic()

# 3. Welch Two-Sample t-Test: Policy Effectiveness by Gender (Bar Chart)
ggplot(climate_data, aes(x = Gender, fill = `Policies Effectiveness`)) +
  geom_bar(position = 'dodge')+
  #stat_summary(fun = "mean", geom = "bar",position = "dodge" ) +
  labs(title = "Policy Effectiveness by Gender",
       x = "Gender",
       y = "Policy Effectiveness Rating") +
  theme_minimal()

# 4. Duncan's Multiple Range Test: Policy Effectiveness by Education (Box Plot or Point Plot)
ggplot(climate_data, aes(x = Education, y = `Policies Effectiveness`, color = Education)) +
  geom_boxplot() +
  labs(title = "Policy Effectiveness by Education Level",
       x = "Education Level",
       y = "Policy Effectiveness") +
  theme_minimal()

# 5. Wilcoxon Signed-Rank Test: Awareness vs Compliance (Paired Bar Chart)
ggplot(climate_data, aes( x = factor(1), y = `Existing Law Awareness`, fill = `Existing Law Awareness`)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_bar(aes(y =`Law Compliance`, fill = `Law Compliance`), stat = "identity", position = "dodge") +
  labs(title = "Awareness vs Compliance Behavior",
       x = "Group",
       y = "Scores",
       fill = "Law Compliance") +
  scale_x_discrete(labels = c("Awareness vs Compliance")) +

  theme_minimal()

# 6. Kruskal-Wallis Test: Participation Level by Age Group (Box Plot)
ggplot(climate_data, aes(y = `Initiatives Participation Level`, fill = `Age Group`, )) +
  geom_bar(position = 'dodge') +
  labs(title = "Participation Level by Age Group",
       x = "Age Group",
       y = "Participation Level") +
  theme_minimal()
colnames(climate_data)
