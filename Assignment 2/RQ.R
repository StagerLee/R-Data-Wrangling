# Load packages
library(dplyr)
library(ggplot2)

# Read in data
responders2009 <- read_csv("Responders2009.csv")
debate2009 <- read_csv("Debate2009.csv")
contentanalysis2009 <- read_csv("Content analysis debate 2009.csv")

# Join dataframe on 'time' variable
data_merged <- left_join(responders2009, debate2009, by = "ID") %>% left_join(contentanalysis2009, by = "ID")

# Create a variable indicating the type of statement made by the candidate
data_merged$statement_type <- ifelse(data_merged$acclaims == 1, "Acclaims", 
                                     ifelse(data_merged$attacks == 1, "Attacks", 
                                            ifelse(data_merged$defenses == 1, "Defenses", "Self-criticisms")))

# Create visualization to explore relationship between type of statement and voting intentions
ggplot(data_merged, aes(x = statement_type, y = votingintention, fill = statement_type)) +
  geom_boxplot() +
  ggtitle("Effect of statement type on voting intentions") +
  xlab("Statement type") +
  ylab("Voting intentions")

# Perform chi-squared test to test for a significant relationship between type of statement and voting intentions
chisq.test(table(data_merged$statement_type, data_merged$votingintention))

# Perform ANOVA to test for a significant relationship between type of statement and voting intentions
aov_test <- aov(votingintention ~ statement_type, data = data_merged)
summary(aov_test)