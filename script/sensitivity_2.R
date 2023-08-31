#########################################
# MSc Main project examining trajectories of change for PTSD patients
# during trauma focused CBT 
#
# Author: 220225638
# Date: 2023-08-13
# Description: Data analysis file growth modelling <=20cases
#########################################
#########################################
# Setup and import data for analysis
#########################################

source(here::here("script", "config.R"))

wide_session8_dataset <- wide_dataset %>% filter(`Total_sessions_@Step` >8 & `Session_number_@Step` == 8)

wide_session8_dataset <- wide_session8_dataset %>% rename("GAD7_@S8" = "GAD7", "PHQ9_Chg_@S8" = "PHQ9_Chg", "GAD7_Chg_@S8" = "GAD7_Chg",
                                                          "GAD7_RI_@S8" = "GAD7_RI_session", "PHQ9_@S8" = "PHQ9")

#########################################
wide_session8_dataset$`GAD7_RI_@S8` <- as.factor(wide_session8_dataset$`GAD7_RI_@S8`)


log_regression8 <- glm(`GAD7_RCSI_@Last_session` ~ `GAD7_@S1` + 
                        `GAD7_RI_@S8`, family = binomial("logit"), data = wide_session8_dataset)

summary(log_regression8)

#########################################
#Confidence intervals for odds ratios 
exp(confint(log_regression8)) # these match spss output. 

#Odds ratios / Exp(B)
exp(log_regression8$coefficients) #matches right side table spss.


########################################
# Calculating probabilities, will do on secondary after too:
# Given log-odds coefficients
log_odds_intercept_8 <- 1.44031
log_odds_GAD7_S1_8 <- -0.13489
log_odds_GAD7_RI_S8_1_8 <- 1.56573
log_odds_GAD7_RI_S8_2_8 <- -1.27610

# Convert log-odds to probabilities using the logistic function
prob_intercept_8 <- 1 / (1 + exp(-log_odds_intercept_8))
prob_GAD7_S1_8 <- 1 / (1 + exp(-log_odds_GAD7_S1_8))
prob_GAD7_RI_S8_1_8 <- 1 / (1 + exp(-log_odds_GAD7_RI_S8_1_8))
prob_GAD7_RI_S8_2_8 <- 1 / (1 + exp(-log_odds_GAD7_RI_S8_2_8))

# Print the probabilities
print(paste("Probability for Intercept_8:", prob_intercept_8))
print(paste("Probability for GAD7_S1_8:", prob_GAD7_S1_8))
print(paste("Probability for GAD7_RI_S8_1_8:", prob_GAD7_RI_S8_1_8))
print(paste("Probability for GAD7_RI_S8_2_8:", prob_GAD7_RI_S8_2_8))


#################################################################
# Plotting the graph:
#Create a data frame
df <- data.frame(
  Variable = c("Intercept_8","GAD7_RI_S8_1_8", "GAD7_RI_S8_2_8"),
  Probability = c(prob_intercept_8, prob_GAD7_RI_S8_1_8, prob_GAD7_RI_S8_2_8))


# Create the bar graph using ggplot2
ggplot(df, aes(x = Variable, y = Probability)) +
  geom_bar(stat = "identity") +
  ggtitle("Impact of Independent Variables on Dependent Variable") +
  xlab("Independent Variables") +
  ylab("Probability") +
  theme_minimal()



#################################################################
#Descriptive statistics:
demo3 <- wide_session8_dataset

summary(demo3$Age)
sd(demo3$Age)
summary(demo3$`GAD7_@S1`)
summary(demo3$`GAD7_@Last_session`)
summary(demo3$`Total_sessions_@Step`)
sd(demo3$`Total_sessions_@Step`)
sd(demo3$`GAD7_@S1`)
sd(demo3$`GAD7_@Last_session`)
summary(demo3$`GAD7_RCSI_@Last_session`)

demo3 %>% group_by(Gender) %>% summarise(per = 100*n()/nrow(demo3))
demo3 %>% group_by(Ethnicity_binary) %>% summarise(per = 100*n()/nrow(demo3))
demo3 %>% group_by(Unemployed_first) %>% summarise(per = 100*n()/nrow(demo3))
demo3 %>% group_by(Meds_first) %>% summarise(per = 100*n()/nrow(demo3))
demo3 %>% group_by(LTC_binary) %>% summarise(per = 100*n()/nrow(demo3))
demo3 %>% group_by(Disability_binary) %>% summarise(per = 100*n()/nrow(demo3))
demo3 %>% group_by(`GAD7_RCSI_@Last_session`) %>% summarise(per = 100*n()/nrow(demo3))
# GAD7_RCSI_@Last_session 0 = no, 1 = yes, n.a. = were not in clinical range 



