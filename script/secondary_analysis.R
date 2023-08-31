#########################################
# MSc Main project examining trajectories of change for PTSD patients
# during trauma focused CBT 
#
# Author: 220225638
# Date: 2023-08-13
# Description: Secondary analysis script for logistic regression
#########################################

#########################################
# Setup and import data for analysis
#########################################

source(here::here("script", "config.R"))

summary(wide_session4_dataset)

#########################################

#Converting GAD7_RI_@S4 to a factor
wide_session4_dataset$`GAD7_RI_@S4` <- as.factor(wide_session4_dataset$`GAD7_RI_@S4`)

levels(wide_session4_dataset$`GAD7_RI_@S4`) #This originally returned NULL.
#Now does the correct dummy codes.

#Converting GAD7_RCSI_@Last_session to a factor
wide_session4_dataset$`GAD7_RCSI_@Last_session` <- as.factor(wide_session4_dataset$`GAD7_RCSI_@Last_session`)
levels(wide_session4_dataset$`GAD7_RCSI_@Last_session`)


#Fitting model without any predictors to cross check spss:
log_regression_step0 <- glm(`GAD7_RCSI_@Last_session` ~ 1, family = binomial("logit"), 
                            data = wide_session4_dataset)

summary(log_regression_step0) #This matches SPSS output.

# Fitting the model:
log_regression <- glm(`GAD7_RCSI_@Last_session` ~ `GAD7_@S1` + 
`GAD7_RI_@S4`, family = binomial("logit"), data = wide_session4_dataset)

summary(log_regression)#coefficents are logit/log odds 

#need to see what I still need to get from R e.g. hosmer and Lemeshow.. 

#Confidence intervals for logit estimates 
confint(log_regression)

#Confidence intervals for odds ratios 
exp(confint(log_regression)) # these match spss output. 

#Odds ratios / Exp(B)
exp(log_regression$coefficients) #matches right side table spss.

######################################################
# Calculating probabilities:
# Given log-odds coefficients
log_odds_intercept <- 1.5156
log_odds_GAD7_S1 <- -0.1286
log_odds_GAD7_RI_S4_1 <- 1.1486
log_odds_GAD7_RI_S4_2 <- -1.0633

# Convert log-odds to probabilities using the logistic function
prob_intercept <- 1 / (1 + exp(-log_odds_intercept))
prob_GAD7_S1 <- 1 / (1 + exp(-log_odds_GAD7_S1))
prob_GAD7_RI_S4_1 <- 1 / (1 + exp(-log_odds_GAD7_RI_S4_1))
prob_GAD7_RI_S4_2 <- 1 / (1 + exp(-log_odds_GAD7_RI_S4_2))

# Print the probabilities
print(paste("Probability for Intercept:", prob_intercept))
print(paste("Probability for GAD7_S1:", prob_GAD7_S1))
print(paste("Probability for GAD7_RI_S4_1:", prob_GAD7_RI_S4_1))
print(paste("Probability for GAD7_RI_S4_2:", prob_GAD7_RI_S4_2))




######################################################

#Hosmer and lemeshow test

hl1_data <- wide_session4_dataset %>% filter(`GAD7_RCSI_@Last_session` == 1 | `GAD7_RCSI_@Last_session` == 0)

hl1_data$`GAD7_RCSI_@Last_session` <- as.numeric(as.character(hl1_data$`GAD7_RCSI_@Last_session`))


hl <- hoslem.test(hl1_data$`GAD7_RCSI_@Last_session`, 
                  fitted(log_regression), g = 10)  
#is significant therefore model is bad. 

hl


# ^not working because of the lenghts. 
length(wide_session4_dataset$`GAD7_RCSI_@Last_session`)
length(fitted(log_regression))
length(log_regression$`GAD7_RCSI_@Last_session`)
length(hl1_data$`GAD7_RCSI_@Last_session`)


###############################################
# descriptive stats
#reliable change at session 4 
wide_session4_dataset %>% group_by(`GAD7_RI_@S4`) %>% summarise(per = 100*n()/nrow(wide_session4_dataset))


#RCSI at last for 4 set 
wide_session4_dataset %>% group_by(`GAD7_RCSI_@Last_session`) %>% summarise(per = 100*n()/nrow(wide_session4_dataset))

# reliable chnage at session 8 
wide_session8_dataset %>% group_by(`GAD7_RI_@S8`) %>% summarise(per = 100*n()/nrow(wide_session8_dataset))

#RCSI at last for 8 set 
wide_session8_dataset %>% group_by(`GAD7_RCSI_@Last_session`) %>% summarise(per = 100*n()/nrow(wide_session8_dataset))

###############################################
################################################ if anything goes wrong delete this:
#Descriptive statistics:
demo2 <- wide_session4_dataset

summary(demo2$Age)
sd(demo2$Age)
summary(demo2$`GAD7_@S1`)
summary(demo2$`GAD7_@Last_session`)
summary(demo2$`Total_sessions_@Step`)
sd(demo2$`Total_sessions_@Step`)
sd(demo2$`GAD7_@S1`)
sd(demo2$`GAD7_@Last_session`)
summary(demo2$`GAD7_RCSI_@Last_session`)

demo2 %>% group_by(Gender) %>% summarise(per = 100*n()/nrow(demo2))
demo2 %>% group_by(Ethnicity_binary) %>% summarise(per = 100*n()/nrow(demo2))
demo2 %>% group_by(Unemployed_first) %>% summarise(per = 100*n()/nrow(demo2))
demo2 %>% group_by(Meds_first) %>% summarise(per = 100*n()/nrow(demo2))
demo2 %>% group_by(LTC_binary) %>% summarise(per = 100*n()/nrow(demo2))
demo2 %>% group_by(Disability_binary) %>% summarise(per = 100*n()/nrow(demo2))
demo2 %>% group_by(`GAD7_RCSI_@Last_session`) %>% summarise(per = 100*n()/nrow(demo2))
# GAD7_RCSI_@Last_session 0 = no, 1 = yes, n.a. = were not in clinical range 

###############################################################################
# Plotting graph:
# y axis = mean predicted probability of recovery (RCSI)
# x axis = no reliable change, reliable improvement, reliable deterioration 

#bar_chart <- ggplot(data = wide_session4_dataset, aes(x = `GAD7_RI_@S4`, y = ``))

log_odds <- predict(log_regression, newdata = wide_session4_dataset)

probabilities <- (1 / (1 + exp(- log_odds)))

predicted_probabilities <- predict(log_regression, newdata = wide_session4_dataset, type = "response")

predict_data <- cbind(wide_session4_dataset, predicted_prob = predicted_probabilities)


ggplot(predict_data, aes(x = `GAD7_RI_@S4` , y = predicted_prob)) + 
  geom_bar(stat = "identity")



#print(predicted_probabilities)


invlogit <- function(x) {1/(1+exp(-x))}

invlogit(1.1486) #reliable improvement 

invlogit(-1.0633) # reliable deterioration 

invlogit(1.5156) # no reliable change 

#for reliable change:
#invlogit(1.5156 + 1.1486)





