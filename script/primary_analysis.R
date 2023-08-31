#########################################
# MSc Main project examining trajectories of change for PTSD patients
# during trauma focused CBT 
#
# Author: 220225638
# Date: 2023-08-13
# Description: Data analysis file for growth curve modelling and regression analysis
#########################################

#########################################
# Setup and import data for analysis
#########################################

source(here::here("script", "config.R"))

processed_data = read_sav(processed_data_path)

#########################################
# Sorting cases by CaseID into ascending order 
processed_data <- processed_data %>% arrange(CaseID)


# Printing the variance covariance matrix of the random effects:
#VarCorr(model)


# Gave me 1 more decimal point for AIC, BIC ... 
#lme4:::.prt.aictab(summary(model2)$AICtab, digits = 10)

# Getting numbers not e-04 etc.
options(scipen = 999)
#options(scipen = 0) #to set back to default.

#Trying to get significance values:
#library(lmerTest) #This didnt do anything (but said: masked lmer..? )
#coef(summary(as(Cubic,"merModLmerTest"))) 

##########################################################################
# Fitting models (default unstructured covariance structure)

Linear <- lmer(GAD7 ~ TIME + (1 + TIME | CaseID), data = processed_data, REML = FALSE) 
summary(Linear)

Log_linear <-  lmer(GAD7 ~ TIME_log +(1 + TIME_log | CaseID), data = processed_data, REML = FALSE)
summary(Log_linear)

Quadratic <- lmer(GAD7 ~ TIME + I(TIME^2) + (1 + TIME | CaseID), data = processed_data, REML = FALSE)
summary(Quadratic)

Cubic <- lmer(GAD7 ~ TIME + I(TIME^2) + I(TIME^3) + (1 + TIME | CaseID), data = processed_data, REML = FALSE)
summary(Cubic)


#######################################################
# Comparing models 

anova(Linear, Log_linear)

anova(Linear, Quadratic)

anova(Quadratic, Cubic)

anova(Linear, Cubic)

#anova(Log_linear, Quadratic)

#####################################################

logl1 <- logLik(Linear) #-2log likelihood linear
print(-2*logl1)

logl2 <- logLik(Log_linear) #-2log likelihood log_linear
print(-2*logl2)

logl3 <- logLik(Quadratic) #-2log likelihood quadratic
print(-2*logl3)

logl4 <- logLik(Cubic) #-2log likelihood cubic 
print(-2*logl4)

######################################################
# Confidence intervals and r2

summary(Cubic)
fixef(Cubic)
confint(Cubic, method="Wald") #this also macthes CI for spss but is +/- 1.96SE confidence intervals.
confint(Cubic) #this works for the confidence intervals and matches SPSS.

# For R2 
r.squaredGLMM(Cubic) # is not giving the same as SPSS 

r2_results <- performance::r2(Cubic)
print(r2_results)

r2(Cubic)

#############################################
# Likelihood ratio test:
lrtest(Linear, Cubic)

#############################################
# calculating ICC
performance::icc(Cubic)

###########################################################################
# Plotting the graph with a cubic line:
#This did not work:
plot <- ggplot(processed_data, aes(x = TIME, y = GAD7)) + 
 # geom_point() +
  labs(x = "Session number", y = "Symptom severity GAD-7")


plot +                               # Add polynomial regression curve
  stat_smooth(method = "lm",
              formula = y ~ poly(x, 3),  #this was 4, looked weird and over fit. 
              se = TRUE) +
  ylim(0, 21) +
  theme(panel.background = element_blank(), 
       panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line = element_line(color = "black")) +
  labs(title = "Average trajectory of symptom change across all patients")

#Saving into figs 
ggsave(filename = primary_graph_path)
###############################################
# Adapted from above:

plot <- ggplot(processed_data, aes(x = TIME, y = GAD7)) +  
  # geom_point() +  # Comment out or delete this line to remove the dots
  labs(x = "Time", y = "GAD7") +  
  theme_minimal()

plot +  
  stat_smooth(method = "lm",  
              formula = y ~ poly(x, 3),  
              se = TRUE)  # Set to TRUE to add an error band

##############################################
# Trying something new: taking into accound ranom effect too
# Note on phone for explanation:

#fit <- lmer(GAD7 ~ poly(TIME, 3) + (1 | CaseID), data = processed_data)

#processed_data$predicted_values <- predict(fit, re.form = NA) # this change my data!!!! added predicted values column to processed data!

#ggplot(processed_data, aes(x = TIME, y = predicted_values)) +
  #geom_line() +  # Use geom_line for continuous line
  #labs(x = "Time", y = "Predicted GAD7") +
  #theme_minimal()

##############################################
# trying to plot different lines for each person 

# stupid idea crashes. 



###############################################
# This does work: (need to take points off, change labels etc.)
ggplot(na.omit(processed_data), aes(x = TIME, y = GAD7)) + 
  #geom_point(aes(color = "Data Points")) + 
  geom_smooth(method = "lm", formula = y ~ poly(x, 3), aes(color = "Cubic Fit"), se = TRUE) + 
  labs(title = "Mixed Effect Model with Cubic Term",
       x = "TIME", 
       y = "GAD7",
       color = "Legend") +
  theme_minimal() +
  ylim(min(processed_data$GAD7), 20) #to keep the y axis same after removing points. 

# Adding ylim gives warning: Removed 1925 rows containing non-finite values (`stat_smooth()`). 
#se = TRUE gives confidence intervals but not sure its accurate?
# removing geom_point(aes(color = "Data Points")) +  gets rid of the points. 
# adding na.omit gives: Warning message: Removed 1815 rows containing non-finite values (`stat_smooth()`). 

###############################################
# Trying out stadardising time.
#processed_data$TIME_standardized <- scale(processed_data$TIME) # this changed my data!!!! added time standardized.

#Cubic_standardized <- lmer(GAD7 ~ TIME_standardized + I(TIME_standardized^2) + I(TIME_standardized^3) + 
                             #(1 + TIME_standardized | CaseID), data = processed_data, REML = FALSE)

#summary(Cubic_standardized)


################################################
#Descriptive statistics:
demo <- wide_dataset %>% filter(`First_session_@Step`== 1 ) #this has the first row for each person

summary(demo)
sd(demo$Age)
summary(demo$`GAD7_@S1`)
summary(demo$`GAD7_@Last_session`)
summary(demo$`Total_sessions_@Step`)
sd(demo$`Total_sessions_@Step`)
sd(demo$`GAD7_@S1`)
sd(demo$`GAD7_@Last_session`)
summary(demo$`GAD7_RCSI_@Last_session`)

demo %>% group_by(Gender) %>% summarise(per = 100*n()/nrow(demo))
demo %>% group_by(Ethnicity_binary) %>% summarise(per = 100*n()/nrow(demo))
demo %>% group_by(Unemployed_first) %>% summarise(per = 100*n()/nrow(demo))
demo %>% group_by(Meds_first) %>% summarise(per = 100*n()/nrow(demo))
demo %>% group_by(LTC_binary) %>% summarise(per = 100*n()/nrow(demo))
demo %>% group_by(Disability_binary) %>% summarise(per = 100*n()/nrow(demo))
demo %>% group_by(`GAD7_RCSI_@Last_session`) %>% summarise(per = 100*n()/nrow(demo))
# GAD7_RCSI_@Last_session 0 = no, 1 = yes, n.a. = were not in clinical range 

###############################################

#labelled::val_labels(Gender)

str(demo$Gender)

min(processed_data$Age)
#mean age = 39.36
# sd age = 12.872
# min age = 16, max = 89
# Number of sessions mean sd min and max 


