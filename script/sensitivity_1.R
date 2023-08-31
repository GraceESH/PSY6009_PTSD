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

sens20 <- processed_data %>% filter(`Session_number_@Step` <= 20)

length(unique(sens20$CaseID))

#########################################

# Fitting models (default unstructured covariance structure)

Linear_20 <- lmer(GAD7 ~ TIME + (1 + TIME | CaseID), data = sens20, REML = FALSE) 
summary(Linear_20)

Log_linear_20 <-  lmer(GAD7 ~ TIME_log +(1 + TIME_log | CaseID), data = sens20, REML = FALSE)
summary(Log_linear_20)

Quadratic_20 <- lmer(GAD7 ~ TIME + I(TIME^2) + (1 + TIME | CaseID), data = sens20, REML = FALSE)
summary(Quadratic_20)

Cubic_20 <- lmer(GAD7 ~ TIME + I(TIME^2) + I(TIME^3) + (1 + TIME | CaseID), data = sens20, REML = FALSE)
summary(Cubic_20)

# Need to continue.. 

anova(Linear_20, Log_linear_20)
anova(Linear_20, Quadratic_20)
anova(Linear_20, Cubic_20)

anova(Quadratic_20, Cubic_20)
##########################################
confint(Linear_20)
confint(Quadratic_20)
########################################
senslogl1 <- logLik(Linear_20) #-2log likelihood linear
print(-2*senslogl1)

senslogl2 <- logLik(Log_linear_20) #-2log likelihood log_linear
print(-2*senslogl2)

senslogl3 <- logLik(Quadratic_20) #-2log likelihood quadratic
print(-2*senslogl3)

senslogl4 <- logLik(Cubic_20) #-2log likelihood cubic 
print(-2*senslogl4)

# if issues with script delete all these^ 

##########################################
# calculating ICC
performance::icc(Linear_20)

#########################################
# Plotting linear graph:
plot <- ggplot(sens20, aes(x = TIME, y = GAD7)) + 
  # geom_point() +
  labs(x = "Session number", y = "Symptom severity GAD-7") 

plot +                               # Add polynomial regression curve
  stat_smooth(method = "lm",
              formula = y ~ x,  #this was 4, looked weird and over fit. 
              se = TRUE) +
  ylim(0, 21) +
  theme(panel.background = element_rect(fill = "white"), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black")) +
  labs(title = "Average trajectory of symptom change until session 20")




#Saving into figs 
ggsave(filename = sensitivity_graph_path)

##########################################
#demographics for 20 sessions 

#demo20 <- sens20 %>% filter(`First_session_@Step`== 1 ) #getting confused.
  
#summary(demo20)
#sd(demo20$Age)
#summary(demo20$`GAD7_@S1`)
#summary(demo20$`GAD7_@Last_session`)
#summary(demo20$`Total_sessions_@Step`)
#sd(demo20$`Total_sessions_@Step`)
#sd(demo20$`GAD7_@S1`)
#sd(demo20$`GAD7_@Last_session`)
#summary(demo20$`GAD7_RCSI_@Last_session`)

#demo20 %>% group_by(Gender) %>% summarise(per = 100*n()/nrow(demo20))
#demo20 %>% group_by(Ethnicity_binary) %>% summarise(per = 100*n()/nrow(demo20))
#demo20 %>% group_by(Unemployed_first) %>% summarise(per = 100*n()/nrow(demo20))
#demo20 %>% group_by(Meds_first) %>% summarise(per = 100*n()/nrow(demo20))
#demo20 %>% group_by(LTC_binary) %>% summarise(per = 100*n()/nrow(demo20))
#demo20 %>% group_by(Disability_binary) %>% summarise(per = 100*n()/nrow(demo20))
#demo20 %>% group_by(`GAD7_RCSI_@Last_session`) %>% summarise(per = 100*n()/nrow(demo20))


