
# This was under sorting cases by case id. 
# fitting the mixed effect model (variance components)

model <- lmer(GAD7 ~ TIME + (1 + TIME | CaseID), data = processed_data, REML = FALSE)

# Printing the model summary
summary(model)

# Printing the variance covariance matrix of the random effects:
VarCorr(model)


# fitting model diagonal 
model2 <- lmer(GAD7 ~ TIME + (1 + TIME || CaseID), data = processed_data, REML = FALSE)

summary(model2)