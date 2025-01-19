# Loading libraries
remove.packages("survival")
install.packages("survival")
library(survival)

install.packages("ggplot2")
library(ggplot2)

install.packages("pROC")
library(pROC)

data(package = "survival")

# Loading the colon dataset
data(colon)

# Overview of dataset
str(colon)
summary(colon)

# Check for missing values and remove them
sum(is.na(colon))
colon_clean <- colon[!is.na(colon$nodes), ]

# Rows after omission
nrow(colon_clean)

# Distribution of treatment groups
table(colon_clean$rx)

# Distribution of event types
table(colon_clean$etype)


# Histogram of age
ggplot(colon_clean, aes(x = age)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black") +
  labs(title = "Age Distribution", x = "Age", y = "Frequency")

# Histogram of nodes
ggplot(colon_clean, aes(x = nodes)) +
  geom_histogram(binwidth = 5, fill = "green", color = "black") +
  labs(title = "Distribution of Positive Lymph Nodes", x = "Number of Nodes", y = "Frequency")

#Histogram of treatment groups
ggplot(colon_clean, aes(x = rx)) +
  geom_bar(fill = "orange", color = "black") +
  labs(title = "Distribution of Treatment Groups", x = "Treatment", y = "Count")

#Exploring Numeric Relationships
#Survival Time vs Age
ggplot(colon_clean, aes(x = age, y = time)) +
  geom_point(alpha = 0.5, color = "purple") +
  geom_smooth(method = "loess", color = "red") +
  labs(title = "Survival Time vs Age", x = "Age", y = "Survival Time (days)")
#Survival Time vs Lymph Nodes
ggplot(colon_clean, aes(x = nodes, y = time)) +
  geom_point(alpha = 0.5, color = "darkgreen") +
  geom_smooth(method = "loess", color = "red") +
  labs(title = "Survival Time vs Positive Lymph Nodes", x = "Number of Nodes", y = "Survival Time (days)")

#Exploring Categorical Relationships
#Surival Time vs Treatment group
ggplot(colon_clean, aes(x = rx, y = time, fill = rx)) +
  geom_boxplot() +
  labs(title = "Survival Time by Treatment Group", x = "Treatment", y = "Survival Time (days)") +
  theme(legend.position = "none")
#Surivial Time vs Event Type
ggplot(colon_clean, aes(x = factor(etype), y = time, fill = factor(etype))) +
geom_boxplot() +
  labs(title = "Survival Time by Event Type", x = "Event Type (1 = Recurrence, 2 = Death)", y = "Survival Time (days)") +
  theme(legend.position = "none")

#Conduct Survival Analysis and Statistical Significance between Groups
# Kaplan-Meier survival curves by treatment group
km_fit <- survfit(Surv(time, status) ~ rx, data = colon_clean)

# Summary of Kaplan-Meier survival analysis
summary(km_fit)

# Kaplan-Meier survival curves
plot(km_fit, 
     col = c("blue", "red", "green"),  
     lty = 1:3,                      
     xlab = "Time (days)", 
     ylab = "Survival Probability", 
     main = "Kaplan-Meier Survival Curves by Treatment Group")
legend("topright", 
       legend = c("Obs", "Lev", "Lev+5FU"),
       col = c("blue", "red", "green"),
       lty = 1:3,
       cex = 1.2,
       box.lwd = 0,
       bg = "white")   
       lty = 1:3)


# Log-rank test to compare survival between groups
logrank_test <- survdiff(Surv(time, status) ~ rx, data = colon_clean)

# Display the test results
logrank_test


# Fit Cox proportional hazards model
cox_model <- coxph(Surv(time, status) ~ rx + age + nodes + sex, data = colon_clean)

# View the summary of the model
summary(cox_model)
cox.zph(cox_model)

# Plot Model
plot(survfit(cox_model), 
     col = c("blue", "red", "green"), 
     xlab = "Time (days)", 
     ylab = "Survival Probability", 
     main = "Survival Curves from Cox Model")
legend("topright", legend = c("Obs", "Lev", "Lev+5FU"), col = c("blue", "red", "green"), lty = 1)


# Logistic regression to predict survival status
logit_model <- glm(status ~ rx + age + nodes + sex, data = colon_clean, family = binomial)

# View the summary of the model
summary(logit_model)
exp(coef(logit_model))  # Odds ratios

# Evaluate Model Fit
library(pROC)
pred <- predict(logit_model, type = "response")
roc_curve <- roc(colon_clean$status, pred)
plot(roc_curve, main = "ROC Curve")
auc(roc_curve)
