d.admit <- read.csv('admission.csv',header = TRUE, sep = ",")
#binary logistic regression

#y (responsive variable): admit 
#x : gre,gpa,rank
logr = glm(admit ~ gre + gpa + rank, data = d.admit, family = binomial("logit"))
summary(logr)
