install.packages("tableone")
install.packages("Matching")
install.packages("MatchIt")
install.packages("ipw")
install.packages("survey")
install.packages("sandwich")

# Data loading
library(tableone)
library(Matching)
library(MatchIt)
library(ipw)
library(survey)
library(sandwich)

data(lalonde)

# Task: Fit a propensity score model. Use a logistic regression
# model, where the outcome is treatment. Include the 8 confounding variables in
# the model as predictors, with no interaction terms or non-linear terms (such as
# squared terms). Obtain the propensity score for each subject. Next, obtain the
# inverse probability of treatment weights for each subject.


# Q1: What are the minimum and maximum weights?
xvars <- colnames(lalonde)
xvars <- xvars[xvars!='treat']
# propensity score model
psmodel<-glm(treat~age+educ+black+hispan+married+nodegree+re74+re75,
             family=binomial(link ="logit"),data=lalonde) 

# value of propensity score for each subject
ps <- predict(psmodel, type = "response")
lalonde$pscore <- ps

# create weights
weight<-ifelse(lalonde$treat==1,1/(ps),1/(1-ps))

# max and min of weights
print(max(weight))
print(min(weight))


# Q2: Standardized mean differnces after weighting
# apply weights to data
weighteddata<-svydesign(ids = ~ 1, data =lalonde, weights = ~ weight)
# weighted table
weightedtable <-svyCreateTableOne(vars = xvars, strata = "treat",
                                  data = weighteddata, test = FALSE)
print(weightedtable, smd = TRUE)

# Q3: Average causal treatment effect after weighting
msm <- (svyglm(re78 ~ treat, design = svydesign(~ 1, weights = ~ weight, data=lalonde)))
coef(msm)
confint(msm)

# Q4: ATE after weight truncating
weightmodel <- ipwpoint(exposure = treat, family = "binomial", link = "logit", numerator = ~ 1, 
                        denominator = ~ age + educ + black + hispan + married + nodegree + re74 + re75, 
                        data = lalonde, trunc = 0.01)
msm_trunc <- (svyglm(re78 ~ treat, design = svydesign(~ 1, weights = ~ weightmodel$weights.trunc, data=lalonde)))
coef(msm_trunc)
confint(msm_trunc)









