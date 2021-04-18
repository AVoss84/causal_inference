
# Week 3 assignement: Propensity score matching

install.packages("tableone")
install.packages("Matching")
install.packages("MatchIt")

library(tableone)
library(Matching)
library(MatchIt)

data(lalonde)


# Question 1: SMD
#-----------------
xvars <- colnames(lalonde)
xvars <- xvars[xvars!='treat']     # only confounders (without treatment variable)

table1<- CreateTableOne(vars=xvars,strata="treat", data=lalonde, test=FALSE) 
print(table1,smd=TRUE) 


# Question 2: unadjusted mean
#----------------------------
treated_mean <- mean(lalonde[lalonde$treat==1,]$re78)
untreated_mean <- mean(lalonde[lalonde$treat==0,]$re78)
print(treated_mean-untreated_mean)


# Question3: propensity score estimation
#----------------------------------------
# Fit binary logistic regression:

psmodel<-glm(treat~age+educ+black+hispan+married+nodegree+re74+re75,
             family=binomial(),data=lalonde) 

summary(psmodel) 

# Propensity scores/Prob. of treatment given control variates/confounders 
pscore<-psmodel$fitted.values 

max(pscore)
min(pscore)


# Questions 4+5: after PSM
#--------------------------
# Match on the propensity score itself, 
# not logit of the propensity score.  

set.seed(931139)

psmatch<-Match(Tr=lalonde$treat,M=1,X=pscore,replace=FALSE) 

matched<-lalonde[unlist(psmatch[c("index.treated","index.control")]), ]

# Obtain the standardized differences for the matched data:

matchedtab1<-CreateTableOne(vars=xvars, strata ="treat", 
                            data=matched, test = FALSE) 

print(matchedtab1, smd = TRUE) 

# Question 6: Redo matching using caliper
#-----------------------------------------
set.seed(931139)
psmatch<-Match(Tr=lalonde$treat,M=1,X=pscore,replace=FALSE, caliper = 0.1) 
matched<-lalonde[unlist(psmatch[c("index.treated","index.control")]), ]

#get standardized differences 
matchedtab2<-CreateTableOne(vars=xvars, strata ="treat", 
                            data=matched, test = FALSE) 

print(matchedtab2, smd = TRUE) 

# Question7:
#------------
treated_mean2 <- mean(matched[matched$treat==1,]$re78)
untreated_mean2 <- mean(matched[matched$treat==0,]$re78)
print(treated_mean2-untreated_mean2)

# Question8: outcome analysis
#------------------------------
y_trt<-matched$re78[matched$treat==1] 

y_con<-matched$re78[matched$treat==0] 

# Pairwise difference in outcome 
diffy = y_trt-y_con 

# Paired t-test 
t.test(diffy) 






