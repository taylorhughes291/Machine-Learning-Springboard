## Regression with binary outcomes
## ═════════════════════════════════

## Logistic regression
## ───────────────────────

##   This far we have used the `lm' function to fit our regression models.
##   `lm' is great, but limited–in particular it only fits models for
##   continuous dependent variables. For categorical dependent variables we
##   can use the `glm()' function.

##   For these models we will use a different dataset, drawn from the
##   National Health Interview Survey. From the [CDC website]:

##         The National Health Interview Survey (NHIS) has monitored
##         the health of the nation since 1957. NHIS data on a broad
##         range of health topics are collected through personal
##         household interviews. For over 50 years, the U.S. Census
##         Bureau has been the data collection agent for the National
##         Health Interview Survey. Survey results have been
##         instrumental in providing data to track health status,
##         health care access, and progress toward achieving national
##         health objectives.

##   Load the National Health Interview Survey data:
NH11 <- readRDS("dataSets/NatHealth2011.rds")
labs <- attributes(NH11)$labels

##   [CDC website] http://www.cdc.gov/nchs/nhis.htm

## Logistic regression example
## ───────────────────────────────

##   Let's predict the probability of being diagnosed with hypertension
##   based on age, sex, sleep, and bmi

str(NH11$hypev) # check stucture of hypev
levels(NH11$hypev) # check levels of hypev
# collapse all missing values to NA
NH11$hypev <- factor(NH11$hypev, levels=c("2 No", "1 Yes"))
# run our regression model
hyp.out <- glm(hypev~age_p+sex+sleep+bmi,
              data=NH11, family="binomial")
coef(summary(hyp.out))

## Logistic regression coefficients
## ────────────────────────────────────

##   Generalized linear models use link functions, so raw coefficients are
##   difficult to interpret. For example, the age coefficient of .06 in the
##   previous model tells us that for every one unit increase in age, the
##   log odds of hypertension diagnosis increases by 0.06. Since most of us
##   are not used to thinking in log odds this is not too helpful!

##   One solution is to transform the coefficients to make them easier to
##   interpret

hyp.out.tab <- coef(summary(hyp.out))
hyp.out.tab[, "Estimate"] <- exp(coef(hyp.out))
hyp.out.tab

## Generating predicted values
## ───────────────────────────────

##   In addition to transforming the log-odds produced by `glm' to odds, we
##   can use the `predict()' function to make direct statements about the
##   predictors in our model. For example, we can ask "How much more likely
##   is a 63 year old female to have hypertension compared to a 33 year old
##   female?".

# Create a dataset with predictors set at desired levels
predDat <- with(NH11,
                expand.grid(age_p = c(33, 63),
                            sex = "2 Female",
                            bmi = mean(bmi, na.rm = TRUE),
                            sleep = mean(sleep, na.rm = TRUE)))
# predict hypertension at those levels
cbind(predDat, predict(hyp.out, type = "response",
                       se.fit = TRUE, interval="confidence",
                       newdata = predDat))

##   This tells us that a 33 year old female has a 13% probability of
##   having been diagnosed with hypertension, while and 63 year old female
##   has a 48% probability of having been diagnosed.

## Packages for  computing and graphing predicted values
## ─────────────────────────────────────────────────────────

##   Instead of doing all this ourselves, we can use the effects package to
##   compute quantities of interest for us (cf. the Zelig package).

library(effects)
plot(allEffects(hyp.out))

## Exercise: logistic regression
## ───────────────────────────────────

##   Use the NH11 data set that we loaded earlier.

##   1. Use glm to conduct a logistic regression to predict ever worked
##      (everwrk) using age (age_p) and marital status (r_maritl).

Pert <- NH11[c("everwrk", "age_p", "r_maritl")]
str(Pert$everwrk)
levels(Pert$everwrk) #Looks like there are some values we should convert to NA
Pert$everwrk <- factor(Pert$everwrk, levels = c("2 No", "1 Yes")) #Let's discount doublecheck
Pert <- Pert[!is.na(Pert$everwrk),]
Pert$everwrk <- ifelse(Pert$everwrk == "2 No", 0, 1)


str(Pert$everwrk)
levels(Pert$everwrk) # Now let's fill in missing values using package mice


# Let's get rid of some of these meaningless levels of r_marital
Pert$r_maritl <- factor(Pert$r_maritl, levels = c("1 Married - spouse in household", "2 Married - spouse not in household", "3 Married - spouse in household unknown", "4 Widowed", "5 Divorced", "6 Separated", "7 Never married", "8 Living with partner"))

# Now we will move on to combining some of the similar categories into more meaningful generalizations.
# We will also impute the missing data using mice package.
levels(Pert$r_maritl) <- c("Married", "Married", "Married", "Widowed", "Separated", "Separated", "Never Married", "Never Married")


# now let's create a train and test set over an 80/20 split
library(caTools)
SplitPert <- sample.split(Pert$everwrk, SplitRatio = 0.8)
PertTrain <- subset(Pert, SplitPert = TRUE)
PertTest <- subset(Pert, SplitPert = FALSE)


# The model
uwrkbro <- glm(everwrk ~ ., data = PertTrain, family = "binomial")
summary(uwrkbro)

# The prediction
PredTest <- predict(uwrkbro, newdata = PertTest, type = "response")
summary(PredTest) # Looks like the mean value is 0.8451. Let's try setting our t-value to the mean and see what happens
PertTest$PredTest <- PredTest

library(ROCR)
ROCRpred <- prediction(as.numeric(PertTest$PredTest), PertTest$everwrk)
ROCRperf <- performance(ROCRpred, 'tpr','fpr')
plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7))

##   2. Predict the probability of working for each level of marital
##      status.

probDat <- with(Pert,
                expand.grid(r_maritl = c("Married", "Widowed", "Separated", "Never Married"),
                            age_p = mean(age_p, na.rm = TRUE)))
# predict if somebody has ever worked at each level of marital status.
cbind(probDat, predict(uwrkbro, type = "response",
                       se.fit = TRUE, interval="confidence",
                       newdata = probDat))

# Clearly there's a reason why our minimum value for our model was above 0.5; Because there's a greater than 
# 50% chance of people in every level of marital status working.

##   Note that the data is not perfectly clean and ready to be modeled. You
##   will need to clean up at least some of the variables before fitting
##   the model.
