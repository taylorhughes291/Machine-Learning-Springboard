#  Introduction
## ══════════════

#   • Learning objectives:
##     • Learn the R formula interface
##     • Specify factor contrasts to test specific hypotheses
##     • Perform model comparisons
##     • Run and interpret variety of regression models in R

## Set working directory
## ─────────────────────────

##   It is often helpful to start your R session by setting your working
##   directory so you don't have to type the full path names to your data
##   and other files

# set the working directory
# setwd("~/Desktop/Rstatistics")
# setwd("C:/Users/dataclass/Desktop/Rstatistics")

##   You might also start by listing the files in your working directory

setwd("/Users/applemac/Documents/Springboard Files/Machine Learning/linear_regression")
getwd() # where am I?
list.files("dataSets") # files in the dataSets folder


## Load the states data
## ────────────────────────

# read the states data
states.data <- readRDS("dataSets/states.rds")
#get labels
states.info <- data.frame(attributes(states.data)[c("names", "var.labels")])
#look at last few labels
tail(states.info, 8)

## Linear regression
## ═══════════════════

## Examine the data before fitting models
## ──────────────────────────────────────────

##   Start by examining the data to check for problems.

# summary of expense and csat columns, all rows
sts.ex.sat <- subset(states.data, select = c("expense", "csat"))
summary(sts.ex.sat)
# correlation between expense and csat
cor(sts.ex.sat)

## Plot the data before fitting models
## ───────────────────────────────────────

##   Plot the data to look for multivariate outliers, non-linear
##   relationships etc.

# scatter plot of expense vs csat
plot(sts.ex.sat)

## Linear regression example
## ─────────────────────────────

##   • Linear regression models can be fit with the `lm()' function
##   • For example, we can use `lm' to predict SAT scores based on
##     per-pupal expenditures:

# Fit our regression model
sat.mod <- lm(csat ~ expense, # regression formula
              data=states.data) # data set
# Summarize and print the results
summary(sat.mod) # show regression coefficients table

## Why is the association between expense and SAT scores /negative/?
## ─────────────────────────────────────────────────────────────────────

##   Many people find it surprising that the per-capita expenditure on
##   students is negatively related to SAT scores. The beauty of multiple
##   regression is that we can try to pull these apart. What would the
##   association between expense and SAT scores be if there were no
##   difference among the states in the percentage of students taking the
##   SAT?

summary(lm(csat ~ expense + percent, data = states.data))

## The lm class and methods
## ────────────────────────────

##   OK, we fit our model. Now what?
##   • Examine the model object:

class(sat.mod)
names(sat.mod)
methods(class = class(sat.mod))[1:9]

##   • Use function methods to get more information about the fit

confint(sat.mod)
hist(residuals(sat.mod))

## Linear Regression Assumptions
## ─────────────────────────────────

##   • Ordinary least squares regression relies on several assumptions,
##     including that the residuals are normally distributed and
##     homoscedastic, the errors are independent and the relationships are
##     linear.

##   • Investigate these assumptions visually by plotting your model:

par(mar = c(4, 4, 2, 2), mfrow = c(1, 2)) #optional
plot(sat.mod, which = c(1, 2)) # "which" argument optional

## Comparing models
## ────────────────────

##   Do congressional voting patterns predict SAT scores over and above
##   expense? Fit two models and compare them:

# fit another model, adding house and senate as predictors
sat.voting.mod <-  lm(csat ~ expense + house + senate,
                      data = na.omit(states.data))
sat.mod <- update(sat.mod, data=na.omit(states.data))
# compare using the anova() function
anova(sat.mod, sat.voting.mod)
coef(summary(sat.voting.mod))

## Exercise: least squares regression
## ────────────────────────────────────────

##   Use the /states.rds/ data set. Fit a model predicting energy consumed
##   per capita (energy) from the percentage of residents living in
##   metropolitan areas (metro). Be sure to
##   1. Examine/plot the data before fitting the model
##   2. Print and interpret the model `summary'
##   3. `plot' the model to look for deviations from modeling assumptions

##   Select one or more additional predictors to add to your model and
##   repeat steps 1-3. Is this model significantly better than the model
##   with /metro/ as the only predictor?

plot(states.data$metro, states.data$energy)
#Surprisingly, doesn't look like there's much of a correlation.

E1 <- lm(states.data$energy ~ states.data$metro, data = states.data)
summary(E1)
#Looks like a pretty bad Adjusted R2 here, 0.097.
plot(E1, "which" = c(1,2))

states.info
#Gotta look at what we're working with here - maybe 1990 population, density, green although they all seem like they're correlated.

plot(states.data$pop, states.data$energy)
plot(states.data$density, states.data$energy)
plot(states.data$green, states.data$energy)
E12 <- lm(states.data$energy ~ states.data$metro + states.data$pop + states.data$density + states.data$green, data = states.data)
summary(E12)
#Adjusted R2 of 0.5767 - Looks like huge correlation with 'green', I will try deleting pop since it likely correlates heavily with the others.
plot(E12, "which" = c(1,2))

E13 <- lm(states.data$energy ~ states.data$metro + states.data$toxic + states.data$green, data = states.data)
summary(E13)
#Adjusted R2 improved 0.5863, let's iterate this one more time taking out metro
plot(E13, "which" = c(1,2))

E14 <- lm(states.data$energy ~ states.data$density + states.data$green, data = states.data)
summary(E14)
#OK so now we have Adjusted R2 score of 0.589 so there was marginal improvement.
plot(E14, "which" = c(1,2))

#Yes, all of these seem way better than just using metro as the indicator.

## Interactions and factors
## ══════════════════════════

## Modeling interactions
## ─────────────────────────

##   Interactions allow us assess the extent to which the association
##   between one predictor and the outcome depends on a second predictor.
##   For example: Does the association between expense and SAT scores
##   depend on the median income in the state?

  #Add the interaction to the model
sat.expense.by.percent <- lm(csat ~ expense*income,
                             data=states.data) 
#Show the results
  coef(summary(sat.expense.by.percent)) # show regression coefficients table

## Regression with categorical predictors
## ──────────────────────────────────────────

##   Let's try to predict SAT scores from region, a categorical variable.
##   Note that you must make sure R does not think your categorical
##   variable is numeric.

# make sure R knows region is categorical
str(states.data$region)
states.data$region <- factor(states.data$region)
#Add region to the model
sat.region <- lm(csat ~ region,
                 data=states.data) 
#Show the results
coef(summary(sat.region)) # show regression coefficients table
anova(sat.region) # show ANOVA table

##   Again, *make sure to tell R which variables are categorical by
##   converting them to factors!*

## Setting factor reference groups and contrasts
## ─────────────────────────────────────────────────

##   In the previous example we use the default contrasts for region. The
##   default in R is treatment contrasts, with the first level as the
##   reference. We can change the reference group or use another coding
##   scheme using the `C' function.

# print default contrasts
contrasts(states.data$region)
# change the reference group
coef(summary(lm(csat ~ C(region, base=4),
                data=states.data)))
# change the coding scheme
coef(summary(lm(csat ~ C(region, contr.helmert),
                data=states.data)))

##   See also `?contrasts', `?contr.treatment', and `?relevel'.

## Exercise: interactions and factors
## ────────────────────────────────────────

##   Use the states data set.

##   1. Add on to the regression equation that you created in exercise 1 by
##      generating an interaction term and testing the interaction.
E15 <- lm(states.data$energy ~ states.data$green*states.data$metro + states.data$toxic, data = states.data)
summary(E15)
#Looks like the Adjusted R2 score has greatly improved to 0.7644

##   2. Try adding region to the model. Are there significant differences
##      across the four regions?

E16 <- lm(states.data$energy ~ states.data$green*states.data$metro + states.data$toxic + states.data$region, data = states.data)
summary(E16)
# There's a small decrease in Adjusted R2 score when adding Region in the mix (0.7617), but we'll continue on
# Here we have the standard dummy contrasts for the factor coefficients. B0 = 219.87135, B1 = -7.19835, B2 = 18.13819, and B3 = -16.37697
# So far, we can tell that Northeastern states on average have a small decrease in energy output per capita than Western states.
# Southern states have a small increase on average of energy consumed per capita than Western states.
# Midwest states have about as much energy consumption per capita as Northeastern states. 
# The differences don't appear to be terribly significant.