library(ISLR)
library(boot)
### LOOCV approach

attach(Auto)
model <- glm(mpg ~horsepower, data = Auto)
MSE_LOOCV <- cv.glm(Auto, model)
MSE_LOOCV$delta[1]
MSE_LOOCV

MSE_LOOCV <- NULL

for (i in 1:10) {
  model <- glm(mpg ~ poly(horsepower, i), data = Auto)
  MSE_LOOCV[i] <- cv.glm(Auto, model)$delta[1]
}



#### K-Fold CV

MSE_10_fold_cv <- NULL

for (i in 1:10) {
  model <- glm(mpg ~ poly(horsepower, i), data = Auto)
  MSE_10_fold_cv[i] <- cv.glm(Auto, model, K = 10)$delta[1]
}

MSE_10_fold_cv