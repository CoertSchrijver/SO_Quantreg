library(quantreg)
#### Raw dataset (PE_raw_SO) ####

# quantile regression (produces singularity error)
LAD.factor <- rq(
  LogProb ~ factor(`Delivery costs`) +
    factor(`Minimum order quantity`) + factor(`Delivery window`) +
    factor(NoneOpt),
  data = PE_raw_SO,
  tau = 0.5
) 

# linear regression to check for singularity
LM.factor <- lm(
  LogProb ~ factor(`Delivery costs`) +
    factor(`Minimum order quantity`) + factor(`Delivery window`) +
    factor(NoneOpt),
  data = PE_raw_SO
)
alias(LM.factor)

# impose assumptions on standard errors
summary(LM.factor, se = "iid")
summary(LM.factor, se = "boot")


#### Manually created dummy variables to get rid of
#### collinearity (PE_dichotomized_SO) ####
LAD.di.factor <- rq(
  LogProb ~ Delivery.costs_1 + Delivery.costs_2 +
    Minimum.order.quantity_1 + Minimum.order.quantity_2 +
    Delivery.window_1 + Delivery.window_2 + factor(NoneOpt),
  data = PE_dichotomized_SO,
  tau = 0.5
)

summary(LAD.di.factor)  #backsolve error

# impose assumptions (unusual results)
summary(LAD.di.factor, se = "iid") 
summary(LAD.di.factor, se = "boot")

# linear regression to check for singularity
LM.di.factor <- lm(
  LogProb ~ Delivery.costs_1 + Delivery.costs_2 +
    Minimum.order.quantity_1 + Minimum.order.quantity_2 +
    Delivery.window_1 + Delivery.window_2 + factor(NoneOpt),
  data = PE_dichotomized_SO
)
alias(LM.di.factor)

summary(LM.di.factor)  #regular results, all significant