`.beta1Spot` <- function(maturity, lambda)
  {
   as.numeric( (1 - exp(-maturity*lambda))/(maturity*lambda))
  }

`.beta2Spot` <- function(maturity, lambda)
  {
   as.numeric(  ((1 - exp(-maturity*lambda))/(maturity*lambda) - exp(-maturity*lambda)) )
  }

`.beta1Forward` <- function(maturity, lambda)
  {
       as.numeric( exp(-maturity*lambda) )
  }

`.beta2Forward` <- function(maturity, lambda)
  {
       as.numeric( exp(-maturity*lambda) * (maturity*lambda) )
  }

`.factorBeta1` <- function(lambda, maturity)
  {
   as.numeric( (1-exp(-lambda*maturity)) / (lambda*maturity) )
  }

`.factorBeta2` <- function(lambda, maturity)
  {
  as.numeric( (1-exp(-lambda*maturity)) / (lambda*maturity) - exp(-lambda*maturity) )
  }

`.NS.estimator` <- function( rate, maturity, lambda )
  {
    beta <- lm( rate ~ 1 + .factorBeta1(lambda,maturity) + 
                           .factorBeta2(lambda,maturity) )
    betaPar <- coef(beta)
    NaValues <- na.omit(betaPar)
    if( length(NaValues)<3 ) betaPar <- c(0,0,0)
    names(betaPar) <- c("beta_0", "beta_1", "beta_2")
    EstResults <- list(Par=betaPar, Res=resid(beta))
    return(EstResults)
  }

`.NSS.estimator` <- function( rate, maturity, lambda1, lambda2 )
  {
    beta <- lm( rate ~ 1 + .beta1Spot(maturity,lambda1) +
                           .beta2Spot(maturity,lambda1) +
                           .beta2Spot(maturity,lambda2) )
    betaPar <- coef(beta)
    NaValues <- na.omit(betaPar)
    if( length(NaValues)<4 ) betaPar <- c(0,0,0,0)
    names(betaPar) <- c("beta_0", "beta_1", "beta_2","beta_3")
    EstResults <- list(Par=betaPar, Res=resid(beta))
    return(EstResults)
  }

