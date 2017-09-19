

### pollution problem in R

library(nloptr)

obj.fn <- function(x){
  return (0.2* x[1]^2+ 0.08* x[2]^2+ 0.018* x[3]^2+ 0.1* x[1]* x[2]+ 0.04* x[1]* x[3]+ 0.06* x[2]* x[3] )
}

x0 <- c(300, 500, 200)

lb= c(0,0,0)

ub= c(Inf, Inf, Inf)

eval_g_eq <- function(x){
  return(x[1]+ x[2]+ x[3]- 1000)
}

eval_g_ineq <- function(x){
  return(-120- 0.14* x[1]- 0.11* x[2]- 0.1* x[3])
}

local_opts <- list( "algorithm" = "NLOPT_LN_AUGLAG_EQ",
                    "xtol_rel" = 1.0e-7)

opts = list("algorithm"="NLOPT_LN_AUGLAG_EQ",
            "xtol_rel"=1.0e-8, "local_opts"=local_opts)

ans <- nloptr(x0, eval_f=obj.fn, lb=lb, ub=ub, eval_g_eq=eval_g_eq, eval_g_ineq=eval_g_ineq, opts=opts)
