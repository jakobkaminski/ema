###https://jruwaard.github.io/aph_ema_handbook/lmm.html#the-mixed-model


# Simulating ema data.
##https://rdrr.io/github/jruwaard/emaph/v
install.packages("remotes")
remotes::install_github("jruwaard/emaph")


library(emaph)
plan <- sample_plan(n_participants = 100, 
                    n_days = 7,
                    times = c("10:00-11:00", 
                              "13:00-14:00", 
                              "16:00-18:00"))

d1 <- sim_ema(plan, 
              mm_par = list(fixed  = c(intercept = 5),
                            random = c(intercept = 1),
                            error = .5),
              lim = c(0, 10))



# Fitting a mixed model with lme.
library(nlme)
fm <- lme(Y ~ 1, random = ~ 1 | id, 
          data = d1)





# Simulating ema data (time effect).
d2 <- sim_ema(plan, 
              mm_par = list(fixed  = c(intercept = 5, time = 0.5),
                            random = c(intercept = 1, time = 0.1),
                            error = .5),
              lim = c(0, 10))

# Two-group simulation.
d3 <- rbind(d1, d2)
d3$group <- factor(c(rep(0, nrow(d1)), rep(1, nrow(d2))), labels = c("control", "treatment"))
d3$id[d3$group == "treatment"] <- d3$id[d3$group == "treatment"] + 100


# A mixed model, with two groups.
library(nlme)
fm <- lme(Y ~ 1 + time * group, random = ~ 1 + time | id, 
          data = d3)
round(summary(fm)$tTable, 2)
#>                     Value Std.Error   DF t-value p-value
#> (Intercept)          4.93      0.11 3998   43.98    0.00
#> time                 0.00      0.02 3998    0.21    0.83
#> grouptreatment       0.18      0.16  198    1.14    0.25
#> time:grouptreatment  0.46      0.03 3998   16.71    0.00

