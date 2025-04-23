options(prompt = 'R> ', continue = '+ ')

# install.packages("nonprobsvy")

library(nonprobsvy) ## for estimation
library(ggplot2)    ## for visualisation

data(jvs)
head(jvs)

jvs_svy <- svydesign(ids = ~ 1, 
                     weights = ~ weight,
                     strata = ~ size + nace + region,
                     data = jvs)

data(admin)
head(admin)

ipw_est1 <- nonprob(
  selection = ~ region + private + nace + size,
  target = ~ single_shift,
  svydesign = jvs_svy,
  data = admin,
  method_selection = "logit" ## this is the default
)

ipw_est1

summary(ipw_est1)

extract(ipw_est1)

ipw_est2 <- nonprob(
  selection = ~ region + private + nace + size,
  target = ~ single_shift,
  svydesign = jvs_svy,
  data = admin,
  method_selection = "logit",
  control_selection = control_sel(gee_h_fun = 1, est_method = "gee")
)

ipw_est2

data.frame(ipw_mle=check_balance(~size-1, ipw_est1, 1)$balance,
           ipw_gee=check_balance(~size-1, ipw_est2, 1)$balance)

mi_est1 <- nonprob(
  outcome = single_shift ~ region + private + nace + size,
  svydesign = jvs_svy,
  data = admin,
  method_outcome = "glm",
  family_outcome = "binomial"
)

mi_est1

set.seed(2024)

mi_est2 <- nonprob(
  outcome = single_shift ~ region + private + nace + size,
  svydesign = jvs_svy,
  data = admin,
  method_outcome = "nn",
  control_outcome = control_out(k=5)
)

mi_est3 <- nonprob(
  outcome = single_shift ~ region + private + nace + size,
  svydesign = jvs_svy,
  data = admin,
  method_outcome = "pmm",
  family_outcome = "binomial", 
  control_outcome = control_out(k=5),
  control_inference = control_inf(var_method = "bootstrap", num_boot = 50)
)

rbind("NN"= extract(mi_est2)[, 2:3], "PMM" = extract(mi_est3)[, 2:3])

dr_est1 <- nonprob(
  selection = ~ region + private + nace + size,
  outcome = single_shift ~ region + private + nace + size,
  svydesign = jvs_svy,
  data = admin,
  method_selection = "logit",
  method_outcome = "glm",
  family_outcome = "binomial"
)
dr_est1

summary(dr_est1)

set.seed(2024)
dr_est2 <- nonprob(
  selection = ~ region + private + nace + size,
  outcome = single_shift ~ region + private + nace + size,
  svydesign = jvs_svy,
  data = admin,
  method_selection = "logit",
  method_outcome = "glm",
  family_outcome = "binomial",
  control_inference = control_inf(bias_correction = TRUE,
                                  vars_combine = TRUE,
                                  vars_selection = TRUE)
)
dr_est2

df_s <- rbind(extract(ipw_est1), extract(ipw_est2), extract(mi_est1),
              extract(mi_est2), extract(mi_est3), extract(dr_est1), 
              extract(dr_est2))

df_s$est <- c("IPW (MLE)", "IPW (GEE)", "MI (GLM)", "MI (NN)", 
              "MI (PMM)", "DR", "DR (BM)")

ggplot(data = df_s, 
       aes(y = est, x = mean, xmin = lower_bound, xmax = upper_bound)) + 
  geom_point() + 
  geom_vline(xintercept = mean(admin$single_shift), 
             linetype = "dotted", color = "red") + 
  geom_errorbar() + 
  labs(x = "Point estimator and confidence interval", y = "Estimators") +
  theme_bw()


set.seed(2024)
ipw_est1_boot <- nonprob(
  selection = ~ region + private + nace + size,
  target = ~ single_shift,
  svydesign = jvs_svy,
  data = admin,
  method_selection = "logit",
  control_inference = control_inf(var_method = "bootstrap", num_boot = 50),
  verbose = FALSE
)

rbind("IPW analytic variance"  = extract(ipw_est1)[, 2:3],
      "IPW bootstrap variance" = extract(ipw_est1_boot)[, 2:3])

head(ipw_est1_boot$boot_sample, n=3)

set.seed(2024)
mi_est1_sel <- nonprob(
  outcome = single_shift ~ region + private + nace + size,
  svydesign = jvs_svy,
  data = admin,
  method_outcome = "glm",
  family_outcome = "binomial" ,
  control_outcome = control_out(nfolds = 5, nlambda = 25, penalty = "lasso"),
  control_inference = control_inf(vars_selection = TRUE),
  verbose = TRUE
)

rbind("MI without var sel" = extract(mi_est1)[, 2:3],
      "MI with var sel"    = extract(mi_est1_sel)[, 2:3])

round(coef(mi_est1_sel)$coef_out[, 1], 4)

round(coef(ipw_est1)$coef_sel[, 1], 4)

nobs(dr_est1)

confint(dr_est1, level = 0.99)

summary(weights(dr_est1))

res_glm <- method_glm(
  y_nons = admin$single_shift,
  X_nons = model.matrix(~ region + private + nace + size, admin),
  X_rand = model.matrix(~ region + private + nace + size, jvs),
  svydesign = jvs_svy)

res_glm

method_ps()

# ?method_ps()
# session info
sessionInfo()