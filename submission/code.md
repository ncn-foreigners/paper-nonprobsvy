

``` r
options(prompt = 'R> ', continue = '+ ')

# install.packages("nonprobsvy")

library(nonprobsvy) ## for estimation
```

```
## Loading required package: survey
```

```
## Loading required package: grid
```

```
## Loading required package: Matrix
```

```
## Loading required package: survival
```

```
## 
## Attaching package: 'survey'
```

```
## The following object is masked from 'package:graphics':
## 
##     dotchart
```

``` r
library(ggplot2)    ## for visualisation

data(jvs)
head(jvs)
```

```
##    id private size nace region weight
## 1 j_1       0    L    O     14      1
## 2 j_2       0    L    O     24      6
## 3 j_3       0    L  R.S     14      1
## 4 j_4       0    L  R.S     14      1
## 5 j_5       0    L  R.S     22      1
## 6 j_6       0    M  R.S     26      1
```

``` r
jvs_svy <- svydesign(ids = ~ 1, 
                     weights = ~ weight,
                     strata = ~ size + nace + region,
                     data = jvs)

data(admin)
head(admin)
```

```
##    id private size nace region single_shift
## 1 j_1       0    L    P     30        FALSE
## 2 j_2       0    L    O     14         TRUE
## 3 j_3       0    L    O     04         TRUE
## 4 j_4       0    L    O     24         TRUE
## 5 j_5       0    L    O     04         TRUE
## 6 j_6       1    L    C     28        FALSE
```

``` r
ipw_est1 <- nonprob(
  selection = ~ region + private + nace + size,
  target = ~ single_shift,
  svydesign = jvs_svy,
  data = admin,
  method_selection = "logit" ## this is the default
)

ipw_est1
```

```
## A nonprob object
##  - estimator type: inverse probability weighting
##  - method: logit (mle)
##  - auxiliary variables source: survey
##  - vars selection: false
##  - variance estimator: analytic
##  - population size fixed: false
##  - naive (uncorrected) estimator: 0.6605
##  - selected estimator: 0.7224 (se=0.0421, ci=(0.6399, 0.8048))
```

``` r
summary(ipw_est1)
```

```
## A nonprob_summary object
##  - call: nonprob(data = admin, selection = ~region + private + nace + 
##     size, target = ~single_shift, svydesign = jvs_svy, method_selection = "logit")
##  - estimator type: inverse probability weighting
##  - nonprob sample size: 9344 (18%)
##  - prob sample size: 6523 (12.6%)
##  - population size: 51870 (fixed: false)
##  - detailed information about models are stored in list element(s): "selection"
## ----------------------------------------------------------------
##  - sum of IPW weights: 52898.13 
##  - distribution of IPW weights (nonprob sample):
##    - min: 1.1693; mean: 5.6612; median: 4.3334; max: 49.9504
##  - distribution of IPW probabilities (nonprob sample):
##    - min: 0.0189; mean: 0.2894; median: 0.2525; max: 0.8552
##  - distribution of IPW probabilities (prob sample):
##    - min: 0.0200; mean: 0.2687; median: 0.2291; max: 0.8552
## ----------------------------------------------------------------
```

``` r
extract(ipw_est1)
```

```
##         target      mean         SE lower_bound upper_bound
## 1 single_shift 0.7223628 0.04207711   0.6398932   0.8048324
```

``` r
ipw_est2 <- nonprob(
  selection = ~ region + private + nace + size,
  target = ~ single_shift,
  svydesign = jvs_svy,
  data = admin,
  method_selection = "logit",
  control_selection = control_sel(gee_h_fun = 1, est_method = "gee")
)

ipw_est2
```

```
## A nonprob object
##  - estimator type: inverse probability weighting
##  - method: logit (gee)
##  - auxiliary variables source: survey
##  - vars selection: false
##  - variance estimator: analytic
##  - population size fixed: false
##  - naive (uncorrected) estimator: 0.6605
##  - selected estimator: 0.7042 (se=0.0398, ci=(0.6262, 0.7822))
```

``` r
data.frame(ipw_mle=check_balance(~size-1, ipw_est1, 1)$balance,
           ipw_gee=check_balance(~size-1, ipw_est2, 1)$balance)
```

```
##       ipw_mle ipw_gee
## sizeL  -367.6       0
## sizeM  -228.5       0
## sizeS  1624.2       0
```

``` r
mi_est1 <- nonprob(
  outcome = single_shift ~ region + private + nace + size,
  svydesign = jvs_svy,
  data = admin,
  method_outcome = "glm",
  family_outcome = "binomial"
)

mi_est1
```

```
## A nonprob object
##  - estimator type: mass imputation
##  - method: glm (binomial)
##  - auxiliary variables source: survey
##  - vars selection: false
##  - variance estimator: analytic
##  - population size fixed: false
##  - naive (uncorrected) estimator: 0.6605
##  - selected estimator: 0.7032 (se=0.0112, ci=(0.6813, 0.7252))
```

``` r
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
  control_outcome = control_out(k=5)
)

rbind("NN"= extract(mi_est2)[, 2:3], "PMM" = extract(mi_est3)[, 2:3])
```

```
##          mean         SE
## NN  0.6799537 0.01568503
## PMM 0.7458724 0.01526712
```

``` r
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
```

```
## A nonprob object
##  - estimator type: doubly robust
##  - method: glm (binomial)
##  - auxiliary variables source: survey
##  - vars selection: false
##  - variance estimator: analytic
##  - population size fixed: false
##  - naive (uncorrected) estimator: 0.6605
##  - selected estimator: 0.7035 (se=0.0117, ci=(0.6806, 0.7263))
```

``` r
summary(dr_est1)
```

```
## A nonprob_summary object
##  - call: nonprob(data = admin, selection = ~region + private + nace + 
##     size, outcome = single_shift ~ region + private + nace + 
##     size, svydesign = jvs_svy, method_selection = "logit", method_outcome = "glm", 
##     family_outcome = "binomial")
##  - estimator type: doubly robust
##  - nonprob sample size: 9344 (18%)
##  - prob sample size: 6523 (12.6%)
##  - population size: 51870 (fixed: false)
##  - detailed information about models are stored in list element(s): "outcome" and "selection"
## ----------------------------------------------------------------
##  - sum of IPW weights: 52898.13 
##  - distribution of IPW weights (nonprob sample):
##    - min: 1.1693; mean: 5.6612; median: 4.3334; max: 49.9504
##  - distribution of IPW probabilities (nonprob sample):
##    - min: 0.0189; mean: 0.2894; median: 0.2525; max: 0.8552
##  - distribution of IPW probabilities (prob sample):
##    - min: 0.0200; mean: 0.2687; median: 0.2291; max: 0.8552
## ----------------------------------------------------------------
##  - distribution of outcome residuals:
##    - single_shift: min: -0.9730; mean: 0.0000; median: 0.1075; max: 0.8564
##  - distribution of outcome predictions (nonprob sample):
##    - single_shift: min: 0.1436; mean: 0.6605; median: 0.6739; max: 0.9758
##  - distribution of outcome predictions (prob sample):
##    - single_shift: min: 0.1436; mean: 0.5930; median: 0.5938; max: 0.9785
## ----------------------------------------------------------------
```

``` r
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
```

```
## A nonprob object
##  - estimator type: doubly robust
##  - method: glm (binomial)
##  - auxiliary variables source: survey
##  - vars selection: true
##  - variance estimator: analytic
##  - population size fixed: false
##  - naive (uncorrected) estimator: 0.6605
##  - selected estimator: 0.7037 (se=0.0104, ci=(0.6833, 0.7240))
```

``` r
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
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-1.png)

``` r
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
```

```
##                             mean         SE
## IPW analytic variance  0.7223628 0.04207711
## IPW bootstrap variance 0.7223628 0.04307590
```

``` r
head(ipw_est1_boot$boot_sample, n=3)
```

```
##      single_shift
## [1,]    0.7406651
## [2,]    0.6989083
## [3,]    0.7667519
```

``` r
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
```

```
## Starting CV fold #1
## Starting CV fold #2
## Starting CV fold #3
## Starting CV fold #4
## Starting CV fold #5
```

``` r
rbind("MI without var sel" = extract(mi_est1)[, 2:3],
      "MI with var sel"    = extract(mi_est1_sel)[, 2:3])
```

```
##                         mean         SE
## MI without var sel 0.7032089 0.01120237
## MI with var sel    0.7019291 0.01102109
```

``` r
round(coef(mi_est1_sel)$coef_out[, 1], 4)
```

```
## (Intercept)    region04    region06    region08    region10    region12 
##      0.2820      0.0025      0.3274      0.3196      0.2120      0.1775 
##    region14    region16    region18    region20    region22    region24 
##      0.0143      0.0792      0.0000      0.0000      0.0047     -0.2554 
##    region26    region28    region30    region32     private     naceD.E 
##      0.1333      0.0000      0.0000      0.0000     -0.6090      0.1759 
##       naceF       naceG       naceH       naceI       naceJ     naceK.L 
##      1.9173     -0.4558     -0.5607     -1.0966      0.9214      1.0370 
##       naceM       naceN       naceO       naceP       naceQ     naceR.S 
##      1.0025     -0.1840      1.4744      0.5368     -0.7116     -0.8138 
##       sizeM       sizeS 
##      0.9972      1.5354
```

``` r
round(coef(ipw_est1)$coef_sel[, 1], 4)
```

```
## (Intercept)    region04    region06    region08    region10    region12 
##     -0.6528      0.8378      0.1995      0.1048     -0.1576     -0.6099 
##    region14    region16    region18    region20    region22    region24 
##     -0.8415      0.7639      1.1781      0.2225     -0.0375     -0.4067 
##    region26    region28    region30    region32     private     naceD.E 
##      0.2029      0.5786     -0.6102      0.3274      0.0590      0.7727 
##       naceF       naceG       naceH       naceI       naceJ     naceK.L 
##     -0.3778     -0.3337     -0.6517      0.4118     -1.4264      0.0617 
##       naceM       naceN       naceO       naceP       naceQ     naceR.S 
##     -0.4068      0.8003     -0.6935      1.2510      0.3029      0.2223 
##       sizeM       sizeS 
##     -0.3641     -1.0292
```

``` r
nobs(dr_est1)
```

```
##    prob nonprob 
##    6523    9344
```

``` r
confint(dr_est1, level = 0.99)
```

```
##         target lower_bound upper_bound
## 1 single_shift   0.6734503   0.7334886
```

``` r
summary(weights(dr_est1))
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   1.169   2.673   4.333   5.661   7.178  49.950
```

``` r
res_glm <- method_glm(
  y_nons = admin$single_shift,
  X_nons = model.matrix(~ region + private + nace + size, admin),
  X_rand = model.matrix(~ region + private + nace + size, jvs),
  svydesign = jvs_svy)

res_glm
```

```
## Mass imputation model (GLM approach). Estimated mean: 0.7039 (se: 0.0115)
```

``` r
method_ps()
```

```
## Propensity score model with logit link
```

``` r
# ?method_ps()
# session info
sessionInfo()
```

```
## R version 4.4.2 (2024-10-31)
## Platform: aarch64-apple-darwin20
## Running under: macOS Sequoia 15.5
## 
## Matrix products: default
## BLAS:   /System/Library/Frameworks/Accelerate.framework/Versions/A/Frameworks/vecLib.framework/Versions/A/libBLAS.dylib 
## LAPACK: /Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/lib/libRlapack.dylib;  LAPACK version 3.12.0
## 
## locale:
## [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
## 
## time zone: Europe/Warsaw
## tzcode source: internal
## 
## attached base packages:
## [1] grid      stats     graphics  grDevices utils     datasets 
## [7] methods   base     
## 
## other attached packages:
## [1] ggplot2_3.5.2    nonprobsvy_0.2.2 survey_4.4-2     survival_3.8-3  
## [5] Matrix_1.7-3    
## 
## loaded via a namespace (and not attached):
##  [1] sandwich_3.1-1       generics_0.1.4       lattice_0.22-7      
##  [4] digest_0.6.37        magrittr_2.0.3       evaluate_1.0.3      
##  [7] RColorBrewer_1.1-3   nleqslv_3.3.5        iterators_1.0.14    
## [10] fastmap_1.2.0        foreach_1.5.2        doParallel_1.0.17   
## [13] operator.tools_1.6.3 DBI_1.2.3            scales_1.4.0        
## [16] codetools_0.2-20     cli_3.6.5            mitools_2.4         
## [19] rlang_1.1.6          miscTools_0.6-28     splines_4.4.2       
## [22] withr_3.0.2          RANN_2.6.2           yaml_2.3.10         
## [25] tools_4.4.2          parallel_4.4.2       ncvreg_3.15.0       
## [28] dplyr_1.1.4          maxLik_1.5-2.1       vctrs_0.6.5         
## [31] R6_2.6.1             zoo_1.8-14           lifecycle_1.0.4     
## [34] rticles_0.27         MASS_7.3-65          pkgconfig_2.0.3     
## [37] pillar_1.10.2        gtable_0.3.6         glue_1.8.0          
## [40] Rcpp_1.0.14          xfun_0.52            tibble_3.2.1        
## [43] tidyselect_1.2.1     rstudioapi_0.17.1    knitr_1.50          
## [46] farver_2.1.2         htmltools_0.5.8.1    rmarkdown_2.29      
## [49] labeling_0.4.3       formula.tools_1.7.1  compiler_4.4.2
```

