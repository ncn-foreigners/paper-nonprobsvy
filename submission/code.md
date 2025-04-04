

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
data.frame(ipw_mle=check_balance(~size, ipw_est1, 1)$balance,
           ipw_gee=check_balance(~size, ipw_est2, 1)$balance)
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
##  - selected estimator: 0.7032 (se=0.0112, ci=(0.6812, 0.7252))
```

``` r
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
```

```
## Bootstrap variance only for the `pmm` method, analytical version during implementation.
```

``` r
rbind("NN"=mi_est2$output, "PMM"=mi_est3$output)
```

```
##          mean         SE
## NN  0.6799537 0.01568503
## PMM 0.7337228 0.02231178
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
df_s <- rbind(cbind(ipw_est1$output, ipw_est1$confidence_interval),
                    cbind(ipw_est2$output, ipw_est2$confidence_interval),
                    cbind(mi_est1$output, mi_est1$confidence_interval),
                    cbind(mi_est2$output, mi_est2$confidence_interval),
                    cbind(mi_est3$output, mi_est3$confidence_interval),
                    cbind(dr_est1$output, dr_est1$confidence_interval),
                    cbind(dr_est2$output, dr_est2$confidence_interval))
rownames(df_s) <- NULL

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

rbind("IPW analytic variance"=ipw_est1$output,
      "IPW bootstrap variance"=ipw_est1_boot$output)
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
rbind("MI without var sel"= mi_est1$output,
      "MI with var sel"   = mi_est1_sel$output)
```

```
##                         mean         SE
## MI without var sel 0.7031991 0.01120162
## MI with var sel    0.7019285 0.01102080
```

``` r
round(coef(mi_est1_sel$outcome$single_shift), 4)
```

```
## (Intercept)    region04    region06    region08 
##      0.2817      0.0023      0.3272      0.3195 
##    region10    region12    region14    region16 
##      0.2118      0.1773      0.0142      0.0791 
##    region18    region20    region22    region24 
##      0.0000      0.0000      0.0046     -0.2555 
##    region26    region28    region30    region32 
##      0.1332      0.0000      0.0000      0.0000 
##     private     naceD.E       naceF       naceG 
##     -0.6087      0.1762      1.9175     -0.4556 
##       naceH       naceI       naceJ     naceK.L 
##     -0.5605     -1.0964      0.9216      1.0372 
##       naceM       naceN       naceO       naceP 
##      1.0027     -0.1839      1.4748      0.5371 
##       naceQ     naceR.S       sizeM       sizeS 
##     -0.7113     -0.8136      0.9971      1.5353
```

``` r
round(ipw_est1$selection$coefficients,4)
```

```
## (Intercept)    region04    region06    region08 
##     -0.6528      0.8378      0.1995      0.1048 
##    region10    region12    region14    region16 
##     -0.1576     -0.6099     -0.8415      0.7639 
##    region18    region20    region22    region24 
##      1.1781      0.2225     -0.0375     -0.4067 
##    region26    region28    region30    region32 
##      0.2029      0.5786     -0.6102      0.3274 
##     private     naceD.E       naceF       naceG 
##      0.0590      0.7727     -0.3778     -0.3337 
##       naceH       naceI       naceJ     naceK.L 
##     -0.6517      0.4118     -1.4264      0.0617 
##       naceM       naceN       naceO       naceP 
##     -0.4068      0.8003     -0.6935      1.2510 
##       naceQ     naceR.S       sizeM       sizeS 
##      0.3029      0.2223     -0.3641     -1.0292
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
## 1 single_shift   0.6734515   0.7334882
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
## Running under: macOS Sequoia 15.4
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
## [1] grid      stats     graphics  grDevices utils    
## [6] datasets  methods   base     
## 
## other attached packages:
## [1] ggplot2_3.5.1    nonprobsvy_0.2.0 survey_4.4-2    
## [4] survival_3.8-3   Matrix_1.7-3    
## 
## loaded via a namespace (and not attached):
##  [1] sandwich_3.1-1       generics_0.1.3      
##  [3] lattice_0.22-6       digest_0.6.37       
##  [5] magrittr_2.0.3       evaluate_1.0.3      
##  [7] nleqslv_3.3.5        iterators_1.0.14    
##  [9] fastmap_1.2.0        foreach_1.5.2       
## [11] doParallel_1.0.17    operator.tools_1.6.3
## [13] DBI_1.2.3            scales_1.3.0        
## [15] codetools_0.2-20     cli_3.6.4           
## [17] mitools_2.4          rlang_1.1.5         
## [19] miscTools_0.6-28     munsell_0.5.1       
## [21] splines_4.4.2        withr_3.0.2         
## [23] RANN_2.6.2           yaml_2.3.10         
## [25] tools_4.4.2          parallel_4.4.2      
## [27] ncvreg_3.15.0        dplyr_1.1.4         
## [29] colorspace_2.1-1     maxLik_1.5-2.1      
## [31] vctrs_0.6.5          R6_2.6.1            
## [33] zoo_1.8-13           lifecycle_1.0.4     
## [35] rticles_0.27         MASS_7.3-65         
## [37] pkgconfig_2.0.3      pillar_1.10.1       
## [39] gtable_0.3.6         rsconnect_1.3.4     
## [41] glue_1.8.0           Rcpp_1.0.14         
## [43] xfun_0.51            tibble_3.2.1        
## [45] tidyselect_1.2.1     rstudioapi_0.17.1   
## [47] knitr_1.50           farver_2.1.2        
## [49] htmltools_0.5.8.1    rmarkdown_2.29      
## [51] labeling_0.4.3       formula.tools_1.7.1 
## [53] compiler_4.4.2
```

