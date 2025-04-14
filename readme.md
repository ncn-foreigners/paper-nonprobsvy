# "nonprobsvy -- An R package for modern methods for non-probability surveys"

-   [the current version of the paper](nonprobsvy-paper/nonprobsvy-paper.pdf)
-   [Arxiv version](https://arxiv.org/abs/2504.04255)

## How to cite

Arxiv version: 

``` tex
@misc{chrostowski2025,
      title={nonprobsvy -- An R package for modern methods for non-probability surveys}, 
      author={Łukasz Chrostowski and Piotr Chlebicki and Maciej Beręsewicz},
      year={2025},
      eprint={2504.04255},
      archivePrefix={arXiv},
      primaryClass={stat.ME},
      url={https://arxiv.org/abs/2504.04255}, 
}
```

## Acknowledgements

The authors' work has been financed by the National Science Centre in
Poland, OPUS 20, grant no. 2020/39/B/HS4/00941.

## Code for reproduction of results

The code runs about 5 minutes on my laptop
(`Time difference of 4.945334 mins`)

``` r
b <- Sys.time()
file.copy("nonprobsvy-paper/nonprobsvy-paper.pdf", "submission/nonprobsvy-paper.pdf", overwrite= TRUE)
knitr::purl("nonprobsvy-paper/nonprobsvy-paper.Rmd", "submission/code-new.R", documentation = 0)
file.append("submission/code-new.R", "submission/session-info.R") 
knitr::spin("submission/code-new.R")
file.rename(c("submission/code-new.R", "code-new.md", "code-new.html"),
            paste0("submission/", c("code.R", "code.md", "code.html")))
unlink("figure", recursive = TRUE)
Sys.time() - b
```

``` r
> sessionInfo()
R version 4.4.2 (2024-10-31)
Platform: aarch64-apple-darwin20
Running under: macOS Sequoia 15.3.2

Matrix products: default
BLAS:   /System/Library/Frameworks/Accelerate.framework/Versions/A/Frameworks/vecLib.framework/Versions/A/libBLAS.dylib 
LAPACK: /Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/lib/libRlapack.dylib;  LAPACK version 3.12.0

locale:
[1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
```
