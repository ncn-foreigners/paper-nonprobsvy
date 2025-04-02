#  "nonprobsvy -- An R package for modern methods for non-probability surveys"

+ [the current version of the paper](nonprobsvy-paper/nonprobsvy-paper.pdf)
+ [Arxiv version]()

## How to cite

Arxiv version: `TBA`

```tex
TBA
```


## Acknowledgements

The authors' work has been financed by the National Science Centre in Poland, OPUS 20, grant no. 2020/39/B/HS4/00941. 

## Code for reproduction of results

```{r}
file.copy("nonprobsvy-paper/nonprobsvy-paper.pdf", "submission/nonprobsvy-paper.pdf", overwrite= TRUE)
knitr::purl("nonprobsvy-paper/nonprobsvy-paper.Rmd", "submission/code-new.R", documentation = 0)
file.append("submission/code-new.R", "submission/session-info.R") 
knitr::spin("submission/code-new.R")
file.rename(c("submission/code-new.R", "code-new.md", "code-new.html"),
            paste0("submission/", c("code.R", "code.md", "code.html")))
unlink("figure", recursive = TRUE)

```
