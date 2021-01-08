# Intro To Categorical Code

This is an RStudio project that uses the `bookdown` R package to make booklet with some of the formulas, tables and code for Alan Agresti's _Introduction to Categorical Data Analysis_ 3rd Edition.


## To build the booklet

1. Download all the files
2. Open the `IntroToCategoricalCode.Rproj` file with R Studio
3. Install the extra packages used to help the aesthetics of the booklet:

```
extra <- c("rmarkdown", "bookdown", conflicted", "tidyverse", "kableExtra", 
   "ggthemes", "RColorBrewer", "officer", "flextable", "igraph")

install.packages(extra)
```

4. Install the packages used in the book itself:
    + The code below uses the order of appearance of analysis packages.  You want to install all of them or at least all of the earlier chapters.
    + `detectseparation` is a new package since the 3rd edition 
    + the last two lines are used to install a package that is not on CRAN:

```
chapter1 <- c("binom", "exactci", "PropCIs")
chapter2 <- c("epitools", "gmodels", "vcd", "vcdExtra")
chapter3 <- c("gam", "car", "statmod")
chapter4 <- c("mfx", "pROC", "plotROC")
chapter5 <- c("MASS", "leaps", "bestglm", "profileModel", "detectseparation"
              "MCMCpack", "logistf")
chapter6 <- c("VGAM")
chapter8 <- c("gee", "multgee", "psych")
chapter9 <- c("geepack")
chapter10 <- c("lme4", "poLCA")
chapter11 <- c("rpart", "rpart.plot", "gplots", "glmnet")

install.packages(c(chapter1, chapter2, chapter3, chapter4, chapter5, chapter6,
  chapter7, chapter8, chapter9, chapter10, chapter11))

if (!requireNamespace("devtools")) install.packages("devtools")
devtools::install_github("tjmckinley/BayesOrd")
```

5. Push the Build Book button (on the Build Tab) in the upper right window pane.
    + If you don't see the build Tab restart R Studio.

## Chapter 99

Chapter 99 contains Ray's cheetsheet for R Markdown.

## The file called ocAME.R
This repository contain a function called `ocAME` which Ray extracted from http://users.stat.ufl.edu/~aa/articles/agresti_tarantola_appendix.pdf.
    
