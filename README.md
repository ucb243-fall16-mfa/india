# Multiple Factor Analysis in R

> Multiple factor analysis (MFA, also called multiple factorial analysis) is an extension
> of principal component analysis (PCA) tailored to handle multiple data tables that
> measure sets of variables collected on the same observations…

This description is from the paper that this project is based upon: [Multiple factor analysis: principal component analysis for multitable and multiblock data sets](https://www.utdallas.edu/~herve/abdi-WiresCS-mfa-2013.pdf) by Hervé Abdi, Lynne J. Williams and Domininique Valentin (2013)  

### Contents

* [R Code](/master/mfa/R)
* [Unit Tests](mfa/tests)
* [Slides](slides/slides.md)
* [Vignettes](/mfa/vignettes)
* [Shiny](https://mfashinyapp.shinyapps.io/MFA_Shiny_App/)
* [License](./LICENSE.txt)

### Quick Start Guide

**In Git Bash**

```
git clone https://github.com/fussballball/stat243FinalProject.git
```

**In R**

```R
setwd('INSERT_PATH_HERE/stat243FinalProject')
library(devtools)
install('mfa')
```



### Package Developers

* Dario Cantore
* Josiah Davis
* Yanli Fan
* Yoni Ackerman




### References

* [Multiple factor analysis: principal component analysis for multitable and multiblock data sets](https://www.utdallas.edu/~herve/abdi-WiresCS-mfa-2013.pdf) by Hervé Abdi, Lynne J. Williams and Domininique Valentin (2013)  
* [Singular Value Decomposition (SVD) and Generalized Singular Value Decomposition (GSVD)](http://www.cimat.mx/~alram/met_num/clases/Abdi-SVD2007-pretty.pdf) by Hervé Abdi (2007)