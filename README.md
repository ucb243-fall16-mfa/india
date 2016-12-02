# Multiple Factor Analysis in R

> Multiple factor analysis (MFA, also called multiple factorial analysis) is an extension
> of principal component analysis (PCA) tailored to handle multiple data tables that
> measure sets of variables collected on the same observations…

This description is from the paper that this project is based upon: [Multiple factor analysis: principal component analysis for multitable and multiblock data sets](https://www.utdallas.edu/~herve/abdi-WiresCS-mfa-2013.pdf) by Hervé Abdi, Lynne J. Williams and Domininique Valentin (2013)  

### Contents

* [R Code](mfa/R)
* [Unit Tests](mfa/tests)
* [Slides](slides/slides.md)
* [Vignettes](/mfa/vignettes)
* [Shiny](https://mfashinyapp.shinyapps.io/MFA_Shiny_App/)
* [License](./LICENSE.txt)

### Quick Start Guide

1. Clone the repository of [download a zip file](https://github.com/fussballball/stat243FinalProject/archive/master.zip)
2. Extract the zip file (If necessary)
3. Make the repository your working directory in R (i.e. `setwd('INSERT_PATH_HERE/stat243FinalProject')`)
4. Load the `devtools` library and run `devtools::install('mfa')`

### Package Developers

* Dario Cantore
* Josiah Davis
* Yanli Fan
* Yoni Ackerman


### References

* [Multiple factor analysis: principal component analysis for multitable and multiblock data sets](https://www.utdallas.edu/~herve/abdi-WiresCS-mfa-2013.pdf) by Hervé Abdi, Lynne J. Williams and Domininique Valentin (2013)  
* [Singular Value Decomposition (SVD) and Generalized Singular Value Decomposition (GSVD)](http://www.cimat.mx/~alram/met_num/clases/Abdi-SVD2007-pretty.pdf) by Hervé Abdi (2007)