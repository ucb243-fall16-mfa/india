Multiple Factor Analysis in R
========================================================
author: Dario Cantore, Josiah Davis, Yanli Fan, Yoni Ackerman
date: 12/2/2016
autosize: false

Agenda
========================================================
1. The Algorithm
2. Case Study
3. Our Package
4. Appendix: GSVD

The Algorithm
========================================================

In a nutshell
========================================================

Multiple factor analysis... is an extension of principal component analysis tailored to handle multiple data tables that measure sets of variables collected on the same observations^1

What is the Multiple Factor Analysis Algorithm?
========================================================

What is the Multiple Factor Analysis Algorithm?
========================================================
1. Collect multiple tables of data related to the same items
2. Calculate the singular values of each table
3. Concatenate the data together tables      (Note: Strange use of "together"?)
4. Normalize and weight the data       (Note: weight or weigh?)
5. Compute a generalized singular value composition on the combined table

_Note:_ k refers to the number of tables, i refers to the number of observations, and j refers to the number of variables.

1: Collect multiple tables of data related to the same items
========================================================
![](slides-figure/step1.png)

Data need not have the same number of variables for in each table (Note: delete for?), however, (replace by "but" (without comma)) all data should have the same number of observations/items.

2: Calculate the singular values of each table
========================================================
![](slides-figure/step2.png)

Singular Value Decomposition is a method of factoring a rectangular matrix into three matrices that consolidate information about the variance in the data.

3: Concatenate the tables together
========================================================
![](slides-figure/step4.png)

At this point, all data points are contained in a single table but have not yet been scaled.

4: Normalize and weight the data
========================================================
![](slides-figure/step3.png)

In this step, each table is divided it's (Note: its?) first singular value and each observation can also be weighted. (This can be thought of as analogous to scaling variables by their variance.)

Step 5. Compute a generalized singular value composition on the combined table (Note: Add a point at the end to be consistent?)
========================================================
![](slides-figure/step5.png)

The Generalized Singular Value decomposition consists of taking the singular value composition of the previously normalized-weighted data. (See appendix for details). 


The Output
========================================================

There are three major types of information to analyze:  

| Information  |  Description |
|---|---|
| Scores  | Consensus view, and how each table differs from the conesensus |
| Contributions | Most important elements: variables, observations, and tables |
| Coefficients | Similarity between the tables |

Case Study
========================================================



Wine Tasting
========================================================
Ten wine assessors were asked to rate 12 different Souvignon blanc's (Note: Sauvignon Blancs) by various criteria (, among them. Because not all are having the same criteria?):
- cat pee
- passion fruit
- green pepper
- mineral
 
There were twelve different wines: four from New Zealand, four from California, and four from France.

Exploratory Data Analysis
========================================================

Sample questions:

1. What wines are most similar to each other and what wine experts differed most from this consensus?
2. Which wine characteristics are the most important in explaining differences?
3. Which wine experts are most similar to each other?

Similarities and Differences
========================================================

![](slides-figure/compromise_scores.png) 

The Factor Scores reveal similarities and differences:
- New Zealand wines are very different from French wines
- French wines 1 and 2 are very similar to each other
- (Note: There is) Greater variety in the Calfornia wines than French or New Zealand wines


Deviations from the Consensus
========================================================

![](slides-figure/partial_factor_scores.png)

The Partial Factor Scores provide a deeper understanding how different wine experts deviated from the consensus:
- Some wine assessors (e.g., 4) thought California(Note: Californian) wines were (Note: are) more similar than the rest of the group
- A few experts (e.g., 7) notices (notice or noticed) less regional distinctions between California and New Zealand wines (californian, new zealandian?)

Our Package
========================================================


Our Package
========================================================
To get started, install the mfa package from github, and call mfa with your dataset and a list of variables.


```r
library(mfa)
mfa1 <- mfa(data, sets)
```

Calling the plot function will give you summary charts to interpret your data.


```r
plot(mfa1)
```


Read our [vignette](https://github.com/fussballball/stat243FinalProject) for detailed tutorials

Available Methods 
========================================================
Here is a list of methods that the package currently supports.

| Method  |  Description |
|---|---|
| `mfa()`  |  Create the mfa object |
|  `plot()` |  Summary plots |
|  `print()` |  Basic information about the mfa object |
|  `contribution_obs_dim()` | Importance of each observation  |
|  `contribution_var_dim()` |  Importance of each variable  |
|  `contribution_table_dim()` |  Importance of each table  |
|  `summary_eigenvalues()` |  Basic information about the eigenvalues  |
|  `RV()` | Similarity between two tables  |
|  `RV_table()` | Simularity between multiples tables  |

Appendix: GSVD
========================================================
The goal of Generalized Singular Value Decomposition (GSVD) is to conduct a  singular value decomposition of the form:

$$A = \tilde{U} \tilde{\Delta} \tilde{V}^\mathsf{T}$$

Notation: 
- $A$ is the data
- $\tilde{U}$ and $\tilde{V}$ are left and right singular vectors of $A$
- $\tilde{\Delta}$ is a diagonal matrix of singular values
- $M$ is a diagonal matrix of observation weights
- $W$ is a diagonal matrix of column weights

Reference: [tutorial by Hervé Abdi](http://www.cimat.mx/~alram/met_num/clases/Abdi-SVD2007-pretty.pdf)


Appendix: GSVD (Continued)
========================================================

The first step is to calculate $\tilde{A}$:

$$\tilde{A} = M^{-1/2} A W^{-1/2}$$

Note: The $W^{-1/2}$ is roughly equivalent to dividing by the singular value

The second step is to calculate the standard SVD of  $\tilde{A}$: 

$$\tilde{A} = P\Delta Q^\mathsf{T}$$

The third step is to calculate $\tilde{U}$, $\tilde{\Delta}$, $\tilde{V}$

$$\tilde{U}=M^{-1/2}P, \tilde{V}=W^{-1/2}Q, \tilde{\Delta}=\Delta$$

References
========================================================

1. WIREs Comput Stat 2013. doi: 10.1002/wics.1246
