## rchemo - Dimension reduction, Regression and Discrimination for Chemometrics  

**Under construction - Non stable test version**

**rchemo** will replace package **rnirs** that will not be developed anymore (only maintenance).

**rchemo** is built on a different logic than **rnirs**, making it more scalable. The predictions are done not inside the main functions but by generic functions **predict**. In the same way, the dimension reductions are done by functions **transform**, etc. The generic functions for model tuning are **gridscore** and **gridcv** (with fast versions for LVs models).

**rchemo** is not push-button oriented. The objective is rather to provide bricks (mainly based on dimension reduction) to build ad'hoc pipelines for predictions in chemometrics or other domains. Only few examples of many possible wrappers are provided in the package.  

### <span style="color:green"> **Dependent packages** </span> 

**rchemo** is dependent to the following packages available on CRAN:

| Package | Which use in rchemo? |
|---|---|
| data.table | Fast data management |
| FNN | Fast search of nearest neighbours |
| signal | Savitsky-Golay derivation, and signal interpolation |

### <span style="color:green"> **Installation** </span> 

Using [**Rstudio**](https://www.rstudio.com/products/rstudio/download/) is recommended for installation and usage

#### <span style="color:green"> 1.  Install package **'remotes'** from CRAN </span>

Use the **Rstudio** menu 

or write in the R console
```{r}
install.packages("remotes")
```

#### <span style="color:green"> 2. Install package **'rchemo'** </span> 

**a) Most recent version**

Write in the R console
```{r}
remotes::install_github("mlesnoff/rchemo", dependencies = TRUE, 
  build_vignettes = TRUE)
```
In case of the following question during installation process:
```{r}
These packages have more recent versions available.
Which would you like to update?"
```
it is recommended to skip updates (usually choice **3** = None)

**b) Any given tagged version**

e.g. with tag "v1.9-0"   (Be carefull, this is not the more recent version)

write in the R console
```{r}
remotes::install_github("mlesnoff/rchemo@v1.9-0", dependencies = TRUE, 
  build_vignettes = FALSE)
```

#### <span style="color:green"> 3. Usage </span>

Write in the R console
```{r}
library(rchemo)
```

### <span style="color:green"> **Author** </span> 

**Matthieu Lesnoff**

- Cirad, [**UMR Selmet**](https://umr-selmet.cirad.fr/en), Montpellier, France

- [**ChemHouse**](https://www.chemproject.org/ChemHouse), Montpellier

**matthieu.lesnoff@cirad.fr**

#### How to cite

Lesnoff, M. 2021. R package rchemo: Dimension reduction, Regression and Discrimination for Chemometrics. https://github.com/mlesnoff/rchemo. CIRAD, UMR SELMET, Montpellier, France




