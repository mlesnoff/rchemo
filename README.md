## rchemo - Dimension reduction, Regression and Discrimination for Chemometrics  
## <span style="color:grey70"> **Version 0.0-1** </span> 

**rchemo** provides elementary functions (mainly focusing on methods of dimension reduction or regularization for high dimensional data) to build ad'hoc pipelines for predictions in chemometrics or other domains. Only few examples of many possible pipelines useful in chemomtrics and machine learning are provided in the package.  

Generic functions such as **transform**, **predict**, **coef** and **summary** are available. The tuning of the prediction models is facilitated by functions **gridscore** (validation dataset) and **gridcv** (cross-validation), with specific fast versions for models based on latent variables (LVs) and ridge regularization.

## <span style="color:green"> **Available functions** </span> 

**Click** [**HERE**](https://github.com/mlesnoff/rchemo/blob/master/doc/rchemo_functions_github.md) **to see the list of the available functions** 

After the package installation, all the functions have a help page with documented examples. 

## <span style="color:green"> **News** </span> 

Click [**HERE**](https://github.com/mlesnoff/rchemo/blob/master/inst/NEWS.md) to see **what changed** in the last version 

or write in the R console
```{r}
news(package = "rchemo")
```

## <span style="color:green"> **Dependent packages** </span> 

**rchemo** is dependent to the following packages available on CRAN:

| Package | Which use in rchemo? |
|---|---|
| data.table | Fast data management |
| FNN | Fast search of nearest neighbours |
| signal | Savitsky-Golay derivation, and signal interpolation |
| e1071 | SVM models |

## <span style="color:green"> **Installation** </span> 

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
  build_vignettes = TRUE)
```

#### <span style="color:green"> 3. Usage </span>

Write in the R console
```{r}
library(rchemo)
```

## <span style="color:green"> **Author** </span> 

**Matthieu Lesnoff**

- Cirad, [**UMR Selmet**](https://umr-selmet.cirad.fr/en), Montpellier, France

- [**ChemHouse**](https://www.chemproject.org/ChemHouse), Montpellier

**matthieu.lesnoff@cirad.fr**

#### How to cite

Lesnoff, M. 2021. R package rchemo: Dimension reduction, Regression and Discrimination for Chemometrics. https://github.com/mlesnoff/rchemo. CIRAD, UMR SELMET, Montpellier, France




