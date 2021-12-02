## NEWS for package **rchemo**

## **Version 0.0-17**

- Function **sampclas** has been renamed to **sampcla** and modified.

- Correction of help page of **plslda**


## **Version 0.0-16**

- Argument **rob** in functions **scordis** and **odis** has been set to **TRUE**.



## **Version 0.0-15**

- Internal modification in all the LWPLS functions: add a stabilization in computed weights



## **Version 0.0-14**

- Correction of a small bug in checkdupl

- Argument **cri** has been added in all the LWPLS functions.



## **Version 0.0-13**

- Functions using RBF, polynomial and sigmoid kernels

    - parameter "sigma" has been replaced by parameter "gamma" (= .5 * 1 / sigma^2)
    - parameter "scale" has been renamed "gamma"
    - parameter "offset" has been renamed "coef0"



## **Version 0.0-12**

- Function **gaprm** has been renamed to **rmgap**.

- Bug correction:

  - **xfit** returned non-correct values for PLS models, which had impact on function **odis**. The bug is fixed now. It was created during the transfer from rnirs to rchemo (**odis** in rnirs returns correct values).






## **Version 0.0-11**

- New functions

  - **rpd**, **rpdr** : Ratios performance to deviation 


- Modifications of functions

  - **mse**


## **Version 0.0-10**

- New functions

  - **cglsr**: CGLSR algorithm (Björck 1996)
  - **dfplsr_cg**, **dfplsr_cov**, **dfplsr_div**: Model complexity estiamtion for PLSR
  - **aicplsr**: Cp and AIC for PLSR models
  
- Code cleaning



## **Version 0.0-9**

- A bug was fixed in function **kpca**




## **Version 0.0-8**

- Code cleaning




## **Version 0.0-7**

- New functions

  - **plsrda_agg**, **plslda_agg**, **plsqda_agg**

  - **lwplslda**, **lwplsqda**




## **Version 0.0-6**

- Modification of functions

  - **eposvd**: changes of arguments



## **Version 0.0-5**

- Modifications of functions

  - **plsr_agg**, **lwplsr_agg**: The syntax of argument "nlv" has been modified. See the examples in the corresponding help pages.




## **Version 0.0-4**

- New functions:

  - **gaprm**: Remove vertical gaps in spectra (e.g. for ASD)
  
  - **eposvd**: Pre-processing data by external parameter orthogonalization (EPO; Roger et al 2003) 

- New dataset: **asdgap**







## **Version 0.0-3**

- Modifications of functions

  - Internal computations in **rr** and **krr** 
  - summary.Pca: now requires argument "X"
  - Function using weights: argument "weights" changed of position

- Code cleaning




## **Version 0.0-2**

- **plotxy**: Modification of default value in argument "asp"





## **Version 0.0-1**

- First version of the package 






