# <span style="color:grey70"> **R Package rchemo** </span>

**https://github.com/mlesnoff/rchemo**

#### <span style="color:#1589F0"> PCA </span>

- ***pcaeigen*** Eigen decomposition
- ***pcaeigenk*** Eigen for wide matrices (kernel form)
- ***pcasvd*** SVD decomposition
- ***pcanipals*** Nipals
- ***pcanipalsna*** Nipals allowing missing data
- ***kpca*** Non Linear Kernel PCA  (KPCA) (Scholkopf et al. 2002)

##### Auxiliary (works also for PLS)

- ***scordis*** Score distances (SD) for a score (T) space
- ***odis*** Orthogonal distances (OD) for a score (T) space
- ***xfit*** Matrix fitting from score (T) and loadings (P) matrices 

#### <span style="color:#1589F0;"> REGRESSION </span>

#### Linear

- ***lmr*** Multiple linear regression
- ***cglsr***: CGLSR algorithm for ill-conditionned systems (Björck 1996)

#### Partial least squares

- ***plskernel*** "Improved Kernel #1" (Dayal & McGregor 1997)
- ***plsnipals*** Nipals
- ***plsrannar*** Kernel version for wide matrices (Rannar et al. 1994)
- ***kplsr*** Non linear kernel PLSR (KPLSR) (Rosipal & Trejo 2001)
- ***dkplsr*** Direct KPLSR (Bennett & Embrechts 2003)

*Auxiliary*

- ***dfplsr_cg***, ***dfplsr_cov***, ***dfplsr_div***: Model complexity estiamtion for PLSR
- ***aicplsr***: Cp and AIC for PLSR models

#### Ridge

- ***rr*** Ridge Regression (RR)
- ***krr*** Non linear kernel ridge regression (KRR) = Least-square SVM regression (LS-SVMR)
- ***dkrr*** Direct KRR

#### Support vector machine

- ***svmr*** SVM regression (SVMR)

#### K-nearest-neighbors

- ***knnr*** KNN regression (KNNR)
- ***lwplsr*** KNN Locally weighted PLSR (KNN-LWPLSR)

#### Ensemblist methods

*Averaging PLSR models with different numbers of LVs*

- ***plsr_agg*** PLSR-AGG
- ***lwplsr_agg*** KNN-LWPLSR-AGG  
  
#### <span style="color:#1589F0"> DISCRIMINATION ANALYSIS </span>
  
#### Factorial discrimination analysis (FDA)

- ***fda*** Eigen decomposition of the compromise "inter/intra"
- ***fdasvd*** Weighted SVD decomposition of the class centers

#### On predicted Y-dummy table

- ***lmrda*** DA on LMR prediction (LMR-DA)
- ***plsrda*** DA on PLSR prediction (PLSR-DA = common "PLSDA")
- ***kplsrda*** DA on KPLSR prediction (KPLSR-DA)
- ***rrda*** DA on RR prediction (RR-DA)
- ***krrda*** DA on KRR prediction (KRR-DA)

#### Probabilistic

- ***lda*** Linear discriminant analysis (LDA)
- ***qda*** Quadratic discriminant analysis (QDA)
- ***plslda*** LDA on PLS latent variables (LVs) (PLS-LDA)
- ***plsqda*** QDA on PLS LVs (PLS-QDA)

#### Support vector machine

- ***svmda*** SVMDA (= SVMC)

#### K-nearest-neighbors

- ***knnda*** KNN-DA
- ***lwplsrda*** KNN Locally weighted PLSR-DA (KNN-LWPLSR-DA)
- ***lwplslda*** KNN Locally weighted PLS-LDA/QDA (KNN-LWPLS-LDA/QDA)

#### Ensemblist methods

*Averaging PLSR models with different numbers of LVs*

- ***plsrda_agg*** PLSRDA-AGG
- ***lwplsrda_agg*** KNN-LWPLSR-DA-AGG
- ***lwplslda_agg*** KNN-LWPLS-LDA-AGG
- ***lwplslda_agg*** KNN-LWPLS-QDA-AGG

#### <span style="color:#1589F0"> TUNING MODELS </span>

#### Validation dataset

- ***gridscore*** Any model
- ***gridscorelv*** Models with LVs (faster)
- ***gridscorelb*** Models with ridge parameter (faster)
  
#### Cross-validation (CV) 

- ***gridcv*** Any model
- ***gridcvlv*** Models with LVs (faster)
- ***gridcvlb*** Models with ridge parameter (faster)  

*Auxiliary*

- ***segmkf*** Building segments for K-fold CV
- ***segmts*** Building segments for test-set CV

#### Performance scores

- ***msep*** MSEP
- ***rmsep*** RMSEP
- ***sep*** SEP
- ***bias*** Bias
- ***r2*** R2
- ***cor2*** Squared correlation 
- ***rpd***, ***rpdr*** Ratio of performance to deviation
- ***mse*** Summary for regression
- ***err*** Classification error rate

#### Heuristic  
  
- ***selwold*** Wold's criterion for models with LVs  

#### <span style="color:#1589F0"> SELECTION OF VARIABLES </span>

- ***covsel*** COVSEL algorithm (Roger et al. 2011)

#### <span style="color:#1589F0"> DATA MANAGEMENT </span>

#### Pre-processing

- ***detrend*** Polynomial detrend transformation
- ***snv*** Standard-normal-deviation transformation
- ***mavg*** Smoothing by moving average
- ***savgol*** Savitsky-Golay filtering (derivation)
- ***dderiv*** Derivation by finite difference
- ***xinterp*** Resampling of spectra by interpolation methods
- ***rmgap** Remove vertical gaps in spectra (e.g. for ASD)
- **eposvd** Pre-processing data by external parameter orthogonalization (EPO; Roger et al 2003) 

#### Sampling

- ***sampks*** Kennard-Stone sampling 
- ***sampdp*** Duplex sampling 
- ***sampcla*** Within-class stratified sampling

#### Checking

- ***checkna*** Find and count NA values in a data set
- ***plotxna*** Plotting missing data in a matrix
- ***checkdupl*** Find duplicated row observations between two data sets 
- ***rmdupl*** Remove duplicated row observations between two data sets

#### Summary

- ***aggmean*** Centers of classes
- ***dtagg*** Summary statistics with data subsets
- ***summ*** Summary of the quantitative variables of a data set

#### Multi-block 

- ***mblocks*** Makes a list of blocks
- ***hconcat*** Horizontal block concatenation 
- ***blockscal*** Block autoscaling

#### Datasets

- ***asdgap** ASD spectra with vertical gaps
- ***cassav*** Tropical shrubs
- ***forages*** Tropical forages
- ***octane*** Gazoline "octane" dataset
- ***ozone*** Los Angeles "ozone" pollution (1976) dataset

#### <span style="color:#1589F0"> GRAPHICS </span>

- ***plotsp*** Plotting spectra, loadings, or more generally row observations of a data set
- ***plostsp1*** Same as  ***plotsp*** but one-by-one row
- ***plotxy*** 2-d scatter plot
- ***plotjit*** Jittered plot
- ***plotscore*** Plotting error rates of prediction models

#### <span style="color:#1589F0"> AUXILIARY </span>

- ***dmnorm*** Multivariate normal probability density
- ***dummy*** Dummy table
- ***euclsq***, ***euclsq_mu** Euclidean distance matrices
- ***mahsq***, ***mahsq_mu** Mahalanobis distance matrices
- ***getknn*** KNN selection
- ***krbf***, ***kpol***, ***ktanh*** Gram matrices for different kernels
- ***headm*** Print the first part of a matrix or data frame
- ***locw*** Working function for locally weighted models
- ***matB***, ***matW*** Between and within covariance matrices
- ***pinv*** Moore-Penrose pseudo-inverse
- ***sourcedir*** Source every R functions in a directory
- ***wdist*** Weights for distances
- Additional working functions in file **zfunctions.R**

#### <span style="color:#1589F0"> AUTHOR </span>

**Matthieu Lesnoff**

- Cirad, [**UMR Selmet**](https://umr-selmet.cirad.fr/en), Montpellier, France

- [**ChemHouse**](https://www.chemproject.org/ChemHouse), Montpellier

**matthieu.lesnoff@cirad.fr**

#### How to cite

Lesnoff, M. 2021. R package rchemo: Dimension reduction, Regression and Discrimination for Chemometrics. https://github.com/mlesnoff/rchemo. CIRAD, UMR SELMET, Montpellier, France
