# GLIOMA CLINICAL TRIAL ANALYSIS

# Dataset
Gliomas are the most common primary tumors of the brain. They can be graded as LGG (Lower-Grade Glioma) or GBM (Glioblastoma Multiforme) depending on the histological/imaging criteria. Clinical and molecular/mutation factors are also very crucial for the grading process. Molecular tests are expensive to help accurately diagnose glioma patients. In this dataset, the most frequently mutated 20 genes and 3 clinical features are considered from TCGA-LGG and TCGA-GBM brain glioma projects.

# Methodology
1) Logistic Regression : IDH1, TP53, PTEN, NF1, PIK3R1, GRIN2A, IDH2 are identified as the significant variables. TP53, PTEN, NF1, PIK3R1, GRIN2A mutation genes increases the log-odds of the outcome meaning cancer grade to be classified as GBM. IDH1, IDH2 are mutated genes which are negatively associated with the outcome meaning cancer grade is less likely to be classified as GBM.
2) Multivariable Adaptive Regression Splines : MARS model used 8 of the 20 total predictors (genes), meaning the other 12 predictors were not deemed useful in this model. IDH1, IDH2, EGFR, NF1 have an inverse relationship with the cancer grade GBM while TP53, PTEN, PIK3R1, GRIN2A genes slightly increase the chance of cancer grade GBM.
3) K-means clustering : Patients with similar gene mutation profiles have been grouped together in each cluster




