# Steroid metabolomics in children
## Aim
Discriminating Adrenocortical tumor carcinomas from Adrenocortical tumor Adenomas with the help of Machine-Learning approaches

## Method
* Spontaneous urine samples are collected from tumor patients before operation.
* 36 Sterroid metabolites are quantified using a targeted GC/MS approach.
* Calcutation of z-score against the control group.

## Material
* 40 Tumor patients : 20 Carcinomas, 17 Adenomas and 3 Unknown diagnose adrenocortical tumor
* 127 control : 63 boys and 64 girls divided into nine age groups.

## Calculating Z-score
The standard score (Z-score) for the tumor patient is calculated against same age groups and same sex, inorder to make the subsequent analysis independent from sex and age.
```
Z_score = x - mean (x)/ std
```
