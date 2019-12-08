# recolumnize

## Introduction
The intended use cases for this package occur when dealing with very messy data found "in the wild." Sometimes, one encounters data that has columns that are transposed or in a meaningless order. That is, a value found in row 1 column 1 may have no connection to the value found in row 2 column 1. Often this occurs due to data entry errors, data not intended to be stored in table format, or from user/survey data that has not been properly validated (e.g., a user puts their ZIP code in the state field and their state in their address 2 field, etc.)

## Installation (with vignette -- may take some time)
```
devtools::install_github("jveech/recolumnize", build = TRUE, build_vignettes = T, force = T)
```

## Installation (without vignette -- see ADD LINK HERE)
```
devtools::install_github("jveech/recolumnize")
```

## Example: one_hot_encode()
```
library(recolumnize)
(mat <- matrix(letters[sample(1:26,20,replace = T)],5,4))
one_hot_encode(mat)
one_hot_encode(mat, min_occurences = 2) # keeps only values that appear in 2 or more rows
one_hot_encode(mat, keep = "sum") # stores number of times each value appears in a given row
one_hot_encode(mat, encode_cols = c(2,3)) # encode only certain columns and leave the rest of them  in place

```

## Example: recategorize()
```
library(recolumnize)
my_mat <- matrix(c("obs1","obs2","obs3","a","a","k","d","o","e"),ncol = 3, nrow = 3)
my_mat <- as.data.frame(my_mat)
colnames(my_mat) <- c("name","1","2")
dict <- rep("consonant",26)
names(dict) <- letters
dict[c("a","e","i","o","u")] <- "vowel"
my_mat
recategorize(my_mat,encode_cols = c(2,3), category_dictionary = dict)
```

## Example: best_categories_approximate()/best_categories_brute_force()
```
library(recolumnize)
dict2 <- rep("consonant",26)
names(dict2) <- letters
dict2[c("a","e","i","o","u")] <- "vowel"
probs <- matrix(0,nrow = 26, ncol = 2)
colnames(probs) <- c("vowel","consonant")
rownames(probs) <- letters
probs[,1] <- abs((dict2 == "vowel") -.001)
probs[25,1] <- 0.25
probs[23,1] <- 0.05
probs[,2] <- 1-probs[,1]
mat <- matrix(c("a","w","x","y","c","w","r","r"),nrow = 4, ncol = 2, byrow=T)
mat
best_categories_approximate(mat,probs)
best_categories_brute_force(mat,probs)
```
