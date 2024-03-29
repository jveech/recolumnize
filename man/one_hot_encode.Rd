% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/one_hot_encode.R
\name{one_hot_encode}
\alias{one_hot_encode}
\title{one_hot_encode}
\usage{
one_hot_encode(df, encode_cols = NULL, keep = "exists",
  min_occurences = 1)
}
\arguments{
\item{df}{A dataframe that we want to one hot encode}

\item{encode_cols}{The names of the columns that should be encoded, default is all columns}

\item{keep}{"exists" for 1 if value exists, 0 else. "sum" for sum of number of appearances in a row, default exists}

\item{min_occurences}{minimum number of appearances in the data before column is added, default 1.}
}
\value{
dataframe with encode_cols removed and replaced with numeric columns
}
\description{
Use one hot encoding to handle meaningless or transposed columns
}
\examples{
(mat <- matrix(letters[sample(1:26,20,replace = T)],5,4))
one_hot_encode(mat)
one_hot_encode(mat, min_occurences = 2) # keeps only values that appear in 2 or more rows
one_hot_encode(mat, keep = "sum") # stores number of times each value appears in a given row
one_hot_encode(mat, encode_cols = c(2,3)) # encode only certain columns and leave the rest of them  in place
}
