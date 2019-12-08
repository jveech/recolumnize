#' one_hot_encode
#'
#' Use one hot encoding to handle meaningless or transposed columns
#' @param df A dataframe that we want to one hot encode
#' @param encode_cols The names of the columns that should be encoded, default is all columns
#' @param keep "exists" for 1 if value exists, 0 else. "sum" for sum of number of appearances in a row, default exists
#' @param min_occurences minimum number of appearances in the data before column is added, default 1.
#'
#' @return dataframe with encode_cols removed and replaced with numeric columns
#' @export
#'
#' @examples
#' (mat <- matrix(letters[sample(1:26,20,replace = T)],5,4))
#' one_hot_encode(mat)
#' one_hot_encode(mat, min_occurences = 2) # keeps only values that appear in 2 or more rows
#' one_hot_encode(mat, keep = "sum") # stores number of times each value appears in a given row
#' one_hot_encode(mat, encode_cols = c(2,3)) # encode only certain columns and leave the rest of them  in place
one_hot_encode <- function(df, encode_cols = NULL, keep = "exists", min_occurences = 1) {
  num_rows <- nrow(df)
  num_cols <- ncol(df)
  # get subsets of df so we can work on only the part we want to encode
  out <- column_difference(encode_cols, colnames(df), num_cols)
  encode_cols  <- out$encode_cols
  keep_cols <- out$keep_cols
  # flatten out the part of the dataframe we want to encode. then extract out all the unique values, these will be our columns
  values <- unique(as.vector(as.matrix(df[,encode_cols, drop = F])))

  num_new_cols <- length(values)
  encoded <- matrix(0, nrow = nrow(df), ncol = num_new_cols)

  if (keep == "sum") {
    for (i in 1:length(values)) {
      encoded[,i] <- rowSums(df[ , !keep_cols, drop = F] == values[i], ) # store number of times a value appears in observation i
    }
  }

  else {
    for (i in 1:length(values)) {
      encoded[,i] <- as.numeric(rowSums(df[,!keep_cols, drop = F] == values[i], ) > 0) # store 1 if the value appears in observation i, 0 else
    }
  }
  keep_features <- colSums(encoded) >= min_occurences # keep only those values that occur a certain number of times in the dataset
  values <- values[keep_features] # remove rare values
  encoded <- encoded[ , keep_features] # remove rare values from the encoding matrix
  # create new data frame containing the columns we are keeping from the old dataframe and new columns for each value we are encoding
  encoded_df <- data.frame(cbind(df[ , keep_cols, drop = F], encoded))
  # if original data has no column names, create some so that the new names we add still line up
  if(is.null(colnames(df)) & ncol(df[,keep_cols,drop=F]) > 0) {
    old_names <- paste("col",(1:ncol(df[,keep_cols,drop=F])),sep="")
  }
  else {
    old_names <- colnames(df[ , keep_cols, drop = F])
  }
  colnames(encoded_df) <- c(old_names, values) # fix column names
  return(encoded_df)
}
