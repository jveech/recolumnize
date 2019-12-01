#' Title
#'
#' @param df df dataframe that contains the data we want to recategorize
#' @param encode_cols encode_cols which columns should be altered
#' @param category_dictionary named list with a mapping from values to the categories they belong to
#' @param ignore_unknown_values T if unknown values should be ignored, F if they should be one hot encoded
#' @param handle_duplicate_categories "first" takes first value in a certain category, "last" takes last value
#'
#' @return
#' @export
#'
#' @examples
#' my_mat <- matrix(c("a","b","c","e","d","o"),ncol = 2, nrow = 3, byrow=TRUE)
#' dict <- c("v","c","c","c","v","c",
#' "c","c","v","c","c","c","c","c","v",
#' "c","c","c","c","c","v","c","c","c","c","c")
#' names(dict) <- letters
#' recategorize(my_mat, category_dictionary = dict)
recategorize <- function(df, encode_cols = NULL, category_dictionary, ignore_unknown_values = T, handle_duplicate_categories ="first") {
  #TODO compatability checks for category_dictionary
  out <- column_difference(encode_cols, names(df))
  encode_cols  <- out$encode_cols
  keep_cols <- out$keep_cols

  groups <- unique(dict)
  num_cats <- length(unique(dict))
  categ_indices <- t(apply(
    cbind(matrix(match(sapply(df,category_from_dictionary,category_dictionary,USE.NAMES=F),
                       groups),dim(df)),
                                 matrix(1:num_cats,nrow=nrow(df),ncol=num_cats, byrow=T)),1,unique))
  ordering <- t(apply(categ_indices,1,order))
  for (i in 1:nrow(df)) {
    df[i,] <- df[i, ordering[i,]]
  }
  colnames(df) <- groups
  return(df)
}
