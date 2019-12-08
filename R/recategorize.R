#' Recategorize
#'
#' Put values into sensible categories
#' @param df dataframe that contains the data we want to recategorize
#' @param category_dictionary vector with names consisting of the keys to be looked up and values consisting of the category
#' @param encode_cols which columns should be altered
#' @param handle_duplicate_categories "first" takes first value in a certain category, "last" takes last value
#'
#' @return dataframe with encode_cols replaced with category columns (NAs are populated if no value in that category is found)
#' @export
#'
#' @examples
#' my_mat <- matrix(c("obs1","obs2","obs3","a","a","k","d","o","e"),ncol = 3, nrow = 3)
#' my_mat <- as.data.frame(my_mat)
#' colnames(my_mat) <- c("name","1","2")
#' dict <- rep("consonant",26)
#' names(dict) <- letters
#' dict[c("a","e","i","o","u")] <- "vowel"
#' my_mat
#' recategorize(my_mat,encode_cols = c(2,3), category_dictionary = dict)
recategorize <- function(df,category_dictionary, encode_cols = NULL,
                         handle_duplicate_categories ="first") {
  # get subsets that we need to work on
  out <- column_difference(encode_cols, colnames(df), ncol(df))
  encode_cols  <- out$encode_cols
  keep_cols <- out$keep_cols
  to_encode <- as.matrix(df[,encode_cols,drop=F])

  # store categories
  groups <- unique(category_dictionary)
  # reorder each row
  to_encode <- t(apply(to_encode, 1, reorder_row_by_group, category_dictionary, groups, handle_duplicate_categories))
  # fix names
  colnames(to_encode) <- groups
  df <- cbind(df[,keep_cols,drop=F], to_encode)
  return(df)
}

# function returns reordered row with each column corresponding to a category in dict, NAs are placed where no value is found
reorder_row_by_group <-  function(vec, dict, groups, handle_duplicate_categories) {
  # get indices from each value in the row
  group_indices <- match(as.character(dict[vec]),groups)
  # put NAs in duplicated categories
  group_indices[duplicated(group_indices, fromLast = handle_duplicate_categories == "last")] <- NA

  remaining_indices <- (1:length(groups))[!1:length(groups) %in% group_indices]
  na_indices <- is.na(group_indices)
  # get order of vector
  replaced <- order(replace(group_indices,na_indices, remaining_indices))
  # replace the not mached values with NA then order according to category
  return((replace(vec,na_indices,NA))[replaced])
}
