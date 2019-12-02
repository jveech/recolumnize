#' Title
#'
#' @param df dataframe that contains the data we want to recategorize
#' @param enc which columns should be altered
#' @param category_dictionary named list with a mapping from values to the categories they belong to
#' @param ignore_unknown_values T if unknown values should be ignored, F if they should be one hot encoded
#' @param handle_duplicate_categories "first" takes first value in a certain category, "last" takes last value
#'
#' @return
#' @export
#'
#' @examples
#' my_mat <- matrix(c("obs1","obs2","obs3","a","a","e","d","o","k"),ncol = 3, nrow = 3)
#' my_mat <- as.data.frame(my_mat)
#' colnames(my_mat) <- c("name","1","2")
#' dict <- rep("consonant",26)
#' names(dict) <- letters
#' dict[c("a","e","i","o","u")] <- "vowel"
#' recategorize(my_mat,c(2,3), category_dictionary = dict)
recategorize <- function(df,category_dictionary, cols_to_encode = NULL,
                         ignore_unknown_values = T, handle_duplicate_categories ="first") {
  #TODO compatability checks for category_dictionary
  out <- column_difference(cols_to_encode, names(df), ncol(df))
  encode_cols  <- out$encode_cols
  keep_cols <- out$keep_cols
  to_encode <- as.matrix(df[,encode_cols,drop=F])

  groups <- unique(category_dictionary)
  to_encode <- t(apply(to_encode, 1, reorder_row_by_group, category_dictionary, groups))
  colnames(to_encode) <- groups
  df <- cbind(df[,keep_cols,drop=F], to_encode)
  return(df)
}

reorder_row_by_group <-  function(vec, dict, groups) {
  group_indices <- match(as.character(dict[vec]),groups)
  group_indices[duplicated(group_indices)] <- NA

  remaining_indices <- (1:length(groups))[!1:length(groups) %in% group_indices]
  na_indices <- is.na(group_indices)
  replaced <- order(replace(group_indices,na_indices, remaining_indices))
  (replace(vec,na_indices,NA))[replaced]
}
