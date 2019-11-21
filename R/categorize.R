#' Title
#'
#' @param df dataframe that contains the data we want to recategorize
#' @param encode_cols which columns should be altered
#' @param category_dictionary named list with a mapping from values to the categories they belong to
#' @param ignore_unknown_values T if unknown values should be ignored, F if they should be one hot encoded
#' @param handle_duplicate_categories "first" takes first value in a certain category, "last" takes last value
#'
#' @return dataframe with encode_cols removed and replaced by m = number of categories columns and possibly some one-hot-encoded columns
#' @export
#'
#' @examples
recategorize <- function(df, encode_cols = NULL, category_dictionary, ignore_unknown_values = T, handle_duplicate_categories ="first") {
  #TODO compatability checks for category_dictionary
  if (!is.null(encode_cols)) {
    keep_cols <- !names(df) %in% encode_cols
  }
  else {
    keep_cols <- rep(FALSE, length(names(df)))
  }
  groups <- unique(dict)
  num_cats <- length(unique(dict))
  categ_indices <- cbind(matrix(match(sapply(df[,!keep_cols],category_from_dictionary,dict,USE.NAMES=F), groups),dim(df[,!keep_cols])),
                         matrix(1:num_cats,nrow=nrow(df)))
                         ordering <- apply(categ_indices,1,order)
}

# helper function to lookup in dictionary and return NA if not found.
category_from_dictionary <- function(value, dict) {
  if (value %in% names(dict)) {
    return(as.character(dict[value]))
  }
  else {
    return(NA)
  }
}
