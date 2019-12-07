#' Title
#'
#' @param df
#' @param category_probabilities
#' @param encode_cols
#'
#' @return
#' @export
#'
#' @examples
best_categories_approximate <- function(df, category_probabilities, encode_cols = NULL) {
  # TODO compatability checks

  # TODO subsetting

  return(t(apply(df,1, function(x) best_categories_approximate_by_row(dict[unlist(x),]))))
}


