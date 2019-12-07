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

best_categories_approximate_by_row <- function(probs) {
  ncat <- ncol(probs)
  ordered_row <- rep(NA, ncat)
  names(ordered_row) <- names(probs)
  likelihood <- 1
  for (i in 1:(ncat-1)) {
    out <- get_best_index_from_row(probs)
    ordered_row[out$col] <- rownames(probs)[out$max_index]
    probs <- probs[-out$max_index,-out$col_index,drop = F]
  }
  ordered_row[names(probs)[1]] <- rownames(probs)[1]
  return(ordered_row)
}


