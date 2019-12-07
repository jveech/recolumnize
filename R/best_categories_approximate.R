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


get_best_index_from_row <- function(dict) {
  first_col <- which.max(apply(dict,2,find_biggest_ratio))
  return(list(col = names(dict)[first_col], col_index = first_col, max_index = which.max(dict[,first_col])))
}

find_biggest_ratio <- function(vec) {
  n <- length(vec)
  biggest <- max(vec)
  nextbiggest <- sort(vec,partial=length(vec)-1)[length(vec)-1]
  if (nextbiggest == 0){ # only happens if all other entries are 0...
    warning("Some columns of probability contain many 0s, can cause numerical issues")
    return(10000)
  }
  pct_diff <- (biggest-nextbiggest)/(nextbiggest)
  return(pct_diff)
}

