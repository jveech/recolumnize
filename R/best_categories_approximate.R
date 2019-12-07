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
  df <- as.data.frame(df) # convert tibbles, matrices and the like
  num_rows <- nrow(df)
  num_cols <- ncol(df)
  out <- column_difference(encode_cols, names(df), num_cols)
  encode_cols  <- out$encode_cols
  keep_cols <- out$keep_cols
  to_be_encoded <- df[,encode_cols, drop = F]
  if (sum(!(unique(as.vector(as.matrix(to_be_encoded))) %in% rownames(category_probabilities) > 0))) {
    stop('Not all values found in category_probabilities, did you make sure that encode_cols are correct?')
  }
  encoded <- (t(apply(to_be_encoded,1, function(x) best_categories_approximate_by_row(dict[unlist(x),]))))
  colnames(encoded) <- colnames(category_probabilities)
  return(cbind(df[,keep_cols],encoded))
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

