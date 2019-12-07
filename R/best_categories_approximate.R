#' best_categories_approximate
#'
#' @param df data frame (or matrix) to be encoded
#' @param category_probabilities matrix or dataframe with rownames containing keys to be looked up, ith column containing probabilities of being in category i
#' @param encode_cols which columns should be encoded (others are left alone)
#'
#' @return dataframe with encode_cols replaced by data encoded into categories from caegory_probabilities
#' @export
#'
#' @examples
#' dict2 <- rep("consonant",26)
#' names(dict2) <- letters
#' dict2[c("a","e","i","o","u")] <- "vowel"

#' probs <- matrix(0,nrow = 26, ncol = 2)
#' colnames(probs) <- c("vowel","consonant")
#' rownames(probs) <- letters
#' probs[,1] <- abs((dict2 == "vowel") -.001)
#' probs[25,1] <- 0.25
#' probs[23,1] <- 0.05
#' probs[,2] <- 1-probs[,1]
#' mat <- matrix(c("a","w","x","y","c","w","r","r"),nrow = 4, ncol = 2, byrow=T)
#' mat
#' best_categories_approximate(mat,probs)

best_categories_approximate <- function(df, category_probabilities, encode_cols = NULL) {
  num_rows <- nrow(df)
  num_cols <- ncol(df)
  out <- column_difference(encode_cols, colnames(df), num_cols)
  encode_cols  <- out$encode_cols
  keep_cols <- out$keep_cols
  to_be_encoded <- df[,encode_cols, drop = F]
  if (sum(!(unique(as.vector(as.matrix(to_be_encoded))) %in% rownames(category_probabilities) > 0))) {
    stop('Not all values found in category_probabilities, did you make sure that encode_cols are correct?')
  }
  encoded <- (t(apply(to_be_encoded,1,
                      function(x) best_categories_approximate_by_row(x,
                                                                     category_probabilities[unlist(unique(x)),]))))
  colnames(encoded) <- colnames(category_probabilities)
  return(cbind(df[,keep_cols],encoded))
}

best_categories_approximate_by_row <- function(row, probs) {
  ncat <- ncol(probs)
  if(is.null(ncat)) {
    return(row)
  }
  ordered_row <- rep(NA, ncat)
  names(ordered_row) <- colnames(probs)
  likelihood <- 1
  for (i in 1:(ncat-1)) {
    if (nrow(probs) == 1){
      break
    }
    out <- get_best_index_from_row(probs)
    ordered_row[out$col] <- rownames(probs)[out$max_index]
    if (sum(row == rownames(probs)[out$max_index]) > 1) {
      row <- row[-out$max_index]
      probs <- probs[ ,-out$col_index,drop = F]
    }
    else {
      probs <- probs[-out$max_index,-out$col_index,drop = F]
    }
  }
  ordered_row[is.na(ordered_row)] <- rownames(probs)[1]
  return(ordered_row)
}


get_best_index_from_row <- function(dict) {
  first_col <- which.max(apply(dict,2,find_biggest_ratio))
  return(list(col = colnames(dict)[first_col], col_index = first_col, max_index = which.max(dict[,first_col])))
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

