#' best_categories_approximate
#'
#' Find the best pairing between values and categories based on a set of probabilities.
#' @param df data frame (or matrix) to be encoded
#' @param category_probabilities matrix or dataframe with rownames containing keys to be looked up, ith column containing probabilities of being in category i
#' @param encode_cols which columns should be encoded (others are left alone)
#'
#' @return A dataframe with encode_cols replaced by data encoded into categories from caegory_probabilities
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
  # store the subsets we need to split the dataframe into
  out <- column_difference(encode_cols, colnames(df), num_cols)
  encode_cols  <- out$encode_cols
  keep_cols <- out$keep_cols
  to_be_encoded <- df[,encode_cols, drop = F]

  # check if the dictionary is compatible. we require that all values can be found in it.
  if (sum(!(unique(as.vector(as.matrix(to_be_encoded))) %in% rownames(category_probabilities) > 0))) {
    stop('Not all values found in category_probabilities, did you make sure that encode_cols are correct?')
  }

  # encode the columns
  encoded <- (t(apply(to_be_encoded,1,
                      function(x) best_categories_approximate_by_row(x,
                                                                     category_probabilities[unlist(unique(x)), ]))))
  # need to unlist and call unique to prevent some edge cases (for example if there are duplicaes in a row)
  colnames(encoded) <- colnames(category_probabilities)
  return(cbind(df[ ,keep_cols], encoded, stringsAsFactors = F))
}

# function works row by row returning the best order
best_categories_approximate_by_row <- function(row, probs) {
  ncat <- ncol(probs)
  # if we have a 1 column probability matrix, all the values are the same
  if(is.null(ncat)) {
    return(row)
  }
  ordered_row <- rep(NA, ncat)
  names(ordered_row) <- colnames(probs)

  # loop until there is only one value left to place
  for (i in 1:(ncat-1)) {
    if (nrow(probs) == 1){ # if there is one value/category left, we want to break out and fill everything remaining with that value
      break
    }
    # get column with largest percentage difference between best value and 2nd best value
    out <- get_best_index_from_row(probs)
    # categorize the value and category pair found in previous step
    ordered_row[out$col] <- rownames(probs)[out$max_index]
    # handle case when there are duplicate values in row
    if (sum(row == rownames(probs)[out$max_index]) > 1) {
      row <- row[-out$max_index]
      probs <- probs[ ,-out$col_index,drop = F]
    }
    else {
      # continue by making probability matrix smaller and repeating
      probs <- probs[-out$max_index,-out$col_index,drop = F]
    }
  }
  ordered_row[is.na(ordered_row)] <- rownames(probs)[1]
  return(ordered_row)
}

# function finds the column that maximizes the percentage difference between best and 2nd best value
get_best_index_from_row <- function(dict) {
  first_col <- which.max(apply(dict,2,find_biggest_ratio))
  return(list(col = colnames(dict)[first_col], col_index = first_col, max_index = which.max(dict[,first_col])))
}

# function finds differences between best and 2nd best value
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

