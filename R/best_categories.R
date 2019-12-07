#' Title
#'
#' @param df
#' @param category_probabilities
#'
#' @return
#' @export
#'
#' @examples
best_categories_brute_force <- function(df, category_probabilities, encode_cols = NULL, ignore_warning = F) {
  df <- as.data.frame(df) # convert tibbles, matrices and the like
  if(((factorial(ncol(category_probabilities))*nrow(df)) > 1e5) & (ignore_warning == F)) {
    stop('Your dataset is very large to apply this method. Try using best_categories_approximate instead, or override this error by setting ignore_warning = T')
  }

  num_rows <- nrow(df)
  num_cols <- ncol(df)
  out <- column_difference(encode_cols, names(df), num_cols)
  encode_cols  <- out$encode_cols
  keep_cols <- out$keep_cols
  to_be_encoded <- df[,encode_cols, drop = F]
  if (sum(!(unique(as.vector(as.matrix(to_be_encoded))) %in% rownames(category_probabilities) > 0))) {
    stop('Not all values found in category_probabilities, did you make sure that encode_cols are correct?')
  }

  perms <- getPerms(1:ncol(category_probabilities))
  arrangement <- matrix(1:ncol(to_be_encoded), nrow = nrow(to_be_encoded), ncol = ncol(to_be_encoded), byrow=T)
  for (j in 1:nrow(to_be_encoded)) {
    out <- get_best_perm(perms,category_probabilities[unlist(to_be_encoded[j, ]), ])
    arrangement[j, ] <- out$perm
  }
  encoded <- t(sapply(1:nrow(to_be_encoded), function(i) to_be_encoded[i,][arrangement[i,]]))
  colnames(encoded) <- colnames(category_probabilities)
  return(cbind(df[,keep_cols],encoded))
}

# function taken from Adrian's solution at
# https://stackoverflow.com/questions/11095992/generating-all-distinct-permutations-of-a-list-in-r
getPerms <- function(x) {
  if (length(x) == 1) {
    return(x)
  }
  else {
    res <- matrix(nrow = 0, ncol = length(x))
    for (i in seq_along(x)) {
      res <- rbind(res, cbind(x[i], Recall(x[-i])))
    }
    return(res)
  }
}

prob_from_perm <- function(perm, probs) {
  likelihood <- 1
  for (i in 1:nrow(probs)) {
    likelihood <- likelihood * (probs[i,perm[i]] + .0001) # add a small amount to make it easier to distinguish between 0
  }
  return(likelihood)
}

get_best_perm <- function(perms,probs) {
  best_prob <- 0
  best_index <- 1
  for (i in 1:nrow(perms)) {
    new <- prob_from_perm(perms[i,], probs)
    if(is.na(new)) {
      warning("Some values not found in dictionary")
      next
    }
    if (new >= best_prob) {
      best_prob <- new
      best_index <- i
    }
  }
  return(list(perm = order(perms[best_index,]), prob = best_prob, index = best_index))
}
