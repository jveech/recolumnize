best_categories_brute_force <- function(df, category_probabilities) {
  # TODO: check compatibility of category_probabilities


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
    if (new >= best_prob) {
      best_prob <- new
      best_index <- i
    }
  }
  return(list(perm = perms[best_index,], prob = best_prob, index = best_index))
}
