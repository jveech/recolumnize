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
