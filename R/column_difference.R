# helper function to get subsets and handle when user provides NULL
column_difference <- function(encode_cols, all_cols, num_cols) {
  # function returns boolean vector of columns not in encode_cols
  if (!is.null(encode_cols)) {
    # check whether user provided names or indices
    if (typeof(encode_cols) == "integer" | typeof(encode_cols) == "double") {
      keep_cols <- !1:num_cols %in% encode_cols
      encode_cols <- !keep_cols
    } else if (typeof(encode_cols) == "character") {
      keep_cols <- !all_cols %in% encode_cols
      encode_cols <- !keep_cols
    } else {
      stop('Invalid encode columns provided, need to provide either names or indices of columns')
    }
  }
  else {
    # use all columns
    keep_cols <- rep(FALSE, num_cols)
    encode_cols <- rep(TRUE, num_cols)
  }
  return(list(encode_cols = encode_cols, keep_cols = keep_cols))
}
