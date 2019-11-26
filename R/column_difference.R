column_difference <- function(encode_cols, all_cols) {
  # function returns boolean vector of columns not in encode_cols
  num_cols <- length(all_cols)
  if (!is.null(encode_cols)) {
    if (typeof(encode_cols) == "integer") {
      keep_cols <- !1:num_cols %in% encode_cols
    } else if (typeof(encode_cols) == "character") {
      keep_cols <- !all_cols %in% encode_cols
    } else {
      stop('Invalid encode columns provided, need to provide either names or indices of columns')
    }
  }
  else {
    keep_cols <- rep(FALSE, num_cols)
    encode_cols <- rep(TRUE, num_cols)
  }
  return(list(encode_cols = encode_cols, keep_cols = keep_cols))
}
