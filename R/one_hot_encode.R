#' Title
#'
#' @param df A dataframe that we want to one hot encode
#' @param encode_cols The names of the columns that should be encoded, should be categorical
#' @param keep "exists" for 1 if value exists, 0 else. "sum" for sum of number of appearances in a row, default exists
#' @param min_occurences minimum number of appearances in the data before column is added, default 1.
#'
#' @return dataframe with encode_cols removed and replaced with numeric columns
#' @export
#'
#' @examples
#' one_hot_encode(Titanic,encode_cols = c("Class","Sex","Age"))
one_hot_encode <- function(df, encode_cols = NULL, keep = "exists", min_occurences = 1) {
  num_rows <- nrow(df)
  num_cols <- ncol(df)
  if (!is.null(encode_cols)) {
    keep_cols <- !names(df) %in% encode_cols
  }
  else {
    keep_cols <- rep(FALSE, length(names(df)))
  }
  # flatten out the part of the dataframe we want to encode. then extract out all the unique values, these will be our columns
  values <- unique(as.vector(as.matrix(df[,encode_cols, drop = F])))
  num_new_cols <- length(values)

  encoded <- matrix(0, nrow = nrow(df), ncol = num_new_cols)

  # TODO: speed this up

  if (keep == "sum") {
    for (i in 1:length(values)) {
      encoded[,i] <- rowSums(df[ , !keep_cols, drop = F] == values[i], )
    }
  }

  else {
    for (i in 1:length(values)) {
      encoded[,i] <- as.numeric(rowSums(df[,!keep_cols, drop = F] == values[i], ) > 0)
    }
  }
  keep_features <- colSums(encoded) >= min_occurences
  values <- values[keep_features]
  encoded <- encoded[ , keep_features]
  # create new data frame containing the columns we are keeping from the old dataframe and new columns for each value we are encoding
  encoded_df <- data.frame(cbind(df[ , keep_cols, drop = F], encoded))
  colnames(encoded_df) <- c(colnames(df[ , keep_cols, drop = F]), values)
  return(encoded_df)
}
