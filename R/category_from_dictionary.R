# helper function to lookup in dictionary and return NA if not found.
category_from_dictionary <- function(value, dict) {
  if (value %in% names(dict)) {
    return(as.character(dict[value]))
  }
  else {
    return(NA)
  }
}
