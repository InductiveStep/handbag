comma_and_enum_str <- function(items,
                               mid_sep,
                               last_sep,
                               none_label) {
  if (length(items) == 0) {
    none_label
  } else if (length(items) == 1) {
    items
  } else if (length(items) == 2) {
    paste0(items[1], last_sep, items[2])
  } else {
    paste0(items[1],
           mid_sep,
           comma_and_enum_str(items[2:length(items)],
                              mid_sep,
                              last_sep))
  }
}

#' Combines (usually multiple) binary vectors into a convenient
#' single vector summarising the pattern
#' 
#' This is useful when you have a collection of yes/no variables
#' representing multiple choices in which more than one option
#' can be selected.
#' 
#' @param data Data frame or tibble containing the binary
#' vectors
#' @param strip_prefix A common prefix to remove from
#' the start of the variable names
#' @param mid_sep Character to separate names, by default a comma
#' @param last_sep The last separator to use, by default an ampersand
#' @param none_label How to represent the empty combination
#' @return A character vector representing combinations of inputs
#' @examples 
#' test1 <- expand.grid(myvar_1 = c(0,1), 
#'                      myvar_2 = c(0,1),
#'                      myvar_3 = c(0,1))
#' test1$pat <- binary_patterns_var(test1, "myvar_")
#' test1
#' @export
binary_patterns_var <- function(data,
                                strip_prefix = "",
                                mid_sep = ", ",
                                last_sep = " & ",
                                none_label = "(None)") {
  stripped_names <- sub(strip_prefix, "", names(data))
  res = rep(NA, nrow(data))
  
  for(r in 1:nrow(data)) {
    items <- stripped_names[as.logical(data[r,])]
    res[r] <- comma_and_enum_str(items,
                                 mid_sep,
                                 last_sep,
                                 none_label)
  }
  
  res
}



