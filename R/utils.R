#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL


#' Add class to index according to interval.
#'
#' @param index A Date or POSIXct object.
#' @param itv A character, like "1M".
#' @return A date related class object.
#' @examples
#' \dontrun{
#' add_idx_class(df$date, "1M")
#' }
add_idx_class <- function(index, itv) {
  category <- itv2list(itv)$category

  if (category == "Y") {
    lubridate::year(index)
  } else if (category == "Q") {
    tsibble::yearquarter(index)
  } else if (category == "M") {
    tsibble::yearmonth(index)
  } else if (category == "W") {
    tsibble::yearweek(index)
  } else if (category == "D") {
    as.Date(index)
  } else if (category == "?") {
    as.character(index)
  } else {
    as.POSIXct(index)
  }
}

#' Change index class compatible with xts.
#'
#' @param index A Date or POSIXct object.
#' @param itv A character, like "1M".
#' @return A date related class object compatible with xts.
#' @examples
#' \dontrun{
#' change_idx_class_for_xts(df$date, "1M")
#' }
change_idx_class_for_xts <- function(index, itv) {
  category <- itv2list(itv)$category

  if (category %in% c("Q", "M", "W")) {
    as.Date(index)
  } else {
    index
  }
}



#' Convert interval to list.
#'
#' @param itv A character, like "1M".
#' @return A list.
#' @examples
#' \dontrun{
#' itv2list("1M")
#' }
#' @export
itv2list <- function(itv) {
  list(
    number = stringr::str_sub(itv, 1, -2) %>% as.integer(),
    category = stringr::str_sub(itv, -1)
  )
}

