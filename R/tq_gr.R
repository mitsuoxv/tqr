#' Growth rate
#'
#' @param x A numeric vector.
#' @param n A positive integer indicating lag.
#' @param annualize A positive integer indicating the number to annualize.
#' @param pct A logical. If TRUE (default), returns percent growth rate.
#' @param order_by A vector to order by.
#'
#' @return A numeric vector of the same length as `x`.
#'
#' @examples
#' \dontrun{
#' # example from base
#' growth_rate(1:10, 4)
#' growth_rate(1:10, 1, annualize = 4)
#' # Use order_by if data not already ordered (example from dplyr)
#' library(dplyr, warn.conflicts = FALSE)
#' tsbl <- tsibble(year = 2000:2005, value = (0:5)^2, index = year)
#' scrambled <- tsbl %>% slice(sample(nrow(tsbl)))
#'
#' wrong <- mutate(scrambled, gr = growth_rate(value))
#' arrange(wrong, year)
#'
#' right <- mutate(scrambled, gr = growth_rate(value, order_by = year))
#' arrange(right, year)
#' }
#' @export
growth_rate <- function(x, n = 12, annualize = 1, pct = TRUE,
                        order_by = NULL) {
  if (n < 1 || annualize < 1) {
    stop("`n` and `annualize` must be positive integers.")
  }
  if (is.null(order_by)) {
    gr_impl(x, n = n, annualize = annualize, pct = pct)
  } else {
    dplyr::with_order(order_by, gr_impl, x, n = n,
                      annualize = annualize, pct = pct)
  }
}

gr_impl <- function(x, n, annualize, pct) {
  if (annualize == 1) {
    gr = (x / dplyr::lag(x, n = n)) - 1
  } else {
    gr = (x / dplyr::lag(x, n = n))^annualize - 1
  }

  if (pct) {
    gr * 100
  } else {
    gr
  }
}

#' Calculate growth rates.
#'
#' @param df_ts A tbl_ts object.
#' @param ... Parameters in growth_rate function.
#' @return A tbl_ts object.
#' @examples
#' \dontrun{
#' tq_gr(df_ts) # default n = 12, annualize = 1, pct = TRUE
#' tq_gr(df_ts, n = 1, annualize = 4) # annualize quarterly growth rates
#' }
#' @export
tq_gr <- cal_time_wise(
  growth_rate
)
