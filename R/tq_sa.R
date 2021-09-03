#' Seasonally adjusted
#'
#' @param x A numeric vector.
#' @param order_by A date vector to order by.
#' @param ... Parameters in [seasonal::seas] function.
#' @return A numeric vector of the same length as `x`.
#' @seealso [seasonal::seas]
#' @examples
#' \dontrun{
#' # example from base
#' season_adjust((0:107)^2, seq(as.Date("2001-01-01"), as.Date("2009-12-01"), "months"))
#' # Automatically order even if data not already ordered (example from dplyr)
#' library(dplyr, warn.conflicts = FALSE)
#' tsbl <- tsibble(month = seq(as.Date("2001-01-01"), as.Date("2009-12-01"), "months"),
#'  value = (0:107)^2, index = month)
#' scrambled <- tsbl %>% slice(sample(nrow(tsbl)))
#'
#' right <- mutate(scrambled, sa = season_adjust(value, month))
#' arrange(right, month)
#' }
#' @export
season_adjust <- function(x, order_by, ...) {
  vec_origin <- tsibble::tsibble(
    time_var = order_by,
    value = x,
    index = time_var
  ) %>%
    stats::as.ts()

  # sanity check
  if (!stats::frequency(vec_origin) %in% c(4, 12)) {
    stop("Frequency must be 4 ('quarterly') or 12 ('monthly')")
  }

  vec_sa <- vec_origin %>%
    seasonal::seas(...) %>%
    seasonal::final() %>%
    as.numeric()

  vec_sa[rank(order_by)]
}

#' Calculate seasonally adjusted values.
#'
#' @param df_ts A tbl_ts object.
#' @param ... Parameters in [seasonal::seas] function.
#' @return A tbl_ts object.
#' @seealso [seasonal::seas]
#' @examples
#' \dontrun{
#' tq_sa(df)
#' }
#' @export
tq_sa <- cal_time_wise(
  season_adjust
)
