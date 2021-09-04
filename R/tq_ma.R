#' Moving average
#'
#' @param x A numeric vector.
#' @param n A positive integer indicating window width.
#' @param na.rm A logical. TRUE or FALSE (default).
#' @param .align A position string:
#' \itemize{
#'  \item "right" (default)
#'  \item "left"
#'  \item "center", if n is odd
#'  \item "center-right" or "center-left", if n is even
#' }
#' @param .step A positive integer indicating the number of elements to shift. 1L (default).
#' @param .complete A logical. If TRUE (default), partial computations are not allowed.
#' @param order_by A vector to order by.
#'
#' @return A numeric vector of the same length as `x`.
#' @seealso [slider::slide_dbl]
#'
#' @examples
#' \dontrun{
#' # example from base
#' moving_average(1:10, 3)
#' moving_average(c(1:5, NA, 7:10), 3, na.rm = TRUE)
#' moving_average(1:10, 3, .align = "left")
#' moving_average(1:10, 3, .step = 2)
#' moving_average(1:10, 3, .complete = FALSE)
#' # Use order_by if data not already ordered (example from dplyr)
#' library(dplyr, warn.conflicts = FALSE)
#' tsbl <- tsibble(year = 2000:2005, value = (0:5)^2, index = year)
#' scrambled <- tsbl %>% slice(sample(nrow(tsbl)))
#'
#' wrong <- mutate(scrambled, ma = moving_average(value))
#' arrange(wrong, year)
#'
#' right <- mutate(scrambled, ma = moving_average(value, order_by = year))
#' arrange(right, year)
#' }
#' @export
moving_average <- function(x, n = 3, na.rm = FALSE,
                           .align = "right", .step = 1L, .complete = TRUE,
                           order_by = NULL) {
  if (n < 1 || .step < 1) {
    stop("`n` and `.step` must be positive integers.")
  }
  if (is.null(order_by)) {
    ma_impl(x, n = n, na.rm = na.rm,
            .align = .align, .step = .step, .complete = .complete)
  } else {
    dplyr::with_order(order_by, ma_impl, x,
                      n = n, na.rm = na.rm,
                      .align = .align, .step = .step, .complete = .complete
    )
  }
}

ma_impl <- function(x, n, na.rm, .align, .step, .complete) {
  if (.align == "right") {
    b = n - 1
    a = 0
  } else if (.align == "left") {
    b = 0
    a = n - 1
  } else if ((n %% 2) == 0) {
    if (.align == "center-left") {
      b = n / 2 - 1
      a = n / 2
    } else if (.align == "center-right") {
      b = n / 2
      a = n / 2 - 1
    } else stop('Set .align either "centre-right", "center-left", "right" or "left"')
  } else if (.align == "center") {
    b = floor(n / 2)
    a = floor(n / 2)
  } else stop('Set .align either "centre", "right" or "left"')

  slider::slide_dbl(x, mean, na.rm = na.rm,
                    .before = b, .after = a,
                    .step = .step, .complete = .complete)
}

#' Calculate moving averages.
#'
#' @param df_ts A tbl_ts object.
#' @param ... Parameters in moving_average function.
#' @return A tbl_ts object.
#' @examples
#' \dontrun{
#' tq_ma(df_ts) # default n = 3, na.rm = FALSE, .align = "right", .step = 1L, .complete = TRUE
#' tq_ma(df_ts, n = 6, na.rm = TRUE, .align = "center-left")
#' }
#' @export
tq_ma <- cal_time_wise(
  moving_average
)
