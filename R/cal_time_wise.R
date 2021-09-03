#' Function factory for adapting time-wise vector function to tsibble.
#'
#' @param fun_num A function to transform a time-wise numeric vector.
#' @return A function to transform a tsibble.
#' @examples
#' \dontrun{
#' tq_diff <- cal_time_wise(tsibble::difference)
#' }
#' @export
cal_time_wise <-
  function(fun_num) {
    force(fun_num)
    function(df_ts, ...) {
      stopifnot(tsibble::is_tsibble(df_ts))

      index_v <- tsibble::index(df_ts)
      key_v <- tsibble::key(df_ts)

      df_ts %>%
        dplyr::group_by(!!!key_v) %>%
        dplyr::mutate(dplyr::across(
          tidyselect::vars_select_helpers$where(is.numeric) & !c(!!index_v),
          fun_num, order_by = !!index_v, ...)) %>%
        dplyr::ungroup()
    }
  }

#' Calculate differences.
#'
#' @param df_ts A tbl_ts object.
#' @param ... Parameters in [tsibble::difference] function.
#' @return A tbl_ts object.
#' @seealso [tsibble::difference]
#' @examples
#' \dontrun{
#' tq_diff(df_ts) # default lag = 1
#' tq_diff(df_ts, lag = 12)
#' }
#' @export
tq_diff <- cal_time_wise(
  tsibble::difference
)
