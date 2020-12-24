#' Function factory for calculation.
#'
#' @param fun_num A function to transform a numeric vector.
#' @param fun_idx A function to transform a numeric vector representing time.
#' @param fun_itv A function to transform a string representing interval.
#' @return A function to transform a tsibble into a tsibble or a tibble.
#' @examples
#' \dontrun{
#' tq_log <- cal_factory(
#' function(num) log(num),
#' function(idx) idx,
#' function(itv) itv
#' )
#' }
#' @export
cal_factory <-
  function(fun_num, fun_idx, fun_itv) {
    force(fun_num)
    force(fun_idx)
    force(fun_itv)
    function(df_ts, ...) {
      stopifnot(tsibble::is_tsibble(df_ts))

      index_variable <- tsibble::index_var(df_ts)
      key_variables <- tsibble::key_vars(df_ts)

      index_v <- tsibble::index(df_ts)
      key_v <- tsibble::key(df_ts)

      interval_input <- df_ts %>%
        tsibble::interval() %>%
        tsibble:::format.interval()

      df <- tibble::as_tibble(df_ts)

      stopifnot(df %>%
                  dplyr::select(-!!index_v, -c(!!!key_v)) %>%
                  purrr::map_lgl(is.numeric)
      )

      mutate_fun <- function(df) {
        df_value <- df %>%
          dplyr::select(-!!index_v) %>%
          purrr::map(fun_num, ...) %>%
          tibble::as_tibble()

        tibble::tibble(!!index_v := fun_idx(df[[index_variable]])) %>%
          dplyr::bind_cols(df_value)
      }

      calculated <- df %>%
        tidyr::nest(data = !any_of(key_variables)) %>%
        dplyr::mutate(data = purrr::map(data, mutate_fun)) %>%
        tidyr::unnest(data)

      interval_output <- fun_itv(interval_input)

      calculated[[index_variable]] <- calculated[[index_variable]] %>%
        add_idx_class(interval_output)

      if (interval_output == "?") {
        calculated[, names(df_ts)]
      } else {
        calculated[, names(df_ts)] %>%
          tsibble::as_tsibble(key = key_variables,
                              index = index_variable)
      }
    }
}


#' Calculate differences.
#'
#' @param df_ts A tbl_ts object.
#' @param ... Parameters for fun_num.
#' @return A tbl_ts object.
#' @examples
#' \dontrun{
#' tq_diff(df)
#' tq_diff(df, n = 12)
#' }
#' @export
tq_diff <- cal_factory(
  function(num, n = 1) {
    num - dplyr::lag(num, n = n)
  },
  function(idx) {
    idx
  },
  function(itv) {
    itv
  }
)


#' Calculate moving averages.
#'
#' @param df_ts A tbl_ts object.
#' @param ... Parameters for fun_num.
#' @return A tbl_ts object.
#' @examples
#' \dontrun{
#' tq_ma(df)
#' tq_ma(df, n = 6, na.rm = TRUE, .align = "center-left")
#' }
#' @export
tq_ma <- cal_factory(
  function(num, n = 3, na.rm = FALSE, .align = "right", .step = 1L, .complete = TRUE) {

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

    slider::slide_dbl(num, mean, na.rm = na.rm,
                      .before = b, .after = a,
                      .step = .step, .complete = .complete)
  },
  function(idx) {
    idx
  },
  function(itv) {
    itv
  }
)


#' Calculate growth rates.
#'
#' @param df_ts A tbl_ts object.
#' @param ... Parameters for fun_num.
#' @return A tbl_ts object.
#' @examples
#' \dontrun{
#' tq_gr(df)
#' tq_gr(df, n = 1, annualize = 4)
#' }
#' @export
tq_gr <- cal_factory(
  function(num, n = 12, annualize = 1) {
    if (annualize == 1) {
      (num / dplyr::lag(num, n = n)) * 100 - 100
    } else {
      (num / dplyr::lag(num, n = n))^annualize * 100 - 100
    }
  },
  function(idx) {
    idx
  },
  function(itv) {
    itv
  }
)

