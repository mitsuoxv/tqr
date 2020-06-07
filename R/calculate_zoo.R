#' Function factory for calculation zoo class.
#'
#' @param fun_num A function to transform a numeric vector or matrix of zoo class.
#' @param fun_idx A function to transform a numeric vector representing time.
#' @param fun_itv A function to transform a string representing interval.
#' @param ... Arguments for fun_num.
#' @return A function to transform a tsibble into a tsibble or a tibble.
#' @examples
#' \dontrun{
#' tq_rollmean <- cal_factory_zoo(
#' function(num) zoo::rollmean(num, ...),
#' function(idx) idx,
#' function(itv) itv
#' )
#' }
#' @export
cal_factory_zoo <-
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
        mat_zoo <- df %>%
          dplyr::select(-!!index_v) %>%
          as.matrix() %>%
          zoo::zoo(order.by = df[[index_variable]])

        calculated_mat <- mat_zoo %>%
          fun_num(...)

        calculated_df <- calculated_mat %>%
          as.data.frame() %>%
          tibble::as_tibble()

        calculated_df[[index_variable]] <- zoo::index(calculated_mat)

        calculated_df
      }

      calculated <- df %>%
        dplyr::group_nest(!!!key_v) %>%
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


