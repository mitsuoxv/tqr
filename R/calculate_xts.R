#' Function factory for calculation.
#'
#' @param fun_num A function to transform a numeric vector.
#' @param fun_idx A function to transform a numeric vector representing time.
#' @param fun_itv A function to transform a string representing interval.
#' @param ... Arguments for fun_num.
#' @return A function to transform a tsibble into a tsibble or a tibble.
#' @examples
#' \dontrun{
#' tq_log <- cal_factory_xts(
#' function(num) log(num),
#' function(idx) idx,
#' function(itv) itv
#' )
#' }
#' @export
cal_factory_xts <-
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
        utils::capture.output()

      df <- tibble::as_tibble(df_ts)

      stopifnot(df %>%
                  dplyr::select(-!!index_v, -c(!!!key_v)) %>%
                  purrr::map_lgl(is.numeric)
      )

      # must be long format, only 1 numeric column
      # stopifnot(df %>%
      #             dplyr::select(-!!index_v, -c(!!!key_v)) %>%
      #             length() == 1
      # )

      df[[index_variable]] <- change_idx_class_for_xts(df[[index_variable]],
                                                       interval_input)

      mutate_fun <- function(df) {
        mat_xts <- df %>%
          dplyr::select(-!!index_v) %>%
          as.matrix() %>%
          xts::xts(order.by = df[[index_variable]])

        calculated_mat <- mat_xts %>%
          fun_num(...)

        calculated_df <- calculated_mat %>%
          as.data.frame() %>%
          tibble::as_tibble()

        calculated_df[[index_variable]] <- zoo::index(calculated_mat)

        calculated_df
      }

      calculated <- df %>%
        dplyr::group_by(!!!key_v) %>%
        tidyr::nest(.key = "data") %>%
        dplyr::mutate(data = purrr::map(data, mutate_fun)) %>%
        tidyr::unnest(data) %>%
        dplyr::ungroup()

      interval_output <- fun_itv(interval_input)

      calculated[[index_variable]] <- calculated[[index_variable]] %>%
        add_idx_class(interval_output)

      if (setequal(names(df), names(calculated))) {
        calculated <- calculated[, names(df)]
      }

      calculated %>%
        tsibble::as_tsibble(key = key_variables,
                            index = index_variable)
    }
  }


