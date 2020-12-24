#' Function factory for calculation utilizing ts class.
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
cal_factory_ts <-
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

      # sanity check
      if (!(interval_input %in% c("1Q", "1M"))) {
        stop("interval must be '1Q' or '1M' for this operation")
      }

      freq <- dplyr::case_when(
        interval_input == "1Q" ~ 4L,
        interval_input == "1M" ~ 12L
      )

      df <- tibble::as_tibble(df_ts)

      stopifnot(df %>%
                  dplyr::select(-!!index_v, -c(!!!key_v)) %>%
                  purrr::map_lgl(is.numeric)
      )

      mutate_fun <- function(df) {
        date_top <- df[[index_variable]][1]

        start_qm <- dplyr::case_when(
          freq == 4L ~ lubridate::quarter(date_top),
          freq == 12L ~ lubridate::month(date_top) %>% as.integer()
        )

        start_y <- lubridate::year(date_top)

        start <- c(start_y, start_qm)

        df_value_only <- df %>%
          dplyr::select(-!!index_v)

        mts_mat <- df_value_only %>%
          stats::ts(start = start, frequency = freq)

        if (is.null(dim(mts_mat))) {
          # ts class
          df_value <- vector("list", 1)
          names(df_value) <- names(df_value_only)

          df_value[[1]] <- mts_mat %>% fun_num()

        } else {
          # mts class
          df_value <- vector("list", length(colnames(mts_mat)))
          names(df_value) <- colnames(mts_mat)

          for (col in colnames(mts_mat)) {
            df_value[[col]] <- mts_mat[, col] %>% fun_num()
          }
        }

        df_value <- as_tibble(df_value)

        tibble::tibble(!!index_v := fun_idx(df[[index_variable]])) %>%
          dplyr::bind_cols(df_value)

      }

      calculated <- df %>%
        tidyr::nest(data = !key_variables) %>%
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


#' Calculate seasonally adjusted values.
#'
#' @param df_ts A tbl_ts object.
#' @param ... Parameters of seasonal::seas().
#' @return A tbl_ts object.
#' @examples
#' \dontrun{
#' tq_sa(df)
#' }
#' @export
tq_sa <- cal_factory_ts(
  function(num_ts, ...) {
    num_ts %>%
      seasonal::seas(...) %>%
      seasonal::final() %>%
      as.numeric()
  },
  function(idx) {
    idx
  },
  function(itv) {
    itv
  }
)
