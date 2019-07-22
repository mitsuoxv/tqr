context("Right calculation")

library(tibble)
library(tidyr)
library(dplyr)

library(tsibble)

eer_small <- tibble(
  date = rep(as.Date("2019-01-01") + 0:9, 2),
  symbol = c(rep("deflator", 10), rep("reer", 10)),
  area1 = c(rep(100, 10), 100:109),
  area2 = c(100:109, rep(100, 10))
    )

eer_small_ts <- eer_small %>%
  as_tsibble(index = date, key = symbol)


# test tq_diff()
res_diff <- eer_small_ts %>%
  tq_diff(n = 1)

test_that("tq_diff: number of NA's", {
  expect_equal(res_diff$area1 %>% is.na() %>% sum(), 2)
  expect_equal(res_diff$area2 %>% is.na() %>% sum(), 2)
})

res_diff_no_na <- res_diff %>%
  filter(!is.na(area1))

test_that("tq_diff: same calculation applied", {
  expect_equal(res_diff_no_na %>% filter(symbol == "deflator") %>% `[[`("area1"),
               res_diff_no_na %>% filter(symbol == "reer") %>% `[[`("area2"))
  expect_equal(res_diff_no_na %>% filter(symbol == "deflator") %>% `[[`("area2"),
               res_diff_no_na %>% filter(symbol == "reer") %>% `[[`("area1"))
})

test_that("tq_diff: right calculation", {
  expect_equal(res_diff_no_na %>% filter(symbol == "deflator") %>% `[[`("area1"),
               rep(0, 9))
  expect_equal(res_diff_no_na %>% filter(symbol == "deflator") %>% `[[`("area2"),
               rep(1, 9))
})

# test tq_ma()
res_ma <- eer_small_ts %>%
  tq_ma(n = 3)

test_that("tq_ma: number of NA's", {
  expect_equal(res_ma$area1 %>% is.na() %>% sum(), 4)
  expect_equal(res_ma$area2 %>% is.na() %>% sum(), 4)
})

res_ma_no_na <- res_ma %>%
  filter(!is.na(area1))

test_that("tq_ma: same calculation applied", {
  expect_equal(res_ma_no_na %>% filter(symbol == "deflator") %>% `[[`("area1"),
               res_ma_no_na %>% filter(symbol == "reer") %>% `[[`("area2"))
  expect_equal(res_ma_no_na %>% filter(symbol == "deflator") %>% `[[`("area2"),
               res_ma_no_na %>% filter(symbol == "reer") %>% `[[`("area1"))
})

test_that("tq_ma: right calculation", {
  expect_equal(res_ma_no_na %>% filter(symbol == "deflator") %>% `[[`("area1"),
               rep(100, 8))
  expect_equal(res_ma_no_na %>% filter(symbol == "deflator") %>% `[[`("area2"),
               101:108)
})


# test tq_gr()
res_gr <- eer_small_ts %>%
  tq_gr(n = 1)

test_that("tq_gr: number of NA's", {
  expect_equal(res_gr$area1 %>% is.na() %>% sum(), 2)
  expect_equal(res_gr$area2 %>% is.na() %>% sum(), 2)
})

res_gr_no_na <- res_gr %>%
  filter(!is.na(area1))

test_that("tq_gr: same calculation applied", {
  expect_equal(res_gr_no_na %>% filter(symbol == "deflator") %>% `[[`("area1"),
               res_gr_no_na %>% filter(symbol == "reer") %>% `[[`("area2"))
  expect_equal(res_gr_no_na %>% filter(symbol == "deflator") %>% `[[`("area2"),
               res_gr_no_na %>% filter(symbol == "reer") %>% `[[`("area1"))
})

test_that("tq_gr: right calculation", {
  expect_equal(res_gr_no_na %>% filter(symbol == "deflator") %>% `[[`("area1"),
               rep(0, 9))
  expect_equal(res_gr_no_na %>% filter(symbol == "deflator") %>% `[[`("area2"),
               c(1, 0.99009901, 0.980392157, 0.970873786, 0.961538462, 0.952380952,
                 0.943396226, 0.934579439, 0.925925926))
})




