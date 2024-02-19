test <- NocMig_meta(lat = 52, lon = 8, date = as.Date("2023-09-24"))

test_that("NocMig_meta works", {
  #expect_equal(test$part1, "Teilliste 1: 24.9-25.9.2023, 19:59-06:45, trocken, 13°C, SE, 7 km/h")
  expect_equal(test$part1, "Teilliste 1: 24.9-25.9.2023, 19:59-06:45, trocken, 13°C, ESE, 5 km/h")
})

