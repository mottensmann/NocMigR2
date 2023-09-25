test <- dusk2dawn(date = as.Date("2023-06-23"))

test_that("Dusk as expected", {
  expect_equal(as.character(test[["dusk"]]), "2023-06-23 22:40:24")
})
