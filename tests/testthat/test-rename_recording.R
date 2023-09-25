test <- rename_recording(path = system.file("extdata", package = "NocMigR2"),
                         format = "wav",
                         .simulate = T)
test_that("rename recordings produces output", {
  expect_equal(test$old.name, "20211220_064253.wav")
})
