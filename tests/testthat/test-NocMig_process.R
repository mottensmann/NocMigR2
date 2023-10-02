dir.tmp <- tempdir()
dir.create(dir.tmp, showWarnings = FALSE)
x <- file.copy(system.file("extdata", "20211220_064253.wav", package = "NocMigR2"),
          file.path(dir.tmp, "20211220_064253.wav"))
NocMig_process(parent.folder = dir.tmp, rename = FALSE, segment_length = 3)
test <- list.files(path = dir.tmp, pattern = ".wav", ignore.case = TRUE)

unlink(dir.tmp, recursive = TRUE)

test_that("Split works", {
  expect_equal(test, c("20211220_064253.wav", "20211220_064256.wav",
                       "20211220_064259.wav", "20211220_064302.wav",
                       "20211220_064305.wav"))
})

