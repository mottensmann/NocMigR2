path <- system.file("extdata", "20211220_064253.wav", package = "NocMigR2")

TD <- find_events(wav.file = path,
                 audacity = FALSE, # Write audacity labels
                 threshold = 8, # SNR in db
                 min_dur = 20, # min length in ms
                 max_dur = 300, # max length in ms
                 LPF = 5000, # low-pass filter at 500 Hz
                 HPF = 1000)

test_that("find_events works", {
  expect_equal(length(TD), 2)
  expect_equal(nrow(TD$data$event_data), 6)
})
