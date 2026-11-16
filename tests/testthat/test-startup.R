reset_warm_up <- function() {
  warm_up_state$done <- character()
  warm_up_state$stop <- FALSE
}

# Run what has been scheduled with later::later(), as Shiny would
run_scheduled <- function(timeout = 60) {
  end <- Sys.time() + timeout
  suppressWarnings(
    while (!later::loop_empty() && Sys.time() < end) later::run_now(0.05)
  )
}

all_steps <- c("chart packages loaded", "series made", "chart analysed")

test_that("The warm-up does each step in turn", {
  reset_warm_up()
  warm_up(ae_data_test)
  expect_length(warm_up_state$done, 0)
  run_scheduled()
  expect_equal(warm_up_state$done, all_steps)
})

test_that("The warm-up stops when a session starts", {
  reset_warm_up()
  warm_up(ae_data_test)
  # The first step only: the next is scheduled after it
  later::run_now(1)
  stop_warm_up()
  run_scheduled()
  expect_equal(warm_up_state$done, all_steps[1])
  reset_warm_up()
})

test_that("The first page request starts the warm-up, and only the first", {
  reset_warm_up()
  ui <- warm_up_on_first_request("the page", ae_data_test)
  expect_equal(ui(list()), "the page")
  expect_equal(ui(list()), "the page")
  run_scheduled()
  expect_equal(warm_up_state$done, all_steps)
  reset_warm_up()
})
