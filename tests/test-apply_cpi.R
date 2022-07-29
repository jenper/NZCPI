test_that(
  "displays error message when given a non-numeric amount arguement",
  {expect_error(apply_cpi("test"), "Check value to convert is numeric type") }
)

