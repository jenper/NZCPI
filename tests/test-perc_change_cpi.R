test_that(
  "displays error message when given a non-numeric amount arguement",
  {expect_error(perc_change_cpi('01-01-2022','01-10-2022'), "Invalid input - check input is in date format 'YYYY-MM-DD'") }
)

