test_that("function returns correct output", {
  library(snackapp)

  # create test dataframe and then use this to run tests - could be something like:
  test_data <- data.frame(
    id = 1,
    Date = seq(from = as.POSIXct("2021-01-01T00:00:00"), by = 15, length.out = 22),
    Event = c(
      "app-state-changed", "app-state-changed", "app-state-changed", "app-state-changed", "app-state-changed",
      "app-state-changed", "app-state-changed", "app-state-changed", "viewed-screen", "viewed-screen", "viewed-screen",
      "viewed-screen", "viewed-screen", "viewed-screen", "viewed-screen", "viewed-screen", "viewed-screen", "viewed-screen",
      "viewed-screen", "viewed-screen", "notification-pressed", "notification-pressed"
    ),
    Metric = c(
      "active", "background", "active", "background", "active", "background", "active", "background", "my-stat", "my-stat",
      "my-stat", "my-stat", "resources", "resources", "resources", "resources", "my-goal", "my-goal", "my-goal", "my-goal",
      "1460", "1460"
    ),
    Goal.Is.Automatic.Set = 1:22,
    Goal.Value.Set = 1:22
  )

  summary <- participant_summary(test_data)

  expect_type(summary, "list")
  expect_equal(summary$total_time, 60)
  expect_equal(summary$average_time, 15)
  expect_equal(summary$std_dev_time, 0)
  expect_equal(summary$minimum_time, 15)
  expect_equal(summary$maximum_time, 15)
  expect_equal(summary$total_stat, 60)
  expect_equal(summary$total_resource, 60)
  expect_equal(summary$total_goal, 60)
  expect_equal(summary$average_stat, 15)
  expect_equal(summary$average_resource, 15)
  expect_equal(summary$average_goal, 15)
  expect_equal(summary$stdev_stat, 0)
  expect_equal(summary$stdev_goal, 0)
  expect_equal(summary$stdev_resources, 0)
  expect_equal(summary$min_stat, 15)
  expect_equal(summary$min_goal, 15)
  expect_equal(summary$min_resource, 15)
  expect_equal(summary$max_stat, 15)
  expect_equal(summary$max_goal, 15)
  expect_equal(summary$max_resource, 15)

})


