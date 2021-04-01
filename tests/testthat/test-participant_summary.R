test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

library(snackapp)

# create test dataframe and then use this to run tests - could be something like:
test_data <- data.frame(
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

participant_summary(test_data)

