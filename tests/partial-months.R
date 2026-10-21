source("global.R", encoding = "UTF-8")

expect_true <- function(ok, msg) {
  if (!isTRUE(ok)) {
    cli::cli_abort(msg)
  }
}

# One line, three months: complete, partial (half the implied total), and
# without averages. Totals match the averages exactly for the complete month.
toy_month <- function(date, total, mdu, msa, mdo) {
  tibble::tibble(
    date = as.Date(date),
    line_number = 9L,
    metric = c("total", "mdu", "msa", "mdo", "max"),
    value = c(total, mdu, msa, mdo, mdu * 1.1)
  )
}
cal <- calendar_monthly |> filter(date == as.Date("2025-03-01"))
full_total <- 100 * cal$bdays + 50 * cal$sats + 20 * cal$other
toy <- bind_rows(
  toy_month("2025-03-01", full_total, 100, 50, 20),
  toy_month("2025-04-01", 1000, 100, 50, 20),
  toy_month("2025-05-01", 1000, NA, NA, NA)
)
kept <- suppressMessages(drop_partial_months(toy))

## Partial months are dropped with all their metrics ----
expect_true(
  !any(kept$date == as.Date("2025-04-01")),
  "A month whose total falls far short of its averages must be dropped."
)
expect_true(
  sum(kept$date == as.Date("2025-03-01")) == 5,
  "A complete month must keep all its metrics."
)
expect_true(
  sum(kept$date == as.Date("2025-05-01")) == 5,
  "A month without averages cannot be checked and must be kept."
)

## The app's line series exclude the known partial month ----
expect_true(
  !any(ent$line_number == "5" & ent$date == as.Date("2026-04-01")),
  "Line 5 entrance for Apr 2026 is a partial month and must be dropped."
)
expect_true(
  any(
    line_entries_monthly$line_number == 5 &
      line_entries_monthly$date == as.Date("2026-04-01")
  ),
  "The download tables must keep the published rows."
)

cli::cli_alert_success("Partial-month guard passed.")
