library(plumber)

plumb("API.build/API examples v1.R") |>
    pr_run(port=8000, host="0.0.0.0")
