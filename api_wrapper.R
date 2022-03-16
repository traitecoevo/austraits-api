library(plumber)
library(magrittr)

plumb("API.build/API examples v1.R") %>%
    pr_run(port=80, host="0.0.0.0")
