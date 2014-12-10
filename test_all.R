library(methods)
library(devtools)
library(testthat)

if (interactive()) {
  load_all(".")
} else {
  library(Rargs)
}

test_dir("tests/testthat")
