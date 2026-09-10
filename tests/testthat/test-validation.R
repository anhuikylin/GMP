test_that("chromosome input is validated", {
  expect_error(GMP(NULL, draw = FALSE), "chr_length")
  expect_error(
    GMP(data.frame(chromosome = c("Chr1", "Chr1"), chr_length = c(10, 20)), draw = FALSE),
    "Duplicated"
  )
  expect_error(
    GMP(data.frame(chromosome = "Chr1", chr_length = 0), draw = FALSE),
    "greater than zero"
  )
})

test_that("feature coordinates are validated", {
  chr <- data.frame(chromosome = "Chr1", chr_length = 100)
  bad_chr <- data.frame(chromosome = "Chr2", start = 1, end = 2, label = "x")
  bad_order <- data.frame(chromosome = "Chr1", start = 10, end = 2, label = "x")
  outside <- data.frame(chromosome = "Chr1", start = 90, end = 110, label = "x")

  expect_error(GMP(chr, bad_chr, draw = FALSE), "Unknown chromosome")
  expect_error(GMP(chr, bad_order, draw = FALSE), "end >= start")
  expect_error(GMP(chr, outside, draw = FALSE), "beyond chromosome")
})
