test_that("GMP returns a reusable plot object", {
  chr <- data.frame(chromosome = c("A01", "B02"), chr_length = c(100, 80))
  gene <- data.frame(
    chromosome = c("A01", "B02"), start = c(10, 20), end = c(15, 25),
    label = c("g1", "g2")
  )

  p <- GMP(chr, gene, draw = FALSE)
  expect_s3_class(p, "gmp_plot")
  expect_equal(p$chromosomes$chromosome, c("A01", "B02"))
  expect_equal(p$features$label, c("g1", "g2"))
  expect_equal(p$parameters$label_side, "auto")
})

test_that("custom chromosome counts do not depend on example objects", {
  chr <- data.frame(chromosome = paste0("scaffold_", 1:4), chr_length = c(100, 90, 80, 70))
  expect_no_error(GMP(chr, draw = FALSE))
})
