test_that("plot method renders without changing the object", {
  chr <- data.frame(chromosome = c("Chr1", "Chr2"), chr_length = c(100, 75))
  gene <- data.frame(
    chromosome = c("Chr1", "Chr1", "Chr2"),
    start = c(10, 12, 30), end = c(11, 13, 31),
    label = c("A", "B", "C"),
    gene_color = c("black", "grey30", "black"),
    label_color = c("black", "black", "black")
  )
  p <- GMP(chr, gene, draw = FALSE, avoid_overlap = TRUE)
  f <- tempfile(fileext = ".pdf")
  grDevices::pdf(f)
  expect_silent(plot(p))
  grDevices::dev.off()
  expect_true(file.exists(f))
  unlink(f)
})

test_that("save_gmp exports PDF", {
  chr <- data.frame(chromosome = "Chr1", chr_length = 100)
  p <- GMP(chr, draw = FALSE)
  f <- tempfile(fileext = ".pdf")
  expect_silent(save_gmp(p, f, width = 5, height = 4))
  expect_true(file.exists(f))
  expect_gt(file.info(f)$size, 0)
  unlink(f)
})
