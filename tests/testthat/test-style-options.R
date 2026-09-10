test_that("publication styling parameters are stored and validated", {
  chr <- data.frame(
    chromosome = c("Chr1", "Chr2", "Chr3"),
    chr_length = c(300e6, 250e6, 220e6)
  )
  gene <- data.frame(
    chromosome = c("Chr1", "Chr2", "Chr3"),
    start = c(10e6, 30e6, 40e6),
    end = c(11e6, 31e6, 41e6),
    label = c("g1", "g2", "g3")
  )

  p <- GMP(
    chr, gene,
    palette = "colorblind",
    chromosome_shape = "rounded",
    feature_shape = "diamond",
    show_y_axis = TRUE,
    y_axis_side = "right",
    y_axis_unit = "Mb",
    y_axis_breaks = 6,
    y_axis_grid = TRUE,
    show_chr_labels = TRUE,
    highlight = "g2",
    title = "Genome map",
    draw = FALSE
  )

  expect_s3_class(p, "gmp_plot")
  expect_true(p$parameters$show_y_axis)
  expect_equal(p$parameters$y_axis_unit, "Mb")
  expect_equal(p$parameters$chromosome_shape, "rounded")
  expect_equal(p$parameters$feature_shape, "diamond")
  expect_equal(p$parameters$highlight, "g2")
  expect_length(unique(p$chromosomes$fill), 3)
})

test_that("user chromosome colours remain authoritative", {
  chr <- data.frame(
    chromosome = c("A", "B"),
    chr_length = c(100, 90),
    fill = c("#111111", "#222222")
  )

  p <- GMP(chr, palette = "colorblind", draw = FALSE)
  expect_equal(p$chromosomes$fill, c("#111111", "#222222"))

  p2 <- GMP(chr, chromosome_fill = "#ABCDEF", draw = FALSE)
  expect_equal(p2$chromosomes$fill, rep("#ABCDEF", 2))
})

test_that("palette helper and style validation work", {
  expect_length(gmp_palette("soft", 12), 12)
  expect_length(gmp_palette("colorblind", 4), 4)
  expect_error(gmp_palette("soft", 0), "positive integer")
  expect_error(
    GMP(
      data.frame(chromosome = "Chr1", chr_length = 100),
      show_y_axis = NA,
      draw = FALSE
    ),
    "show_y_axis"
  )
  expect_error(
    GMP(
      data.frame(chromosome = "Chr1", chr_length = 100),
      y_axis_breaks = 1,
      draw = FALSE
    ),
    "y_axis_breaks"
  )
})
