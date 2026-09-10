#' Save a GMP plot
#'
#' Saves a `gmp_plot` object as PDF, SVG, PNG, JPEG, or TIFF. PDF and SVG
#' preserve vector graphics.
#'
#' @param x A `gmp_plot` object.
#' @param filename Output filename. Extension determines the device.
#' @param width Device width in inches.
#' @param height Device height in inches.
#' @param res Resolution in dots per inch for raster formats.
#' @return The output path, invisibly.
#' @export
save_gmp <- function(x, filename, width = 10, height = 8, res = 300) {
  if (!inherits(x, "gmp_plot")) stop("`x` must be a `gmp_plot` object.", call. = FALSE)
  if (!is.character(filename) || length(filename) != 1L || !nzchar(filename)) {
    stop("`filename` must be one non-empty path.", call. = FALSE)
  }
  width <- .gmp_scalar_numeric(width, "width", lower = 0, strict_lower = TRUE)
  height <- .gmp_scalar_numeric(height, "height", lower = 0, strict_lower = TRUE)
  res <- .gmp_scalar_numeric(res, "res", lower = 0, strict_lower = TRUE)
  ext <- tolower(tools::file_ext(filename))

  if (ext == "pdf") {
    grDevices::pdf(filename, width = width, height = height, onefile = TRUE)
  } else if (ext == "svg") {
    grDevices::svg(filename, width = width, height = height, onefile = TRUE)
  } else if (ext == "png") {
    grDevices::png(filename, width = width, height = height, units = "in", res = res)
  } else if (ext %in% c("jpg", "jpeg")) {
    grDevices::jpeg(filename, width = width, height = height, units = "in", res = res, quality = 95)
  } else if (ext %in% c("tif", "tiff")) {
    grDevices::tiff(filename, width = width, height = height, units = "in", res = res, compression = "lzw")
  } else {
    stop("Unsupported output extension. Use pdf, svg, png, jpg/jpeg, or tif/tiff.", call. = FALSE)
  }

  on.exit(grDevices::dev.off(), add = TRUE)
  graphics::plot(x, newpage = TRUE)
  invisible(normalizePath(filename, winslash = "/", mustWork = FALSE))
}
