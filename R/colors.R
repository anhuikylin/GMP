#' Publication-oriented chromosome colour palettes
#'
#' Returns a reproducible vector of colours designed for clean chromosome-map
#' figures. Palettes are dependency-free and can be used directly in
#' `chr_length$fill` or through the `palette` argument of [GMP()].
#'
#' @param name Palette name: `"soft"`, `"colorblind"`, `"earth"`, or `"mono"`.
#' @param n Number of colours to return.
#'
#' @return A character vector of hexadecimal colours.
#' @export
gmp_palette <- function(name = c("soft", "colorblind", "earth", "mono"), n = 10L) {
  name <- match.arg(name)
  if (!is.numeric(n) || length(n) != 1L || !is.finite(n) || n < 1 || n != as.integer(n)) {
    stop("`n` must be one positive integer.", call. = FALSE)
  }
  n <- as.integer(n)

  base <- switch(
    name,
    soft = c(
      "#A9C6D9", "#C9D9B8", "#E7C6A5", "#D6B5C9", "#B8CFCA",
      "#D7C8A7", "#C5C9DD", "#D8C1B4", "#B7D3D0", "#D0C5D9"
    ),
    colorblind = c(
      "#0072B2", "#E69F00", "#009E73", "#D55E00",
      "#CC79A7", "#56B4E9", "#F0E442", "#4D4D4D"
    ),
    earth = c(
      "#6B8E73", "#A98467", "#6C8EA4", "#C9A66B",
      "#8E7DBE", "#B46A55", "#6B9D8A", "#A7A15A"
    ),
    mono = c("#E7EEF2", "#BBCBD5", "#89A6B6", "#587E91", "#2D596F")
  )

  if (n <= length(base)) return(base[seq_len(n)])
  grDevices::colorRampPalette(base)(n)
}

.gmp_alpha_color <- function(col, alpha) {
  grDevices::adjustcolor(col, alpha.f = alpha)
}

.gmp_axis_labels <- function(values, unit) {
  divisor <- switch(unit, bp = 1, kb = 1e3, Mb = 1e6)
  scaled <- values / divisor
  digits <- if (unit == "bp") 0 else if (max(abs(scaled), na.rm = TRUE) < 10) 2 else 1
  out <- format(round(scaled, digits), trim = TRUE, scientific = FALSE, nsmall = 0)
  sub("\\.0+$", "", out)
}
