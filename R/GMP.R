#' Draw a chromosome-level genomic feature map
#'
#' `GMP()` is the compatibility entry point for the package. It delegates to
#' [plot_genome_map()] and returns a reusable `gmp_plot` object.
#'
#' @param chr_length Data frame with `chromosome` and `chr_length`. Optional
#'   columns `fill` and `color` control chromosome appearance.
#' @param gene_info Optional data frame with `chromosome`, `start`, `end`, and
#'   `label`. Optional columns `gene_color` and `label_color` control features.
#' @param radian Non-negative corner radius in normalized device coordinates.
#' @param label_side Label placement: `"auto"`, `"left"`, or `"right"`.
#' @param avoid_overlap Logical; adjust nearby labels vertically when possible.
#' @param label_offset Horizontal distance between chromosome and label.
#' @param label_cex Label text scaling factor.
#' @param chromosome_width Optional chromosome width in normalized coordinates.
#' @param draw Logical; draw immediately when `TRUE`.
#'
#' @return A `gmp_plot` object. If `draw = TRUE`, the plot is also rendered.
#' @export
GMP <- function(chr_length, gene_info = NULL, radian = 0,
                label_side = c("auto", "left", "right"),
                avoid_overlap = TRUE, label_offset = 0.018,
                label_cex = 0.9, chromosome_width = NULL,
                draw = TRUE) {
  plot_genome_map(
    chr_length = chr_length,
    gene_info = gene_info,
    radian = radian,
    label_side = label_side,
    avoid_overlap = avoid_overlap,
    label_offset = label_offset,
    label_cex = label_cex,
    chromosome_width = chromosome_width,
    draw = draw
  )
}
