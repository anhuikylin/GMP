#' Draw a publication-ready chromosome-level genomic feature map
#'
#' `GMP()` is the compatibility entry point for the package. It delegates to
#' [plot_genome_map()] and returns a reusable `gmp_plot` object. The first
#' arguments remain compatible with GMP 0.1.0; additional arguments control
#' publication-oriented colour, shape, labels, axes, and annotation styling.
#'
#' @param chr_length Data frame with `chromosome` and `chr_length`. Optional
#'   columns `fill` and `color` control chromosome appearance.
#' @param gene_info Optional data frame with `chromosome`, `start`, `end`, and
#'   `label`. Optional columns `gene_color` and `label_color` control features.
#' @param radian Corner radius used when `chromosome_shape = "rounded"`.
#' @param label_side Feature-label placement: `"auto"`, `"left"`, or `"right"`.
#' @param avoid_overlap Logical; adjust nearby labels vertically.
#' @param label_offset Horizontal distance between chromosome and label.
#' @param label_cex Feature-label text scaling factor.
#' @param chromosome_width Optional chromosome width in normalized coordinates.
#' @param draw Logical; draw immediately when `TRUE`.
#' @param palette Chromosome palette used only when `chr_length$fill` is absent.
#'   One of `"soft"`, `"colorblind"`, `"earth"`, or `"mono"`.
#' @param chromosome_shape Chromosome body shape: `"capsule"`, `"rounded"`,
#'   or `"rect"`.
#' @param chromosome_fill Optional scalar or per-chromosome fill override.
#' @param chromosome_border Optional scalar or per-chromosome border override.
#' @param chromosome_border_width Chromosome border line width.
#' @param chromosome_alpha Chromosome fill/border alpha in `[0, 1]`.
#' @param feature_shape Feature glyph: `"band"`, `"tick"`, `"point"`, or
#'   `"diamond"`.
#' @param feature_color Optional scalar or per-feature colour override.
#' @param feature_border Optional feature outline colour. `NULL` uses the
#'   feature colour.
#' @param feature_border_width Feature outline/line width.
#' @param feature_alpha Feature alpha in `[0, 1]`.
#' @param feature_width Feature glyph width relative to chromosome width.
#' @param feature_min_height Minimum visible feature height in normalized units.
#' @param show_labels Logical; show feature labels.
#' @param label_color Optional scalar or per-feature label-colour override.
#' @param label_fontface Label font face.
#' @param label_angle Feature-label rotation in degrees.
#' @param label_connector Logical; draw chromosome-to-label connector lines.
#' @param connector_color Connector colour. `NULL` uses feature colour.
#' @param connector_width Connector line width.
#' @param connector_lty Connector line type.
#' @param highlight Optional feature labels to highlight.
#' @param highlight_color Highlight feature colour.
#' @param highlight_label_color Highlight label colour.
#' @param show_y_axis Logical; display a genomic-position Y axis.
#' @param y_axis_side Y-axis side: `"left"` or `"right"`.
#' @param y_axis_unit Y-axis unit: `"Mb"`, `"kb"`, or `"bp"`.
#' @param y_axis_breaks Approximate number of Y-axis breaks.
#' @param y_axis_cex Y-axis text scaling factor.
#' @param y_axis_color Axis, tick, and text colour.
#' @param y_axis_title Axis title. `NULL` creates an automatic title.
#' @param y_axis_grid Logical; draw horizontal genomic-position guide lines.
#' @param y_axis_grid_color Grid-line colour.
#' @param y_axis_grid_width Grid-line width.
#' @param show_chr_labels Logical; display chromosome names.
#' @param chromosome_label_position Chromosome-name position: `"top"` or
#'   `"bottom"`.
#' @param chromosome_label_cex Chromosome-name text scaling factor.
#' @param chromosome_label_color Chromosome-name colour.
#' @param chromosome_label_fontface Chromosome-name font face.
#' @param chromosome_label_angle Chromosome-name rotation in degrees.
#' @param title Optional figure title.
#' @param subtitle Optional figure subtitle.
#' @param title_cex Title text scaling factor.
#' @param subtitle_cex Subtitle text scaling factor.
#' @param title_color Title colour.
#' @param subtitle_color Subtitle colour.
#' @param background Plot background colour.
#' @param plot_padding Horizontal outer padding in normalized coordinates.
#'
#' @return A `gmp_plot` object. If `draw = TRUE`, the plot is also rendered.
#' @export
GMP <- function(
    chr_length, gene_info = NULL, radian = 0.012,
    label_side = c("auto", "left", "right"),
    avoid_overlap = TRUE, label_offset = 0.018,
    label_cex = 0.9, chromosome_width = NULL,
    draw = TRUE,
    palette = c("soft", "colorblind", "earth", "mono"),
    chromosome_shape = c("capsule", "rounded", "rect"),
    chromosome_fill = NULL, chromosome_border = NULL,
    chromosome_border_width = 1.1, chromosome_alpha = 1,
    feature_shape = c("band", "tick", "point", "diamond"),
    feature_color = NULL, feature_border = NULL,
    feature_border_width = 0.8, feature_alpha = 1,
    feature_width = 0.90, feature_min_height = 0.0055,
    show_labels = TRUE, label_color = NULL,
    label_fontface = c("plain", "bold", "italic", "bold.italic"),
    label_angle = 0, label_connector = TRUE,
    connector_color = "#98A2AD", connector_width = 0.8,
    connector_lty = 1,
    highlight = NULL, highlight_color = "#D55E00",
    highlight_label_color = "#B44A00",
    show_y_axis = FALSE, y_axis_side = c("left", "right"),
    y_axis_unit = c("Mb", "kb", "bp"), y_axis_breaks = 5,
    y_axis_cex = 0.78, y_axis_color = "#4A5560",
    y_axis_title = NULL, y_axis_grid = FALSE,
    y_axis_grid_color = "#E6EBEF", y_axis_grid_width = 0.6,
    show_chr_labels = TRUE,
    chromosome_label_position = c("top", "bottom"),
    chromosome_label_cex = 1,
    chromosome_label_color = "#263238",
    chromosome_label_fontface = c("bold", "plain", "italic"),
    chromosome_label_angle = 0,
    title = NULL, subtitle = NULL, title_cex = 1.25,
    subtitle_cex = 0.9, title_color = "#1F2933",
    subtitle_color = "#5B6770", background = "white",
    plot_padding = 0.08) {

  plot_genome_map(
    chr_length = chr_length,
    gene_info = gene_info,
    radian = radian,
    label_side = label_side,
    avoid_overlap = avoid_overlap,
    label_offset = label_offset,
    label_cex = label_cex,
    chromosome_width = chromosome_width,
    draw = draw,
    palette = palette,
    chromosome_shape = chromosome_shape,
    chromosome_fill = chromosome_fill,
    chromosome_border = chromosome_border,
    chromosome_border_width = chromosome_border_width,
    chromosome_alpha = chromosome_alpha,
    feature_shape = feature_shape,
    feature_color = feature_color,
    feature_border = feature_border,
    feature_border_width = feature_border_width,
    feature_alpha = feature_alpha,
    feature_width = feature_width,
    feature_min_height = feature_min_height,
    show_labels = show_labels,
    label_color = label_color,
    label_fontface = label_fontface,
    label_angle = label_angle,
    label_connector = label_connector,
    connector_color = connector_color,
    connector_width = connector_width,
    connector_lty = connector_lty,
    highlight = highlight,
    highlight_color = highlight_color,
    highlight_label_color = highlight_label_color,
    show_y_axis = show_y_axis,
    y_axis_side = y_axis_side,
    y_axis_unit = y_axis_unit,
    y_axis_breaks = y_axis_breaks,
    y_axis_cex = y_axis_cex,
    y_axis_color = y_axis_color,
    y_axis_title = y_axis_title,
    y_axis_grid = y_axis_grid,
    y_axis_grid_color = y_axis_grid_color,
    y_axis_grid_width = y_axis_grid_width,
    show_chr_labels = show_chr_labels,
    chromosome_label_position = chromosome_label_position,
    chromosome_label_cex = chromosome_label_cex,
    chromosome_label_color = chromosome_label_color,
    chromosome_label_fontface = chromosome_label_fontface,
    chromosome_label_angle = chromosome_label_angle,
    title = title,
    subtitle = subtitle,
    title_cex = title_cex,
    subtitle_cex = subtitle_cex,
    title_color = title_color,
    subtitle_color = subtitle_color,
    background = background,
    plot_padding = plot_padding
  )
}
