#' Build a reusable genome-map plot object
#'
#' Validates chromosome and genomic-feature tables and constructs a reusable
#' `gmp_plot` object. Set `draw = FALSE` to prepare a plot without opening a
#' graphics page.
#'
#' @inheritParams GMP
#' @return A `gmp_plot` object containing validated input data and plot settings.
#' @export
plot_genome_map <- function(
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

  chr_input <- as.data.frame(chr_length, stringsAsFactors = FALSE)
  chr_has_fill <- "fill" %in% names(chr_input)
  chr_has_border <- "color" %in% names(chr_input)

  chr <- .gmp_validate_chr(chr_length)
  gene <- .gmp_validate_gene(gene_info, chr)

  label_side <- match.arg(label_side)
  palette <- match.arg(palette)
  chromosome_shape <- match.arg(chromosome_shape)
  feature_shape <- match.arg(feature_shape)
  label_fontface <- match.arg(label_fontface)
  y_axis_side <- match.arg(y_axis_side)
  y_axis_unit <- match.arg(y_axis_unit)
  chromosome_label_position <- match.arg(chromosome_label_position)
  chromosome_label_fontface <- match.arg(chromosome_label_fontface)

  avoid_overlap <- .gmp_flag(avoid_overlap, "avoid_overlap")
  draw <- .gmp_flag(draw, "draw")
  show_labels <- .gmp_flag(show_labels, "show_labels")
  label_connector <- .gmp_flag(label_connector, "label_connector")
  show_y_axis <- .gmp_flag(show_y_axis, "show_y_axis")
  y_axis_grid <- .gmp_flag(y_axis_grid, "y_axis_grid")
  show_chr_labels <- .gmp_flag(show_chr_labels, "show_chr_labels")

  radian <- .gmp_scalar_numeric(radian, "radian", lower = 0, upper = 0.25)
  label_offset <- .gmp_scalar_numeric(label_offset, "label_offset", lower = 0, upper = 0.25)
  label_cex <- .gmp_scalar_numeric(label_cex, "label_cex", lower = 0, upper = 10, strict_lower = TRUE)
  label_angle <- .gmp_scalar_numeric(label_angle, "label_angle", lower = -360, upper = 360)
  chromosome_label_cex <- .gmp_scalar_numeric(
    chromosome_label_cex, "chromosome_label_cex", lower = 0, upper = 10, strict_lower = TRUE
  )
  chromosome_label_angle <- .gmp_scalar_numeric(
    chromosome_label_angle, "chromosome_label_angle", lower = -360, upper = 360
  )
  chromosome_border_width <- .gmp_scalar_numeric(
    chromosome_border_width, "chromosome_border_width", lower = 0, upper = 20
  )
  chromosome_alpha <- .gmp_scalar_numeric(
    chromosome_alpha, "chromosome_alpha", lower = 0, upper = 1
  )
  feature_border_width <- .gmp_scalar_numeric(
    feature_border_width, "feature_border_width", lower = 0, upper = 20
  )
  feature_alpha <- .gmp_scalar_numeric(feature_alpha, "feature_alpha", lower = 0, upper = 1)
  feature_width <- .gmp_scalar_numeric(
    feature_width, "feature_width", lower = 0.05, upper = 1.6
  )
  feature_min_height <- .gmp_scalar_numeric(
    feature_min_height, "feature_min_height", lower = 0, upper = 0.08, strict_lower = TRUE
  )
  connector_width <- .gmp_scalar_numeric(connector_width, "connector_width", lower = 0, upper = 20)
  connector_lty <- .gmp_validate_lty(connector_lty, "connector_lty")
  y_axis_breaks <- .gmp_scalar_integer(y_axis_breaks, "y_axis_breaks", lower = 2L, upper = 20L)
  y_axis_cex <- .gmp_scalar_numeric(y_axis_cex, "y_axis_cex", lower = 0, upper = 10, strict_lower = TRUE)
  y_axis_grid_width <- .gmp_scalar_numeric(
    y_axis_grid_width, "y_axis_grid_width", lower = 0, upper = 20
  )
  title_cex <- .gmp_scalar_numeric(title_cex, "title_cex", lower = 0, upper = 10, strict_lower = TRUE)
  subtitle_cex <- .gmp_scalar_numeric(
    subtitle_cex, "subtitle_cex", lower = 0, upper = 10, strict_lower = TRUE
  )
  plot_padding <- .gmp_scalar_numeric(plot_padding, "plot_padding", lower = 0.02, upper = 0.25)

  if (!is.null(chromosome_width)) {
    chromosome_width <- .gmp_scalar_numeric(
      chromosome_width, "chromosome_width", lower = 0, upper = 0.3, strict_lower = TRUE
    )
  }

  title <- .gmp_optional_text(title, "title")
  subtitle <- .gmp_optional_text(subtitle, "subtitle")
  y_axis_title <- .gmp_optional_text(y_axis_title, "y_axis_title", empty_as_null = FALSE)
  highlight <- .gmp_validate_highlight(highlight)

  background <- .gmp_recycle_color(background, 1L, "background")
  chromosome_label_color <- .gmp_recycle_color(
    chromosome_label_color, 1L, "chromosome_label_color"
  )
  y_axis_color <- .gmp_recycle_color(y_axis_color, 1L, "y_axis_color")
  y_axis_grid_color <- .gmp_recycle_color(y_axis_grid_color, 1L, "y_axis_grid_color")
  title_color <- .gmp_recycle_color(title_color, 1L, "title_color")
  subtitle_color <- .gmp_recycle_color(subtitle_color, 1L, "subtitle_color")
  highlight_color <- .gmp_recycle_color(highlight_color, 1L, "highlight_color")
  highlight_label_color <- .gmp_recycle_color(
    highlight_label_color, 1L, "highlight_label_color"
  )
  connector_color <- .gmp_recycle_color(connector_color, 1L, "connector_color")

  if (!is.null(chromosome_fill)) {
    chr$fill <- .gmp_recycle_color(chromosome_fill, nrow(chr), "chromosome_fill")
  } else if (!chr_has_fill) {
    chr$fill <- gmp_palette(palette, nrow(chr))
  }

  if (!is.null(chromosome_border)) {
    chr$color <- .gmp_recycle_color(chromosome_border, nrow(chr), "chromosome_border")
  } else if (!chr_has_border) {
    chr$color <- rep("#52636F", nrow(chr))
  }

  if (nrow(gene)) {
    if (!is.null(feature_color)) {
      gene$gene_color <- .gmp_recycle_color(feature_color, nrow(gene), "feature_color")
    }
    if (!is.null(label_color)) {
      gene$label_color <- .gmp_recycle_color(label_color, nrow(gene), "label_color")
    }
  } else {
    if (!is.null(feature_color)) .gmp_recycle_color(feature_color, 1L, "feature_color")
    if (!is.null(label_color)) .gmp_recycle_color(label_color, 1L, "label_color")
  }

  if (!is.null(feature_border)) {
    feature_border <- .gmp_recycle_color(feature_border, 1L, "feature_border")
  }

  x <- structure(
    list(
      chromosomes = chr,
      features = gene,
      parameters = list(
        radian = radian,
        label_side = label_side,
        avoid_overlap = avoid_overlap,
        label_offset = label_offset,
        label_cex = label_cex,
        chromosome_width = chromosome_width,
        palette = palette,
        chromosome_shape = chromosome_shape,
        chromosome_border_width = chromosome_border_width,
        chromosome_alpha = chromosome_alpha,
        feature_shape = feature_shape,
        feature_border = feature_border,
        feature_border_width = feature_border_width,
        feature_alpha = feature_alpha,
        feature_width = feature_width,
        feature_min_height = feature_min_height,
        show_labels = show_labels,
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
      ),
      created_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    ),
    class = "gmp_plot"
  )

  if (isTRUE(draw)) graphics::plot(x)
  x
}

.gmp_adjust_labels <- function(y, min_gap = 0.026, lower = 0.055, upper = 0.925) {
  if (length(y) <= 1L) return(y)
  max_gap <- (upper - lower) / (length(y) - 1L)
  gap <- min(min_gap, max_gap)
  ord <- order(y)
  z <- y[ord]
  for (i in 2:length(z)) z[i] <- max(z[i], z[i - 1L] + gap)
  if (max(z) > upper) z <- z - (max(z) - upper)
  if (min(z) < lower) z <- z + (lower - min(z))
  out <- numeric(length(y))
  out[ord] <- z
  out
}

.gmp_feature_sides <- function(gene, label_side, chr, x_pos) {
  if (!nrow(gene)) return(character())
  if (label_side != "auto") return(rep(label_side, nrow(gene)))

  chr_index <- match(gene$chromosome, chr$chromosome)
  side <- ifelse(x_pos[chr_index] < 0.48, "left", "right")
  middle_chr <- unique(chr_index[x_pos[chr_index] >= 0.48 & x_pos[chr_index] <= 0.52])

  for (k in middle_chr) {
    idx <- which(chr_index == k)
    side[idx] <- ifelse(seq_along(idx) %% 2L == 1L, "right", "left")
  }
  side
}

.gmp_draw_feature <- function(
    shape, x, y, width, height, fill, border, lwd, min_height) {

  if (shape == "band") {
    grid::grid.roundrect(
      x = grid::unit(x, "npc"), y = grid::unit(y, "npc"),
      width = grid::unit(width, "npc"), height = grid::unit(height, "npc"),
      r = grid::unit(min(height / 2, 0.004), "npc"),
      gp = grid::gpar(fill = fill, col = border, lwd = lwd)
    )
  } else if (shape == "tick") {
    grid::grid.segments(
      x0 = grid::unit(x - width / 2, "npc"), x1 = grid::unit(x + width / 2, "npc"),
      y0 = grid::unit(y, "npc"), y1 = grid::unit(y, "npc"),
      gp = grid::gpar(col = fill, lwd = max(1.2, lwd * 2))
    )
  } else if (shape == "point") {
    grid::grid.points(
      x = grid::unit(x, "npc"), y = grid::unit(y, "npc"),
      pch = 21, size = grid::unit(max(min_height * 2.1, 0.009), "npc"),
      gp = grid::gpar(fill = fill, col = border, lwd = lwd)
    )
  } else {
    half_x <- max(width * 0.18, 0.0045)
    half_y <- max(min_height * 1.1, 0.005)
    grid::grid.polygon(
      x = grid::unit(c(x, x + half_x, x, x - half_x), "npc"),
      y = grid::unit(c(y + half_y, y, y - half_y, y), "npc"),
      gp = grid::gpar(fill = fill, col = border, lwd = lwd)
    )
  }
}

.gmp_axis_break_values <- function(max_len, n) {
  values <- pretty(c(0, max_len), n = n)
  values[values >= 0 & values <= max_len]
}

.gmp_draw_y_axis <- function(prm, x_pos, chr_width, top, height_scale, max_len) {
  values <- .gmp_axis_break_values(max_len, prm$y_axis_breaks)
  yy <- top - height_scale * values / max_len

  side_sign <- if (prm$y_axis_side == "left") -1 else 1
  axis_x <- if (prm$y_axis_side == "left") {
    min(x_pos) - chr_width / 2 - 0.038
  } else {
    max(x_pos) + chr_width / 2 + 0.038
  }

  if (isTRUE(prm$y_axis_grid)) {
    for (y in yy) {
      grid::grid.segments(
        x0 = grid::unit(min(x_pos) - chr_width / 2, "npc"),
        x1 = grid::unit(max(x_pos) + chr_width / 2, "npc"),
        y0 = grid::unit(y, "npc"), y1 = grid::unit(y, "npc"),
        gp = grid::gpar(
          col = prm$y_axis_grid_color,
          lwd = prm$y_axis_grid_width
        )
      )
    }
  }

  grid::grid.segments(
    x0 = grid::unit(axis_x, "npc"), x1 = grid::unit(axis_x, "npc"),
    y0 = grid::unit(top, "npc"), y1 = grid::unit(top - height_scale, "npc"),
    gp = grid::gpar(col = prm$y_axis_color, lwd = 0.8)
  )

  tick <- 0.006
  labels <- .gmp_axis_labels(values, prm$y_axis_unit)
  for (i in seq_along(values)) {
    grid::grid.segments(
      x0 = grid::unit(axis_x, "npc"),
      x1 = grid::unit(axis_x + side_sign * tick, "npc"),
      y0 = grid::unit(yy[i], "npc"), y1 = grid::unit(yy[i], "npc"),
      gp = grid::gpar(col = prm$y_axis_color, lwd = 0.8)
    )
    grid::grid.text(
      labels[i],
      x = grid::unit(axis_x + side_sign * 0.011, "npc"),
      y = grid::unit(yy[i], "npc"),
      just = if (prm$y_axis_side == "left") "right" else "left",
      gp = grid::gpar(
        col = prm$y_axis_color,
        fontsize = 9 * prm$y_axis_cex
      )
    )
  }

  axis_title <- prm$y_axis_title
  if (is.null(axis_title)) axis_title <- paste0("Position (", prm$y_axis_unit, ")")
  if (!is.null(axis_title) && nzchar(axis_title)) {
    grid::grid.text(
      axis_title,
      x = grid::unit(axis_x + side_sign * 0.050, "npc"),
      y = grid::unit(top - height_scale / 2, "npc"),
      rot = if (prm$y_axis_side == "left") 90 else -90,
      gp = grid::gpar(
        col = prm$y_axis_color,
        fontsize = 10 * prm$y_axis_cex,
        fontface = "plain"
      )
    )
  }
}

#' Methods for GMP plot objects
#'
#' @param x A `gmp_plot` object.
#' @param newpage Logical; start a new grid graphics page before drawing.
#' @param ... Additional arguments reserved for future extensions.
#' @return Both methods return `x` invisibly. `plot()` also renders the map.
#' @name gmp_plot
NULL

#' @rdname gmp_plot
#' @export
plot.gmp_plot <- function(x, newpage = TRUE, ...) {
  chr <- x$chromosomes
  gene <- x$features
  prm <- x$parameters
  n_chr <- nrow(chr)

  if (isTRUE(newpage)) grid::grid.newpage()

  grid::grid.rect(
    x = grid::unit(0.5, "npc"), y = grid::unit(0.5, "npc"),
    width = grid::unit(1, "npc"), height = grid::unit(1, "npc"),
    gp = grid::gpar(fill = prm$background, col = NA)
  )

  has_title <- !is.null(prm$title)
  has_subtitle <- !is.null(prm$subtitle)
  if (has_title) {
    grid::grid.text(
      prm$title, x = grid::unit(0.5, "npc"), y = grid::unit(0.975, "npc"),
      gp = grid::gpar(
        col = prm$title_color, fontsize = 14 * prm$title_cex, fontface = "bold"
      )
    )
  }
  if (has_subtitle) {
    grid::grid.text(
      prm$subtitle, x = grid::unit(0.5, "npc"),
      y = grid::unit(if (has_title) 0.943 else 0.972, "npc"),
      gp = grid::gpar(
        col = prm$subtitle_color, fontsize = 10 * prm$subtitle_cex
      )
    )
  }

  title_space <- if (has_title && has_subtitle) 0.075 else if (has_title || has_subtitle) 0.045 else 0
  top <- 0.90 - title_space
  bottom <- 0.085
  height_scale <- top - bottom
  max_len <- max(chr$chr_length)

  axis_left <- isTRUE(prm$show_y_axis) && prm$y_axis_side == "left"
  axis_right <- isTRUE(prm$show_y_axis) && prm$y_axis_side == "right"
  left_bound <- prm$plot_padding + 0.035 + if (axis_left) 0.065 else 0
  right_bound <- 1 - prm$plot_padding - 0.035 - if (axis_right) 0.065 else 0

  if (left_bound >= right_bound) {
    stop("Plot padding and Y-axis settings leave no horizontal drawing space.", call. = FALSE)
  }

  x_pos <- if (n_chr == 1L) {
    (left_bound + right_bound) / 2
  } else {
    seq(left_bound, right_bound, length.out = n_chr)
  }

  spacing <- if (n_chr == 1L) 0.13 else min(diff(x_pos))
  chr_width <- if (is.null(prm$chromosome_width)) {
    min(0.052, spacing * 0.46)
  } else {
    prm$chromosome_width
  }

  chr_height <- height_scale * chr$chr_length / max_len
  chr_center_y <- top - chr_height / 2

  if (isTRUE(prm$show_y_axis)) {
    .gmp_draw_y_axis(prm, x_pos, chr_width, top, height_scale, max_len)
  }

  for (i in seq_len(n_chr)) {
    fill_i <- .gmp_alpha_color(chr$fill[i], prm$chromosome_alpha)
    border_i <- .gmp_alpha_color(chr$color[i], prm$chromosome_alpha)

    if (prm$chromosome_shape == "rect") {
      grid::grid.rect(
        x = grid::unit(x_pos[i], "npc"),
        y = grid::unit(chr_center_y[i], "npc"),
        width = grid::unit(chr_width, "npc"),
        height = grid::unit(chr_height[i], "npc"),
        gp = grid::gpar(
          fill = fill_i, col = border_i, lwd = prm$chromosome_border_width
        )
      )
    } else {
      radius <- if (prm$chromosome_shape == "capsule") {
        min(chr_width / 2, chr_height[i] / 2)
      } else {
        min(prm$radian, chr_width / 2, chr_height[i] / 2)
      }
      grid::grid.roundrect(
        x = grid::unit(x_pos[i], "npc"),
        y = grid::unit(chr_center_y[i], "npc"),
        width = grid::unit(chr_width, "npc"),
        height = grid::unit(chr_height[i], "npc"),
        r = grid::unit(radius, "npc"),
        gp = grid::gpar(
          fill = fill_i, col = border_i, lwd = prm$chromosome_border_width
        )
      )
    }

    if (isTRUE(prm$show_chr_labels)) {
      if (prm$chromosome_label_position == "top") {
        label_y <- top + 0.030
        just <- "centre"
      } else {
        label_y <- top - chr_height[i] - 0.024
        just <- "centre"
      }
      grid::grid.text(
        chr$chromosome[i],
        x = grid::unit(x_pos[i], "npc"),
        y = grid::unit(label_y, "npc"),
        just = just,
        rot = prm$chromosome_label_angle,
        gp = grid::gpar(
          col = prm$chromosome_label_color,
          fontsize = 11 * prm$chromosome_label_cex,
          fontface = prm$chromosome_label_fontface
        )
      )
    }
  }

  if (!nrow(gene)) return(invisible(x))

  chr_index <- match(gene$chromosome, chr$chromosome)
  midpoint <- gene$start + (gene$end - gene$start) / 2
  gene_y <- top - height_scale * midpoint / max_len
  gene_h <- pmax(height_scale * (gene$end - gene$start) / max_len, prm$feature_min_height)
  gene_x <- x_pos[chr_index]
  sides <- .gmp_feature_sides(gene, prm$label_side, chr, x_pos)
  label_y <- gene_y

  if (isTRUE(prm$avoid_overlap) && isTRUE(prm$show_labels)) {
    groups <- split(seq_len(nrow(gene)), paste(gene$chromosome, sides, sep = "\r"))
    for (idx in groups) {
      label_y[idx] <- .gmp_adjust_labels(
        gene_y[idx],
        lower = bottom + 0.01,
        upper = top - 0.01
      )
    }
  }

  for (j in seq_len(nrow(gene))) {
    highlighted <- gene$label[j] %in% prm$highlight
    feature_col <- if (highlighted) prm$highlight_color else gene$gene_color[j]
    label_col <- if (highlighted) prm$highlight_label_color else gene$label_color[j]
    border_col <- if (is.null(prm$feature_border)) feature_col else prm$feature_border
    fill_col <- .gmp_alpha_color(feature_col, prm$feature_alpha)
    border_col <- .gmp_alpha_color(border_col, prm$feature_alpha)
    glyph_width <- chr_width * prm$feature_width

    .gmp_draw_feature(
      shape = prm$feature_shape,
      x = gene_x[j], y = gene_y[j],
      width = glyph_width, height = gene_h[j],
      fill = fill_col, border = border_col,
      lwd = prm$feature_border_width,
      min_height = prm$feature_min_height
    )

    if (!isTRUE(prm$show_labels)) next

    direction <- if (sides[j] == "right") 1 else -1
    label_x <- gene_x[j] + direction * (chr_width / 2 + prm$label_offset)
    edge_x <- gene_x[j] + direction * chr_width / 2
    just <- if (direction == 1) "left" else "right"

    if (isTRUE(prm$label_connector)) {
      connector_col <- if (is.null(prm$connector_color)) feature_col else prm$connector_color
      if (highlighted) connector_col <- prm$highlight_color
      grid::grid.segments(
        x0 = grid::unit(edge_x, "npc"), y0 = grid::unit(gene_y[j], "npc"),
        x1 = grid::unit(label_x, "npc"), y1 = grid::unit(label_y[j], "npc"),
        gp = grid::gpar(
          col = .gmp_alpha_color(connector_col, 0.9),
          lwd = prm$connector_width,
          lty = prm$connector_lty
        )
      )
    }

    grid::grid.text(
      gene$label[j],
      x = grid::unit(label_x, "npc"),
      y = grid::unit(label_y[j], "npc"),
      just = just,
      rot = prm$label_angle,
      gp = grid::gpar(
        col = label_col,
        fontsize = 10 * prm$label_cex,
        fontface = prm$label_fontface
      )
    )
  }

  invisible(x)
}

#' @rdname gmp_plot
#' @export
print.gmp_plot <- function(x, ...) {
  cat("<gmp_plot>\n")
  cat("  chromosomes: ", nrow(x$chromosomes), "\n", sep = "")
  cat("  features:    ", nrow(x$features), "\n", sep = "")
  cat("  chromosome:  ", x$parameters$chromosome_shape, "\n", sep = "")
  cat("  feature:     ", x$parameters$feature_shape, "\n", sep = "")
  cat("  palette:     ", x$parameters$palette, "\n", sep = "")
  cat("  Y axis:      ", if (x$parameters$show_y_axis) x$parameters$y_axis_unit else "hidden", "\n", sep = "")
  invisible(x)
}
