#' Build a reusable genome-map plot object
#'
#' Validates chromosome and genomic-feature tables and constructs a reusable
#' `gmp_plot` object. Set `draw = FALSE` to prepare a plot without opening a
#' graphics page.
#'
#' @inheritParams GMP
#' @return A `gmp_plot` object containing validated input data and plot settings.
#' @export
plot_genome_map <- function(chr_length, gene_info = NULL, radian = 0,
                            label_side = c("auto", "left", "right"),
                            avoid_overlap = TRUE, label_offset = 0.018,
                            label_cex = 0.9, chromosome_width = NULL,
                            draw = TRUE) {
  chr <- .gmp_validate_chr(chr_length)
  gene <- .gmp_validate_gene(gene_info, chr)
  label_side <- match.arg(label_side)
  if (!is.logical(avoid_overlap) || length(avoid_overlap) != 1L || is.na(avoid_overlap)) {
    stop("`avoid_overlap` must be TRUE or FALSE.", call. = FALSE)
  }
  radian <- .gmp_scalar_numeric(radian, "radian", lower = 0, upper = 0.5)
  label_offset <- .gmp_scalar_numeric(label_offset, "label_offset", lower = 0, upper = 0.25)
  label_cex <- .gmp_scalar_numeric(label_cex, "label_cex", lower = 0, upper = 10, strict_lower = TRUE)
  if (!is.null(chromosome_width)) {
    chromosome_width <- .gmp_scalar_numeric(
      chromosome_width, "chromosome_width", lower = 0, upper = 0.3, strict_lower = TRUE
    )
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
        chromosome_width = chromosome_width
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

.gmp_feature_sides <- function(gene, label_side) {
  if (!nrow(gene)) return(character())
  if (label_side != "auto") return(rep(label_side, nrow(gene)))
  side <- character(nrow(gene))
  split_idx <- split(seq_len(nrow(gene)), gene$chromosome)
  for (idx in split_idx) {
    side[idx] <- ifelse(seq_along(idx) %% 2L == 1L, "right", "left")
  }
  side
}

#' @export
plot.gmp_plot <- function(x, newpage = TRUE, ...) {
  chr <- x$chromosomes
  gene <- x$features
  prm <- x$parameters
  n_chr <- nrow(chr)

  if (isTRUE(newpage)) grid::grid.newpage()

  x_pos <- if (n_chr == 1L) 0.45 else seq(0.08, 0.82, length.out = n_chr)
  spacing <- if (n_chr == 1L) 0.12 else min(diff(x_pos))
  chr_width <- if (is.null(prm$chromosome_width)) min(0.055, spacing * 0.48) else prm$chromosome_width
  top <- 0.90
  height_scale <- 0.78
  max_len <- max(chr$chr_length)
  chr_height <- height_scale * chr$chr_length / max_len
  chr_center_y <- top - chr_height / 2

  for (i in seq_len(n_chr)) {
    grid::grid.roundrect(
      x = grid::unit(x_pos[i], "npc"),
      y = grid::unit(chr_center_y[i], "npc"),
      width = grid::unit(chr_width, "npc"),
      height = grid::unit(chr_height[i], "npc"),
      r = grid::unit(prm$radian, "npc"),
      gp = grid::gpar(fill = chr$fill[i], col = chr$color[i], lwd = 1.5)
    )
    grid::grid.text(
      chr$chromosome[i],
      x = grid::unit(x_pos[i], "npc"),
      y = grid::unit(0.955, "npc"),
      gp = grid::gpar(fontsize = 11, fontface = "bold")
    )
  }

  if (!nrow(gene)) return(invisible(x))

  chr_index <- match(gene$chromosome, chr$chromosome)
  midpoint <- gene$start + (gene$end - gene$start) / 2
  gene_y <- top - height_scale * midpoint / max_len
  gene_h <- pmax(height_scale * (gene$end - gene$start) / max_len, 0.0055)
  gene_x <- x_pos[chr_index]
  sides <- .gmp_feature_sides(gene, prm$label_side)
  label_y <- gene_y

  if (isTRUE(prm$avoid_overlap)) {
    groups <- split(seq_len(nrow(gene)), paste(gene$chromosome, sides, sep = "\r"))
    for (idx in groups) label_y[idx] <- .gmp_adjust_labels(gene_y[idx])
  }

  for (j in seq_len(nrow(gene))) {
    grid::grid.rect(
      x = grid::unit(gene_x[j], "npc"),
      y = grid::unit(gene_y[j], "npc"),
      width = grid::unit(chr_width * 0.95, "npc"),
      height = grid::unit(gene_h[j], "npc"),
      gp = grid::gpar(fill = gene$gene_color[j], col = gene$gene_color[j])
    )

    direction <- if (sides[j] == "right") 1 else -1
    label_x <- gene_x[j] + direction * (chr_width / 2 + prm$label_offset)
    edge_x <- gene_x[j] + direction * chr_width / 2
    just <- if (direction == 1) "left" else "right"

    grid::grid.segments(
      x0 = grid::unit(edge_x, "npc"), y0 = grid::unit(gene_y[j], "npc"),
      x1 = grid::unit(label_x, "npc"), y1 = grid::unit(label_y[j], "npc"),
      gp = grid::gpar(col = gene$gene_color[j], lwd = 0.8)
    )
    grid::grid.text(
      gene$label[j],
      x = grid::unit(label_x, "npc"),
      y = grid::unit(label_y[j], "npc"),
      just = just,
      gp = grid::gpar(col = gene$label_color[j], fontsize = 10 * prm$label_cex)
    )
  }

  invisible(x)
}

#' @export
print.gmp_plot <- function(x, ...) {
  cat("<gmp_plot>\n")
  cat("  chromosomes:", nrow(x$chromosomes), "\n")
  cat("  features:   ", nrow(x$features), "\n")
  cat("  label side: ", x$parameters$label_side, "\n")
  invisible(x)
}
