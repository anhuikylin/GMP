.gmp_numeric <- function(x, name) {
  out <- suppressWarnings(as.numeric(as.character(x)))
  if (length(out) != length(x) || any(!is.finite(out))) {
    stop(sprintf("`%s` must contain only finite numeric values.", name), call. = FALSE)
  }
  out
}

.gmp_validate_chr <- function(chr_length) {
  if (is.null(chr_length)) {
    stop("`chr_length` is required.", call. = FALSE)
  }
  x <- as.data.frame(chr_length, stringsAsFactors = FALSE)
  required <- c("chromosome", "chr_length")
  missing_cols <- setdiff(required, names(x))
  if (length(missing_cols)) {
    stop(sprintf("`chr_length` is missing required column(s): %s.", paste(missing_cols, collapse = ", ")), call. = FALSE)
  }
  if (!nrow(x)) {
    stop("`chr_length` must contain at least one chromosome.", call. = FALSE)
  }
  x$chromosome <- as.character(x$chromosome)
  if (any(is.na(x$chromosome) | !nzchar(x$chromosome))) {
    stop("`chr_length$chromosome` cannot contain missing or empty names.", call. = FALSE)
  }
  if (anyDuplicated(x$chromosome)) {
    dup <- unique(x$chromosome[duplicated(x$chromosome)])
    stop(sprintf("Duplicated chromosome name(s): %s.", paste(dup, collapse = ", ")), call. = FALSE)
  }
  x$chr_length <- .gmp_numeric(x$chr_length, "chr_length$chr_length")
  if (any(x$chr_length <= 0)) {
    stop("All chromosome lengths must be greater than zero.", call. = FALSE)
  }
  if (!"fill" %in% names(x)) x$fill <- "grey85"
  if (!"color" %in% names(x)) x$color <- "grey35"
  x$fill <- as.character(x$fill)
  x$color <- as.character(x$color)
  if (any(is.na(x$fill) | !nzchar(x$fill) | is.na(x$color) | !nzchar(x$color))) {
    stop("Chromosome `fill` and `color` values cannot be missing or empty.", call. = FALSE)
  }
  x[, c("chromosome", "chr_length", "fill", "color"), drop = FALSE]
}

.gmp_validate_gene <- function(gene_info, chr) {
  if (is.null(gene_info)) {
    return(data.frame(
      chromosome = character(), start = numeric(), end = numeric(),
      label = character(), gene_color = character(), label_color = character(),
      stringsAsFactors = FALSE
    ))
  }
  x <- as.data.frame(gene_info, stringsAsFactors = FALSE)
  required <- c("chromosome", "start", "end", "label")
  missing_cols <- setdiff(required, names(x))
  if (length(missing_cols)) {
    stop(sprintf("`gene_info` is missing required column(s): %s.", paste(missing_cols, collapse = ", ")), call. = FALSE)
  }
  if (!nrow(x)) {
    return(data.frame(
      chromosome = character(), start = numeric(), end = numeric(),
      label = character(), gene_color = character(), label_color = character(),
      stringsAsFactors = FALSE
    ))
  }
  x$chromosome <- as.character(x$chromosome)
  x$label <- as.character(x$label)
  if (any(is.na(x$chromosome) | !nzchar(x$chromosome))) {
    stop("`gene_info$chromosome` cannot contain missing or empty names.", call. = FALSE)
  }
  if (any(is.na(x$label) | !nzchar(x$label))) {
    stop("`gene_info$label` cannot contain missing or empty labels.", call. = FALSE)
  }
  unknown <- setdiff(unique(x$chromosome), chr$chromosome)
  if (length(unknown)) {
    stop(sprintf("Unknown chromosome(s) in `gene_info`: %s.", paste(unknown, collapse = ", ")), call. = FALSE)
  }
  x$start <- .gmp_numeric(x$start, "gene_info$start")
  x$end <- .gmp_numeric(x$end, "gene_info$end")
  if (any(x$start < 0)) stop("Feature start coordinates cannot be negative.", call. = FALSE)
  if (any(x$end < x$start)) stop("Every feature must satisfy `end >= start`.", call. = FALSE)
  chr_len <- chr$chr_length[match(x$chromosome, chr$chromosome)]
  outside <- which(x$end > chr_len)
  if (length(outside)) {
    i <- outside[1]
    stop(sprintf(
      "Feature '%s' ends at %s, beyond chromosome %s length %s.",
      x$label[i], format(x$end[i], scientific = FALSE), x$chromosome[i],
      format(chr_len[i], scientific = FALSE)
    ), call. = FALSE)
  }
  if (!"gene_color" %in% names(x)) x$gene_color <- "black"
  if (!"label_color" %in% names(x)) x$label_color <- "black"
  x$gene_color <- as.character(x$gene_color)
  x$label_color <- as.character(x$label_color)
  if (any(is.na(x$gene_color) | !nzchar(x$gene_color) | is.na(x$label_color) | !nzchar(x$label_color))) {
    stop("Feature `gene_color` and `label_color` values cannot be missing or empty.", call. = FALSE)
  }
  x[, c("chromosome", "start", "end", "label", "gene_color", "label_color"), drop = FALSE]
}

.gmp_scalar_numeric <- function(x, name, lower = -Inf, upper = Inf, strict_lower = FALSE) {
  if (!is.numeric(x) || length(x) != 1L || !is.finite(x)) {
    stop(sprintf("`%s` must be one finite numeric value.", name), call. = FALSE)
  }
  bad_lower <- if (strict_lower) x <= lower else x < lower
  if (bad_lower || x > upper) {
    op <- if (strict_lower) ">" else ">="
    stop(sprintf("`%s` must be %s %s and <= %s.", name, op, lower, upper), call. = FALSE)
  }
  x
}
