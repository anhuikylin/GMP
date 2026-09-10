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
    stop(sprintf(
      "`chr_length` is missing required column(s): %s.",
      paste(missing_cols, collapse = ", ")
    ), call. = FALSE)
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

  if (!"fill" %in% names(x)) x$fill <- "#DCE6EC"
  if (!"color" %in% names(x)) x$color <- "#455A64"
  x$fill <- .gmp_recycle_color(x$fill, nrow(x), "chr_length$fill")
  x$color <- .gmp_recycle_color(x$color, nrow(x), "chr_length$color")

  x[, c("chromosome", "chr_length", "fill", "color"), drop = FALSE]
}

.gmp_validate_gene <- function(gene_info, chr) {
  empty <- function() {
    data.frame(
      chromosome = character(), start = numeric(), end = numeric(),
      label = character(), gene_color = character(), label_color = character(),
      stringsAsFactors = FALSE
    )
  }

  if (is.null(gene_info)) return(empty())

  x <- as.data.frame(gene_info, stringsAsFactors = FALSE)
  required <- c("chromosome", "start", "end", "label")
  missing_cols <- setdiff(required, names(x))
  if (length(missing_cols)) {
    stop(sprintf(
      "`gene_info` is missing required column(s): %s.",
      paste(missing_cols, collapse = ", ")
    ), call. = FALSE)
  }
  if (!nrow(x)) return(empty())

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
    stop(sprintf(
      "Unknown chromosome(s) in `gene_info`: %s.",
      paste(unknown, collapse = ", ")
    ), call. = FALSE)
  }

  x$start <- .gmp_numeric(x$start, "gene_info$start")
  x$end <- .gmp_numeric(x$end, "gene_info$end")
  if (any(x$start < 0)) {
    stop("Feature start coordinates cannot be negative.", call. = FALSE)
  }
  if (any(x$end < x$start)) {
    stop("Every feature must satisfy `end >= start`.", call. = FALSE)
  }

  chr_len <- chr$chr_length[match(x$chromosome, chr$chromosome)]
  outside <- which(x$end > chr_len)
  if (length(outside)) {
    i <- outside[1]
    stop(sprintf(
      "Feature '%s' ends at %s, beyond chromosome %s length %s.",
      x$label[i],
      format(x$end[i], scientific = FALSE),
      x$chromosome[i],
      format(chr_len[i], scientific = FALSE)
    ), call. = FALSE)
  }

  if (!"gene_color" %in% names(x)) x$gene_color <- "#2F5D7E"
  if (!"label_color" %in% names(x)) x$label_color <- "#263238"
  x$gene_color <- .gmp_recycle_color(x$gene_color, nrow(x), "gene_info$gene_color")
  x$label_color <- .gmp_recycle_color(x$label_color, nrow(x), "gene_info$label_color")

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

.gmp_scalar_integer <- function(x, name, lower = 1L, upper = .Machine$integer.max) {
  if (!is.numeric(x) || length(x) != 1L || !is.finite(x) || x != as.integer(x)) {
    stop(sprintf("`%s` must be one integer.", name), call. = FALSE)
  }
  x <- as.integer(x)
  if (x < lower || x > upper) {
    stop(sprintf("`%s` must be between %s and %s.", name, lower, upper), call. = FALSE)
  }
  x
}

.gmp_flag <- function(x, name) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    stop(sprintf("`%s` must be TRUE or FALSE.", name), call. = FALSE)
  }
  x
}

.gmp_optional_text <- function(x, name, empty_as_null = TRUE) {
  if (is.null(x)) return(NULL)
  if (!is.character(x) || length(x) != 1L || is.na(x)) {
    stop(sprintf("`%s` must be NULL or one character string.", name), call. = FALSE)
  }
  if (empty_as_null && !nzchar(x)) return(NULL)
  x
}

.gmp_recycle_color <- function(x, n, name) {
  if (is.null(x)) return(NULL)
  if (!is.character(x) || any(is.na(x)) || any(!nzchar(x))) {
    stop(sprintf("`%s` must contain valid non-empty colour strings.", name), call. = FALSE)
  }
  if (!(length(x) %in% c(1L, n))) {
    stop(sprintf("`%s` must have length 1 or %s.", name, n), call. = FALSE)
  }
  out <- rep(x, length.out = n)
  ok <- vapply(out, function(z) {
    !inherits(try(grDevices::col2rgb(z), silent = TRUE), "try-error")
  }, logical(1))
  if (!all(ok)) {
    stop(sprintf("`%s` contains invalid R colour value(s).", name), call. = FALSE)
  }
  out
}

.gmp_validate_lty <- function(x, name) {
  if (length(x) != 1L || is.na(x)) {
    stop(sprintf("`%s` must be one line type.", name), call. = FALSE)
  }
  if (is.numeric(x)) {
    if (!is.finite(x) || x < 0) stop(sprintf("`%s` must be a valid line type.", name), call. = FALSE)
    return(x)
  }
  if (is.character(x) && nzchar(x)) return(x)
  stop(sprintf("`%s` must be a numeric or character line type.", name), call. = FALSE)
}

.gmp_validate_highlight <- function(x) {
  if (is.null(x)) return(character())
  x <- as.character(x)
  if (any(is.na(x) | !nzchar(x))) {
    stop("`highlight` cannot contain missing or empty labels.", call. = FALSE)
  }
  unique(x)
}
