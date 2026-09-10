# GMP

**GMP (Genome Map Plotting)** is a lightweight R package for publication-ready chromosome maps with gene or genomic-feature annotations.

GMP 0.1.1 combines validated genomic coordinates with flexible scientific styling: reusable plot objects, multiple chromosome/feature shapes, colour palettes, optional genomic-position axes, label collision reduction, highlighting, and vector export.

## Main features

- Any number of chromosomes and arbitrary chromosome names.
- Chromosome heights proportional to physical length.
- Strict validation of chromosome lengths and feature coordinates.
- Publication-oriented `soft`, `colorblind`, `earth`, and `mono` palettes.
- `capsule`, `rounded`, and rectangular chromosome bodies.
- `band`, `tick`, `point`, and `diamond` feature glyphs.
- Optional genomic-position Y axis in `bp`, `kb`, or `Mb`.
- Automatic feature-label placement with overlap reduction.
- Configurable labels, connectors, title/subtitle, background, borders, alpha, and highlighting.
- User-supplied colours remain authoritative unless explicitly overridden.
- Reusable `gmp_plot` objects.
- PDF/SVG vector export and high-resolution PNG/JPEG/TIFF export.
- Minimal runtime dependencies: base R, `grid`, and standard R graphics packages.
- Automated tests on Linux, Windows, and macOS.

## Installation

```r
# install.packages("pak")
pak::pak("anhuikylin/GMP")
```

For a private repository, authenticate GitHub before installation.

## Quick start

```r
library(GMP)

chr <- data.frame(
  chromosome = paste0("Chr", 1:5),
  chr_length = c(301284077, 237032434, 232132502, 241403054, 217807808)
)

genes <- data.frame(
  chromosome = c("Chr3", "Chr4", "Chr3"),
  start = c(1260877, 233800183, 210103369),
  end = c(1262711, 233811237, 210107016),
  label = c("WRKY93", "CTS3", "RINGLET2")
)

p <- GMP(
  chr_length = chr,
  gene_info = genes,
  draw = FALSE
)

plot(p)
```

When `chr_length` does not contain a `fill` column, GMP automatically uses the muted `soft` palette. If `fill` or `gene_color` columns are supplied, those colours are respected.

## Publication-style example

```r
p <- GMP(
  chr_length = chr,
  gene_info = genes,
  palette = "colorblind",
  chromosome_shape = "capsule",
  chromosome_border = "#43515C",
  chromosome_border_width = 1.1,
  feature_shape = "diamond",
  feature_color = "#2F5D7E",
  feature_border = "white",
  feature_border_width = 0.7,
  show_y_axis = TRUE,
  y_axis_side = "left",
  y_axis_unit = "Mb",
  y_axis_breaks = 6,
  y_axis_grid = TRUE,
  label_side = "auto",
  avoid_overlap = TRUE,
  label_connector = TRUE,
  highlight = "WRKY93",
  highlight_color = "#D55E00",
  title = "Genomic distribution of candidate genes",
  subtitle = "Physical positions across maize chromosomes",
  background = "white",
  draw = FALSE
)

plot(p)
save_gmp(p, "GMP_genome_map.pdf", width = 10, height = 8)
```

## Show or hide the Y axis

The Y axis is hidden by default to preserve a clean chromosome-map layout.

```r
# Show physical position in megabases
p1 <- GMP(
  chr, genes,
  show_y_axis = TRUE,
  y_axis_unit = "Mb",
  y_axis_side = "left",
  draw = FALSE
)

# Hide it explicitly
p2 <- GMP(chr, genes, show_y_axis = FALSE, draw = FALSE)
```

Available axis controls include:

```r
show_y_axis
y_axis_side
y_axis_unit
y_axis_breaks
y_axis_cex
y_axis_color
y_axis_title
y_axis_grid
y_axis_grid_color
y_axis_grid_width
```

## Colour palettes

```r
gmp_palette("soft", 5)
gmp_palette("colorblind", 5)
gmp_palette("earth", 5)
gmp_palette("mono", 5)
```

Set `palette = ...` in `GMP()` when no `fill` column is supplied. To force a specific chromosome colour regardless of input:

```r
GMP(chr, genes, chromosome_fill = "#DCE6EC")
```

## Shape and appearance controls

Chromosome controls:

```r
chromosome_shape
chromosome_fill
chromosome_border
chromosome_border_width
chromosome_alpha
chromosome_width
radian
```

Feature controls:

```r
feature_shape
feature_color
feature_border
feature_border_width
feature_alpha
feature_width
feature_min_height
```

Label and annotation controls:

```r
show_labels
label_side
avoid_overlap
label_offset
label_cex
label_color
label_fontface
label_angle
label_connector
connector_color
connector_width
connector_lty
highlight
highlight_color
highlight_label_color
```

Figure controls:

```r
show_chr_labels
chromosome_label_position
chromosome_label_cex
chromosome_label_color
chromosome_label_fontface
chromosome_label_angle
title
subtitle
title_cex
subtitle_cex
title_color
subtitle_color
background
plot_padding
```

## Input format

`chr_length` requires:

- `chromosome`: unique chromosome/contig/scaffold identifier.
- `chr_length`: positive physical length.

Optional chromosome columns:

- `fill`
- `color`

`gene_info` requires:

- `chromosome`: chromosome identifier matching `chr_length`.
- `start`: feature start coordinate.
- `end`: feature end coordinate.
- `label`: feature label.

Optional feature columns:

- `gene_color`
- `label_color`

Coordinates must satisfy `0 <= start <= end <= chr_length`.

## Export

```r
save_gmp(p, "genome_map.pdf", width = 10, height = 8)
save_gmp(p, "genome_map.svg", width = 10, height = 8)
save_gmp(p, "genome_map.png", width = 10, height = 8, res = 600)
```

PDF and SVG preserve editable vector graphics.

## Example files

Small input examples are bundled in `inst/extdata/`:

```r
system.file("extdata", "maize_chr_length.csv", package = "GMP")
system.file("extdata", "maize_genes.csv", package = "GMP")
```

## Design principle

GMP separates **validated genomic data**, **plot parameters**, and **rendering**. A call with `draw = FALSE` produces a self-contained `gmp_plot` object that can be redrawn or exported later.

## License

MIT.
