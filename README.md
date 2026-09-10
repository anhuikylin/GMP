# GMP

**GMP (Genome Map Plotting)** is a lightweight R package for publication-ready chromosome maps with gene or genomic-feature annotations.

GMP 0.1.0 focuses on one task and does it reproducibly: convert chromosome lengths plus genomic coordinates into a reusable plot object that can be redrawn or exported as vector graphics.

## Main features

- Any number of chromosomes and arbitrary chromosome names.
- Chromosome heights proportional to physical length.
- Gene or feature coordinates validated against chromosome lengths.
- Per-chromosome and per-feature colors.
- Automatic left/right feature labels with basic overlap avoidance.
- Reusable `gmp_plot` objects.
- PDF and SVG vector export, plus PNG/JPEG/TIFF raster export.
- Minimal runtime dependencies: plotting uses base R and `grid`.
- Automated tests on Linux, Windows, and macOS.

## Installation

```r
# install.packages("pak")
pak::pak("anhuikylin/GMP")
```

For a private repository, authenticate GitHub in the usual way before installation.

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
  label_side = "auto",
  avoid_overlap = TRUE,
  draw = FALSE
)

p
plot(p)
```

The compatibility function `GMP()` delegates to the more explicit `plot_genome_map()` API.

## Input format

`chr_length` requires:

- `chromosome`: unique chromosome/contig/scaffold identifier.
- `chr_length`: positive physical length.

Optional chromosome columns are `fill` and `color`.

`gene_info` requires:

- `chromosome`: chromosome identifier matching `chr_length`.
- `start`: feature start coordinate.
- `end`: feature end coordinate.
- `label`: feature label.

Optional feature columns are `gene_color` and `label_color`.

Coordinates must satisfy `0 <= start <= end <= chr_length`.

## Export

```r
save_gmp(p, "genome_map.pdf", width = 10, height = 8)
save_gmp(p, "genome_map.svg", width = 10, height = 8)
save_gmp(p, "genome_map.png", width = 10, height = 8, res = 600)
```

## Example files

Small input examples are bundled in `inst/extdata/`:

```r
system.file("extdata", "maize_chr_length.csv", package = "GMP")
system.file("extdata", "maize_genes.csv", package = "GMP")
```

## Design principle

GMP separates **validated genomic data**, **plot parameters**, and **rendering**. A call with `draw = FALSE` produces a self-contained `gmp_plot` object, making the visualization reproducible and suitable for later redrawing or export.

## License

MIT.
