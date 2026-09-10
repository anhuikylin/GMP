# GMP 0.1.1

* Added publication-oriented `soft`, `colorblind`, `earth`, and `mono` chromosome palettes through `gmp_palette()`.
* Improved default chromosome colours and feature colours for cleaner scientific figures.
* Added `chromosome_shape = "capsule"`, `"rounded"`, or `"rect"` plus fill, border, alpha, width, and border-width controls.
* Added `feature_shape = "band"`, `"tick"`, `"point"`, or `"diamond"` plus colour, outline, width, alpha, and minimum-size controls.
* Added optional genomic-position Y axes with left/right placement, `bp`/`kb`/`Mb` units, configurable breaks, title, colour, and optional guide lines.
* Added figure title/subtitle, plot background, chromosome-label formatting, feature-label font/angle controls, connector styling, and feature highlighting.
* Improved automatic label-side placement to favour the outer side of the chromosome layout and reduce crowding between neighbouring chromosomes.
* Kept user-supplied chromosome and feature colours authoritative unless an explicit override is requested.
* Added regression tests for the new styling and axis parameters.

# GMP 0.1.0

* Rebuilt GMP as a focused chromosome and genomic-feature visualization package.
* Fixed the original dependency on the example-only `chr_length_default` object.
* Preserved user-supplied chromosome names instead of forcing `Chr1`, `Chr2`, and so on.
* Added strict validation for chromosome lengths, genomic coordinates, labels, and colors.
* Added reusable `gmp_plot` objects with `print()` and `plot()` methods.
* Added automatic left/right label placement and basic collision avoidance.
* Added PDF, SVG, PNG, JPEG, and TIFF export through `save_gmp()`.
* Removed the unnecessary `tidyverse`/`dplyr` runtime dependency.
* Added example input files, unit tests, GitHub Actions R CMD check, README, and complete help pages.
