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
