#' @keywords internal
"_PACKAGE"

#' ChartKit: Publication-Ready Data Visualization for R
#'
#' @description
#' ChartKit is a comprehensive R package providing 48 chart types optimized for
#' scientific publications. Built on ggplot2 with colorblind-friendly palettes
#' and publication-ready themes based on best practices from leading scientific
#' journals.
#'
#' @details
#' ## Main Features
#'
#' - **48 Chart Types**: From basic scatter plots to advanced Sankey diagrams
#' - **Publication-Ready Themes**: Based on theme_bw() with refined typography
#' - **Colorblind-Friendly Palettes**: Wong (2011) palette optimized for accessibility
#' - **Consistent API**: All visualization functions follow \code{viz_*()} convention
#' - **High-Quality Output**: 300+ DPI export for journals
#' - **Comprehensive Examples**: 150+ working examples across 7 example files
#'
#' ## Chart Categories
#'
#' ### Basic Charts (16 types)
#' - Scatter, Line, Bar, Histogram, Boxplot, Violin
#' - Density, Pie, Donut, Area, Ribbon, Step
#' - Error Bar, Jitter, Q-Q, ECDF
#'
#' ### Intermediate Charts (15 types)
#' - Heatmap, Correlation, Contour, 2D Density, Ridgeline
#' - Waterfall, Funnel, Radar, Bubble, Lollipop
#' - Dumbbell, Cleveland Dot, Violin-Box, Raincloud, Polar Bar
#'
#' ### Advanced Charts (17 types)
#' - Sankey, Treemap, Sunburst, Network, Parallel Coordinates
#' - Chord, Calendar Heatmap, Hexbin, Stream, Alluvial
#' - Circular Bar, Bullet, Slope, Wind Rose, Gantt
#' - Mosaic, Marimekko
#'
#' ## Quick Start
#'
#' ```r
#' library(ChartKit)
#' library(dplyr)
#'
#' # Basic scatter plot
#' mtcars_clean <- mtcars %>%
#'   mutate(cyl_factor = factor(cyl))
#'
#' viz_scatter(mtcars_clean,
#'             x = wt,
#'             y = mpg,
#'             color = cyl_factor,
#'             title = "Fuel Efficiency vs Weight")
#'
#' # View all available charts
#' print_chart_catalog()
#' ```
#'
#' ## Themes
#'
#' ChartKit provides three publication-ready themes:
#'
#' - \code{\link{theme_publication}}: Clean theme with subtle grid lines (recommended)
#' - \code{\link{theme_minimal_pub}}: Minimalist theme without grid lines
#' - \code{\link{theme_classic_pub}}: Classic theme with strong borders
#'
#' All themes are based on \code{theme_bw()} with optimized typography
#' (11pt base font suitable for most journals).
#'
#' ## Color Palettes
#'
#' All palettes are research-based and colorblind-friendly:
#'
#' - \code{\link{scale_color_publication}}: Discrete color scale (Wong 2011)
#' - \code{\link{scale_fill_publication}}: Discrete fill scale (Wong 2011)
#'
#' Available palettes: "default" (Wong 2011), "vibrant", "muted", "dark"
#'
#' ## Saving Plots
#'
#' Use \code{\link{save_publication}} for high-quality output:
#'
#' ```r
#' p <- viz_scatter(mtcars, x = wt, y = mpg)
#'
#' # PNG at 300 DPI (journal standard)
#' save_publication("figure1.png", p, width = 7, height = 5, dpi = 300)
#'
#' # High-resolution TIFF for print
#' save_publication("figure1.tiff", p, width = 7, height = 5, dpi = 600)
#'
#' # Vector PDF for ultimate quality
#' save_publication("figure1.pdf", p, width = 7, height = 5)
#' ```
#'
#' ## Example Files
#'
#' ChartKit includes 7 comprehensive example files with 150+ working examples:
#'
#' 1. \code{basic_examples.R} - Basic chart types
#' 2. \code{distribution_examples.R} - Statistical distributions (30+ examples)
#' 3. \code{scatter_advanced_examples.R} - Interactive and analytical scatter plots
#' 4. \code{categorical_examples.R} - Bar plots and categorical data (40+ examples)
#' 5. \code{circular_examples.R} - Radar, circular bar, wind rose diagrams
#' 6. \code{specialized_examples.R} - Lollipop, dumbbell, treemap, wordcloud
#' 7. \code{intermediate_examples.R} - Heatmaps, bubble charts, waterfalls
#' 8. \code{advanced_examples.R} - Sankey, network, chord diagrams
#'
#' Load examples:
#' ```r
#' source(system.file("examples/distribution_examples.R", package = "ChartKit"))
#' ```
#'
#' @references
#' Wong, B. (2011). Points of view: Color blindness. Nature Methods 8(6), 441.
#' https://doi.org/10.1038/nmeth.1618
#'
#' @seealso
#' Useful links:
#' - GitHub: https://github.com/DataConceptz/ChartKit
#' - Report bugs: https://github.com/DataConceptz/ChartKit/issues
#'
#' @author DataConceptz R-Vis Team
#'
#' @docType package
#' @name ChartKit-package
#' @aliases ChartKit
NULL
