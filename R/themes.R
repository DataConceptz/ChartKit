#' Publication-Ready Theme
#'
#' A clean, professional theme suitable for academic publications and reports.
#'
#' @param base_size Base font size (default: 12)
#' @param base_family Base font family (default: "sans")
#' @param grid Whether to show grid lines (default: TRUE)
#' @return A ggplot2 theme object
#' @export
#' @examples
#' library(ggplot2)
#' ggplot(mtcars, aes(x = wt, y = mpg)) +
#'   geom_point() +
#'   theme_publication()
theme_publication <- function(base_size = 12, base_family = "sans", grid = TRUE) {
  ggplot2::theme_minimal(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      # Text elements
      plot.title = ggplot2::element_text(face = "bold", size = rel(1.2), hjust = 0,
                                margin = ggplot2::margin(0, 0, 10, 0)),
      plot.subtitle = ggplot2::element_text(size = rel(1), hjust = 0,
                                   margin = ggplot2::margin(0, 0, 10, 0)),
      plot.caption = ggplot2::element_text(size = rel(0.8), hjust = 1, face = "italic",
                                  margin = ggplot2::margin(10, 0, 0, 0)),

      # Axis elements
      axis.title = ggplot2::element_text(face = "bold", size = rel(1)),
      axis.text = ggplot2::element_text(size = rel(0.9)),
      axis.line = ggplot2::element_line(colour = "black", size = 0.5),
      axis.ticks = ggplot2::element_line(colour = "black", size = 0.5),

      # Legend elements
      legend.title = ggplot2::element_text(face = "bold", size = rel(1)),
      legend.text = ggplot2::element_text(size = rel(0.9)),
      legend.position = "right",
      legend.key = ggplot2::element_blank(),
      legend.background = ggplot2::element_blank(),

      # Panel elements
      panel.grid.major = if(grid) ggplot2::element_line(colour = "grey90", size = 0.25) else ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.border = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank(),

      # Strip elements (for facets)
      strip.background = ggplot2::element_rect(fill = "grey95", colour = NA),
      strip.text = ggplot2::element_text(face = "bold", size = rel(1)),

      # Plot background
      plot.background = ggplot2::element_rect(fill = "white", colour = NA),
      plot.margin = ggplot2::margin(10, 10, 10, 10)
    )
}

#' Minimal Publication Theme
#'
#' A minimalist theme with no grid lines for cleaner plots.
#'
#' @param base_size Base font size (default: 12)
#' @param base_family Base font family (default: "sans")
#' @return A ggplot2 theme object
#' @export
theme_minimal_pub <- function(base_size = 12, base_family = "sans") {
  theme_publication(base_size = base_size, base_family = base_family, grid = FALSE)
}

#' Classic Publication Theme
#'
#' A classic theme with borders and no grid lines.
#'
#' @param base_size Base font size (default: 12)
#' @param base_family Base font family (default: "sans")
#' @return A ggplot2 theme object
#' @export
theme_classic_pub <- function(base_size = 12, base_family = "sans") {
  theme_publication(base_size = base_size, base_family = base_family, grid = FALSE) +
    ggplot2::theme(
      panel.border = ggplot2::element_rect(colour = "black", fill = NA, size = 0.5)
    )
}

#' Publication Color Scale (Discrete)
#'
#' A colorblind-friendly discrete color scale for publications.
#'
#' @param palette Palette name: "default", "vibrant", "muted", "dark" (default: "default")
#' @param ... Additional arguments passed to scale_color_manual
#' @return A ggplot2 color scale
#' @export
scale_color_publication <- function(palette = "default", ...) {
  palettes <- list(
    default = c("#0072B2", "#D55E00", "#009E73", "#CC79A7", "#F0E442",
                "#56B4E9", "#E69F00", "#999999"),
    vibrant = c("#EE7733", "#0077BB", "#33BBEE", "#EE3377", "#CC3311",
                "#009988", "#BBBBBB"),
    muted = c("#332288", "#88CCEE", "#44AA99", "#117733", "#999933",
              "#DDCC77", "#CC6677", "#882255", "#AA4499"),
    dark = c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E",
             "#E6AB02", "#A6761D", "#666666")
  )

  pal <- palettes[[palette]]
  if (is.null(pal)) pal <- palettes$default

  ggplot2::scale_color_manual(values = pal, ...)
}

#' Publication Fill Scale (Discrete)
#'
#' A colorblind-friendly discrete fill scale for publications.
#'
#' @param palette Palette name: "default", "vibrant", "muted", "dark" (default: "default")
#' @param ... Additional arguments passed to scale_fill_manual
#' @return A ggplot2 fill scale
#' @export
scale_fill_publication <- function(palette = "default", ...) {
  palettes <- list(
    default = c("#0072B2", "#D55E00", "#009E73", "#CC79A7", "#F0E442",
                "#56B4E9", "#E69F00", "#999999"),
    vibrant = c("#EE7733", "#0077BB", "#33BBEE", "#EE3377", "#CC3311",
                "#009988", "#BBBBBB"),
    muted = c("#332288", "#88CCEE", "#44AA99", "#117733", "#999933",
              "#DDCC77", "#CC6677", "#882255", "#AA4499"),
    dark = c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E",
             "#E6AB02", "#A6761D", "#666666")
  )

  pal <- palettes[[palette]]
  if (is.null(pal)) pal <- palettes$default

  ggplot2::scale_fill_manual(values = pal, ...)
}

#' Save Plot for Publication
#'
#' Save a plot with publication-ready settings.
#'
#' @param filename File name to save the plot
#' @param plot Plot object (if NULL, uses last plot)
#' @param width Width in inches (default: 8)
#' @param height Height in inches (default: 6)
#' @param dpi Resolution in dots per inch (default: 300)
#' @param device Device to use: "png", "pdf", "tiff", "svg" (default: "png")
#' @param ... Additional arguments passed to ggsave
#' @export
#' @examples
#' \dontrun{
#' p <- ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point()
#' save_publication("my_plot.png", p, width = 10, height = 7)
#' }
save_publication <- function(filename, plot = NULL, width = 8, height = 6,
                            dpi = 300, device = "png", ...) {
  if (is.null(plot)) {
    plot <- ggplot2::last_plot()
  }

  ggplot2::ggsave(
    filename = filename,
    plot = plot,
    width = width,
    height = height,
    dpi = dpi,
    device = device,
    ...
  )

  message("Plot saved to: ", filename)
}
