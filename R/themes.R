#' Publication-Ready Theme
#'
#' A clean, professional theme suitable for academic publications and reports.
#' Based on theme_bw() with refined typography and spacing optimized for
#' scientific publications.
#'
#' @param base_size Base font size (default: 11 for most journals)
#' @param base_family Base font family (default: "sans")
#' @param grid Whether to show grid lines (default: TRUE)
#' @return A ggplot2 theme object
#' @export
#' @examples
#' library(ggplot2)
#' ggplot(mtcars, aes(x = wt, y = mpg)) +
#'   geom_point() +
#'   theme_publication()
theme_publication <- function(base_size = 11, base_family = "sans", grid = TRUE) {
  # Start with theme_bw as it's the cleanest base
  ggplot2::theme_bw(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      # Text hierarchy - important for readability
      plot.title = ggplot2::element_text(
        face = "bold",
        size = ggplot2::rel(1.2),
        hjust = 0,
        margin = ggplot2::margin(0, 0, 8, 0)
      ),
      plot.subtitle = ggplot2::element_text(
        size = ggplot2::rel(0.95),
        hjust = 0,
        color = "grey30",
        margin = ggplot2::margin(0, 0, 8, 0)
      ),
      plot.caption = ggplot2::element_text(
        size = ggplot2::rel(0.8),
        hjust = 1,
        color = "grey50",
        margin = ggplot2::margin(8, 0, 0, 0)
      ),

      # Axis styling - clean and professional
      axis.title.x = ggplot2::element_text(
        size = ggplot2::rel(1),
        face = "bold",
        margin = ggplot2::margin(8, 0, 0, 0)
      ),
      axis.title.y = ggplot2::element_text(
        size = ggplot2::rel(1),
        face = "bold",
        angle = 90,
        margin = ggplot2::margin(0, 8, 0, 0)
      ),
      axis.text = ggplot2::element_text(
        size = ggplot2::rel(0.9),
        color = "grey20"
      ),
      axis.ticks = ggplot2::element_line(color = "grey60", linewidth = 0.25),
      axis.line = ggplot2::element_line(color = "black", linewidth = 0.5),

      # Legend - clean and unobtrusive
      legend.title = ggplot2::element_text(
        face = "bold",
        size = ggplot2::rel(0.95)
      ),
      legend.text = ggplot2::element_text(size = ggplot2::rel(0.85)),
      legend.key = ggplot2::element_blank(),
      legend.background = ggplot2::element_blank(),
      legend.position = "right",
      legend.key.size = ggplot2::unit(1.2, "lines"),

      # Panel and grid - subtle and clean
      panel.background = ggplot2::element_rect(fill = "white", color = NA),
      panel.border = ggplot2::element_rect(color = "grey60", fill = NA, linewidth = 0.5),
      panel.grid.major = if(grid) {
        ggplot2::element_line(color = "grey90", linewidth = 0.25)
      } else {
        ggplot2::element_blank()
      },
      panel.grid.minor = ggplot2::element_blank(),

      # Strip (for facets) - clear separation
      strip.background = ggplot2::element_rect(
        fill = "grey95",
        color = "grey60",
        linewidth = 0.5
      ),
      strip.text = ggplot2::element_text(
        face = "bold",
        size = ggplot2::rel(0.95),
        margin = ggplot2::margin(4, 4, 4, 4)
      ),

      # Plot background and margins
      plot.background = ggplot2::element_rect(fill = "white", color = NA),
      plot.margin = ggplot2::margin(10, 10, 10, 10)
    )
}

#' Minimal Publication Theme
#'
#' A minimalist theme with no grid lines for cleaner plots.
#'
#' @param base_size Base font size (default: 11)
#' @param base_family Base font family (default: "sans")
#' @return A ggplot2 theme object
#' @export
theme_minimal_pub <- function(base_size = 11, base_family = "sans") {
  theme_publication(base_size = base_size, base_family = base_family, grid = FALSE)
}

#' Classic Publication Theme
#'
#' A classic theme with strong borders and no grid lines.
#' Good for traditional scientific publications.
#'
#' @param base_size Base font size (default: 11)
#' @param base_family Base font family (default: "sans")
#' @return A ggplot2 theme object
#' @export
theme_classic_pub <- function(base_size = 11, base_family = "sans") {
  theme_publication(base_size = base_size, base_family = base_family, grid = FALSE) +
    ggplot2::theme(
      panel.border = ggplot2::element_rect(color = "black", fill = NA, linewidth = 0.8),
      axis.line = ggplot2::element_blank()
    )
}

#' Publication Color Scale (Discrete)
#'
#' Colorblind-friendly discrete color scales based on scientific palette research.
#' Default palette is from Wong (2011) Nature Methods, optimized for colorblind readers.
#'
#' @param palette Palette name: "default", "vibrant", "muted", "dark" (default: "default")
#' @param ... Additional arguments passed to scale_color_manual
#' @return A ggplot2 color scale
#' @export
#' @references
#' Wong, B. (2011). Points of view: Color blindness. Nature Methods 8(6), 441.
scale_color_publication <- function(palette = "default", ...) {
  palettes <- list(
    # Wong 2011 - optimal for colorblindness
    default = c("#0072B2", "#D55E00", "#009E73", "#CC79A7", "#F0E442",
                "#56B4E9", "#E69F00", "#999999"),
    # High contrast vibrant
    vibrant = c("#EE7733", "#0077BB", "#33BBEE", "#EE3377", "#CC3311",
                "#009988", "#BBBBBB"),
    # Paul Tol muted scheme
    muted = c("#332288", "#88CCEE", "#44AA99", "#117733", "#999933",
              "#DDCC77", "#CC6677", "#882255", "#AA4499"),
    # Dark colors for light backgrounds
    dark = c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E",
             "#E6AB02", "#A6761D", "#666666")
  )

  pal <- palettes[[palette]]
  if (is.null(pal)) pal <- palettes$default

  ggplot2::scale_color_manual(values = pal, ...)
}

#' Publication Fill Scale (Discrete)
#'
#' Colorblind-friendly discrete fill scales based on scientific palette research.
#'
#' @param palette Palette name: "default", "vibrant", "muted", "dark" (default: "default")
#' @param ... Additional arguments passed to scale_fill_manual
#' @return A ggplot2 fill scale
#' @export
#' @references
#' Wong, B. (2011). Points of view: Color blindness. Nature Methods 8(6), 441.
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
#' Defaults to 300 DPI which is standard for most journals.
#'
#' @param filename File name to save the plot
#' @param plot Plot object (if NULL, uses last plot)
#' @param width Width in inches (default: 7)
#' @param height Height in inches (default: 5)
#' @param dpi Resolution in dots per inch (default: 300)
#' @param device Device to use: "png", "pdf", "tiff", "svg", "eps" (default: "png")
#' @param ... Additional arguments passed to ggsave
#' @export
#' @examples
#' \dontrun{
#' p <- ggplot(mtcars, aes(x = wt, y = mpg)) +
#'   geom_point() +
#'   theme_publication()
#' save_publication("figure1.png", p, width = 7, height = 5)
#'
#' # For higher quality or specific journal requirements
#' save_publication("figure1.tiff", p, width = 7, height = 5, dpi = 600)
#' }
save_publication <- function(filename, plot = NULL, width = 7, height = 5,
                            dpi = 300, device = NULL, ...) {
  if (is.null(plot)) {
    plot <- ggplot2::last_plot()
  }

  # Auto-detect device from filename if not specified
  if (is.null(device)) {
    ext <- tools::file_ext(filename)
    device <- switch(tolower(ext),
                     png = "png",
                     pdf = "pdf",
                     tiff = "tiff",
                     tif = "tiff",
                     svg = "svg",
                     eps = "eps",
                     jpg = "jpeg",
                     jpeg = "jpeg",
                     "png")  # default
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

  message(sprintf("Plot saved to: %s (%d DPI, %.1f x %.1f inches)",
                  filename, dpi, width, height))
  invisible(filename)
}
