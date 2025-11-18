\name{save_publication}
\alias{save_publication}
\title{Save Plot for Publication}
\description{
  Save a ggplot2 plot with publication-ready settings. Automatically handles
  high-resolution output, proper dimensions, and file format detection.
}
\usage{
save_publication(filename, plot = NULL, width = 7, height = 5,
                 dpi = 300, device = NULL, ...)
}
\arguments{
  \item{filename}{File name to save the plot. Extension determines format
        (.png, .pdf, .tiff, .svg, .eps, .jpg)}
  \item{plot}{Plot object to save. If NULL, uses the last plot displayed.}
  \item{width}{Plot width in inches. Default is 7, suitable for single-column
        figures in most journals.}
  \item{height}{Plot height in inches. Default is 5, maintaining good aspect ratio.}
  \item{dpi}{Resolution in dots per inch. Default is 300, the minimum for most
        journals. Use 600 for high-quality print.}
  \item{device}{Graphics device to use. If NULL (default), auto-detected from
        filename extension. Options: "png", "pdf", "tiff", "svg", "eps", "jpeg".}
  \item{...}{Additional arguments passed to \code{ggsave}.}
}
\value{
  Invisibly returns the filename. Prints a message with save details.
}
\details{
  \strong{Recommended Settings by Output Type:}

  \emph{Journal submission (raster):}
  \itemize{
    \item Format: PNG or TIFF
    \item DPI: 300 (minimum) to 600 (high quality)
    \item Size: 7" x 5" for single column, 10-14" wide for full page
  }

  \emph{Journal submission (vector):}
  \itemize{
    \item Format: PDF or EPS
    \item DPI: Not applicable (vector)
    \item Size: Same as raster
    \item Advantage: Infinite scaling, smaller file size
  }

  \emph{Presentations:}
  \itemize{
    \item Format: PNG (compatibility) or PDF (quality)
    \item DPI: 150-200 (sufficient for screens)
    \item Size: 10" x 7" or larger for visibility
  }

  \emph{Web/Online:}
  \itemize{
    \item Format: PNG or SVG
    \item DPI: 72-150 (faster loading)
    \item Size: Optimize for display size
  }

  \strong{File Format Selection:}
  \itemize{
    \item \strong{PNG}: Best for general use, wide compatibility, moderate file size
    \item \strong{TIFF}: Required by some journals, larger files, high quality
    \item \strong{PDF}: Vector format, infinite scaling, small size, best for lines/text
    \item \strong{SVG}: Vector format for web, editable in Illustrator
    \item \strong{EPS}: Vector format for older journal systems
  }

  \strong{Common Journal Requirements:}
  \itemize{
    \item Nature: TIFF, 300-600 DPI, max 180mm width
    \item Science: PDF or TIFF, 300 DPI minimum
    \item PLOS: TIFF or EPS, 300-600 DPI
    \item Cell: PDF preferred, 300 DPI for raster
  }
}
\examples{
library(ggplot2)

# Create a plot
p <- ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point() +
  theme_publication() +
  labs(title = "Fuel Efficiency vs Weight")

\dontrun{
# Save for journal submission (PNG)
save_publication("figure1.png", p, width = 7, height = 5, dpi = 300)

# High-resolution TIFF for print
save_publication("figure1.tiff", p, width = 7, height = 5, dpi = 600)

# Vector PDF for perfect scaling
save_publication("figure1.pdf", p, width = 7, height = 5)

# Full-page figure for two-column layout
save_publication("figure2.png", p, width = 14, height = 8, dpi = 300)

# Presentation slide
save_publication("slide.png", p, width = 10, height = 7, dpi = 150)

# Save last plot displayed (NULL plot argument)
ggplot(iris, aes(x = Species, y = Sepal.Length)) + geom_boxplot()
save_publication("iris_boxplot.png", width = 6, height = 4)

# Custom device settings
save_publication("figure.png", p,
                width = 8, height = 6,
                dpi = 400,
                bg = "white",  # white background
                scale = 1.2)   # scale all elements
}
}
\note{
  \strong{Best Practices:}
  \itemize{
    \item Always check your target journal's figure requirements
    \item Use 300 DPI as minimum for publication
    \item Vector formats (PDF, SVG, EPS) are preferred when possible
    \item Test print your figures at intended size
    \item Use consistent dimensions across figures in same manuscript
    \item Include adequate whitespace around plot borders
  }

  \strong{Troubleshooting:}
  \itemize{
    \item If text is too small: Increase base_size in theme
    \item If file is too large: Use PDF instead of TIFF
    \item If colors don't print well: Use colorblind-friendly palettes
    \item If figure is rejected: Check journal's specific requirements
  }
}
\seealso{
  \code{\link{theme_publication}} for publication-ready themes,
  \code{\link{scale_color_publication}} for colorblind-friendly colors,
  \code{\link[ggplot2]{ggsave}} for additional save options
}
