\name{theme_publication}
\alias{theme_publication}
\alias{theme_minimal_pub}
\alias{theme_classic_pub}
\title{Publication-Ready Themes}
\description{
  Clean, professional ggplot2 themes optimized for academic publications and
  scientific reports. All themes are based on \code{theme_bw()} with refined
  typography and spacing.
}
\usage{
theme_publication(base_size = 11, base_family = "sans", grid = TRUE)
theme_minimal_pub(base_size = 11, base_family = "sans")
theme_classic_pub(base_size = 11, base_family = "sans")
}
\arguments{
  \item{base_size}{Base font size in points. Default is 11, suitable for most
        journals. Increase to 12-14 for presentations.}
  \item{base_family}{Base font family. Default is "sans" for clean appearance.
        Use "serif" for traditional academic style.}
  \item{grid}{Logical. Whether to show grid lines. Default is TRUE for
        \code{theme_publication}.}
}
\value{
  A ggplot2 theme object that can be added to plots.
}
\details{
  \strong{theme_publication (recommended):}
  \itemize{
    \item Clean, professional appearance with subtle grid lines
    \item Bold axis titles and plot titles
    \item Optimized spacing and margins
    \item Gray panel borders and axis lines
    \item Based on standards from Nature, Science, and PLOS journals
  }

  \strong{theme_minimal_pub:}
  \itemize{
    \item Minimalist style without grid lines
    \item Calls \code{theme_publication(..., grid = FALSE)}
    \item Best for simple plots with few data points
  }

  \strong{theme_classic_pub:}
  \itemize{
    \item Strong black borders, no grid lines
    \item Traditional academic journal style
    \item Best for bar charts and categorical data
  }

  All themes use:
  \itemize{
    \item 11pt base font (standard for most journals)
    \item Bold axis titles for emphasis
    \item Proper hierarchy for titles, subtitles, and captions
    \item High-contrast colors for readability
    \item Adequate spacing for printed materials
  }
}
\examples{
library(ggplot2)

# Default publication theme
ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point() +
  theme_publication() +
  labs(title = "Fuel Efficiency vs Weight")

# Minimal theme (no grid)
ggplot(mtcars, aes(x = factor(cyl), y = mpg)) +
  geom_boxplot() +
  theme_minimal_pub() +
  labs(title = "MPG by Cylinder Count")

# Classic theme (strong borders)
ggplot(mtcars, aes(x = factor(cyl))) +
  geom_bar() +
  theme_classic_pub() +
  labs(title = "Car Count by Cylinder")

# Customize base size for presentations
ggplot(mtcars, aes(x = wt, y = mpg, color = factor(cyl))) +
  geom_point(size = 3) +
  theme_publication(base_size = 14) +
  labs(title = "Larger Text for Presentations")

# Use serif font for traditional style
ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point() +
  theme_publication(base_family = "serif") +
  labs(title = "Traditional Serif Font")
}
\references{
  Theme design inspired by best practices from:
  \itemize{
    \item Nature journals style guidelines
    \item Science magazine figure standards
    \item PLOS ONE formatting requirements
  }
}
\seealso{
  \code{\link{scale_color_publication}} for colorblind-friendly palettes,
  \code{\link{save_publication}} for saving high-quality figures
}
