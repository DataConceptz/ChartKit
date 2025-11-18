\name{ChartKit-package}
\alias{ChartKit-package}
\alias{ChartKit}
\docType{package}
\title{
  Publication-Ready Data Visualization for R
}
\description{
  ChartKit is a comprehensive R package providing 48 chart types optimized for
  scientific publications. Built on ggplot2 with colorblind-friendly palettes
  and publication-ready themes based on best practices from leading scientific journals.
}
\details{
  \tabular{ll}{
    Package: \tab ChartKit\cr
    Type: \tab Package\cr
    Version: \tab 0.1.0\cr
    License: \tab MIT\cr
  }

  \strong{Main Features:}
  \itemize{
    \item 48 chart types from basic to advanced
    \item Publication-ready themes based on theme_bw()
    \item Colorblind-friendly palettes (Wong 2011)
    \item Consistent viz_*() function API
    \item High-quality 300+ DPI output
    \item 150+ working examples across 7 example files
  }

  \strong{Chart Categories:}

  \emph{Basic Charts (16 types):}
  scatter, line, bar, histogram, boxplot, violin, density, pie, donut, area,
  ribbon, step, errorbar, jitter, qq, ecdf

  \emph{Intermediate Charts (15 types):}
  heatmap, correlation, contour, density2d, ridgeline, waterfall, funnel, radar,
  bubble, lollipop, dumbbell, cleveland_dot, violin_box, raincloud, polar_bar

  \emph{Advanced Charts (17 types):}
  sankey, treemap, sunburst, network, parallel, chord, calendar_heatmap, hexbin,
  stream, alluvial, circular_bar, bullet, slope, wind_rose, gantt, mosaic, marimekko
}
\section{Quick Start}{
  \preformatted{
library(ChartKit)
library(dplyr)

# View all available charts
print_chart_catalog()

# Basic scatter plot
mtcars_clean <- mtcars \%>\%
  mutate(cyl_factor = factor(cyl))

viz_scatter(mtcars_clean,
            x = wt,
            y = mpg,
            color = cyl_factor,
            title = "Fuel Efficiency vs Weight")

# Save for publication
save_publication("figure1.png", width = 7, height = 5, dpi = 300)
  }
}
\section{Themes}{
  Three publication-ready themes are available:
  \itemize{
    \item \code{\link{theme_publication}}: Clean with subtle grid (recommended)
    \item \code{\link{theme_minimal_pub}}: Minimalist without grid
    \item \code{\link{theme_classic_pub}}: Classic with strong borders
  }

  All themes use 11pt base font suitable for most journals.
}
\section{Color Palettes}{
  Research-based colorblind-friendly palettes:
  \itemize{
    \item \code{\link{scale_color_publication}}: Discrete color scale
    \item \code{\link{scale_fill_publication}}: Discrete fill scale
  }

  Available palettes: "default" (Wong 2011), "vibrant", "muted", "dark"
}
\section{Example Files}{
  ChartKit includes 7 comprehensive example files:
  \enumerate{
    \item \code{basic_examples.R} - Basic chart types
    \item \code{distribution_examples.R} - Statistical distributions (30+ examples)
    \item \code{scatter_advanced_examples.R} - Interactive scatter plots
    \item \code{categorical_examples.R} - Bar plots (40+ examples)
    \item \code{circular_examples.R} - Radar, circular bar, wind rose
    \item \code{specialized_examples.R} - Lollipop, treemap, wordcloud
    \item \code{advanced_examples.R} - Sankey, network, chord diagrams
  }

  Load examples with:
  \preformatted{
source(system.file("examples/distribution_examples.R", package = "ChartKit"))
  }
}
\section{Key Functions}{
  \strong{Basic Charts:}
  \itemize{
    \item \code{\link{viz_scatter}} - Scatter plots
    \item \code{\link{viz_line}} - Line plots
    \item \code{\link{viz_bar}} - Bar charts
    \item \code{\link{viz_histogram}} - Histograms
    \item \code{\link{viz_boxplot}} - Box plots
    \item \code{\link{viz_violin}} - Violin plots
  }

  \strong{Intermediate Charts:}
  \itemize{
    \item \code{\link{viz_heatmap}} - Heatmaps
    \item \code{\link{viz_correlation}} - Correlation matrices
    \item \code{\link{viz_ridgeline}} - Ridgeline plots
    \item \code{\link{viz_bubble}} - Bubble charts
    \item \code{\link{viz_lollipop}} - Lollipop charts
    \item \code{\link{viz_dumbbell}} - Dumbbell charts
  }

  \strong{Advanced Charts:}
  \itemize{
    \item \code{\link{viz_sankey}} - Sankey diagrams
    \item \code{\link{viz_network}} - Network graphs
    \item \code{\link{viz_treemap}} - Treemaps
    \item \code{\link{viz_chord}} - Chord diagrams
  }

  \strong{Utilities:}
  \itemize{
    \item \code{\link{get_chart_catalog}} - Get chart catalog
    \item \code{\link{print_chart_catalog}} - Print chart list
    \item \code{\link{save_publication}} - Save high-quality plots
  }
}
\author{
  DataConceptz R-Vis Team

  Maintainer: DataConceptz R-Vis <contact@dataconceptz.com>
}
\references{
  Wong, B. (2011). Points of view: Color blindness. \emph{Nature Methods} 8(6), 441.
  \url{https://doi.org/10.1038/nmeth.1618}
}
\keyword{package}
\seealso{
  \itemize{
    \item GitHub: \url{https://github.com/DataConceptz/ChartKit}
    \item Report bugs: \url{https://github.com/DataConceptz/ChartKit/issues}
  }
}
\examples{
library(ChartKit)

# View all 48 available charts
print_chart_catalog()

# Basic scatter plot
viz_scatter(mtcars, x = wt, y = mpg,
            title = "Fuel Efficiency vs Weight")

# With color grouping
library(dplyr)
mtcars_clean <- mtcars \%>\%
  mutate(cyl_factor = factor(cyl))
viz_scatter(mtcars_clean, x = wt, y = mpg, color = cyl_factor,
            title = "MPG by Weight and Cylinders")

# Bar chart with error bars
iris_summary <- iris \%>\%
  group_by(Species) \%>\%
  summarise(mean_length = mean(Sepal.Length),
            se = sd(Sepal.Length) / sqrt(n()))

library(ggplot2)
ggplot(iris_summary, aes(x = Species, y = mean_length, fill = Species)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean_length - se, ymax = mean_length + se),
                width = 0.2) +
  scale_fill_publication() +
  theme_publication() +
  labs(title = "Mean Sepal Length by Species")
}
