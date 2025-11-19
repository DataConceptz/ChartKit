\name{viz_scatter}
\alias{viz_scatter}
\title{Scatter Plot}
\description{
  Create a publication-ready scatter plot showing the relationship between
  two continuous variables. Supports color grouping and variable point sizes
  for multivariate visualization.
}
\usage{
viz_scatter(data, x, y, color = NULL, size = NULL,
            title = NULL, subtitle = NULL,
            xlab = NULL, ylab = NULL,
            alpha = 0.7, point_size = 3)
}
\arguments{
  \item{data}{Data frame containing the variables to plot}
  \item{x}{Column name for x-axis (unquoted, using NSE)}
  \item{y}{Column name for y-axis (unquoted, using NSE)}
  \item{color}{Optional column name for color grouping (unquoted). For best
               results, convert to factor before plotting.}
  \item{size}{Optional column name for point size (unquoted). Creates a
              bubble chart effect.}
  \item{title}{Plot title (character string)}
  \item{subtitle}{Plot subtitle (character string)}
  \item{xlab}{X-axis label (character string, defaults to column name)}
  \item{ylab}{Y-axis label (character string, defaults to column name)}
  \item{alpha}{Point transparency (numeric, 0-1). Default is 0.7 for
               good visibility with overlapping points.}
  \item{point_size}{Base point size when size aesthetic is not used (numeric).
                    Default is 3.}
}
\value{
  A ggplot2 object that can be further customized with additional ggplot2
  layers, themes, or scales.
}
\details{
  \strong{Data Preparation:}

  For best results with categorical color grouping:
  \itemize{
    \item Convert categorical variables to factors before plotting
    \item Use meaningful factor labels for better legend readability
    \item Order factor levels intentionally for legend organization
  }

  \strong{When to Use Scatter Plots:}
  \itemize{
    \item Examining relationships between two continuous variables
    \item Identifying correlations (positive, negative, or none)
    \item Detecting outliers and unusual patterns
    \item Comparing groups when using color aesthetic
    \item Showing three variables when using size aesthetic (bubble chart)
  }

  \strong{Customization:}

  The returned ggplot2 object can be customized:
  \itemize{
    \item Add trend lines: \code{+ geom_smooth()}
    \item Add text labels: \code{+ geom_text_repel()}
    \item Facet by category: \code{+ facet_wrap(~category)}
    \item Change theme: \code{+ theme_minimal_pub()}
    \item Adjust scales: \code{+ scale_x_log10()}
  }
}
\examples{
library(dplyr)

# Basic scatter plot
viz_scatter(mtcars, x = wt, y = mpg,
            title = "Fuel Efficiency vs Weight",
            xlab = "Weight (1000 lbs)",
            ylab = "Miles per Gallon")

# With color grouping (IMPORTANT: convert to factor first)
mtcars_clean <- mtcars \%>\%
  mutate(cyl_factor = factor(cyl, labels = paste(unique(sort(cyl)), "cylinders")))

viz_scatter(mtcars_clean, x = wt, y = mpg, color = cyl_factor,
            title = "MPG by Weight and Engine Size",
            subtitle = "Heavier cars with more cylinders show lower MPG")

# Bubble chart with three variables
viz_scatter(mtcars, x = wt, y = mpg, size = hp,
            title = "Three-Variable Analysis",
            subtitle = "Size represents horsepower",
            alpha = 0.6)

# With color and size
mtcars_full <- mtcars \%>\%
  mutate(transmission = factor(am, labels = c("Automatic", "Manual")))

viz_scatter(mtcars_full, x = wt, y = mpg,
            color = transmission, size = hp,
            title = "Complete Multivariate Analysis")

# Customize further with ggplot2
library(ggplot2)
viz_scatter(mtcars, x = wt, y = mpg) +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  labs(caption = "Data source: 1974 Motor Trend magazine")

# Using with iris data
viz_scatter(iris, x = Sepal.Length, y = Sepal.Width,
            color = Species,
            title = "Iris Sepal Dimensions by Species")

# High transparency for dense data
set.seed(123)
dense_data <- data.frame(
  x = rnorm(1000),
  y = rnorm(1000),
  group = sample(c("A", "B", "C"), 1000, replace = TRUE)
)

viz_scatter(dense_data, x = x, y = y, color = group,
            alpha = 0.3,
            point_size = 2,
            title = "Dense Data with High Transparency")
}
\note{
  \strong{Common Issues and Solutions:}
  \itemize{
    \item \emph{Colors not showing}: Make sure color variable is a factor
    \item \emph{Too many overlapping points}: Reduce point_size or increase alpha
    \item \emph{Legend shows numbers}: Convert categorical variables to factors
    \item \emph{Size legend overwhelming}: Manually set size range with scale_size_continuous()
  }

  \strong{Best Practices:}
  \itemize{
    \item Always include informative axis labels with units
    \item Use color for categories (discrete), size for magnitude (continuous)
    \item Keep scatter plots simple - maximum 3-4 variables
    \item For large datasets (>10000 points), consider hexbin or density plots
    \item Test plots in grayscale to ensure colorblind accessibility
  }
}
\seealso{
  Related visualization functions:
  \itemize{
    \item \code{\link{viz_bubble}} - Specialized three-variable scatter plots
    \item \code{\link{viz_hexbin}} - For very large datasets
    \item \code{\link{viz_jitter}} - For categorical x-axis with many points
  }

  Customization:
  \itemize{
    \item \code{\link{theme_publication}} - Default theme
    \item \code{\link{scale_color_publication}} - Colorblind-friendly colors
    \item \code{\link{save_publication}} - Save high-quality output
  }

  ggplot2 documentation:
  \itemize{
    \item \code{\link[ggplot2]{geom_point}} - Underlying geometry
    \item \code{\link[ggplot2]{geom_smooth}} - Add trend lines
  }
}
\references{
  Cleveland, W. S. (1993). \emph{Visualizing Data}. Hobart Press.

  Tufte, E. R. (2001). \emph{The Visual Display of Quantitative Information}.
  Graphics Press.
}
