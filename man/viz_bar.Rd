\name{viz_bar}
\alias{viz_bar}
\title{Bar Chart}
\description{
  Create publication-ready bar charts for comparing values across categories.
  Supports both vertical and horizontal orientations, grouped and stacked
  layouts, and customizable styling.
}
\usage{
viz_bar(data, x, y, fill = NULL,
        position = "dodge",
        title = NULL, subtitle = NULL,
        xlab = NULL, ylab = NULL,
        horizontal = FALSE)
}
\arguments{
  \item{data}{Data frame containing the variables to plot. For frequency counts,
              data should be pre-aggregated.}
  \item{x}{Column name for x-axis categories (unquoted)}
  \item{y}{Column name for y-axis values (unquoted)}
  \item{fill}{Optional column name for fill color grouping (unquoted). Creates
              grouped or stacked bars depending on position argument.}
  \item{position}{Bar positioning. Options: "dodge" (side-by-side), "stack"
                  (stacked), "fill" (100\% stacked). Default is "dodge".}
  \item{title}{Plot title (character string)}
  \item{subtitle}{Plot subtitle (character string)}
  \item{xlab}{X-axis label (character string, defaults to column name)}
  \item{ylab}{Y-axis label (character string, defaults to column name)}
  \item{horizontal}{Logical. If TRUE, creates horizontal bars (useful for long
                    category names). Default is FALSE.}
}
\value{
  A ggplot2 object that can be further customized.
}
\details{
  \strong{Data Preparation:}

  Bar charts require aggregated data:
  \itemize{
    \item Calculate means, sums, or counts before plotting
    \item Use dplyr::group_by() and summarise() for aggregation
    \item For frequency counts, use count() or table()
  }

  \strong{Bar Chart Types:}

  \emph{Simple Bar Chart:}
  One bar per category, showing values

  \emph{Grouped Bar Chart (position = "dodge"):}
  Multiple bars per category, side-by-side comparison

  \emph{Stacked Bar Chart (position = "stack"):}
  Bars stacked on top of each other, showing totals

  \emph{100\% Stacked (position = "fill"):}
  Bars normalized to 100\%, showing proportions

  \strong{When to Use:}
  \itemize{
    \item Comparing values across discrete categories
    \item Showing frequency distributions
    \item Comparing groups within categories
    \item Displaying rankings or ordered data
  }
}
\examples{
library(dplyr)

# Basic bar chart (data must be aggregated first)
iris_summary <- iris \%>\%
  group_by(Species) \%>\%
  summarise(mean_length = mean(Sepal.Length))

viz_bar(iris_summary, x = Species, y = mean_length,
        title = "Average Sepal Length by Species",
        ylab = "Mean Length (cm)")

# Grouped bar chart
mtcars_summary <- mtcars \%>\%
  mutate(
    cyl_factor = factor(cyl),
    transmission = factor(am, labels = c("Automatic", "Manual"))
  ) \%>\%
  group_by(cyl_factor, transmission) \%>\%
  summarise(mean_mpg = mean(mpg), .groups = "drop")

viz_bar(mtcars_summary, x = cyl_factor, y = mean_mpg,
        fill = transmission,
        position = "dodge",
        title = "Fuel Efficiency: Cylinders and Transmission",
        xlab = "Number of Cylinders",
        ylab = "Average MPG")

# Stacked bar chart
viz_bar(mtcars_summary, x = cyl_factor, y = mean_mpg,
        fill = transmission,
        position = "stack",
        title = "Stacked Bar Chart")

# 100% stacked (proportions)
viz_bar(mtcars_summary, x = cyl_factor, y = mean_mpg,
        fill = transmission,
        position = "fill",
        title = "Proportional Comparison",
        ylab = "Proportion")

# Horizontal bars (better for long labels)
category_data <- data.frame(
  category = c("Very Long Category Name A",
               "Another Extended Label B",
               "Short C"),
  value = c(45, 62, 38)
)

viz_bar(category_data, x = category, y = value,
        horizontal = TRUE,
        title = "Horizontal Bar Chart")

# Add error bars manually
iris_full <- iris \%>\%
  group_by(Species) \%>\%
  summarise(
    mean_length = mean(Sepal.Length),
    se = sd(Sepal.Length) / sqrt(n())
  )

library(ggplot2)
viz_bar(iris_full, x = Species, y = mean_length, fill = Species) +
  geom_errorbar(aes(ymin = mean_length - se, ymax = mean_length + se),
                width = 0.2) +
  theme(legend.position = "none")

# Frequency bar chart
mtcars_counts <- mtcars \%>\%
  count(cyl) \%>\%
  mutate(cyl = factor(cyl))

viz_bar(mtcars_counts, x = cyl, y = n,
        title = "Car Count by Cylinder",
        ylab = "Frequency")
}
\note{
  \strong{Common Mistakes:}
  \itemize{
    \item \emph{Plotting raw data}: Bar charts need aggregated data
    \item \emph{Too many categories}: Consider horizontal layout or alternative chart
    \item \emph{Starting y-axis above zero}: Misleading! Always start at zero
    \item \emph{3D bars}: Never use 3D effects (misleading and unprofessional)
  }

  \strong{Best Practices:}
  \itemize{
    \item Always start y-axis at zero (never truncate)
    \item Order categories meaningfully (by value, alphabetically, or logically)
    \item Use horizontal bars for >5 categories or long names
    \item Add data labels for exact values when needed
    \item Keep bars uniform width within same chart
    \item Use color only when meaningful (not just decoration)
  }

  \strong{Error Bars:}
  Add error bars after creating the plot:
  \preformatted{
+ geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.2)
  }
}
\seealso{
  Related functions:
  \itemize{
    \item \code{\link{viz_lollipop}} - Alternative to bars, less visual weight
    \item \code{\link{viz_cleveland_dot}} - Better for precise comparisons
    \item \code{\link{viz_histogram}} - For continuous data distributions
  }

  Examples:
  \itemize{
    \item See \code{inst/examples/categorical_examples.R} for 40+ bar chart variations
  }
}
\references{
  Few, S. (2012). \emph{Show Me the Numbers: Designing Tables and Graphs
  to Enlighten}. Analytics Press.
}
