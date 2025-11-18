\name{print_chart_catalog}
\alias{print_chart_catalog}
\alias{get_chart_catalog}
\title{Chart Catalog Functions}
\description{
  View all available chart types in ChartKit. \code{get_chart_catalog()} returns
  a data frame with chart information, while \code{print_chart_catalog()} prints
  a formatted list to the console.
}
\usage{
get_chart_catalog()
print_chart_catalog()
}
\value{
  \code{get_chart_catalog} returns a data frame with columns:
  \itemize{
    \item \code{category}: Chart category (Basic, Intermediate, Advanced)
    \item \code{chart_type}: Short name of the chart type
    \item \code{function_name}: Function name to call (e.g., "viz_scatter")
    \item \code{description}: Description of the chart type and its use case
  }

  \code{print_chart_catalog} invisibly returns the catalog data frame while
  printing a formatted list to the console.
}
\details{
  ChartKit provides 48 chart types organized into three categories:
  \itemize{
    \item \strong{Basic Charts (16 types)}: Fundamental visualizations like
          scatter plots, line charts, bar charts, and histograms
    \item \strong{Intermediate Charts (15 types)}: More specialized visualizations
          like heatmaps, correlation matrices, and ridgeline plots
    \item \strong{Advanced Charts (17 types)}: Complex visualizations like
          Sankey diagrams, network graphs, and treemaps
  }

  All functions follow the \code{viz_*()} naming convention for consistency.
}
\examples{
# Get catalog as data frame
catalog <- get_chart_catalog()
head(catalog)

# Print formatted list
print_chart_catalog()

# Filter to specific category
library(dplyr)
catalog \%>\%
  filter(category == "Basic") \%>\%
  select(function_name, description)

# Search for specific chart type
catalog \%>\%
  filter(grepl("network|flow", description, ignore.case = TRUE))
}
\seealso{
  \code{\link{ChartKit-package}} for package overview
}
