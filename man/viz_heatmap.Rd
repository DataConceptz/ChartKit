\name{viz_heatmap}
\alias{viz_heatmap}
\title{Heatmap Visualization}
\description{
  Create publication-ready heatmaps for matrix data visualization. Heatmaps use
  color intensity to represent values in a two-dimensional grid, making them
  ideal for showing patterns in large datasets.
}
\usage{
viz_heatmap(data, x, y, value,
            title = NULL, subtitle = NULL,
            xlab = NULL, ylab = NULL,
            show_values = FALSE,
            color_palette = "plasma")
}
\arguments{
  \item{data}{Data frame in long format with x, y, and value columns}
  \item{x}{Column name for x-axis categories (unquoted)}
  \item{y}{Column name for y-axis categories (unquoted)}
  \item{value}{Column name for the values to visualize as colors (unquoted)}
  \item{title}{Plot title (character string)}
  \item{subtitle}{Plot subtitle (character string)}
  \item{xlab}{X-axis label (character string, defaults to column name)}
  \item{ylab}{Y-axis label (character string, defaults to column name)}
  \item{show_values}{Logical. If TRUE, displays numeric values on each tile.
                     Default is FALSE.}
  \item{color_palette}{Viridis color palette name. Options: "viridis", "magma",
                       "plasma" (default), "inferno", "cividis", "rocket", "turbo"}
}
\value{
  A ggplot2 object representing the heatmap.
}
\details{
  \strong{Data Format:}

  Heatmaps require data in long format:
  \preformatted{
   x      y    value
   Var1   A     23.5
   Var1   B     18.3
   Var2   A     31.2
   Var2   B     27.8
  }

  Convert wide format using tidyr::pivot_longer().

  \strong{When to Use Heatmaps:}
  \itemize{
    \item Visualizing correlation matrices
    \item Showing patterns in gene expression data
    \item Displaying temporal patterns across categories
    \item Comparing multiple variables across groups
    \item Revealing clusters or patterns in large datasets
  }

  \strong{Color Scales:}

  All palettes are perceptually uniform and colorblind-friendly:
  \itemize{
    \item \strong{plasma} (default): Purple-orange, great for general use
    \item \strong{viridis}: Blue-yellow, excellent for data with natural ordering
    \item \strong{magma}: Black-purple-yellow, good for dark backgrounds
    \item \strong{inferno}: Black-red-yellow, high contrast
    \item \strong{cividis}: Blue-yellow, optimized for colorblindness
  }

  \strong{Customization:}
  \itemize{
    \item Cluster rows/columns before plotting
    \item Use dendrograms for hierarchical patterns
    \item Add annotations for significant values
    \item Scale values for better color distribution
  }
}
\examples{
library(dplyr)
library(tidyr)

# Basic heatmap from wide data
# First, create long format data
heat_data <- expand.grid(
  x = paste0("Var", 1:8),
  y = paste0("Sample", 1:6)
) \%>\%
  mutate(value = rnorm(n(), mean = 0, sd = 2))

viz_heatmap(heat_data, x = x, y = y, value = value,
            title = "Basic Heatmap",
            subtitle = "8 variables across 6 samples")

# With values displayed
viz_heatmap(heat_data, x = x, y = y, value = value,
            show_values = TRUE,
            title = "Heatmap with Values")

# Different color palettes
viz_heatmap(heat_data, x = x, y = y, value = value,
            color_palette = "viridis",
            title = "Viridis Color Palette")

viz_heatmap(heat_data, x = x, y = y, value = value,
            color_palette = "magma",
            title = "Magma Color Palette")

# Real example: Monthly patterns
monthly_pattern <- expand.grid(
  month = month.abb,
  city = c("New York", "Los Angeles", "Chicago", "Houston")
) \%>\%
  mutate(
    month = factor(month, levels = month.abb),
    temperature = 50 + 20 * sin((match(month, month.abb) - 3) * pi / 6) +
                  rnorm(n(), 0, 3)
  )

viz_heatmap(monthly_pattern, x = month, y = city, value = temperature,
            title = "Average Monthly Temperatures",
            xlab = "Month",
            ylab = "City",
            color_palette = "inferno")

# Gene expression style heatmap
set.seed(123)
gene_expr <- expand.grid(
  gene = paste0("Gene", 1:20),
  sample = paste0("S", 1:10)
) \%>\%
  mutate(expression = rnorm(n(), mean = 5, sd = 2))

viz_heatmap(gene_expr, x = sample, y = gene, value = expression,
            title = "Gene Expression Heatmap",
            subtitle = "20 genes across 10 samples",
            color_palette = "plasma")

# Further customization with ggplot2
library(ggplot2)
viz_heatmap(heat_data, x = x, y = y, value = value) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red",
                       midpoint = 0) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
}
\note{
  \strong{Performance Tips:}
  \itemize{
    \item For large matrices (>100x100), consider sampling or binning
    \item Use show_values = FALSE for matrices >20x20 (text becomes unreadable)
    \item Pre-scale data for better color distribution
    \item Consider faceting instead of very wide heatmaps
  }

  \strong{Best Practices:}
  \itemize{
    \item Always include a color scale legend
    \item Choose color palette appropriate for your data type
    \item Order rows and columns meaningfully (e.g., by clustering)
    \item For correlation matrices, use diverging color scales
    \item Ensure adequate contrast between adjacent cells
    \item Label axes clearly with variable names
  }

  \strong{Data Transformation:}
  Often beneficial to transform data before heatmap:
  \itemize{
    \item Log-transform for skewed data
    \item Z-score normalization for cross-sample comparisons
    \item Rank-based transformation for robustness
  }
}
\seealso{
  Related functions:
  \itemize{
    \item \code{\link{viz_correlation}} - Specialized correlation matrix heatmap
    \item \code{\link{viz_calendar_heatmap}} - Temporal heatmap visualization
  }

  Additional resources:
  \itemize{
    \item \code{\link[viridis]{scale_fill_viridis}} - Color scale documentation
    \item See \code{inst/examples/intermediate_examples.R} for heatmap variations
  }
}
\references{
  Wilkinson, L. and Friendly, M. (2009). The History of the Cluster Heat Map.
  \emph{The American Statistician} 63(2), 179-184.
}
