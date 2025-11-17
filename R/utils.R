#' Null-coalescing operator
#'
#' @param x Value to check
#' @param y Default value if x is NULL
#' @keywords internal
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

#' Get chart type catalog
#'
#' Returns a data frame with all available chart types and their categories.
#'
#' @return A data frame with chart information
#' @export
#' @examples
#' get_chart_catalog()
get_chart_catalog <- function() {
  data.frame(
    category = c(
      rep("Basic", 16),
      rep("Intermediate", 15),
      rep("Advanced", 16)
    ),
    chart_type = c(
      # Basic
      "scatter", "line", "bar", "histogram", "boxplot", "violin",
      "density", "pie", "donut", "area", "ribbon", "step",
      "errorbar", "jitter", "qq", "ecdf",
      # Intermediate
      "heatmap", "correlation", "contour", "density2d", "ridgeline",
      "waterfall", "funnel", "radar", "bubble", "lollipop",
      "dumbbell", "cleveland_dot", "violin_box", "raincloud", "polar_bar",
      # Advanced
      "sankey", "treemap", "sunburst", "network", "parallel",
      "chord", "calendar_heatmap", "hexbin", "stream", "alluvial",
      "circular_bar", "bullet", "slope", "wind_rose", "gantt",
      "mosaic", "marimekko"
    ),
    function_name = c(
      # Basic
      "viz_scatter", "viz_line", "viz_bar", "viz_histogram", "viz_boxplot",
      "viz_violin", "viz_density", "viz_pie", "viz_donut", "viz_area",
      "viz_ribbon", "viz_step", "viz_errorbar", "viz_jitter", "viz_qq",
      "viz_ecdf",
      # Intermediate
      "viz_heatmap", "viz_correlation", "viz_contour", "viz_density2d",
      "viz_ridgeline", "viz_waterfall", "viz_funnel", "viz_radar",
      "viz_bubble", "viz_lollipop", "viz_dumbbell", "viz_cleveland_dot",
      "viz_violin_box", "viz_raincloud", "viz_polar_bar",
      # Advanced
      "viz_sankey", "viz_treemap", "viz_sunburst", "viz_network",
      "viz_parallel", "viz_chord", "viz_calendar_heatmap", "viz_hexbin",
      "viz_stream", "viz_alluvial", "viz_circular_bar", "viz_bullet",
      "viz_slope", "viz_wind_rose", "viz_gantt", "viz_mosaic",
      "viz_marimekko"
    ),
    description = c(
      # Basic
      "Scatter plot for showing relationships between two continuous variables",
      "Line plot for time series and trend visualization",
      "Bar chart for categorical comparisons",
      "Histogram for distribution of continuous data",
      "Box plot for showing distribution and outliers",
      "Violin plot for showing distribution shape",
      "Density plot for smooth distribution visualization",
      "Pie chart for showing proportions",
      "Donut chart for showing proportions with center space",
      "Area plot for cumulative or stacked data over time",
      "Ribbon plot for confidence intervals",
      "Step plot for discrete changes over time",
      "Error bar plot for showing uncertainty in measurements",
      "Jitter plot for categorical data with many points",
      "Q-Q plot for comparing distributions",
      "ECDF plot for empirical cumulative distribution",
      # Intermediate
      "Heatmap for matrix visualization",
      "Correlation matrix heatmap",
      "Contour plot for 3D surface visualization",
      "2D density plot for bivariate distributions",
      "Ridgeline plot for comparing distributions across categories",
      "Waterfall chart for cumulative effect visualization",
      "Funnel chart for conversion or process stages",
      "Radar chart for multivariate data comparison",
      "Bubble chart for three-variable relationships",
      "Lollipop chart for ranked categories",
      "Dumbbell chart for comparing two values",
      "Cleveland dot plot for precise value comparison",
      "Combined violin and box plot",
      "Raincloud plot combining violin, box, and points",
      "Polar bar chart for circular comparisons",
      # Advanced
      "Sankey diagram for flow visualization",
      "Treemap for hierarchical data",
      "Sunburst chart for hierarchical proportions",
      "Network graph for relationship visualization",
      "Parallel coordinates for multivariate analysis",
      "Chord diagram for inter-relationships",
      "Calendar heatmap for temporal patterns",
      "Hexbin plot for large datasets",
      "Stream graph for stacked time series",
      "Alluvial diagram for categorical flows",
      "Circular bar chart for cyclical data",
      "Bullet chart for KPI visualization",
      "Slope chart for comparing two time points",
      "Wind rose for directional data",
      "Gantt chart for project timelines",
      "Mosaic plot for categorical associations",
      "Marimekko chart for market share visualization"
    ),
    stringsAsFactors = FALSE
  )
}

#' Print chart catalog
#'
#' Prints a formatted list of all available charts.
#'
#' @export
#' @examples
#' print_chart_catalog()
print_chart_catalog <- function() {
  catalog <- get_chart_catalog()

  cat("\n=== ChartKit Chart Catalog ===\n")
  cat(sprintf("\nTotal Charts Available: %d\n\n", nrow(catalog)))

  for (cat_name in unique(catalog$category)) {
    cat(sprintf("\n--- %s Charts ---\n", cat_name))
    subset_data <- catalog[catalog$category == cat_name, ]

    for (i in 1:nrow(subset_data)) {
      cat(sprintf("  %2d. %-20s - %s\n",
                  i,
                  subset_data$function_name[i],
                  subset_data$description[i]))
    }
  }

  cat("\n")
  invisible(catalog)
}
