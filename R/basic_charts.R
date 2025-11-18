#' Scatter Plot
#'
#' Create a publication-ready scatter plot showing the relationship between
#' two continuous variables. Supports color grouping and variable point sizes.
#'
#' @param data Data frame containing the variables to plot
#' @param x Column name for x-axis (unquoted)
#' @param y Column name for y-axis (unquoted)
#' @param color Optional column name for color grouping (unquoted)
#' @param size Optional column name for point size (unquoted)
#' @param title Plot title (character string)
#' @param subtitle Plot subtitle (character string)
#' @param xlab X-axis label (character string, defaults to column name)
#' @param ylab Y-axis label (character string, defaults to column name)
#' @param alpha Point transparency (numeric, 0-1, default: 0.7)
#' @param point_size Point size when size aesthetic is not used (numeric, default: 3)
#' @return A ggplot2 object that can be further customized
#' @export
#' @examples
#' library(dplyr)
#'
#' # Basic scatter plot
#' viz_scatter(mtcars, x = wt, y = mpg,
#'             title = "Fuel Efficiency vs Weight")
#'
#' # With color grouping (convert to factor first)
#' mtcars_clean <- mtcars %>%
#'   mutate(cyl_factor = factor(cyl))
#' viz_scatter(mtcars_clean, x = wt, y = mpg, color = cyl_factor,
#'             title = "MPG by Weight and Cylinders",
#'             xlab = "Weight (1000 lbs)",
#'             ylab = "Miles per Gallon")
#'
#' # With variable point sizes
#' viz_scatter(mtcars, x = wt, y = mpg, size = hp,
#'             title = "Bubble Chart: Weight, MPG, and Horsepower")
#'
#' @seealso \code{\link{viz_bubble}} for three-variable relationships,
#'          \code{\link{theme_publication}} for the default theme
viz_scatter <- function(data, x, y, color = NULL, size = NULL,
                       title = NULL, subtitle = NULL, xlab = NULL, ylab = NULL,
                       alpha = 0.7, point_size = 3) {
  x_var <- rlang::enquo(x)
  y_var <- rlang::enquo(y)
  color_var <- rlang::enquo(color)
  size_var <- rlang::enquo(size)

  p <- ggplot2::ggplot(data, ggplot2::aes(x = !!x_var, y = !!y_var))

  if (!rlang::quo_is_null(color_var) && !rlang::quo_is_null(size_var)) {
    p <- p + ggplot2::geom_point(ggplot2::aes(color = !!color_var, size = !!size_var), alpha = alpha)
  } else if (!rlang::quo_is_null(color_var)) {
    p <- p + ggplot2::geom_point(ggplot2::aes(color = !!color_var), size = point_size, alpha = alpha)
  } else if (!rlang::quo_is_null(size_var)) {
    p <- p + ggplot2::geom_point(ggplot2::aes(size = !!size_var), alpha = alpha)
  } else {
    p <- p + ggplot2::geom_point(size = point_size, alpha = alpha)
  }

  p <- p + theme_publication() +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      x = xlab %||% rlang::as_name(x_var),
      y = ylab %||% rlang::as_name(y_var)
    )

  if (!rlang::quo_is_null(color_var)) {
    p <- p + scale_color_publication()
  }

  return(p)
}

#' Line Plot
#'
#' Create a publication-ready line plot.
#'
#' @param data Data frame
#' @param x Column name for x-axis
#' @param y Column name for y-axis
#' @param group Optional column name for grouping lines
#' @param color Optional column name for color grouping
#' @param title Plot title
#' @param subtitle Plot subtitle
#' @param xlab X-axis label
#' @param ylab Y-axis label
#' @param line_size Line width (default: 1)
#' @return A ggplot2 object
#' @export
viz_line <- function(data, x, y, group = NULL, color = NULL,
                    title = NULL, subtitle = NULL, xlab = NULL, ylab = NULL,
                    line_size = 1) {
  x_var <- rlang::enquo(x)
  y_var <- rlang::enquo(y)
  group_var <- rlang::enquo(group)
  color_var <- rlang::enquo(color)

  p <- ggplot2::ggplot(data, ggplot2::aes(x = !!x_var, y = !!y_var))

  if (!rlang::quo_is_null(color_var)) {
    p <- p + ggplot2::geom_line(ggplot2::aes(color = !!color_var, group = !!color_var),
                                size = line_size)
    p <- p + scale_color_publication()
  } else if (!rlang::quo_is_null(group_var)) {
    p <- p + ggplot2::geom_line(ggplot2::aes(group = !!group_var), linewidth = line_size)
  } else {
    p <- p + ggplot2::geom_line(linewidth = line_size)
  }

  p <- p + theme_publication() +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      x = xlab %||% rlang::as_name(x_var),
      y = ylab %||% rlang::as_name(y_var)
    )

  return(p)
}

#' Bar Plot
#'
#' Create a publication-ready bar plot.
#'
#' @param data Data frame
#' @param x Column name for x-axis (categories)
#' @param y Column name for y-axis (values)
#' @param fill Optional column name for fill color grouping
#' @param horizontal Create horizontal bars (default: FALSE)
#' @param position Position adjustment: "stack", "dodge", "fill" (default: "stack")
#' @param title Plot title
#' @param subtitle Plot subtitle
#' @param xlab X-axis label
#' @param ylab Y-axis label
#' @return A ggplot2 object
#' @export
viz_bar <- function(data, x, y, fill = NULL, horizontal = FALSE,
                   position = "stack", title = NULL, subtitle = NULL,
                   xlab = NULL, ylab = NULL) {
  x_var <- rlang::enquo(x)
  y_var <- rlang::enquo(y)
  fill_var <- rlang::enquo(fill)

  p <- ggplot2::ggplot(data, ggplot2::aes(x = !!x_var, y = !!y_var))

  if (!rlang::quo_is_null(fill_var)) {
    p <- p + ggplot2::geom_col(ggplot2::aes(fill = !!fill_var), position = position)
    p <- p + scale_fill_publication()
  } else {
    p <- p + ggplot2::geom_col(fill = "#0072B2")
  }

  if (horizontal) {
    p <- p + ggplot2::coord_flip()
  }

  p <- p + theme_publication() +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      x = xlab %||% rlang::as_name(x_var),
      y = ylab %||% rlang::as_name(y_var)
    )

  return(p)
}

#' Histogram
#'
#' Create a publication-ready histogram.
#'
#' @param data Data frame
#' @param x Column name for values
#' @param bins Number of bins (default: 30)
#' @param fill Fill color (default: "#0072B2")
#' @param title Plot title
#' @param subtitle Plot subtitle
#' @param xlab X-axis label
#' @param ylab Y-axis label
#' @return A ggplot2 object
#' @export
viz_histogram <- function(data, x, bins = 30, fill = "#0072B2",
                         title = NULL, subtitle = NULL, xlab = NULL, ylab = "Count") {
  x_var <- rlang::enquo(x)

  p <- ggplot2::ggplot(data, ggplot2::aes(x = !!x_var)) +
    ggplot2::geom_histogram(bins = bins, fill = fill, color = "white", alpha = 0.8) +
    theme_publication() +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      x = xlab %||% rlang::as_name(x_var),
      y = ylab
    )

  return(p)
}

#' Boxplot
#'
#' Create a publication-ready boxplot.
#'
#' @param data Data frame
#' @param x Column name for categories (optional)
#' @param y Column name for values
#' @param fill Optional column name for fill color
#' @param title Plot title
#' @param subtitle Plot subtitle
#' @param xlab X-axis label
#' @param ylab Y-axis label
#' @param outlier_color Color for outliers (default: "red")
#' @return A ggplot2 object
#' @export
viz_boxplot <- function(data, x = NULL, y, fill = NULL,
                       title = NULL, subtitle = NULL, xlab = NULL, ylab = NULL,
                       outlier_color = "red") {
  x_var <- rlang::enquo(x)
  y_var <- rlang::enquo(y)
  fill_var <- rlang::enquo(fill)

  if (!rlang::quo_is_null(x_var)) {
    p <- ggplot2::ggplot(data, ggplot2::aes(x = !!x_var, y = !!y_var))
  } else {
    p <- ggplot2::ggplot(data, ggplot2::aes(x = "", y = !!y_var))
  }

  if (!rlang::quo_is_null(fill_var)) {
    p <- p + ggplot2::geom_boxplot(ggplot2::aes(fill = !!fill_var),
                                   outlier.color = outlier_color)
    p <- p + scale_fill_publication()
  } else {
    p <- p + ggplot2::geom_boxplot(fill = "#0072B2", outlier.color = outlier_color)
  }

  p <- p + theme_publication() +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      x = xlab %||% if(!rlang::quo_is_null(x_var)) rlang::as_name(x_var) else "",
      y = ylab %||% rlang::as_name(y_var)
    )

  return(p)
}

#' Violin Plot
#'
#' Create a publication-ready violin plot.
#'
#' @param data Data frame
#' @param x Column name for categories (optional)
#' @param y Column name for values
#' @param fill Optional column name for fill color
#' @param title Plot title
#' @param subtitle Plot subtitle
#' @param xlab X-axis label
#' @param ylab Y-axis label
#' @return A ggplot2 object
#' @export
viz_violin <- function(data, x = NULL, y, fill = NULL,
                      title = NULL, subtitle = NULL, xlab = NULL, ylab = NULL) {
  x_var <- rlang::enquo(x)
  y_var <- rlang::enquo(y)
  fill_var <- rlang::enquo(fill)

  if (!rlang::quo_is_null(x_var)) {
    p <- ggplot2::ggplot(data, ggplot2::aes(x = !!x_var, y = !!y_var))
  } else {
    p <- ggplot2::ggplot(data, ggplot2::aes(x = "", y = !!y_var))
  }

  if (!rlang::quo_is_null(fill_var)) {
    p <- p + ggplot2::geom_violin(ggplot2::aes(fill = !!fill_var))
    p <- p + scale_fill_publication()
  } else {
    p <- p + ggplot2::geom_violin(fill = "#0072B2", alpha = 0.7)
  }

  p <- p + theme_publication() +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      x = xlab %||% if(!rlang::quo_is_null(x_var)) rlang::as_name(x_var) else "",
      y = ylab %||% rlang::as_name(y_var)
    )

  return(p)
}

#' Density Plot
#'
#' Create a publication-ready density plot.
#'
#' @param data Data frame
#' @param x Column name for values
#' @param fill Optional column name for fill color grouping
#' @param title Plot title
#' @param subtitle Plot subtitle
#' @param xlab X-axis label
#' @param ylab Y-axis label
#' @param alpha Fill transparency (default: 0.5)
#' @return A ggplot2 object
#' @export
viz_density <- function(data, x, fill = NULL,
                       title = NULL, subtitle = NULL, xlab = NULL, ylab = "Density",
                       alpha = 0.5) {
  x_var <- rlang::enquo(x)
  fill_var <- rlang::enquo(fill)

  p <- ggplot2::ggplot(data, ggplot2::aes(x = !!x_var))

  if (!rlang::quo_is_null(fill_var)) {
    p <- p + ggplot2::geom_density(ggplot2::aes(fill = !!fill_var), alpha = alpha)
    p <- p + scale_fill_publication()
  } else {
    p <- p + ggplot2::geom_density(fill = "#0072B2", alpha = alpha)
  }

  p <- p + theme_publication() +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      x = xlab %||% rlang::as_name(x_var),
      y = ylab
    )

  return(p)
}

#' Pie Chart
#'
#' Create a publication-ready pie chart.
#'
#' @param data Data frame
#' @param category Column name for categories
#' @param value Column name for values
#' @param title Plot title
#' @param subtitle Plot subtitle
#' @return A ggplot2 object
#' @export
viz_pie <- function(data, category, value, title = NULL, subtitle = NULL) {
  cat_var <- rlang::enquo(category)
  val_var <- rlang::enquo(value)

  # Calculate percentages
  plot_data <- data %>%
    dplyr::mutate(
      category = !!cat_var,
      value = !!val_var,
      percentage = value / sum(value) * 100,
      label = paste0(category, "\n", round(percentage, 1), "%")
    )

  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = "", y = value, fill = category)) +
    ggplot2::geom_col(width = 1, color = "white") +
    ggplot2::coord_polar(theta = "y", start = 0) +
    scale_fill_publication() +
    theme_publication() +
    ggplot2::theme(
      axis.text = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.line = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank()
    ) +
    ggplot2::labs(title = title, subtitle = subtitle)

  return(p)
}

#' Donut Chart
#'
#' Create a publication-ready donut chart.
#'
#' @param data Data frame
#' @param category Column name for categories
#' @param value Column name for values
#' @param title Plot title
#' @param subtitle Plot subtitle
#' @param hole_size Size of the center hole (0-1, default: 0.5)
#' @return A ggplot2 object
#' @export
viz_donut <- function(data, category, value, title = NULL, subtitle = NULL, hole_size = 0.5) {
  cat_var <- rlang::enquo(category)
  val_var <- rlang::enquo(value)

  # Calculate percentages
  plot_data <- data %>%
    dplyr::mutate(
      category = !!cat_var,
      value = !!val_var,
      percentage = value / sum(value) * 100,
      label = paste0(category, "\n", round(percentage, 1), "%")
    )

  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = 2, y = value, fill = category)) +
    ggplot2::geom_col(width = 1, color = "white") +
    ggplot2::coord_polar(theta = "y", start = 0) +
    ggplot2::xlim(c(0.5, 2.5)) +
    scale_fill_publication() +
    theme_publication() +
    ggplot2::theme(
      axis.text = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.line = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank()
    ) +
    ggplot2::labs(title = title, subtitle = subtitle)

  return(p)
}

#' Area Plot
#'
#' Create a publication-ready area plot.
#'
#' @param data Data frame
#' @param x Column name for x-axis
#' @param y Column name for y-axis
#' @param fill Optional column name for fill grouping
#' @param position Position adjustment: "stack", "identity", "fill" (default: "stack")
#' @param title Plot title
#' @param subtitle Plot subtitle
#' @param xlab X-axis label
#' @param ylab Y-axis label
#' @param alpha Fill transparency (default: 0.7)
#' @return A ggplot2 object
#' @export
viz_area <- function(data, x, y, fill = NULL, position = "stack",
                    title = NULL, subtitle = NULL, xlab = NULL, ylab = NULL,
                    alpha = 0.7) {
  x_var <- rlang::enquo(x)
  y_var <- rlang::enquo(y)
  fill_var <- rlang::enquo(fill)

  p <- ggplot2::ggplot(data, ggplot2::aes(x = !!x_var, y = !!y_var))

  if (!rlang::quo_is_null(fill_var)) {
    p <- p + ggplot2::geom_area(ggplot2::aes(fill = !!fill_var, group = !!fill_var),
                                position = position, alpha = alpha)
    p <- p + scale_fill_publication()
  } else {
    p <- p + ggplot2::geom_area(fill = "#0072B2", alpha = alpha)
  }

  p <- p + theme_publication() +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      x = xlab %||% rlang::as_name(x_var),
      y = ylab %||% rlang::as_name(y_var)
    )

  return(p)
}

#' Ribbon Plot
#'
#' Create a publication-ready ribbon plot with confidence intervals.
#'
#' @param data Data frame
#' @param x Column name for x-axis
#' @param y Column name for y-axis (center line)
#' @param ymin Column name for lower bound
#' @param ymax Column name for upper bound
#' @param title Plot title
#' @param subtitle Plot subtitle
#' @param xlab X-axis label
#' @param ylab Y-axis label
#' @param fill Fill color (default: "#0072B2")
#' @param alpha Fill transparency (default: 0.3)
#' @return A ggplot2 object
#' @export
viz_ribbon <- function(data, x, y, ymin, ymax,
                      title = NULL, subtitle = NULL, xlab = NULL, ylab = NULL,
                      fill = "#0072B2", alpha = 0.3) {
  x_var <- rlang::enquo(x)
  y_var <- rlang::enquo(y)
  ymin_var <- rlang::enquo(ymin)
  ymax_var <- rlang::enquo(ymax)

  p <- ggplot2::ggplot(data, ggplot2::aes(x = !!x_var, y = !!y_var)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = !!ymin_var, ymax = !!ymax_var),
                        fill = fill, alpha = alpha) +
    ggplot2::geom_line(color = fill, linewidth = 1) +
    theme_publication() +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      x = xlab %||% rlang::as_name(x_var),
      y = ylab %||% rlang::as_name(y_var)
    )

  return(p)
}

#' Step Plot
#'
#' Create a publication-ready step plot.
#'
#' @param data Data frame
#' @param x Column name for x-axis
#' @param y Column name for y-axis
#' @param color Optional column name for color grouping
#' @param direction Direction: "hv", "vh", "mid" (default: "hv")
#' @param title Plot title
#' @param subtitle Plot subtitle
#' @param xlab X-axis label
#' @param ylab Y-axis label
#' @return A ggplot2 object
#' @export
viz_step <- function(data, x, y, color = NULL, direction = "hv",
                    title = NULL, subtitle = NULL, xlab = NULL, ylab = NULL) {
  x_var <- rlang::enquo(x)
  y_var <- rlang::enquo(y)
  color_var <- rlang::enquo(color)

  p <- ggplot2::ggplot(data, ggplot2::aes(x = !!x_var, y = !!y_var))

  if (!rlang::quo_is_null(color_var)) {
    p <- p + ggplot2::geom_step(ggplot2::aes(color = !!color_var), direction = direction, linewidth = 1)
    p <- p + scale_color_publication()
  } else {
    p <- p + ggplot2::geom_step(direction = direction, linewidth = 1, color = "#0072B2")
  }

  p <- p + theme_publication() +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      x = xlab %||% rlang::as_name(x_var),
      y = ylab %||% rlang::as_name(y_var)
    )

  return(p)
}

#' Error Bar Plot
#'
#' Create a publication-ready error bar plot.
#'
#' @param data Data frame
#' @param x Column name for x-axis
#' @param y Column name for y-axis (mean values)
#' @param ymin Column name for lower error bound
#' @param ymax Column name for upper error bound
#' @param color Optional column name for color grouping
#' @param title Plot title
#' @param subtitle Plot subtitle
#' @param xlab X-axis label
#' @param ylab Y-axis label
#' @return A ggplot2 object
#' @export
viz_errorbar <- function(data, x, y, ymin, ymax, color = NULL,
                        title = NULL, subtitle = NULL, xlab = NULL, ylab = NULL) {
  x_var <- rlang::enquo(x)
  y_var <- rlang::enquo(y)
  ymin_var <- rlang::enquo(ymin)
  ymax_var <- rlang::enquo(ymax)
  color_var <- rlang::enquo(color)

  p <- ggplot2::ggplot(data, ggplot2::aes(x = !!x_var, y = !!y_var))

  if (!rlang::quo_is_null(color_var)) {
    p <- p +
      ggplot2::geom_errorbar(ggplot2::aes(ymin = !!ymin_var, ymax = !!ymax_var, color = !!color_var),
                            width = 0.2) +
      ggplot2::geom_point(ggplot2::aes(color = !!color_var), size = 3) +
      scale_color_publication()
  } else {
    p <- p +
      ggplot2::geom_errorbar(ggplot2::aes(ymin = !!ymin_var, ymax = !!ymax_var),
                            width = 0.2, color = "#0072B2") +
      ggplot2::geom_point(size = 3, color = "#0072B2")
  }

  p <- p + theme_publication() +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      x = xlab %||% rlang::as_name(x_var),
      y = ylab %||% rlang::as_name(y_var)
    )

  return(p)
}

#' Jitter Plot
#'
#' Create a publication-ready jitter plot for categorical data.
#'
#' @param data Data frame
#' @param x Column name for categories
#' @param y Column name for values
#' @param color Optional column name for color grouping
#' @param title Plot title
#' @param subtitle Plot subtitle
#' @param xlab X-axis label
#' @param ylab Y-axis label
#' @param alpha Point transparency (default: 0.6)
#' @return A ggplot2 object
#' @export
viz_jitter <- function(data, x, y, color = NULL,
                      title = NULL, subtitle = NULL, xlab = NULL, ylab = NULL,
                      alpha = 0.6) {
  x_var <- rlang::enquo(x)
  y_var <- rlang::enquo(y)
  color_var <- rlang::enquo(color)

  p <- ggplot2::ggplot(data, ggplot2::aes(x = !!x_var, y = !!y_var))

  if (!rlang::quo_is_null(color_var)) {
    p <- p + ggplot2::geom_jitter(ggplot2::aes(color = !!color_var),
                                  width = 0.2, alpha = alpha, size = 2)
    p <- p + scale_color_publication()
  } else {
    p <- p + ggplot2::geom_jitter(width = 0.2, alpha = alpha, size = 2, color = "#0072B2")
  }

  p <- p + theme_publication() +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      x = xlab %||% rlang::as_name(x_var),
      y = ylab %||% rlang::as_name(y_var)
    )

  return(p)
}

#' Q-Q Plot
#'
#' Create a publication-ready quantile-quantile plot.
#'
#' @param data Data frame
#' @param sample Column name for sample values
#' @param distribution Theoretical distribution (default: "norm")
#' @param title Plot title
#' @param subtitle Plot subtitle
#' @return A ggplot2 object
#' @export
viz_qq <- function(data, sample, distribution = "norm",
                  title = NULL, subtitle = NULL) {
  sample_var <- rlang::enquo(sample)

  p <- ggplot2::ggplot(data, ggplot2::aes(sample = !!sample_var)) +
    ggplot2::stat_qq(color = "#0072B2", size = 2, alpha = 0.7) +
    ggplot2::stat_qq_line(color = "#D55E00", linewidth = 1, linetype = "dashed") +
    theme_publication() +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      x = "Theoretical Quantiles",
      y = "Sample Quantiles"
    )

  return(p)
}

#' ECDF Plot
#'
#' Create a publication-ready empirical cumulative distribution function plot.
#'
#' @param data Data frame
#' @param x Column name for values
#' @param color Optional column name for color grouping
#' @param title Plot title
#' @param subtitle Plot subtitle
#' @param xlab X-axis label
#' @return A ggplot2 object
#' @export
viz_ecdf <- function(data, x, color = NULL,
                    title = NULL, subtitle = NULL, xlab = NULL) {
  x_var <- rlang::enquo(x)
  color_var <- rlang::enquo(color)

  p <- ggplot2::ggplot(data, ggplot2::aes(x = !!x_var))

  if (!rlang::quo_is_null(color_var)) {
    p <- p + ggplot2::stat_ecdf(ggplot2::aes(color = !!color_var), linewidth = 1)
    p <- p + scale_color_publication()
  } else {
    p <- p + ggplot2::stat_ecdf(linewidth = 1, color = "#0072B2")
  }

  p <- p + theme_publication() +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      x = xlab %||% rlang::as_name(x_var),
      y = "ECDF"
    )

  return(p)
}
