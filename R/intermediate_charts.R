#' Heatmap
#'
#' Create a publication-ready heatmap.
#'
#' @param data Data frame in long format with x, y, and value columns
#' @param x Column name for x-axis
#' @param y Column name for y-axis
#' @param value Column name for cell values
#' @param title Plot title
#' @param subtitle Plot subtitle
#' @param xlab X-axis label
#' @param ylab Y-axis label
#' @param show_values Show values in cells (default: FALSE)
#' @return A ggplot2 object
#' @export
viz_heatmap <- function(data, x, y, value,
                       title = NULL, subtitle = NULL, xlab = NULL, ylab = NULL,
                       show_values = FALSE) {
  x_var <- rlang::enquo(x)
  y_var <- rlang::enquo(y)
  value_var <- rlang::enquo(value)

  p <- ggplot2::ggplot(data, ggplot2::aes(x = !!x_var, y = !!y_var, fill = !!value_var)) +
    ggplot2::geom_tile(color = "white", linewidth = 0.5) +
    viridis::scale_fill_viridis(option = "plasma") +
    theme_publication() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
    ) +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      x = xlab %||% rlang::as_name(x_var),
      y = ylab %||% rlang::as_name(y_var),
      fill = rlang::as_name(value_var)
    )

  if (show_values) {
    p <- p + ggplot2::geom_text(ggplot2::aes(label = round(!!value_var, 2)),
                               color = "white", size = 3)
  }

  return(p)
}

#' Correlation Matrix
#'
#' Create a publication-ready correlation matrix heatmap.
#'
#' @param data Data frame with numeric columns
#' @param method Correlation method: "pearson", "spearman", "kendall" (default: "pearson")
#' @param title Plot title
#' @param subtitle Plot subtitle
#' @param show_values Show correlation values (default: TRUE)
#' @return A ggplot2 object
#' @export
viz_correlation <- function(data, method = "pearson",
                           title = NULL, subtitle = NULL, show_values = TRUE) {
  # Calculate correlation matrix
  cor_mat <- stats::cor(data, use = "complete.obs", method = method)

  # Convert to long format
  cor_data <- as.data.frame(as.table(cor_mat))
  names(cor_data) <- c("Var1", "Var2", "Correlation")

  p <- ggplot2::ggplot(cor_data, ggplot2::aes(x = Var1, y = Var2, fill = Correlation)) +
    ggplot2::geom_tile(color = "white") +
    ggplot2::scale_fill_gradient2(
      low = "#0072B2", mid = "white", high = "#D55E00",
      midpoint = 0, limits = c(-1, 1)
    ) +
    theme_publication() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
    ) +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      x = "", y = ""
    )

  if (show_values) {
    p <- p + ggplot2::geom_text(ggplot2::aes(label = round(Correlation, 2)),
                               size = 3)
  }

  return(p)
}

#' Contour Plot
#'
#' Create a publication-ready contour plot.
#'
#' @param data Data frame
#' @param x Column name for x-axis
#' @param y Column name for y-axis
#' @param z Column name for z values
#' @param title Plot title
#' @param subtitle Plot subtitle
#' @param xlab X-axis label
#' @param ylab Y-axis label
#' @param bins Number of contour bins (default: 10)
#' @return A ggplot2 object
#' @export
viz_contour <- function(data, x, y, z,
                       title = NULL, subtitle = NULL, xlab = NULL, ylab = NULL,
                       bins = 10) {
  x_var <- rlang::enquo(x)
  y_var <- rlang::enquo(y)
  z_var <- rlang::enquo(z)

  p <- ggplot2::ggplot(data, ggplot2::aes(x = !!x_var, y = !!y_var, z = !!z_var)) +
    ggplot2::geom_contour_filled(bins = bins) +
    viridis::scale_fill_viridis(discrete = TRUE, option = "viridis") +
    theme_publication() +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      x = xlab %||% rlang::as_name(x_var),
      y = ylab %||% rlang::as_name(y_var),
      fill = rlang::as_name(z_var)
    )

  return(p)
}

#' 2D Density Plot
#'
#' Create a publication-ready 2D density plot.
#'
#' @param data Data frame
#' @param x Column name for x-axis
#' @param y Column name for y-axis
#' @param title Plot title
#' @param subtitle Plot subtitle
#' @param xlab X-axis label
#' @param ylab Y-axis label
#' @return A ggplot2 object
#' @export
viz_density2d <- function(data, x, y,
                         title = NULL, subtitle = NULL, xlab = NULL, ylab = NULL) {
  x_var <- rlang::enquo(x)
  y_var <- rlang::enquo(y)

  p <- ggplot2::ggplot(data, ggplot2::aes(x = !!x_var, y = !!y_var)) +
    ggplot2::geom_density_2d_filled(alpha = 0.8) +
    ggplot2::geom_point(size = 0.5, alpha = 0.3) +
    viridis::scale_fill_viridis(discrete = TRUE, option = "magma") +
    theme_publication() +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      x = xlab %||% rlang::as_name(x_var),
      y = ylab %||% rlang::as_name(y_var)
    )

  return(p)
}

#' Ridgeline Plot
#'
#' Create a publication-ready ridgeline (joy) plot.
#'
#' @param data Data frame
#' @param x Column name for values
#' @param y Column name for categories
#' @param fill Optional column name for fill
#' @param title Plot title
#' @param subtitle Plot subtitle
#' @param xlab X-axis label
#' @param ylab Y-axis label
#' @return A ggplot2 object
#' @export
viz_ridgeline <- function(data, x, y, fill = NULL,
                         title = NULL, subtitle = NULL, xlab = NULL, ylab = NULL) {
  x_var <- rlang::enquo(x)
  y_var <- rlang::enquo(y)
  fill_var <- rlang::enquo(fill)

  if (!rlang::quo_is_null(fill_var)) {
    p <- ggplot2::ggplot(data, ggplot2::aes(x = !!x_var, y = !!y_var, fill = !!fill_var))
  } else {
    p <- ggplot2::ggplot(data, ggplot2::aes(x = !!x_var, y = !!y_var, fill = !!y_var))
  }

  p <- p +
    ggridges::geom_density_ridges(alpha = 0.7, scale = 2) +
    scale_fill_publication() +
    theme_publication() +
    ggplot2::theme(legend.position = "none") +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      x = xlab %||% rlang::as_name(x_var),
      y = ylab %||% rlang::as_name(y_var)
    )

  return(p)
}

#' Waterfall Chart
#'
#' Create a publication-ready waterfall chart.
#'
#' @param data Data frame with category and value columns
#' @param category Column name for categories
#' @param value Column name for values
#' @param title Plot title
#' @param subtitle Plot subtitle
#' @return A ggplot2 object
#' @export
viz_waterfall <- function(data, category, value,
                         title = NULL, subtitle = NULL) {
  cat_var <- rlang::enquo(category)
  val_var <- rlang::enquo(value)

  # Calculate cumulative values
  plot_data <- data %>%
    dplyr::mutate(
      category = !!cat_var,
      value = !!val_var,
      id = dplyr::row_number(),
      end = cumsum(value),
      start = dplyr::lag(end, default = 0),
      type = ifelse(value >= 0, "Positive", "Negative")
    )

  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = reorder(category, id))) +
    ggplot2::geom_rect(ggplot2::aes(xmin = id - 0.4, xmax = id + 0.4,
                                    ymin = start, ymax = end, fill = type)) +
    ggplot2::geom_segment(ggplot2::aes(x = id + 0.4, xend = id + 1.4,
                                       y = end, yend = end),
                         linetype = "dashed", color = "grey50") +
    scale_fill_publication() +
    theme_publication() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      x = "", y = "Value",
      fill = "Change"
    )

  return(p)
}

#' Funnel Chart
#'
#' Create a publication-ready funnel chart.
#'
#' @param data Data frame with stage and value columns
#' @param stage Column name for stages
#' @param value Column name for values
#' @param title Plot title
#' @param subtitle Plot subtitle
#' @return A ggplot2 object
#' @export
viz_funnel <- function(data, stage, value,
                      title = NULL, subtitle = NULL) {
  stage_var <- rlang::enquo(stage)
  value_var <- rlang::enquo(value)

  plot_data <- data %>%
    dplyr::mutate(
      stage = !!stage_var,
      value = !!value_var,
      percentage = value / max(value) * 100
    )

  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = reorder(stage, -value), y = value, fill = stage)) +
    ggplot2::geom_col(width = 0.7) +
    ggplot2::coord_flip() +
    scale_fill_publication() +
    theme_publication() +
    ggplot2::theme(legend.position = "none") +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      x = "", y = "Value"
    )

  return(p)
}

#' Radar Chart
#'
#' Create a publication-ready radar (spider) chart.
#'
#' @param data Data frame with variables and values
#' @param variables Column name for variable names
#' @param values Column name for values
#' @param group Optional column name for grouping
#' @param title Plot title
#' @param subtitle Plot subtitle
#' @return A ggplot2 object
#' @export
viz_radar <- function(data, variables, values, group = NULL,
                     title = NULL, subtitle = NULL) {
  var_var <- rlang::enquo(variables)
  val_var <- rlang::enquo(values)
  group_var <- rlang::enquo(group)

  if (!rlang::quo_is_null(group_var)) {
    p <- ggplot2::ggplot(data, ggplot2::aes(x = !!var_var, y = !!val_var,
                                            group = !!group_var, color = !!group_var, fill = !!group_var))
  } else {
    p <- ggplot2::ggplot(data, ggplot2::aes(x = !!var_var, y = !!val_var, group = 1))
  }

  p <- p +
    ggplot2::geom_polygon(alpha = 0.2, linewidth = 1) +
    ggplot2::geom_point(size = 3) +
    ggplot2::coord_polar() +
    scale_color_publication() +
    scale_fill_publication() +
    theme_publication() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(size = 10)
    ) +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      x = "", y = ""
    )

  return(p)
}

#' Bubble Chart
#'
#' Create a publication-ready bubble chart.
#'
#' @param data Data frame
#' @param x Column name for x-axis
#' @param y Column name for y-axis
#' @param size Column name for bubble size
#' @param color Optional column name for color grouping
#' @param title Plot title
#' @param subtitle Plot subtitle
#' @param xlab X-axis label
#' @param ylab Y-axis label
#' @param alpha Bubble transparency (default: 0.6)
#' @return A ggplot2 object
#' @export
viz_bubble <- function(data, x, y, size, color = NULL,
                      title = NULL, subtitle = NULL, xlab = NULL, ylab = NULL,
                      alpha = 0.6) {
  x_var <- rlang::enquo(x)
  y_var <- rlang::enquo(y)
  size_var <- rlang::enquo(size)
  color_var <- rlang::enquo(color)

  p <- ggplot2::ggplot(data, ggplot2::aes(x = !!x_var, y = !!y_var, size = !!size_var))

  if (!rlang::quo_is_null(color_var)) {
    p <- p + ggplot2::geom_point(ggplot2::aes(color = !!color_var), alpha = alpha)
    p <- p + scale_color_publication()
  } else {
    p <- p + ggplot2::geom_point(alpha = alpha, color = "#0072B2")
  }

  p <- p +
    ggplot2::scale_size_continuous(range = c(2, 20)) +
    theme_publication() +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      x = xlab %||% rlang::as_name(x_var),
      y = ylab %||% rlang::as_name(y_var),
      size = rlang::as_name(size_var)
    )

  return(p)
}

#' Lollipop Chart
#'
#' Create a publication-ready lollipop chart.
#'
#' @param data Data frame
#' @param x Column name for categories
#' @param y Column name for values
#' @param horizontal Create horizontal chart (default: FALSE)
#' @param title Plot title
#' @param subtitle Plot subtitle
#' @param xlab X-axis label
#' @param ylab Y-axis label
#' @return A ggplot2 object
#' @export
viz_lollipop <- function(data, x, y, horizontal = FALSE,
                        title = NULL, subtitle = NULL, xlab = NULL, ylab = NULL) {
  x_var <- rlang::enquo(x)
  y_var <- rlang::enquo(y)

  p <- ggplot2::ggplot(data, ggplot2::aes(x = reorder(!!x_var, !!y_var), y = !!y_var)) +
    ggplot2::geom_segment(ggplot2::aes(xend = !!x_var, yend = 0),
                         color = "#0072B2", size = 1) +
    ggplot2::geom_point(color = "#D55E00", size = 4) +
    theme_publication() +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      x = xlab %||% rlang::as_name(x_var),
      y = ylab %||% rlang::as_name(y_var)
    )

  if (horizontal) {
    p <- p + ggplot2::coord_flip()
  } else {
    p <- p + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  }

  return(p)
}

#' Dumbbell Chart
#'
#' Create a publication-ready dumbbell chart for comparing two values.
#'
#' @param data Data frame
#' @param category Column name for categories
#' @param value1 Column name for first value
#' @param value2 Column name for second value
#' @param title Plot title
#' @param subtitle Plot subtitle
#' @return A ggplot2 object
#' @export
viz_dumbbell <- function(data, category, value1, value2,
                        title = NULL, subtitle = NULL) {
  cat_var <- rlang::enquo(category)
  val1_var <- rlang::enquo(value1)
  val2_var <- rlang::enquo(value2)

  plot_data <- data %>%
    dplyr::mutate(
      category = !!cat_var,
      value1 = !!val1_var,
      value2 = !!val2_var
    )

  p <- ggplot2::ggplot(plot_data) +
    ggplot2::geom_segment(ggplot2::aes(x = value1, xend = value2,
                                       y = reorder(category, value1), yend = category),
                         color = "grey70", size = 2) +
    ggplot2::geom_point(ggplot2::aes(x = value1, y = category),
                       color = "#0072B2", size = 4) +
    ggplot2::geom_point(ggplot2::aes(x = value2, y = category),
                       color = "#D55E00", size = 4) +
    theme_publication() +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      x = "Value", y = ""
    )

  return(p)
}

#' Cleveland Dot Plot
#'
#' Create a publication-ready Cleveland dot plot.
#'
#' @param data Data frame
#' @param x Column name for values
#' @param y Column name for categories
#' @param group Optional column name for grouping
#' @param title Plot title
#' @param subtitle Plot subtitle
#' @return A ggplot2 object
#' @export
viz_cleveland_dot <- function(data, x, y, group = NULL,
                             title = NULL, subtitle = NULL) {
  x_var <- rlang::enquo(x)
  y_var <- rlang::enquo(y)
  group_var <- rlang::enquo(group)

  p <- ggplot2::ggplot(data, ggplot2::aes(x = !!x_var, y = reorder(!!y_var, !!x_var)))

  if (!rlang::quo_is_null(group_var)) {
    p <- p + ggplot2::geom_point(ggplot2::aes(color = !!group_var), size = 4)
    p <- p + scale_color_publication()
  } else {
    p <- p + ggplot2::geom_point(size = 4, color = "#0072B2")
  }

  p <- p +
    ggplot2::geom_segment(ggplot2::aes(xend = 0, yend = !!y_var),
                         color = "grey70") +
    theme_publication() +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      x = rlang::as_name(x_var),
      y = ""
    )

  return(p)
}

#' Violin-Box Combo Plot
#'
#' Create a publication-ready combined violin and box plot.
#'
#' @param data Data frame
#' @param x Column name for categories
#' @param y Column name for values
#' @param fill Optional column name for fill
#' @param title Plot title
#' @param subtitle Plot subtitle
#' @param xlab X-axis label
#' @param ylab Y-axis label
#' @return A ggplot2 object
#' @export
viz_violin_box <- function(data, x, y, fill = NULL,
                          title = NULL, subtitle = NULL, xlab = NULL, ylab = NULL) {
  x_var <- rlang::enquo(x)
  y_var <- rlang::enquo(y)
  fill_var <- rlang::enquo(fill)

  p <- ggplot2::ggplot(data, ggplot2::aes(x = !!x_var, y = !!y_var))

  if (!rlang::quo_is_null(fill_var)) {
    p <- p +
      ggplot2::geom_violin(ggplot2::aes(fill = !!fill_var), alpha = 0.5) +
      ggplot2::geom_boxplot(ggplot2::aes(fill = !!fill_var), width = 0.2, alpha = 0.8) +
      scale_fill_publication()
  } else {
    p <- p +
      ggplot2::geom_violin(fill = "#0072B2", alpha = 0.5) +
      ggplot2::geom_boxplot(fill = "#0072B2", width = 0.2, alpha = 0.8)
  }

  p <- p +
    theme_publication() +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      x = xlab %||% rlang::as_name(x_var),
      y = ylab %||% rlang::as_name(y_var)
    )

  return(p)
}

#' Raincloud Plot
#'
#' Create a publication-ready raincloud plot (combination of violin, box, and jitter).
#'
#' @param data Data frame
#' @param x Column name for categories
#' @param y Column name for values
#' @param fill Optional column name for fill
#' @param title Plot title
#' @param subtitle Plot subtitle
#' @param xlab X-axis label
#' @param ylab Y-axis label
#' @return A ggplot2 object
#' @export
viz_raincloud <- function(data, x, y, fill = NULL,
                         title = NULL, subtitle = NULL, xlab = NULL, ylab = NULL) {
  x_var <- rlang::enquo(x)
  y_var <- rlang::enquo(y)
  fill_var <- rlang::enquo(fill)

  p <- ggplot2::ggplot(data, ggplot2::aes(x = !!x_var, y = !!y_var))

  if (!rlang::quo_is_null(fill_var)) {
    p <- p +
      ggplot2::geom_violin(ggplot2::aes(fill = !!fill_var), alpha = 0.5, trim = FALSE) +
      ggplot2::geom_boxplot(ggplot2::aes(fill = !!fill_var), width = 0.2, alpha = 0.8,
                           position = ggplot2::position_nudge(x = 0.2)) +
      ggplot2::geom_jitter(ggplot2::aes(color = !!fill_var), width = 0.1, alpha = 0.3,
                          position = ggplot2::position_nudge(x = -0.2)) +
      scale_fill_publication() +
      scale_color_publication()
  } else {
    p <- p +
      ggplot2::geom_violin(fill = "#0072B2", alpha = 0.5, trim = FALSE) +
      ggplot2::geom_boxplot(fill = "#0072B2", width = 0.2, alpha = 0.8) +
      ggplot2::geom_jitter(width = 0.1, alpha = 0.3, color = "#0072B2")
  }

  p <- p +
    theme_publication() +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      x = xlab %||% rlang::as_name(x_var),
      y = ylab %||% rlang::as_name(y_var)
    )

  return(p)
}

#' Polar Bar Chart
#'
#' Create a publication-ready polar bar chart (circular bar chart).
#'
#' @param data Data frame
#' @param category Column name for categories
#' @param value Column name for values
#' @param title Plot title
#' @param subtitle Plot subtitle
#' @return A ggplot2 object
#' @export
viz_polar_bar <- function(data, category, value,
                         title = NULL, subtitle = NULL) {
  cat_var <- rlang::enquo(category)
  val_var <- rlang::enquo(value)

  p <- ggplot2::ggplot(data, ggplot2::aes(x = !!cat_var, y = !!val_var, fill = !!cat_var)) +
    ggplot2::geom_col(width = 1, color = "white") +
    ggplot2::coord_polar() +
    scale_fill_publication() +
    theme_publication() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(size = 9),
      legend.position = "none"
    ) +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      x = "", y = ""
    )

  return(p)
}
