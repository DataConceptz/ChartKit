#' Sankey Diagram
#'
#' Create a publication-ready Sankey diagram for flow visualization.
#'
#' @param data Data frame with source, target, and value columns
#' @param source Column name for source nodes
#' @param target Column name for target nodes
#' @param value Column name for flow values
#' @param title Plot title
#' @param subtitle Plot subtitle
#' @return A ggplot2 object using ggalluvial
#' @export
viz_sankey <- function(data, source, target, value,
                      title = NULL, subtitle = NULL) {
  source_var <- rlang::enquo(source)
  target_var <- rlang::enquo(target)
  value_var <- rlang::enquo(value)

  # Prepare data for alluvial format
  plot_data <- data %>%
    dplyr::mutate(
      source = !!source_var,
      target = !!target_var,
      value = !!value_var
    )

  p <- ggplot2::ggplot(plot_data,
                      ggplot2::aes(y = value, axis1 = source, axis2 = target)) +
    ggalluvial::geom_alluvium(ggplot2::aes(fill = source), width = 1/12, alpha = 0.7) +
    ggalluvial::geom_stratum(width = 1/12, fill = "grey80", color = "white") +
    ggalluvial::stat_stratum(geom = "text", ggplot2::aes(label = ggplot2::after_stat(stratum))) +
    scale_fill_publication() +
    theme_publication() +
    ggplot2::theme(
      axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank()
    ) +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      x = "", y = "Flow"
    )

  return(p)
}

#' Treemap
#'
#' Create a publication-ready treemap.
#'
#' @param data Data frame
#' @param area Column name for area size
#' @param fill Column name for fill color/grouping
#' @param label Column name for labels
#' @param title Plot title
#' @param subtitle Plot subtitle
#' @return A ggplot2 object
#' @export
viz_treemap <- function(data, area, fill, label,
                       title = NULL, subtitle = NULL) {
  area_var <- rlang::enquo(area)
  fill_var <- rlang::enquo(fill)
  label_var <- rlang::enquo(label)

  p <- ggplot2::ggplot(data, ggplot2::aes(area = !!area_var, fill = !!fill_var,
                                          label = !!label_var)) +
    treemapify::geom_treemap(color = "white", size = 2) +
    treemapify::geom_treemap_text(colour = "white", place = "centre", grow = TRUE) +
    scale_fill_publication() +
    theme_publication() +
    ggplot2::theme(
      legend.position = "bottom",
      axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.line = ggplot2::element_blank()
    ) +
    ggplot2::labs(
      title = title,
      subtitle = subtitle
    )

  return(p)
}

#' Sunburst Chart
#'
#' Create a publication-ready sunburst (hierarchical pie) chart.
#'
#' @param data Data frame with hierarchical structure
#' @param parent Column name for parent category
#' @param child Column name for child category
#' @param value Column name for values
#' @param title Plot title
#' @param subtitle Plot subtitle
#' @return A ggplot2 object
#' @export
viz_sunburst <- function(data, parent, child, value,
                        title = NULL, subtitle = NULL) {
  parent_var <- rlang::enquo(parent)
  child_var <- rlang::enquo(child)
  value_var <- rlang::enquo(value)

  # Create hierarchical structure
  plot_data <- data %>%
    dplyr::mutate(
      parent = !!parent_var,
      child = !!child_var,
      value = !!value_var,
      label = paste(parent, child, sep = " - ")
    )

  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = parent, y = value, fill = child)) +
    ggplot2::geom_col(width = 1, color = "white") +
    ggplot2::coord_polar(theta = "y") +
    scale_fill_publication() +
    theme_publication() +
    ggplot2::theme(
      axis.text = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.line = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank()
    ) +
    ggplot2::labs(
      title = title,
      subtitle = subtitle
    )

  return(p)
}

#' Network Graph
#'
#' Create a publication-ready network graph.
#'
#' @param nodes Data frame with node information (id, label, etc.)
#' @param edges Data frame with edge information (from, to, weight, etc.)
#' @param node_id Column name for node IDs in nodes data
#' @param edge_from Column name for source nodes in edges data
#' @param edge_to Column name for target nodes in edges data
#' @param title Plot title
#' @param subtitle Plot subtitle
#' @param layout Layout algorithm: "fr", "kk", "circle", "star" (default: "fr")
#' @return A ggplot2 object
#' @export
viz_network <- function(nodes, edges, node_id, edge_from, edge_to,
                       title = NULL, subtitle = NULL, layout = "fr") {
  node_id_var <- rlang::enquo(node_id)
  from_var <- rlang::enquo(edge_from)
  to_var <- rlang::enquo(edge_to)

  # Create igraph object
  graph <- igraph::graph_from_data_frame(
    d = edges %>% dplyr::select(!!from_var, !!to_var),
    vertices = nodes %>% dplyr::select(!!node_id_var),
    directed = FALSE
  )

  p <- ggraph::ggraph(graph, layout = layout) +
    ggraph::geom_edge_link(alpha = 0.5, color = "grey70") +
    ggraph::geom_node_point(size = 5, color = "#0072B2", alpha = 0.8) +
    ggraph::geom_node_text(ggplot2::aes(label = name), repel = TRUE, size = 3) +
    theme_publication() +
    ggplot2::theme(
      axis.text = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.line = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank()
    ) +
    ggplot2::labs(
      title = title,
      subtitle = subtitle
    )

  return(p)
}

#' Parallel Coordinates Plot
#'
#' Create a publication-ready parallel coordinates plot.
#'
#' @param data Data frame
#' @param vars Vector of column names to include
#' @param group Optional column name for grouping/coloring
#' @param title Plot title
#' @param subtitle Plot subtitle
#' @param alpha Line transparency (default: 0.5)
#' @return A ggplot2 object
#' @export
viz_parallel <- function(data, vars, group = NULL,
                        title = NULL, subtitle = NULL, alpha = 0.5) {
  group_var <- rlang::enquo(group)

  # Convert to long format
  plot_data <- data %>%
    dplyr::select(dplyr::all_of(vars), !!group_var) %>%
    dplyr::mutate(id = dplyr::row_number()) %>%
    tidyr::pivot_longer(cols = dplyr::all_of(vars),
                       names_to = "variable",
                       values_to = "value")

  if (!rlang::quo_is_null(group_var)) {
    p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = variable, y = value,
                                                 group = id, color = !!group_var))
  } else {
    p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = variable, y = value, group = id))
  }

  p <- p +
    ggplot2::geom_line(alpha = alpha) +
    theme_publication() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      x = "", y = "Value"
    )

  if (!rlang::quo_is_null(group_var)) {
    p <- p + scale_color_publication()
  }

  return(p)
}

#' Chord Diagram
#'
#' Create a publication-ready chord diagram for relationship visualization.
#'
#' @param data Data frame with from, to, and value columns
#' @param from Column name for source
#' @param to Column name for target
#' @param value Column name for connection strength
#' @param title Plot title
#' @param subtitle Plot subtitle
#' @return A ggplot2 object
#' @export
viz_chord <- function(data, from, to, value,
                     title = NULL, subtitle = NULL) {
  from_var <- rlang::enquo(from)
  to_var <- rlang::enquo(to)
  value_var <- rlang::enquo(value)

  # Prepare data
  plot_data <- data %>%
    dplyr::mutate(
      from = !!from_var,
      to = !!to_var,
      value = !!value_var
    )

  # Create network for circular layout
  graph <- igraph::graph_from_data_frame(plot_data, directed = TRUE)

  p <- ggraph::ggraph(graph, layout = 'linear', circular = TRUE) +
    ggraph::geom_edge_arc(ggplot2::aes(width = value, alpha = value),
                         strength = 0.5, color = "#0072B2") +
    ggraph::geom_node_point(size = 5, color = "#D55E00") +
    ggraph::geom_node_text(ggplot2::aes(label = name), repel = TRUE) +
    ggraph::scale_edge_width(range = c(0.5, 3)) +
    theme_publication() +
    ggplot2::theme(
      axis.text = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.line = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      legend.position = "none"
    ) +
    ggplot2::labs(
      title = title,
      subtitle = subtitle
    )

  return(p)
}

#' Calendar Heatmap
#'
#' Create a publication-ready calendar heatmap.
#'
#' @param data Data frame with date and value columns
#' @param date Column name for dates
#' @param value Column name for values
#' @param title Plot title
#' @param subtitle Plot subtitle
#' @return A ggplot2 object
#' @export
viz_calendar_heatmap <- function(data, date, value,
                                title = NULL, subtitle = NULL) {
  date_var <- rlang::enquo(date)
  value_var <- rlang::enquo(value)

  plot_data <- data %>%
    dplyr::mutate(
      date = !!date_var,
      value = !!value_var,
      week = lubridate::week(date),
      weekday = lubridate::wday(date, label = TRUE),
      month = lubridate::month(date, label = TRUE)
    )

  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = week, y = weekday, fill = value)) +
    ggplot2::geom_tile(color = "white", linewidth = 0.5) +
    ggplot2::facet_wrap(~ month, scales = "free_x") +
    viridis::scale_fill_viridis(option = "plasma") +
    theme_publication() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank()
    ) +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      x = "Week", y = "",
      fill = "Value"
    )

  return(p)
}

#' Hexbin Plot
#'
#' Create a publication-ready hexagonal binning plot for large datasets.
#'
#' @param data Data frame
#' @param x Column name for x-axis
#' @param y Column name for y-axis
#' @param bins Number of bins (default: 30)
#' @param title Plot title
#' @param subtitle Plot subtitle
#' @param xlab X-axis label
#' @param ylab Y-axis label
#' @return A ggplot2 object
#' @export
viz_hexbin <- function(data, x, y, bins = 30,
                      title = NULL, subtitle = NULL, xlab = NULL, ylab = NULL) {
  x_var <- rlang::enquo(x)
  y_var <- rlang::enquo(y)

  p <- ggplot2::ggplot(data, ggplot2::aes(x = !!x_var, y = !!y_var)) +
    ggplot2::geom_hex(bins = bins) +
    viridis::scale_fill_viridis(option = "inferno") +
    theme_publication() +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      x = xlab %||% rlang::as_name(x_var),
      y = ylab %||% rlang::as_name(y_var),
      fill = "Count"
    )

  return(p)
}

#' Stream Graph
#'
#' Create a publication-ready stream graph (stacked area with centered baseline).
#'
#' @param data Data frame
#' @param x Column name for x-axis (usually time)
#' @param y Column name for values
#' @param fill Column name for categories
#' @param title Plot title
#' @param subtitle Plot subtitle
#' @param xlab X-axis label
#' @return A ggplot2 object
#' @export
viz_stream <- function(data, x, y, fill,
                      title = NULL, subtitle = NULL, xlab = NULL) {
  x_var <- rlang::enquo(x)
  y_var <- rlang::enquo(y)
  fill_var <- rlang::enquo(fill)

  p <- ggplot2::ggplot(data, ggplot2::aes(x = !!x_var, y = !!y_var, fill = !!fill_var)) +
    ggplot2::geom_area(alpha = 0.8, position = "stack") +
    scale_fill_publication() +
    theme_publication() +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      x = xlab %||% rlang::as_name(x_var),
      y = ""
    )

  return(p)
}

#' Alluvial Diagram
#'
#' Create a publication-ready alluvial diagram for categorical flow data.
#'
#' @param data Data frame with categorical variables
#' @param axes Vector of column names for axes
#' @param freq Column name for frequencies
#' @param title Plot title
#' @param subtitle Plot subtitle
#' @return A ggplot2 object
#' @export
viz_alluvial <- function(data, axes, freq,
                        title = NULL, subtitle = NULL) {
  freq_var <- rlang::enquo(freq)

  p <- ggplot2::ggplot(data,
                      ggplot2::aes(y = !!freq_var,
                                  axis1 = .data[[axes[1]]],
                                  axis2 = .data[[axes[2]]])) +
    ggalluvial::geom_alluvium(ggplot2::aes(fill = .data[[axes[1]]]),
                             width = 1/12, alpha = 0.7) +
    ggalluvial::geom_stratum(width = 1/12, fill = "grey80", color = "white") +
    ggalluvial::stat_stratum(geom = "text", ggplot2::aes(label = ggplot2::after_stat(stratum)),
                      size = 3) +
    scale_fill_publication() +
    theme_publication() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(size = 10),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank()
    ) +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      x = "", y = "Frequency"
    )

  return(p)
}

#' Circular Bar Chart
#'
#' Create a publication-ready circular bar chart (race track plot).
#'
#' @param data Data frame
#' @param category Column name for categories
#' @param value Column name for values
#' @param title Plot title
#' @param subtitle Plot subtitle
#' @return A ggplot2 object
#' @export
viz_circular_bar <- function(data, category, value,
                            title = NULL, subtitle = NULL) {
  cat_var <- rlang::enquo(category)
  val_var <- rlang::enquo(value)

  plot_data <- data %>%
    dplyr::mutate(
      category = !!cat_var,
      value = !!val_var,
      id = dplyr::row_number()
    )

  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = reorder(category, value), y = value,
                                               fill = category)) +
    ggplot2::geom_col(width = 0.85) +
    ggplot2::coord_polar() +
    ggplot2::ylim(-max(plot_data$value) * 0.5, max(plot_data$value)) +
    scale_fill_publication() +
    theme_publication() +
    ggplot2::theme(
      axis.text.y = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      legend.position = "none"
    ) +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      x = "", y = ""
    )

  return(p)
}

#' Bullet Chart
#'
#' Create a publication-ready bullet chart for KPI visualization.
#'
#' @param data Data frame
#' @param category Column name for categories/metrics
#' @param value Column name for actual values
#' @param target Column name for target values
#' @param title Plot title
#' @param subtitle Plot subtitle
#' @return A ggplot2 object
#' @export
viz_bullet <- function(data, category, value, target,
                      title = NULL, subtitle = NULL) {
  cat_var <- rlang::enquo(category)
  val_var <- rlang::enquo(value)
  target_var <- rlang::enquo(target)

  plot_data <- data %>%
    dplyr::mutate(
      category = !!cat_var,
      value = !!val_var,
      target = !!target_var
    )

  p <- ggplot2::ggplot(plot_data) +
    ggplot2::geom_col(ggplot2::aes(x = category, y = target),
                     fill = "grey80", width = 0.5) +
    ggplot2::geom_col(ggplot2::aes(x = category, y = value),
                     fill = "#0072B2", width = 0.3) +
    ggplot2::geom_point(ggplot2::aes(x = category, y = target),
                       shape = "|", size = 10, color = "#D55E00") +
    ggplot2::coord_flip() +
    theme_publication() +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      x = "", y = "Value"
    )

  return(p)
}

#' Slope Chart
#'
#' Create a publication-ready slope chart for comparing two time points.
#'
#' @param data Data frame
#' @param category Column name for categories
#' @param time Column name for time periods (should have exactly 2 values)
#' @param value Column name for values
#' @param title Plot title
#' @param subtitle Plot subtitle
#' @return A ggplot2 object
#' @export
viz_slope <- function(data, category, time, value,
                     title = NULL, subtitle = NULL) {
  cat_var <- rlang::enquo(category)
  time_var <- rlang::enquo(time)
  val_var <- rlang::enquo(value)

  p <- ggplot2::ggplot(data, ggplot2::aes(x = !!time_var, y = !!val_var,
                                          group = !!cat_var, color = !!cat_var)) +
    ggplot2::geom_line(linewidth = 1.5, alpha = 0.7) +
    ggplot2::geom_point(size = 4) +
    ggrepel::geom_text_repel(ggplot2::aes(label = !!cat_var),
                            data = data %>% dplyr::filter(!!time_var == min(!!time_var)),
                            hjust = 1.5, direction = "y") +
    ggrepel::geom_text_repel(ggplot2::aes(label = !!cat_var),
                            data = data %>% dplyr::filter(!!time_var == max(!!time_var)),
                            hjust = -0.5, direction = "y") +
    scale_color_publication() +
    theme_publication() +
    ggplot2::theme(legend.position = "none") +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      x = "", y = "Value"
    )

  return(p)
}

#' Wind Rose
#'
#' Create a publication-ready wind rose diagram.
#'
#' @param data Data frame
#' @param direction Column name for wind direction (in degrees or categories)
#' @param speed Column name for wind speed
#' @param title Plot title
#' @param subtitle Plot subtitle
#' @return A ggplot2 object
#' @export
viz_wind_rose <- function(data, direction, speed,
                         title = NULL, subtitle = NULL) {
  dir_var <- rlang::enquo(direction)
  speed_var <- rlang::enquo(speed)

  p <- ggplot2::ggplot(data, ggplot2::aes(x = !!dir_var, fill = !!speed_var)) +
    ggplot2::geom_bar(width = 1) +
    ggplot2::coord_polar(start = 0) +
    viridis::scale_fill_viridis(option = "turbo") +
    theme_publication() +
    ggplot2::theme(
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank()
    ) +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      x = "Direction", y = "",
      fill = "Speed"
    )

  return(p)
}

#' Gantt Chart
#'
#' Create a publication-ready Gantt chart for project timelines.
#'
#' @param data Data frame
#' @param task Column name for task names
#' @param start Column name for start dates
#' @param end Column name for end dates
#' @param category Optional column name for task categories
#' @param title Plot title
#' @param subtitle Plot subtitle
#' @return A ggplot2 object
#' @export
viz_gantt <- function(data, task, start, end, category = NULL,
                     title = NULL, subtitle = NULL) {
  task_var <- rlang::enquo(task)
  start_var <- rlang::enquo(start)
  end_var <- rlang::enquo(end)
  cat_var <- rlang::enquo(category)

  p <- ggplot2::ggplot(data, ggplot2::aes(y = reorder(!!task_var, !!start_var)))

  if (!rlang::quo_is_null(cat_var)) {
    p <- p + ggplot2::geom_segment(ggplot2::aes(x = !!start_var, xend = !!end_var,
                                                yend = !!task_var, color = !!cat_var),
                                   size = 10)
    p <- p + scale_color_publication()
  } else {
    p <- p + ggplot2::geom_segment(ggplot2::aes(x = !!start_var, xend = !!end_var,
                                                yend = !!task_var),
                                   size = 10, color = "#0072B2")
  }

  p <- p +
    theme_publication() +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      x = "Time", y = "",
      color = "Category"
    )

  return(p)
}

#' Mosaic Plot
#'
#' Create a publication-ready mosaic plot for categorical data.
#'
#' @param data Data frame
#' @param x Column name for x-axis categories
#' @param y Column name for y-axis categories
#' @param fill Optional column name for fill
#' @param title Plot title
#' @param subtitle Plot subtitle
#' @return A ggplot2 object
#' @export
viz_mosaic <- function(data, x, y, fill = NULL,
                      title = NULL, subtitle = NULL) {
  x_var <- rlang::enquo(x)
  y_var <- rlang::enquo(y)
  fill_var <- rlang::enquo(fill)

  # Calculate proportions
  plot_data <- data %>%
    dplyr::count(!!x_var, !!y_var) %>%
    dplyr::group_by(!!x_var) %>%
    dplyr::mutate(prop = n / sum(n))

  if (!rlang::quo_is_null(fill_var)) {
    plot_data <- plot_data %>% dplyr::mutate(fill_col = !!fill_var)
    p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = !!x_var, y = prop, fill = !!y_var))
  } else {
    p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = !!x_var, y = prop, fill = !!y_var))
  }

  p <- p +
    ggplot2::geom_col(width = 1, color = "white") +
    scale_fill_publication() +
    theme_publication() +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      x = rlang::as_name(x_var),
      y = "Proportion"
    )

  return(p)
}

#' Marimekko Chart
#'
#' Create a publication-ready Marimekko (mosaic with variable widths) chart.
#'
#' @param data Data frame
#' @param x Column name for x-axis categories
#' @param y Column name for y-axis categories
#' @param size Column name for segment sizes
#' @param title Plot title
#' @param subtitle Plot subtitle
#' @return A ggplot2 object
#' @export
viz_marimekko <- function(data, x, y, size,
                         title = NULL, subtitle = NULL) {
  x_var <- rlang::enquo(x)
  y_var <- rlang::enquo(y)
  size_var <- rlang::enquo(size)

  # Calculate positions
  plot_data <- data %>%
    dplyr::mutate(
      x_cat = !!x_var,
      y_cat = !!y_var,
      value = !!size_var
    ) %>%
    dplyr::group_by(x_cat) %>%
    dplyr::mutate(
      x_total = sum(value),
      y_prop = value / sum(value)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      x_end = cumsum(x_total) / sum(x_total),
      x_start = dplyr::lag(x_end, default = 0)
    ) %>%
    dplyr::group_by(x_cat) %>%
    dplyr::mutate(
      y_end = cumsum(y_prop),
      y_start = dplyr::lag(y_end, default = 0)
    )

  p <- ggplot2::ggplot(plot_data) +
    ggplot2::geom_rect(ggplot2::aes(xmin = x_start, xmax = x_end,
                                    ymin = y_start, ymax = y_end,
                                    fill = y_cat),
                      color = "white", size = 1) +
    scale_fill_publication() +
    theme_publication() +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      x = "", y = "Proportion",
      fill = rlang::as_name(y_var)
    )

  return(p)
}
