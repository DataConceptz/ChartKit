# ==============================================================================
# ChartKit: Circular and Radial Chart Examples
# ==============================================================================
# Radar/spider charts, circular bar plots, wind roses, and circular stacked bars
# Following r-graph-gallery.com standards
# ==============================================================================

library(ChartKit)
library(dplyr)
library(ggplot2)

# ==============================================================================
# 1. RADAR/SPIDER CHARTS
# ==============================================================================

# -----------------------------------------------------------------------------
# 1.1 Basic Radar Chart (Single Individual)
# -----------------------------------------------------------------------------
# Create sample data
skills_data <- data.frame(
  variable = c("Programming", "Statistics", "Communication",
               "Design", "Management"),
  value = c(85, 75, 90, 70, 65)
)

viz_radar(skills_data,
          variables = variable,
          values = value,
          title = "Skills Assessment - Individual Profile",
          subtitle = "Scale: 0-100")

# -----------------------------------------------------------------------------
# 1.2 Radar Chart with Multiple Individuals
# -----------------------------------------------------------------------------
# Compare three people
team_skills <- data.frame(
  variable = rep(c("Programming", "Statistics", "Communication",
                   "Design", "Management"), 3),
  value = c(85, 75, 90, 70, 65,  # Person A
            70, 90, 75, 85, 80,  # Person B
            90, 80, 65, 75, 90), # Person C
  person = rep(c("Alice", "Bob", "Charlie"), each = 5)
)

viz_radar(team_skills,
          variables = variable,
          values = value,
          group = person,
          title = "Team Skills Comparison",
          subtitle = "Three team members across five dimensions")

# -----------------------------------------------------------------------------
# 1.3 Custom Radar Chart with Styling
# -----------------------------------------------------------------------------
library(ggradar)

# Prepare data in format needed for ggradar (rows = individuals, cols = variables)
if (requireNamespace("ggradar", quietly = TRUE)) {
  team_wide <- team_skills %>%
    tidyr::pivot_wider(names_from = variable, values_from = value) %>%
    # Normalize to 0-1 scale for ggradar
    mutate(across(where(is.numeric), ~ ./100))

  p <- ggradar::ggradar(
    team_wide,
    group.point.size = 3,
    group.line.width = 1.2,
    gridline.mid.colour = "grey",
    grid.label.size = 4,
    axis.label.size = 4,
    legend.text.size = 10,
    plot.title = "Professional Skills Radar"
  ) +
    theme(plot.title = element_text(size = 14, face = "bold"))
  print(p)
}

# -----------------------------------------------------------------------------
# 1.4 Radar Chart with Highlighted Range
# -----------------------------------------------------------------------------
# Show min/max range with individual overlay
team_summary <- team_skills %>%
  group_by(variable) %>%
  summarise(
    min_val = min(value),
    max_val = max(value),
    mean_val = mean(value)
  )

# Create polygon for range
p <- ggplot(team_summary, aes(x = variable, group = 1)) +
  geom_polygon(aes(y = max_val), fill = "#0072B2", alpha = 0.2) +
  geom_polygon(aes(y = min_val), fill = "white", alpha = 1) +
  geom_line(aes(y = mean_val), color = "#D55E00",
            linewidth = 1.5, alpha = 0.8) +
  geom_point(aes(y = mean_val), color = "#D55E00",
             size = 4, alpha = 0.9) +
  coord_polar() +
  ylim(0, 100) +
  theme_publication() +
  labs(
    title = "Team Skills Range and Average",
    subtitle = "Shaded area = min-max range, orange line = mean",
    x = "",
    y = "Score"
  )
print(p)

# ==============================================================================
# 2. CIRCULAR BAR PLOTS
# ==============================================================================

# -----------------------------------------------------------------------------
# 2.1 Basic Circular Bar Plot
# -----------------------------------------------------------------------------
# Monthly data
monthly_data <- data.frame(
  month = factor(month.abb, levels = month.abb),
  value = c(45, 52, 61, 73, 84, 92, 98, 95, 82, 68, 54, 48)
)

viz_circular_bar(monthly_data,
                 category = month,
                 value = value,
                 title = "Monthly Temperature Pattern",
                 subtitle = "Circular layout emphasizes cyclical nature")

# -----------------------------------------------------------------------------
# 2.2 Circular Bar Plot with Custom Styling
# -----------------------------------------------------------------------------
p <- ggplot(monthly_data, aes(x = month, y = value, fill = value)) +
  geom_bar(stat = "identity", alpha = 0.8) +
  scale_fill_viridis_c(option = "plasma", name = "Temperature") +
  coord_polar(start = 0) +
  ylim(-20, max(monthly_data$value)) +  # Add space in center
  theme_publication() +
  theme(
    axis.text.y = element_blank(),
    axis.ticks = element_blank()
  ) +
  labs(
    title = "Annual Temperature Cycle",
    subtitle = "Gradient colors show temperature intensity",
    x = "",
    y = ""
  )
print(p)

# -----------------------------------------------------------------------------
# 2.3 Circular Bar Plot with Labels
# -----------------------------------------------------------------------------
monthly_labeled <- monthly_data %>%
  mutate(
    angle = 90 - (as.numeric(month) - 1) * 360 / n(),
    hjust = ifelse(angle < -90, 1, 0),
    angle = ifelse(angle < -90, angle + 180, angle)
  )

p <- ggplot(monthly_labeled, aes(x = month, y = value, fill = month)) +
  geom_bar(stat = "identity", alpha = 0.8, width = 0.9) +
  geom_text(aes(y = value + 5, label = month, angle = angle, hjust = hjust),
            size = 3.5, fontface = "bold") +
  scale_fill_publication() +
  coord_polar(start = 0) +
  ylim(-30, max(monthly_labeled$value) * 1.2) +
  theme_publication() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    legend.position = "none",
    panel.grid = element_blank()
  ) +
  labs(title = "Monthly Pattern with Radial Labels")
print(p)

# ==============================================================================
# 3. CIRCULAR STACKED BAR PLOTS
# ==============================================================================

# -----------------------------------------------------------------------------
# 3.1 Circular Stacked Bar Plot
# -----------------------------------------------------------------------------
# Create stacked data
circular_stack_data <- data.frame(
  category = rep(LETTERS[1:8], each = 3),
  subcategory = rep(c("Type1", "Type2", "Type3"), 8),
  value = runif(24, 10, 30)
)

p <- ggplot(circular_stack_data, aes(x = category, y = value, fill = subcategory)) +
  geom_bar(stat = "identity", position = "stack", alpha = 0.8) +
  scale_fill_publication() +
  coord_polar(start = 0) +
  ylim(-20, max(tapply(circular_stack_data$value,
                       circular_stack_data$category, sum)) * 1.1) +
  theme_publication() +
  labs(
    title = "Circular Stacked Bar Plot",
    subtitle = "Each segment represents a subcategory",
    x = "",
    fill = "Subcategory"
  )
print(p)

# -----------------------------------------------------------------------------
# 3.2 Circular 100% Stacked (Proportions)
# -----------------------------------------------------------------------------
circular_pct <- circular_stack_data %>%
  group_by(category) %>%
  mutate(
    pct = value / sum(value) * 100,
    total = sum(value)
  ) %>%
  ungroup()

p <- ggplot(circular_pct, aes(x = category, y = pct, fill = subcategory)) +
  geom_bar(stat = "identity", position = "stack", alpha = 0.8, width = 1) +
  scale_fill_publication() +
  coord_polar(start = 0) +
  theme_publication() +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  labs(
    title = "Circular 100% Stacked Bar Plot",
    subtitle = "Shows proportional composition of each category",
    x = "",
    y = "",
    fill = "Subcategory"
  )
print(p)

# -----------------------------------------------------------------------------
# 3.3 Multi-Level Circular Stacked Plot
# -----------------------------------------------------------------------------
# Create hierarchical circular data
multi_level <- data.frame(
  inner_cat = rep(c("Q1", "Q2", "Q3", "Q4"), each = 6),
  outer_cat = rep(rep(c("Jan-Feb", "Mar-Apr", "May-Jun",
                        "Jul-Aug", "Sep-Oct", "Nov-Dec"), 4)),
  value = runif(24, 50, 150)
) %>%
  mutate(
    id = row_number(),
    angle = 90 - (id - 0.5) * 360 / n()
  )

p <- ggplot(multi_level, aes(x = id, y = value)) +
  # Inner ring (quarters)
  geom_bar(aes(y = 50, fill = inner_cat),
           stat = "identity", position = "dodge", width = 1) +
  # Outer ring (months)
  geom_bar(aes(y = value, alpha = outer_cat),
           stat = "identity", fill = "#0072B2", width = 0.9) +
  coord_polar(start = 0) +
  scale_fill_publication() +
  scale_alpha_manual(values = rep(c(0.4, 0.6, 0.8), 4)) +
  ylim(-50, max(multi_level$value) * 1.1) +
  theme_publication() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank()
  ) +
  labs(
    title = "Multi-Level Circular Chart",
    subtitle = "Inner ring = quarters, outer ring = bi-monthly data",
    fill = "Quarter"
  ) +
  guides(alpha = "none")
print(p)

# ==============================================================================
# 4. WIND ROSE PLOTS
# ==============================================================================

# -----------------------------------------------------------------------------
# 4.1 Basic Wind Rose
# -----------------------------------------------------------------------------
# Simulate wind data
set.seed(123)
wind_data <- data.frame(
  direction = sample(c("N", "NE", "E", "SE", "S", "SW", "W", "NW"),
                     200, replace = TRUE,
                     prob = c(0.15, 0.1, 0.08, 0.12, 0.18, 0.15, 0.12, 0.1)),
  speed = abs(rnorm(200, 15, 5))
) %>%
  mutate(
    speed_bin = cut(speed,
                    breaks = c(0, 5, 10, 15, 20, Inf),
                    labels = c("0-5", "5-10", "10-15", "15-20", ">20"))
  )

viz_wind_rose(wind_data,
              direction = direction,
              speed = speed,
              title = "Wind Rose Diagram",
              subtitle = "Wind direction and speed distribution")

# -----------------------------------------------------------------------------
# 4.2 Custom Wind Rose with Speed Bins
# -----------------------------------------------------------------------------
wind_summary <- wind_data %>%
  count(direction, speed_bin) %>%
  mutate(
    direction = factor(direction, levels = c("N", "NE", "E", "SE",
                                              "S", "SW", "W", "NW"))
  )

p <- ggplot(wind_summary, aes(x = direction, y = n, fill = speed_bin)) +
  geom_bar(stat = "identity", position = "stack", width = 0.9, alpha = 0.8) +
  scale_fill_viridis_d(option = "turbo", name = "Wind Speed\n(km/h)") +
  coord_polar(start = 0) +
  theme_publication() +
  labs(
    title = "Wind Rose with Speed Categories",
    subtitle = "Radial bars show wind frequency by direction",
    x = "Direction",
    y = "Frequency"
  )
print(p)

# ==============================================================================
# 5. POLAR COORDINATE PLOTS
# ==============================================================================

# -----------------------------------------------------------------------------
# 5.1 Polar Area Chart (Nightingale Rose)
# -----------------------------------------------------------------------------
# Monthly sales data
monthly_sales <- data.frame(
  month = factor(month.abb, levels = month.abb),
  sales = c(120, 145, 168, 195, 210, 235, 250, 245, 220, 185, 155, 135)
)

viz_polar_bar(monthly_sales,
              category = month,
              value = sales,
              title = "Polar Area Chart (Nightingale Rose)",
              subtitle = "Annual sales pattern in polar coordinates")

# -----------------------------------------------------------------------------
# 5.2 Coxcomb Chart (Multiple Series)
# -----------------------------------------------------------------------------
quarterly_metrics <- data.frame(
  quarter = rep(paste0("Q", 1:4), 3),
  metric = rep(c("Revenue", "Costs", "Profit"), each = 4),
  value = c(100, 120, 135, 125,   # Revenue
            60, 70, 75, 72,        # Costs
            40, 50, 60, 53)        # Profit
) %>%
  mutate(
    quarter = factor(quarter, levels = paste0("Q", 1:4))
  )

p <- ggplot(quarterly_metrics, aes(x = quarter, y = value, fill = metric)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.8, width = 0.9) +
  scale_fill_publication() +
  coord_polar(start = 0) +
  theme_publication() +
  labs(
    title = "Coxcomb Chart - Quarterly Metrics",
    subtitle = "Comparing multiple metrics in polar layout",
    x = "",
    y = "Value",
    fill = "Metric"
  )
print(p)

# -----------------------------------------------------------------------------
# 5.3 Radial Line Plot (Circular Time Series)
# -----------------------------------------------------------------------------
# Hourly data for a day
hourly_data <- data.frame(
  hour = 0:23,
  activity = 50 + 30 * sin((0:23) * pi / 12) + rnorm(24, 0, 5)
)

p <- ggplot(hourly_data, aes(x = hour, y = activity)) +
  geom_area(fill = "#0072B2", alpha = 0.4) +
  geom_line(color = "#0072B2", linewidth = 1.2) +
  geom_point(color = "#D55E00", size = 2) +
  coord_polar(start = 0) +
  scale_x_continuous(breaks = seq(0, 23, 3),
                     labels = paste0(seq(0, 23, 3), ":00")) +
  theme_publication() +
  labs(
    title = "24-Hour Activity Pattern (Radial)",
    subtitle = "Circular representation emphasizes daily cycle",
    x = "Hour of Day",
    y = "Activity Level"
  )
print(p)

# ==============================================================================
# 6. ADVANCED CIRCULAR TECHNIQUES
# ==============================================================================

# -----------------------------------------------------------------------------
# 6.1 Circular Heatmap
# -----------------------------------------------------------------------------
# Create circular heatmap data
circular_heat <- expand.grid(
  angle = 1:24,
  radius = 1:5
) %>%
  mutate(
    value = sin(angle * pi / 12) * radius + rnorm(n(), 0, 0.5),
    angle_rad = angle * 2 * pi / 24
  )

p <- ggplot(circular_heat, aes(x = angle, y = radius, fill = value)) +
  geom_tile() +
  scale_fill_viridis_c(option = "magma", name = "Value") +
  coord_polar(start = 0) +
  theme_publication() +
  theme(
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  ) +
  labs(
    title = "Circular Heatmap",
    subtitle = "Radial and angular dimensions",
    x = "Angle",
    y = ""
  )
print(p)

# -----------------------------------------------------------------------------
# 6.2 Sunburst-style Radial Plot
# -----------------------------------------------------------------------------
# Hierarchical circular data
sunburst_data <- data.frame(
  level1 = rep(c("A", "B", "C"), each = 8),
  level2 = rep(paste0("Cat", 1:8), 3),
  value = runif(24, 5, 20)
) %>%
  arrange(level1, level2) %>%
  mutate(id = row_number())

p <- ggplot(sunburst_data, aes(x = id, y = value)) +
  # Inner ring
  geom_bar(aes(y = 10, fill = level1),
           stat = "identity", width = 1, alpha = 0.6) +
  # Outer ring
  geom_bar(aes(y = value + 10, fill = level2),
           stat = "identity", width = 1, alpha = 0.9) +
  coord_polar(start = 0) +
  scale_fill_publication() +
  ylim(0, max(sunburst_data$value) + 15) +
  theme_publication() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank()
  ) +
  labs(title = "Sunburst-Style Radial Hierarchy")
print(p)

# ==============================================================================
# 7. SAVING EXAMPLES
# ==============================================================================

# Save any plot for publication
# save_publication("radar_chart.png", p, width = 8, height = 8, dpi = 300)
# save_publication("circular_barplot.pdf", p, width = 10, height = 10)

message("✓ All circular/radial chart examples completed successfully!")
message("  - Radar/spider charts: single and multiple individuals")
message("  - Circular bar plots: basic, styled, with labels")
message("  - Circular stacked bars: single and multi-level")
message("  - Wind rose diagrams with speed bins")
message("  - Polar area and coxcomb charts")
message("  - Circular heatmaps and sunburst-style plots")
