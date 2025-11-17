# ==============================================================================
# ChartKit: Intermediate Charts - Working Examples
# ==============================================================================
# Tested examples for intermediate visualization types
# All examples use real data and include proper preparation steps

library(ChartKit)
library(dplyr)
library(tidyr)

# ==============================================================================
# 1. HEATMAP - Matrix Visualization
# ==============================================================================

# Example 1: Simple heatmap from correlation-style data
# Create sample heatmap data
set.seed(123)
heat_data <- expand.grid(
  x = paste0("Var", 1:8),
  y = paste0("Sample", 1:6)
) %>%
  mutate(value = rnorm(n(), mean = 0, sd = 2))

viz_heatmap(heat_data, x = x, y = y, value = value,
            title = "Expression Heatmap",
            subtitle = "Gene expression across samples",
            show_values = FALSE)

# Example 2: Heatmap with values shown (for smaller datasets)
small_heat <- expand.grid(
  x = paste0("T", 1:4),
  y = paste0("G", 1:5)
) %>%
  mutate(value = round(runif(n(), -2, 2), 2))

viz_heatmap(small_heat, x = x, y = y, value = value,
            title = "Treatment Effects by Gene",
            show_values = TRUE)

# ==============================================================================
# 2. CORRELATION MATRIX - Built-in correlation calculation
# ==============================================================================

# Select only numeric columns from mtcars
mtcars_numeric <- mtcars %>%
  select(mpg, disp, hp, drat, wt, qsec)

viz_correlation(mtcars_numeric,
                method = "pearson",
                title = "Vehicle Characteristics Correlation Matrix",
                subtitle = "Pearson correlation coefficients",
                show_values = TRUE)

# Example with iris data
iris_numeric <- iris %>%
  select(-Species)

viz_correlation(iris_numeric,
                method = "spearman",
                title = "Iris Measurements Correlation",
                subtitle = "Spearman rank correlation",
                show_values = TRUE)

# ==============================================================================
# 3. BUBBLE CHART - Three-variable relationships
# ==============================================================================

mtcars_bubble <- mtcars %>%
  mutate(cyl_factor = factor(cyl))

viz_bubble(mtcars_bubble,
           x = wt,
           y = mpg,
           size = hp,
           color = cyl_factor,
           title = "Vehicle Performance Profile",
           subtitle = "Bubble size represents horsepower",
           xlab = "Weight (1000 lbs)",
           ylab = "Miles per Gallon",
           alpha = 0.6)

# ==============================================================================
# 4. LOLLIPOP CHART - Ranked categorical data
# ==============================================================================

# Top 10 most fuel-efficient cars
top_mpg <- mtcars %>%
  tibble::rownames_to_column("car") %>%
  arrange(desc(mpg)) %>%
  head(10) %>%
  select(car, mpg)

viz_lollipop(top_mpg,
             x = car,
             y = mpg,
             horizontal = TRUE,
             title = "Top 10 Most Fuel-Efficient Vehicles",
             ylab = "Miles per Gallon")

# ==============================================================================
# 5. DUMBBELL CHART - Before/After Comparison
# ==============================================================================

# Compare automatic vs manual transmission MPG
comparison_data <- mtcars %>%
  mutate(cyl_cat = factor(cyl)) %>%
  group_by(cyl_cat) %>%
  summarise(
    auto_mpg = mean(mpg[am == 0]),
    manual_mpg = mean(mpg[am == 1]),
    .groups = "drop"
  ) %>%
  mutate(category = paste(cyl_cat, "cylinders"))

viz_dumbbell(comparison_data,
             category = category,
             value1 = auto_mpg,
             value2 = manual_mpg,
             title = "MPG Comparison: Automatic vs Manual Transmission",
             subtitle = "Blue = Automatic, Orange = Manual")

# ==============================================================================
# 6. WATERFALL CHART - Cumulative Effects
# ==============================================================================

# Financial waterfall example
financial_data <- data.frame(
  category = c("Revenue", "COGS", "Marketing", "R&D", "Admin", "Net Income"),
  value = c(1000, -400, -150, -200, -100, 150)
)

viz_waterfall(financial_data,
              category = category,
              value = value,
              title = "Income Statement Waterfall",
              subtitle = "$ in thousands")

# ==============================================================================
# 7. RIDGELINE PLOT - Distribution Comparison
# ==============================================================================

# Create data for ridgeline plot
set.seed(123)
ridgeline_data <- data.frame(
  value = c(rnorm(100, mean = 10, sd = 2),
            rnorm(100, mean = 12, sd = 2.5),
            rnorm(100, mean = 14, sd = 3)),
  category = rep(c("Group A", "Group B", "Group C"), each = 100)
)

viz_ridgeline(ridgeline_data,
              x = value,
              y = category,
              title = "Distribution Comparison Across Groups",
              xlab = "Measurement Value")

# ==============================================================================
# 8. RADAR CHART - Multivariate Comparison
# ==============================================================================

# Compare vehicle profiles (normalize to 0-100 scale)
radar_data <- mtcars %>%
  filter(cyl %in% c(4, 6, 8)) %>%
  group_by(cyl) %>%
  summarise(
    MPG = mean(mpg),
    HP = mean(hp),
    Weight = mean(wt),
    .groups = "drop"
  ) %>%
  pivot_longer(cols = c(MPG, HP, Weight),
               names_to = "metric",
               values_to = "value") %>%
  group_by(metric) %>%
  mutate(value_scaled = (value - min(value)) / (max(value) - min(value)) * 100) %>%
  ungroup() %>%
  mutate(cyl_label = paste(cyl, "cyl"))

viz_radar(radar_data,
          variables = metric,
          values = value_scaled,
          group = cyl_label,
          title = "Vehicle Profile Comparison",
          subtitle = "Normalized metrics (0-100 scale)")

# ==============================================================================
# 9. VIOLIN-BOX COMBO - Distribution with Quartiles
# ==============================================================================

mtcars_factor <- mtcars %>%
  mutate(cyl_factor = factor(cyl))

viz_violin_box(mtcars_factor,
               x = cyl_factor,
               y = mpg,
               fill = cyl_factor,
               title = "MPG Distribution with Quartiles",
               subtitle = "Violin plot shows density, box shows quartiles",
               xlab = "Number of Cylinders",
               ylab = "Miles per Gallon")

# ==============================================================================
# 10. RAINCLOUD PLOT - Ultimate Distribution Visualization
# ==============================================================================

viz_raincloud(mtcars_factor,
              x = cyl_factor,
              y = mpg,
              fill = cyl_factor,
              title = "Complete MPG Distribution View",
              subtitle = "Combines violin, box plot, and raw data points",
              xlab = "Number of Cylinders",
              ylab = "Miles per Gallon")

# ==============================================================================
# 11. FUNNEL CHART - Conversion or Process Stages
# ==============================================================================

# Sales funnel example
funnel_data <- data.frame(
  stage = c("Visitors", "Sign-ups", "Active Users", "Paying Customers", "Renewals"),
  value = c(10000, 2500, 1200, 500, 350)
)

viz_funnel(funnel_data,
           stage = stage,
           value = value,
           title = "Customer Conversion Funnel",
           subtitle = "Drop-off at each stage")

# ==============================================================================
# IMPORTANT NOTES FOR INTERMEDIATE CHARTS:
# ==============================================================================
# 1. Heatmaps require data in long format (x, y, value)
# 2. Correlation matrix works with wide format (automatic conversion)
# 3. Bubble charts need size aesthetic - use continuous variable
# 4. Lollipop charts work best horizontally for long labels
# 5. Dumbbell charts compare exactly two values per category
# 6. Waterfall shows cumulative effects (positive and negative)
# 7. Ridgeline plots need many observations per group
# 8. Radar charts should use normalized data for fair comparison
# 9. Raincloud plots work best with 50+ observations per group
# 10. Always provide clear subtitles explaining the visualization
# ==============================================================================
