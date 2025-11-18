# ==============================================================================
# ChartKit: Categorical and Bar Chart Examples
# ==============================================================================
# Error bars, grouped/stacked bars, custom colors, observation counts,
# and advanced categorical visualizations
# ==============================================================================

library(ChartKit)
library(dplyr)
library(ggplot2)

# ==============================================================================
# 1. BAR PLOTS WITH ERROR BARS
# ==============================================================================

# -----------------------------------------------------------------------------
# 1.1 Bar Plot with Standard Error
# -----------------------------------------------------------------------------
# Calculate summary statistics
iris_summary <- iris %>%
  group_by(Species) %>%
  summarise(
    mean_length = mean(Sepal.Length),
    se = sd(Sepal.Length) / sqrt(n()),
    sd = sd(Sepal.Length),
    n = n()
  )

p <- ggplot(iris_summary, aes(x = Species, y = mean_length, fill = Species)) +
  geom_bar(stat = "identity", alpha = 0.7, width = 0.7) +
  geom_errorbar(aes(ymin = mean_length - se, ymax = mean_length + se),
                width = 0.2, linewidth = 0.8, color = "gray20") +
  scale_fill_publication() +
  theme_publication() +
  labs(
    title = "Sepal Length by Species (Mean ± SE)",
    subtitle = "Error bars show standard error",
    x = "Species",
    y = "Mean Sepal Length (cm)"
  ) +
  theme(legend.position = "none")
print(p)

# -----------------------------------------------------------------------------
# 1.2 Bar Plot with Standard Deviation
# -----------------------------------------------------------------------------
p <- ggplot(iris_summary, aes(x = Species, y = mean_length, fill = Species)) +
  geom_bar(stat = "identity", alpha = 0.7, width = 0.7) +
  geom_errorbar(aes(ymin = mean_length - sd, ymax = mean_length + sd),
                width = 0.25, linewidth = 1, color = "#D55E00") +
  scale_fill_publication() +
  theme_publication() +
  labs(
    title = "Sepal Length by Species (Mean ± SD)",
    subtitle = "Orange error bars show standard deviation",
    x = "Species",
    y = "Mean Sepal Length (cm)"
  ) +
  theme(legend.position = "none")
print(p)

# -----------------------------------------------------------------------------
# 1.3 Bar Plot with 95% Confidence Interval
# -----------------------------------------------------------------------------
iris_summary_ci <- iris_summary %>%
  mutate(
    ci_lower = mean_length - 1.96 * se,
    ci_upper = mean_length + 1.96 * se
  )

p <- ggplot(iris_summary_ci, aes(x = Species, y = mean_length, fill = Species)) +
  geom_bar(stat = "identity", alpha = 0.7, width = 0.6) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper),
                width = 0.2, linewidth = 1.2, color = "gray10") +
  geom_text(aes(label = sprintf("%.2f", mean_length)),
            vjust = -2.5, size = 3.5, fontface = "bold") +
  scale_fill_publication() +
  theme_publication() +
  labs(
    title = "Mean Sepal Length with 95% Confidence Intervals",
    subtitle = "Error bars = 95% CI, numbers show mean values",
    x = "Species",
    y = "Mean Sepal Length (cm)"
  ) +
  theme(legend.position = "none") +
  ylim(0, max(iris_summary_ci$ci_upper) * 1.15)
print(p)

# ==============================================================================
# 2. CUSTOM COLORS AND STYLING
# ==============================================================================

# -----------------------------------------------------------------------------
# 2.1 Bar Plot with Manual Colors
# -----------------------------------------------------------------------------
custom_colors <- c("#E69F00", "#56B4E9", "#009E73")

p <- ggplot(iris_summary, aes(x = Species, y = mean_length, fill = Species)) +
  geom_bar(stat = "identity", width = 0.7, alpha = 0.9) +
  scale_fill_manual(values = custom_colors) +
  theme_publication() +
  labs(
    title = "Custom Color Palette",
    subtitle = "Manually specified colors for each category",
    x = "Species",
    y = "Mean Sepal Length (cm)"
  ) +
  theme(legend.position = "none")
print(p)

# -----------------------------------------------------------------------------
# 2.2 Gradient Fill Bar Plot
# -----------------------------------------------------------------------------
p <- ggplot(iris_summary, aes(x = reorder(Species, mean_length),
                               y = mean_length, fill = mean_length)) +
  geom_bar(stat = "identity", width = 0.7) +
  scale_fill_viridis_c(option = "plasma", name = "Mean\nLength") +
  theme_publication() +
  labs(
    title = "Bar Plot with Gradient Fill",
    subtitle = "Color intensity represents value magnitude",
    x = "Species",
    y = "Mean Sepal Length (cm)"
  )
print(p)

# -----------------------------------------------------------------------------
# 2.3 Bar Plot with Border and Fill
# -----------------------------------------------------------------------------
p <- ggplot(iris_summary, aes(x = Species, y = mean_length, fill = Species)) +
  geom_bar(stat = "identity", color = "black", linewidth = 1.2,
           width = 0.65, alpha = 0.7) +
  scale_fill_publication() +
  theme_publication() +
  labs(
    title = "Bar Plot with Black Borders",
    subtitle = "Bold borders enhance visual separation",
    x = "Species",
    y = "Mean Sepal Length (cm)"
  ) +
  theme(legend.position = "none")
print(p)

# ==============================================================================
# 3. BAR PLOTS WITH OBSERVATION COUNTS
# ==============================================================================

# -----------------------------------------------------------------------------
# 3.1 Bar Plot with Count Labels
# -----------------------------------------------------------------------------
p <- ggplot(iris_summary, aes(x = Species, y = mean_length, fill = Species)) +
  geom_bar(stat = "identity", alpha = 0.7, width = 0.7) +
  geom_text(aes(label = paste0("n=", n)),
            vjust = -0.5, size = 4, fontface = "bold") +
  scale_fill_publication() +
  theme_publication() +
  labs(
    title = "Bar Plot with Sample Sizes",
    subtitle = "Labels show number of observations per group",
    x = "Species",
    y = "Mean Sepal Length (cm)"
  ) +
  theme(legend.position = "none") +
  ylim(0, max(iris_summary$mean_length) * 1.15)
print(p)

# -----------------------------------------------------------------------------
# 3.2 Count Bar Plot (Frequency)
# -----------------------------------------------------------------------------
# Count observations by category
mtcars_counts <- mtcars %>%
  mutate(cyl_label = factor(cyl, labels = paste(unique(sort(cyl)), "cyl"))) %>%
  count(cyl_label)

p <- ggplot(mtcars_counts, aes(x = cyl_label, y = n, fill = cyl_label)) +
  geom_bar(stat = "identity", alpha = 0.8, width = 0.7) +
  geom_text(aes(label = n), vjust = -0.5, size = 5, fontface = "bold") +
  scale_fill_publication() +
  theme_publication() +
  labs(
    title = "Frequency Bar Plot with Counts",
    subtitle = "Height and labels show observation frequency",
    x = "Engine Configuration",
    y = "Number of Cars"
  ) +
  theme(legend.position = "none") +
  ylim(0, max(mtcars_counts$n) * 1.15)
print(p)

# -----------------------------------------------------------------------------
# 3.3 Percentage Bar Plot
# -----------------------------------------------------------------------------
mtcars_pct <- mtcars_counts %>%
  mutate(
    pct = n / sum(n) * 100,
    label = sprintf("%.1f%%\n(n=%d)", pct, n)
  )

p <- ggplot(mtcars_pct, aes(x = cyl_label, y = pct, fill = cyl_label)) +
  geom_bar(stat = "identity", alpha = 0.8, width = 0.7) +
  geom_text(aes(label = label), vjust = -0.3, size = 3.5,
            fontface = "bold", lineheight = 0.9) +
  scale_fill_publication() +
  theme_publication() +
  labs(
    title = "Percentage Distribution with Counts",
    subtitle = "Labels show percentage and sample size",
    x = "Engine Configuration",
    y = "Percentage of Cars (%)"
  ) +
  theme(legend.position = "none") +
  ylim(0, max(mtcars_pct$pct) * 1.2)
print(p)

# ==============================================================================
# 4. GROUPED BAR PLOTS
# ==============================================================================

# -----------------------------------------------------------------------------
# 4.1 Basic Grouped Bar Plot
# -----------------------------------------------------------------------------
# Prepare grouped data
iris_grouped <- iris %>%
  mutate(Size = ifelse(Sepal.Length > median(Sepal.Length),
                       "Large", "Small")) %>%
  group_by(Species, Size) %>%
  summarise(mean_width = mean(Sepal.Width), .groups = "drop")

p <- ggplot(iris_grouped, aes(x = Species, y = mean_width, fill = Size)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7, alpha = 0.8) +
  scale_fill_publication() +
  theme_publication() +
  labs(
    title = "Grouped Bar Plot: Sepal Width by Species and Size",
    subtitle = "Bars grouped by size category",
    x = "Species",
    y = "Mean Sepal Width (cm)"
  )
print(p)

# -----------------------------------------------------------------------------
# 4.2 Grouped Bar Plot with mtcars
# -----------------------------------------------------------------------------
mtcars_grouped <- mtcars %>%
  mutate(
    cyl_label = factor(cyl),
    transmission = factor(am, labels = c("Automatic", "Manual"))
  ) %>%
  group_by(cyl_label, transmission) %>%
  summarise(
    mean_mpg = mean(mpg),
    se = sd(mpg) / sqrt(n()),
    .groups = "drop"
  )

p <- ggplot(mtcars_grouped, aes(x = cyl_label, y = mean_mpg, fill = transmission)) +
  geom_bar(stat = "identity", position = position_dodge(0.8),
           width = 0.7, alpha = 0.8) +
  geom_errorbar(aes(ymin = mean_mpg - se, ymax = mean_mpg + se),
                position = position_dodge(0.8), width = 0.25,
                linewidth = 0.8) +
  scale_fill_publication() +
  theme_publication() +
  labs(
    title = "Fuel Efficiency: Cylinders and Transmission",
    subtitle = "Grouped bars with error bars (SE)",
    x = "Number of Cylinders",
    y = "Mean MPG",
    fill = "Transmission"
  )
print(p)

# ==============================================================================
# 5. STACKED BAR PLOTS
# ==============================================================================

# -----------------------------------------------------------------------------
# 5.1 Basic Stacked Bar Plot
# -----------------------------------------------------------------------------
p <- ggplot(iris_grouped, aes(x = Species, y = mean_width, fill = Size)) +
  geom_bar(stat = "identity", position = "stack", alpha = 0.8) +
  scale_fill_publication() +
  theme_publication() +
  labs(
    title = "Stacked Bar Plot",
    subtitle = "Total height shows combined value",
    x = "Species",
    y = "Total Mean Sepal Width (cm)"
  )
print(p)

# -----------------------------------------------------------------------------
# 5.2 100% Stacked Bar Plot (Proportions)
# -----------------------------------------------------------------------------
mtcars_stacked <- mtcars %>%
  mutate(
    cyl_label = factor(cyl),
    transmission = factor(am, labels = c("Automatic", "Manual"))
  ) %>%
  count(cyl_label, transmission) %>%
  group_by(cyl_label) %>%
  mutate(
    pct = n / sum(n) * 100,
    label = sprintf("%.0f%%", pct)
  ) %>%
  ungroup()

p <- ggplot(mtcars_stacked, aes(x = cyl_label, y = pct, fill = transmission)) +
  geom_bar(stat = "identity", position = "stack", alpha = 0.8) +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5),
            size = 4, fontface = "bold", color = "white") +
  scale_fill_publication() +
  theme_publication() +
  labs(
    title = "100% Stacked Bar Plot",
    subtitle = "Shows proportion of each transmission type",
    x = "Number of Cylinders",
    y = "Percentage (%)",
    fill = "Transmission"
  )
print(p)

# -----------------------------------------------------------------------------
# 5.3 Stacked Bar with Counts
# -----------------------------------------------------------------------------
p <- ggplot(mtcars_stacked, aes(x = cyl_label, y = n, fill = transmission)) +
  geom_bar(stat = "identity", position = "stack", alpha = 0.8) +
  geom_text(aes(label = n), position = position_stack(vjust = 0.5),
            size = 4.5, fontface = "bold", color = "white") +
  scale_fill_publication() +
  theme_publication() +
  labs(
    title = "Stacked Bar Plot with Counts",
    subtitle = "Numbers show count in each segment",
    x = "Number of Cylinders",
    y = "Number of Cars",
    fill = "Transmission"
  )
print(p)

# ==============================================================================
# 6. HORIZONTAL BAR PLOTS
# ==============================================================================

# -----------------------------------------------------------------------------
# 6.1 Horizontal Bar Plot with Long Labels
# -----------------------------------------------------------------------------
# Create data with longer category names
category_data <- data.frame(
  category = c("Very Long Category Name A",
               "Another Extended Category B",
               "Short C",
               "Medium Length D"),
  value = c(45, 62, 38, 55),
  group = c("Group1", "Group2", "Group1", "Group2")
)

p <- ggplot(category_data, aes(x = value, y = reorder(category, value),
                                fill = group)) +
  geom_bar(stat = "identity", alpha = 0.8) +
  geom_text(aes(label = value), hjust = -0.2, size = 4, fontface = "bold") +
  scale_fill_publication() +
  theme_publication() +
  labs(
    title = "Horizontal Bar Plot",
    subtitle = "Better for long category names",
    x = "Value",
    y = "Category"
  ) +
  xlim(0, max(category_data$value) * 1.15)
print(p)

# -----------------------------------------------------------------------------
# 6.2 Ordered Horizontal Bar Plot
# -----------------------------------------------------------------------------
iris_ordered <- iris_summary %>%
  arrange(desc(mean_length))

p <- ggplot(iris_ordered, aes(x = mean_length,
                               y = reorder(Species, mean_length),
                               fill = Species)) +
  geom_bar(stat = "identity", alpha = 0.8) +
  geom_errorbarh(aes(xmin = mean_length - se, xmax = mean_length + se),
                 height = 0.2, linewidth = 1) +
  scale_fill_publication() +
  theme_publication() +
  labs(
    title = "Ranked Horizontal Bar Plot",
    subtitle = "Ordered by mean value with error bars",
    x = "Mean Sepal Length (cm)",
    y = "Species"
  ) +
  theme(legend.position = "none")
print(p)

# ==============================================================================
# 7. ADVANCED BAR PLOT TECHNIQUES
# ==============================================================================

# -----------------------------------------------------------------------------
# 7.1 Bar Plot with Significance Indicators
# -----------------------------------------------------------------------------
# Add significance markers
iris_summary_sig <- iris_summary %>%
  mutate(
    significance = c("***", "**", "*")  # Example significance levels
  )

p <- ggplot(iris_summary_sig, aes(x = Species, y = mean_length, fill = Species)) +
  geom_bar(stat = "identity", alpha = 0.7, width = 0.6) +
  geom_errorbar(aes(ymin = mean_length - se, ymax = mean_length + se),
                width = 0.2, linewidth = 0.8) +
  geom_text(aes(label = significance, y = mean_length + se),
            vjust = -0.5, size = 6, fontface = "bold") +
  scale_fill_publication() +
  theme_publication() +
  labs(
    title = "Bar Plot with Significance Levels",
    subtitle = "* p<0.05, ** p<0.01, *** p<0.001",
    x = "Species",
    y = "Mean Sepal Length (cm)"
  ) +
  theme(legend.position = "none") +
  ylim(0, max(iris_summary_sig$mean_length + iris_summary_sig$se) * 1.2)
print(p)

# -----------------------------------------------------------------------------
# 7.2 Bar Plot with Value and Percentage Labels
# -----------------------------------------------------------------------------
mtcars_dual <- mtcars_counts %>%
  mutate(
    pct = n / sum(n) * 100,
    label = sprintf("%d\n(%.1f%%)", n, pct)
  )

p <- ggplot(mtcars_dual, aes(x = cyl_label, y = n, fill = cyl_label)) +
  geom_bar(stat = "identity", alpha = 0.8, width = 0.7) +
  geom_text(aes(label = label), vjust = -0.3, size = 4,
            fontface = "bold", lineheight = 0.9) +
  scale_fill_publication() +
  theme_publication() +
  labs(
    title = "Bar Plot with Dual Labels",
    subtitle = "Shows both count and percentage",
    x = "Engine Configuration",
    y = "Number of Cars"
  ) +
  theme(legend.position = "none") +
  ylim(0, max(mtcars_dual$n) * 1.25)
print(p)

# -----------------------------------------------------------------------------
# 7.3 Dodged Bar Plot with Pattern Fills (for B&W printing)
# -----------------------------------------------------------------------------
if (requireNamespace("ggpattern", quietly = TRUE)) {
  p <- ggplot(mtcars_grouped, aes(x = cyl_label, y = mean_mpg, fill = transmission)) +
    ggpattern::geom_bar_pattern(
      aes(pattern = transmission),
      stat = "identity",
      position = position_dodge(0.8),
      width = 0.7,
      alpha = 0.7,
      pattern_density = 0.1,
      pattern_spacing = 0.025
    ) +
    scale_fill_publication() +
    theme_publication() +
    labs(
      title = "Patterned Bar Plot (B&W Print-Friendly)",
      subtitle = "Patterns distinguish groups without color",
      x = "Number of Cylinders",
      y = "Mean MPG"
    )
  print(p)
}

# ==============================================================================
# 8. MULTI-PANEL BAR PLOTS
# ==============================================================================

# -----------------------------------------------------------------------------
# 8.1 Faceted Bar Plots
# -----------------------------------------------------------------------------
# Create multi-variable summary
iris_multi <- iris %>%
  tidyr::pivot_longer(cols = c(Sepal.Length, Sepal.Width,
                                Petal.Length, Petal.Width),
                      names_to = "variable",
                      values_to = "value") %>%
  group_by(Species, variable) %>%
  summarise(mean_value = mean(value), .groups = "drop")

p <- ggplot(iris_multi, aes(x = Species, y = mean_value, fill = Species)) +
  geom_bar(stat = "identity", alpha = 0.8) +
  facet_wrap(~ variable, scales = "free_y", ncol = 2) +
  scale_fill_publication() +
  theme_publication() +
  labs(
    title = "Multi-Panel Bar Plot Comparison",
    subtitle = "All iris measurements across species",
    x = "Species",
    y = "Mean Value (cm)"
  ) +
  theme(legend.position = "none")
print(p)

# ==============================================================================
# 9. SAVING EXAMPLES
# ==============================================================================

# Save any plot for publication
# save_publication("barplot_error.png", p, width = 8, height = 6, dpi = 300)
# save_publication("grouped_barplot.pdf", p, width = 10, height = 7)

message("✓ All categorical/bar plot examples completed successfully!")
message("  - Error bars: SE, SD, 95% CI")
message("  - Custom colors and gradient fills")
message("  - Observation counts and percentages")
message("  - Grouped and stacked bar plots")
message("  - Horizontal and ordered layouts")
message("  - Significance indicators and dual labels")
message("  - Multi-panel faceted comparisons")
