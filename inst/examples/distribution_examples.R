# ==============================================================================
# ChartKit: Distribution Visualization Examples
# ==============================================================================
# High-quality examples for violin, density, histogram, boxplot, ridgeline,
# and beeswarm plots following r-graph-gallery.com standards
# ==============================================================================

library(ChartKit)
library(dplyr)
library(ggplot2)

# ==============================================================================
# 1. VIOLIN PLOTS - Distribution Shape Visualization
# ==============================================================================

# -----------------------------------------------------------------------------
# 1.1 Basic Violin Plot
# -----------------------------------------------------------------------------
data(iris)

# Simple violin plot - shows distribution shape
viz_violin(iris,
           x = Species,
           y = Sepal.Length,
           title = "Sepal Length Distribution by Species",
           xlab = "Species",
           ylab = "Sepal Length (cm)")

# -----------------------------------------------------------------------------
# 1.2 Violin Plot with Color
# -----------------------------------------------------------------------------
viz_violin(iris,
           x = Species,
           y = Sepal.Length,
           fill = Species,
           title = "Sepal Length Distribution with Color Coding",
           subtitle = "Each species shows distinct distribution patterns",
           xlab = "Species",
           ylab = "Sepal Length (cm)")

# -----------------------------------------------------------------------------
# 1.3 Violin Plot with Box Plot Overlay (Publication Quality)
# -----------------------------------------------------------------------------
# This combines distribution shape with quartile information
viz_violin_box(iris,
               x = Species,
               y = Sepal.Length,
               fill = Species,
               title = "Combined Violin and Box Plot",
               subtitle = "Shows both distribution shape and quartiles",
               xlab = "Species",
               ylab = "Sepal Length (cm)")

# -----------------------------------------------------------------------------
# 1.4 Violin Plot with Statistical Summary
# -----------------------------------------------------------------------------
# Enhanced violin plot with mean and median indicators
p <- ggplot(iris, aes(x = Species, y = Sepal.Length, fill = Species)) +
  geom_violin(alpha = 0.6, trim = TRUE) +
  geom_boxplot(width = 0.15, alpha = 0.8, outlier.shape = NA) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3,
               fill = "white", color = "black") +
  scale_fill_publication() +
  theme_publication() +
  labs(
    title = "Sepal Length: Violin + Box + Mean",
    subtitle = "Diamond = mean, box = quartiles, shape = full distribution",
    x = "Species",
    y = "Sepal Length (cm)"
  ) +
  theme(legend.position = "none")
print(p)

# ==============================================================================
# 2. DENSITY PLOTS - Smooth Distribution Curves
# ==============================================================================

# -----------------------------------------------------------------------------
# 2.1 Basic Density Plot
# -----------------------------------------------------------------------------
viz_density(iris,
            x = Sepal.Length,
            title = "Overall Sepal Length Distribution",
            subtitle = "Smoothed density estimate",
            xlab = "Sepal Length (cm)",
            ylab = "Density")

# -----------------------------------------------------------------------------
# 2.2 Multiple Density Curves with Transparency
# -----------------------------------------------------------------------------
viz_density(iris,
            x = Sepal.Length,
            fill = Species,
            alpha = 0.5,
            title = "Sepal Length Distribution by Species",
            subtitle = "Overlapping density curves show species differences",
            xlab = "Sepal Length (cm)",
            ylab = "Density")

# -----------------------------------------------------------------------------
# 2.3 Stacked Density Plot (Area Style)
# -----------------------------------------------------------------------------
# Shows relative proportions
p <- ggplot(iris, aes(x = Sepal.Length, fill = Species)) +
  geom_density(alpha = 0.7, position = "stack") +
  scale_fill_publication() +
  theme_publication() +
  labs(
    title = "Stacked Density Plot",
    subtitle = "Total height shows combined distribution",
    x = "Sepal Length (cm)",
    y = "Stacked Density"
  )
print(p)

# -----------------------------------------------------------------------------
# 2.4 Density Plot with Rug (Individual Data Points)
# -----------------------------------------------------------------------------
p <- ggplot(iris, aes(x = Sepal.Length, fill = Species, color = Species)) +
  geom_density(alpha = 0.4, linewidth = 1) +
  geom_rug(alpha = 0.5, length = unit(0.05, "npc")) +
  scale_fill_publication() +
  scale_color_publication() +
  theme_publication() +
  labs(
    title = "Density Plot with Rug Marks",
    subtitle = "Bottom ticks show actual data points",
    x = "Sepal Length (cm)",
    y = "Density"
  )
print(p)

# ==============================================================================
# 3. HISTOGRAMS - Frequency Distributions
# ==============================================================================

# -----------------------------------------------------------------------------
# 3.1 Basic Histogram with Optimal Binning
# -----------------------------------------------------------------------------
viz_histogram(iris,
              x = Sepal.Length,
              bins = 30,
              title = "Sepal Length Frequency Distribution",
              subtitle = "30 bins for detailed view",
              xlab = "Sepal Length (cm)",
              ylab = "Count")

# -----------------------------------------------------------------------------
# 3.2 Histogram with Density Overlay
# -----------------------------------------------------------------------------
p <- ggplot(iris, aes(x = Sepal.Length)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30,
                 fill = "#0072B2", alpha = 0.6, color = "white") +
  geom_density(linewidth = 1.2, color = "#D55E00") +
  theme_publication() +
  labs(
    title = "Histogram with Density Curve Overlay",
    subtitle = "Orange line = smoothed density estimate",
    x = "Sepal Length (cm)",
    y = "Density"
  )
print(p)

# -----------------------------------------------------------------------------
# 3.3 Grouped Histogram (Faceted by Species)
# -----------------------------------------------------------------------------
p <- ggplot(iris, aes(x = Sepal.Length, fill = Species)) +
  geom_histogram(bins = 20, alpha = 0.8, color = "white") +
  facet_wrap(~ Species, ncol = 1) +
  scale_fill_publication() +
  theme_publication() +
  labs(
    title = "Sepal Length Distribution by Species",
    subtitle = "Faceted histograms for clear comparison",
    x = "Sepal Length (cm)",
    y = "Count"
  ) +
  theme(legend.position = "none")
print(p)

# -----------------------------------------------------------------------------
# 3.4 Stacked Histogram
# -----------------------------------------------------------------------------
p <- ggplot(iris, aes(x = Sepal.Length, fill = Species)) +
  geom_histogram(bins = 25, alpha = 0.8, color = "white", position = "stack") +
  scale_fill_publication() +
  theme_publication() +
  labs(
    title = "Stacked Histogram by Species",
    subtitle = "Total height shows overall distribution",
    x = "Sepal Length (cm)",
    y = "Count"
  )
print(p)

# ==============================================================================
# 4. BOX PLOTS - Quartile Visualization
# ==============================================================================

# -----------------------------------------------------------------------------
# 4.1 Basic Box Plot
# -----------------------------------------------------------------------------
viz_boxplot(iris,
            x = Species,
            y = Sepal.Length,
            title = "Sepal Length by Species",
            subtitle = "Shows median, quartiles, and outliers",
            xlab = "Species",
            ylab = "Sepal Length (cm)")

# -----------------------------------------------------------------------------
# 4.2 Box Plot with Color and Notches
# -----------------------------------------------------------------------------
# Notches show confidence interval around median
p <- ggplot(iris, aes(x = Species, y = Sepal.Length, fill = Species)) +
  geom_boxplot(notch = TRUE, alpha = 0.7, outlier.color = "red",
               outlier.size = 2) +
  scale_fill_publication() +
  theme_publication() +
  labs(
    title = "Box Plot with Notches",
    subtitle = "Notches = 95% CI around median, red points = outliers",
    x = "Species",
    y = "Sepal Length (cm)"
  ) +
  theme(legend.position = "none")
print(p)

# -----------------------------------------------------------------------------
# 4.3 Box Plot with Jittered Points Overlay
# -----------------------------------------------------------------------------
p <- ggplot(iris, aes(x = Species, y = Sepal.Length, fill = Species)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.3, size = 2) +
  scale_fill_publication() +
  theme_publication() +
  labs(
    title = "Box Plot with Individual Data Points",
    subtitle = "Jittered points show actual observations",
    x = "Species",
    y = "Sepal Length (cm)"
  ) +
  theme(legend.position = "none")
print(p)

# -----------------------------------------------------------------------------
# 4.4 Horizontal Box Plot
# -----------------------------------------------------------------------------
p <- ggplot(iris, aes(x = Sepal.Length, y = Species, fill = Species)) +
  geom_boxplot(alpha = 0.7) +
  scale_fill_publication() +
  theme_publication() +
  labs(
    title = "Horizontal Box Plot",
    subtitle = "Useful for long category names",
    x = "Sepal Length (cm)",
    y = "Species"
  ) +
  theme(legend.position = "none")
print(p)

# ==============================================================================
# 5. RIDGELINE PLOTS - Distribution Comparison
# ==============================================================================

# -----------------------------------------------------------------------------
# 5.1 Basic Ridgeline Plot
# -----------------------------------------------------------------------------
viz_ridgeline(iris,
              x = Sepal.Length,
              y = Species,
              title = "Sepal Length Distribution Comparison",
              subtitle = "Ridgeline plot shows overlapping distributions",
              xlab = "Sepal Length (cm)",
              ylab = "Species")

# -----------------------------------------------------------------------------
# 5.2 Ridgeline Plot with Fill Gradient
# -----------------------------------------------------------------------------
viz_ridgeline(iris,
              x = Sepal.Length,
              y = Species,
              fill = Species,
              title = "Colored Ridgeline Plot",
              subtitle = "Each species highlighted with distinct color",
              xlab = "Sepal Length (cm)",
              ylab = "Species")

# -----------------------------------------------------------------------------
# 5.3 Ridgeline Plot with Quantile Lines
# -----------------------------------------------------------------------------
library(ggridges)
p <- ggplot(iris, aes(x = Sepal.Length, y = Species, fill = Species)) +
  geom_density_ridges(
    alpha = 0.6,
    quantile_lines = TRUE,
    quantiles = 2  # Median line
  ) +
  scale_fill_publication() +
  theme_publication() +
  labs(
    title = "Ridgeline Plot with Median Lines",
    subtitle = "Vertical line in each distribution shows median",
    x = "Sepal Length (cm)",
    y = "Species"
  ) +
  theme(legend.position = "none")
print(p)

# -----------------------------------------------------------------------------
# 5.4 Using mtcars for Multiple Categories
# -----------------------------------------------------------------------------
# Prepare mtcars data
mtcars_labeled <- mtcars %>%
  mutate(cyl_label = factor(cyl, labels = paste(unique(sort(cyl)), "cylinders")))

viz_ridgeline(mtcars_labeled,
              x = mpg,
              y = cyl_label,
              fill = cyl_label,
              title = "Fuel Efficiency Distribution by Engine Size",
              subtitle = "Ridgeline plot reveals distinct MPG patterns",
              xlab = "Miles per Gallon",
              ylab = "Engine Configuration")

# ==============================================================================
# 6. BEESWARM PLOTS - Individual Point Distribution
# ==============================================================================
# Note: Beeswarm requires ggbeeswarm package
# If not available, we'll use geom_jitter as alternative

# -----------------------------------------------------------------------------
# 6.1 Jitter Plot (Alternative to Beeswarm)
# -----------------------------------------------------------------------------
viz_jitter(iris,
           x = Species,
           y = Sepal.Length,
           color = Species,
           title = "Individual Data Points with Jitter",
           subtitle = "Shows every observation without overlap",
           xlab = "Species",
           ylab = "Sepal Length (cm)")

# -----------------------------------------------------------------------------
# 6.2 Beeswarm-style with ggbeeswarm (if available)
# -----------------------------------------------------------------------------
if (requireNamespace("ggbeeswarm", quietly = TRUE)) {
  p <- ggplot(iris, aes(x = Species, y = Sepal.Length, color = Species)) +
    ggbeeswarm::geom_beeswarm(alpha = 0.6, size = 2.5, cex = 3) +
    scale_color_publication() +
    theme_publication() +
    labs(
      title = "Beeswarm Plot",
      subtitle = "Optimized point spacing shows distribution shape",
      x = "Species",
      y = "Sepal Length (cm)"
    ) +
    theme(legend.position = "none")
  print(p)

  # Beeswarm with box plot overlay
  p <- ggplot(iris, aes(x = Species, y = Sepal.Length, color = Species)) +
    ggbeeswarm::geom_beeswarm(alpha = 0.5, size = 2, cex = 3) +
    geom_boxplot(aes(fill = Species), alpha = 0.3, outlier.shape = NA,
                 width = 0.3, color = "black") +
    scale_color_publication() +
    scale_fill_publication() +
    theme_publication() +
    labs(
      title = "Beeswarm + Box Plot Combination",
      subtitle = "Best of both: all points + statistical summary",
      x = "Species",
      y = "Sepal Length (cm)"
    ) +
    theme(legend.position = "none")
  print(p)
} else {
  message("Install ggbeeswarm for true beeswarm plots: install.packages('ggbeeswarm')")
}

# -----------------------------------------------------------------------------
# 6.3 Sina Plot (Violin-shaped Jitter)
# -----------------------------------------------------------------------------
# Uses ggforce::geom_sina
if (requireNamespace("ggforce", quietly = TRUE)) {
  p <- ggplot(iris, aes(x = Species, y = Sepal.Length, color = Species)) +
    ggforce::geom_sina(alpha = 0.6, size = 2.5) +
    scale_color_publication() +
    theme_publication() +
    labs(
      title = "Sina Plot (Violin-shaped Point Cloud)",
      subtitle = "Points arranged to show distribution density",
      x = "Species",
      y = "Sepal Length (cm)"
    ) +
    theme(legend.position = "none")
  print(p)

  # Sina with violin outline
  p <- ggplot(iris, aes(x = Species, y = Sepal.Length, color = Species, fill = Species)) +
    geom_violin(alpha = 0.2, color = NA) +
    ggforce::geom_sina(alpha = 0.6, size = 2) +
    scale_color_publication() +
    scale_fill_publication() +
    theme_publication() +
    labs(
      title = "Sina Plot with Violin Outline",
      subtitle = "Combines distribution shape with individual points",
      x = "Species",
      y = "Sepal Length (cm)"
    ) +
    theme(legend.position = "none")
  print(p)
}

# ==============================================================================
# 7. ADVANCED COMBINATIONS - Publication-Ready Figures
# ==============================================================================

# -----------------------------------------------------------------------------
# 7.1 Raincloud Plot - Ultimate Distribution Visualization
# -----------------------------------------------------------------------------
# Combines violin + boxplot + individual points
viz_raincloud(iris,
              x = Species,
              y = Sepal.Length,
              fill = Species,
              title = "Raincloud Plot: Complete Distribution View",
              subtitle = "Half-violin + box + jittered points",
              xlab = "Species",
              ylab = "Sepal Length (cm)")

# -----------------------------------------------------------------------------
# 7.2 Multi-Panel Comparison
# -----------------------------------------------------------------------------
library(patchwork)

# Create individual plots
p1 <- viz_boxplot(iris, x = Species, y = Sepal.Length, fill = Species,
                  title = "Box Plot") + theme(legend.position = "none")

p2 <- viz_violin(iris, x = Species, y = Sepal.Length, fill = Species,
                 title = "Violin Plot") + theme(legend.position = "none")

p3 <- viz_ridgeline(iris, x = Sepal.Length, y = Species, fill = Species,
                    title = "Ridgeline Plot") + theme(legend.position = "none")

p4 <- viz_jitter(iris, x = Species, y = Sepal.Length, color = Species,
                 title = "Jitter Plot") + theme(legend.position = "none")

# Combine into publication panel
combined <- (p1 | p2) / (p3 | p4) +
  plot_annotation(
    title = "Four Ways to Visualize the Same Distribution",
    subtitle = "Each method reveals different aspects of the data",
    theme = theme_publication()
  )
print(combined)

# -----------------------------------------------------------------------------
# 7.3 Density + Histogram Combination
# -----------------------------------------------------------------------------
p <- ggplot(iris, aes(x = Sepal.Length)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30,
                 fill = "#0072B2", alpha = 0.4, color = "white") +
  geom_density(linewidth = 1.5, color = "#D55E00") +
  geom_rug(alpha = 0.3, color = "#009E73", length = unit(0.03, "npc")) +
  theme_publication() +
  labs(
    title = "Histogram + Density + Rug: Triple View",
    subtitle = "Bars = counts, orange = smooth density, green ticks = data points",
    x = "Sepal Length (cm)",
    y = "Density"
  )
print(p)

# ==============================================================================
# 8. SAVING EXAMPLES
# ==============================================================================

# Save any plot for publication
# save_publication("figure_violin.png", p1, width = 7, height = 5, dpi = 300)
# save_publication("figure_combined.pdf", combined, width = 10, height = 8)

message("✓ All distribution visualization examples completed successfully!")
message("  - Violin plots: Basic, colored, with overlays, with statistics")
message("  - Density plots: Basic, multiple groups, stacked, with rug")
message("  - Histograms: Basic, with density overlay, faceted, stacked")
message("  - Box plots: Basic, with notches, with jitter, horizontal")
message("  - Ridgeline plots: Basic, colored, with quantile lines")
message("  - Beeswarm/Sina: Jitter, true beeswarm, violin-shaped")
message("  - Advanced: Raincloud, multi-panel, combined visualizations")
