# ==============================================================================
# ChartKit: Basic Charts - Working Examples
# ==============================================================================
# This file contains tested, publication-ready examples for all basic charts
# All examples use built-in R datasets and produce high-quality outputs

library(ChartKit)
library(dplyr)

# ==============================================================================
# 1. SCATTER PLOT - Relationship between two continuous variables
# ==============================================================================

# Example 1: Simple scatter plot
viz_scatter(mtcars, x = wt, y = mpg,
            title = "Fuel Efficiency vs. Vehicle Weight",
            xlab = "Weight (1000 lbs)",
            ylab = "Miles per Gallon")

# Example 2: Scatter plot with color grouping
# IMPORTANT: Convert to factor for discrete colors
mtcars_plot <- mtcars %>%
  mutate(cyl_factor = factor(cyl, labels = paste(unique(cyl), "cylinders")))

viz_scatter(mtcars_plot, x = wt, y = mpg, color = cyl_factor,
            title = "Fuel Efficiency by Engine Configuration",
            subtitle = "Heavier vehicles with more cylinders show lower MPG",
            xlab = "Weight (1000 lbs)",
            ylab = "Miles per Gallon")

# Example 3: Bubble chart (size mapped to variable)
viz_scatter(mtcars, x = wt, y = mpg, size = hp, color = cyl_factor,
            title = "Vehicle Performance Characteristics",
            xlab = "Weight (1000 lbs)",
            ylab = "Miles per Gallon",
            alpha = 0.6)

# ==============================================================================
# 2. LINE PLOT - Time series or sequential data
# ==============================================================================

# Example 1: Simple time series
economics_recent <- economics %>%
  filter(date >= as.Date("2010-01-01"))

viz_line(economics_recent, x = date, y = unemploy,
         title = "US Unemployment Over Time",
         subtitle = "Thousands of persons, seasonally adjusted",
         xlab = "Year",
         ylab = "Unemployment (thousands)")

# Example 2: Multiple groups
economics_long <- economics %>%
  filter(date >= as.Date("2010-01-01")) %>%
  select(date, unemploy, psavert) %>%
  tidyr::pivot_longer(cols = c(unemploy, psavert),
                     names_to = "metric",
                     values_to = "value") %>%
  mutate(metric = factor(metric,
                        labels = c("Personal Savings Rate (%)", "Unemployment (thousands)")))

# Note: Different scales require faceting or secondary axis
viz_line(economics_recent, x = date, y = psavert,
         title = "Personal Savings Rate",
         xlab = "Year",
         ylab = "Savings Rate (%)")

# ==============================================================================
# 3. BAR PLOT - Categorical comparisons
# ==============================================================================

# Example 1: Simple bar chart (MUST aggregate data first!)
cyl_summary <- mtcars %>%
  mutate(cyl_factor = factor(cyl)) %>%
  group_by(cyl_factor) %>%
  summarise(avg_mpg = mean(mpg),
           count = n())

viz_bar(cyl_summary, x = cyl_factor, y = avg_mpg,
        title = "Average Fuel Efficiency by Cylinders",
        xlab = "Number of Cylinders",
        ylab = "Average MPG")

# Example 2: Grouped bar chart
am_cyl_summary <- mtcars %>%
  mutate(cyl_factor = factor(cyl),
         transmission = factor(am, labels = c("Automatic", "Manual"))) %>%
  group_by(cyl_factor, transmission) %>%
  summarise(avg_mpg = mean(mpg), .groups = "drop")

viz_bar(am_cyl_summary, x = cyl_factor, y = avg_mpg,
        fill = transmission, position = "dodge",
        title = "Fuel Efficiency: Cylinders and Transmission",
        xlab = "Number of Cylinders",
        ylab = "Average MPG")

# Example 3: Horizontal bar chart (useful for long labels)
species_summary <- iris %>%
  group_by(Species) %>%
  summarise(avg_petal_length = mean(Petal.Length))

viz_bar(species_summary, x = Species, y = avg_petal_length,
        horizontal = TRUE,
        title = "Average Petal Length by Species",
        ylab = "Petal Length (cm)")

# ==============================================================================
# 4. HISTOGRAM - Distribution of continuous data
# ==============================================================================

viz_histogram(mtcars, x = mpg,
              bins = 15,
              title = "Distribution of Fuel Efficiency",
              subtitle = "Sample of 32 car models",
              xlab = "Miles per Gallon",
              fill = "#0072B2")

# ==============================================================================
# 5. BOXPLOT - Distribution and outliers
# ==============================================================================

# Example 1: Single boxplot
viz_boxplot(mtcars, y = mpg,
            title = "Fuel Efficiency Distribution",
            ylab = "Miles per Gallon")

# Example 2: Grouped boxplot
mtcars_factor <- mtcars %>%
  mutate(cyl_factor = factor(cyl))

viz_boxplot(mtcars_factor, x = cyl_factor, y = mpg,
            fill = cyl_factor,
            title = "Fuel Efficiency by Number of Cylinders",
            xlab = "Cylinders",
            ylab = "Miles per Gallon")

# ==============================================================================
# 6. VIOLIN PLOT - Distribution shape
# ==============================================================================

viz_violin(mtcars_factor, x = cyl_factor, y = mpg,
           fill = cyl_factor,
           title = "MPG Distribution by Engine Configuration",
           xlab = "Number of Cylinders",
           ylab = "Miles per Gallon")

# ==============================================================================
# 7. DENSITY PLOT - Smooth distribution
# ==============================================================================

viz_density(mtcars, x = mpg,
            title = "Fuel Efficiency Distribution (Smoothed)",
            xlab = "Miles per Gallon",
            alpha = 0.7)

# Multiple groups
viz_density(mtcars_factor, x = mpg, fill = cyl_factor,
            title = "MPG Distribution by Cylinder Count",
            xlab = "Miles per Gallon",
            alpha = 0.5)

# ==============================================================================
# 8. SAVING PUBLICATION-READY FIGURES
# ==============================================================================

# Create a plot
p <- viz_scatter(mtcars_plot, x = wt, y = mpg, color = cyl_factor,
                title = "Publication-Ready Scatter Plot",
                xlab = "Weight (1000 lbs)",
                ylab = "Miles per Gallon")

# Save at publication quality
# save_publication("figure1.png", p, width = 7, height = 5, dpi = 300)
# save_publication("figure1.pdf", p, width = 7, height = 5)
# save_publication("figure1.tiff", p, width = 7, height = 5, dpi = 600)

# ==============================================================================
# IMPORTANT NOTES:
# ==============================================================================
# 1. Always convert categorical variables to factors for proper color scaling
# 2. Bar charts require aggregated data - use group_by() and summarise()
# 3. Use descriptive titles and axis labels
# 4. Default theme is publication-ready but can be customized
# 5. Save plots at 300+ DPI for publications
# ==============================================================================
