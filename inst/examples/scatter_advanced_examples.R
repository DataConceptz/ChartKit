# ==============================================================================
# ChartKit: Advanced Scatter Plot Examples
# ==============================================================================
# Interactive bubbles, connected scatter, 2D density, polynomial fitting,
# and correlation analysis following r-graph-gallery.com standards
# ==============================================================================

library(ChartKit)
library(dplyr)
library(ggplot2)

# ==============================================================================
# 1. INTERACTIVE BUBBLE CHARTS
# ==============================================================================

# -----------------------------------------------------------------------------
# 1.1 Basic Bubble Chart
# -----------------------------------------------------------------------------
# Using mtcars: weight vs mpg, sized by horsepower
mtcars_bubble <- mtcars %>%
  mutate(
    cyl_factor = factor(cyl, labels = paste(unique(sort(cyl)), "cyl")),
    car_name = rownames(mtcars)
  )

viz_bubble(mtcars_bubble,
           x = wt,
           y = mpg,
           size = hp,
           color = cyl_factor,
           title = "Car Performance Analysis",
           subtitle = "Bubble size = horsepower",
           xlab = "Weight (1000 lbs)",
           ylab = "Miles per Gallon")

# -----------------------------------------------------------------------------
# 1.2 Interactive Bubble with Plotly
# -----------------------------------------------------------------------------
library(plotly)

p <- ggplot(mtcars_bubble, aes(x = wt, y = mpg, size = hp,
                                color = cyl_factor, text = car_name)) +
  geom_point(alpha = 0.7) +
  scale_size_continuous(range = c(3, 15), name = "Horsepower") +
  scale_color_publication() +
  theme_publication() +
  labs(
    title = "Interactive Bubble Chart",
    subtitle = "Hover for car names",
    x = "Weight (1000 lbs)",
    y = "Miles per Gallon"
  )

# Make it interactive
interactive_p <- ggplotly(p, tooltip = c("text", "x", "y", "size"))
print(interactive_p)

# -----------------------------------------------------------------------------
# 1.3 Multi-variable Bubble Chart
# -----------------------------------------------------------------------------
# Using iris data with 4 dimensions
p <- ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width,
                      size = Petal.Length, color = Petal.Width)) +
  geom_point(alpha = 0.6) +
  scale_size_continuous(range = c(2, 12), name = "Petal Length") +
  scale_color_viridis_c(option = "plasma", name = "Petal Width") +
  facet_wrap(~ Species) +
  theme_publication() +
  labs(
    title = "Four-Dimensional Iris Analysis",
    subtitle = "Size = petal length, Color = petal width",
    x = "Sepal Length (cm)",
    y = "Sepal Width (cm)"
  )
print(p)

# ==============================================================================
# 2. CONNECTED SCATTER PLOTS (Time-based)
# ==============================================================================

# -----------------------------------------------------------------------------
# 2.1 Basic Connected Scatter Plot
# -----------------------------------------------------------------------------
# Using economics data - unemployment over time
library(ggplot2)  # for economics dataset

economics_recent <- economics %>%
  filter(date >= as.Date("2010-01-01")) %>%
  mutate(year = as.numeric(format(date, "%Y")))

p <- ggplot(economics_recent, aes(x = unemploy, y = pop)) +
  geom_path(color = "#0072B2", linewidth = 1, alpha = 0.7,
            arrow = arrow(length = unit(0.2, "cm"), type = "closed")) +
  geom_point(aes(color = year), size = 3, alpha = 0.8) +
  scale_color_viridis_c(option = "viridis", name = "Year") +
  theme_publication() +
  labs(
    title = "Connected Scatter: Unemployment vs Population",
    subtitle = "Path shows temporal progression (2010-2015)",
    x = "Unemployment (thousands)",
    y = "Population (thousands)"
  )
print(p)

# -----------------------------------------------------------------------------
# 2.2 Connected Scatter with Annotations
# -----------------------------------------------------------------------------
# Mark first and last points
first_point <- economics_recent %>% slice(1)
last_point <- economics_recent %>% slice(n())

p <- ggplot(economics_recent, aes(x = unemploy, y = pop)) +
  geom_path(color = "#0072B2", linewidth = 1.2, alpha = 0.5) +
  geom_point(aes(color = year), size = 2.5, alpha = 0.7) +
  geom_point(data = first_point, color = "#009E73", size = 5, shape = 21,
             fill = "#009E73", stroke = 2) +
  geom_point(data = last_point, color = "#D55E00", size = 5, shape = 21,
             fill = "#D55E00", stroke = 2) +
  geom_text(data = first_point, aes(x = unemploy, y = pop),
            label = "START", vjust = -1, fontface = "bold", size = 4) +
  geom_text(data = last_point, aes(x = unemploy, y = pop),
            label = "END", vjust = 2, fontface = "bold", size = 4) +
  scale_color_viridis_c(option = "plasma", name = "Year") +
  theme_publication() +
  labs(
    title = "Temporal Path with Start/End Markers",
    subtitle = "Green = start, Orange = end",
    x = "Unemployment (thousands)",
    y = "Population (thousands)"
  )
print(p)

# -----------------------------------------------------------------------------
# 2.3 Multiple Connected Scatter Plots
# -----------------------------------------------------------------------------
# Compare different variables over time
economics_long <- economics_recent %>%
  select(date, unemploy, pop, psavert) %>%
  tidyr::pivot_longer(cols = c(unemploy, psavert),
                      names_to = "variable",
                      values_to = "value")

p <- ggplot(economics_long, aes(x = value, y = pop, color = variable)) +
  geom_path(linewidth = 1.2, alpha = 0.7,
            arrow = arrow(length = unit(0.15, "cm"))) +
  geom_point(size = 2, alpha = 0.6) +
  scale_color_publication() +
  theme_publication() +
  labs(
    title = "Multiple Temporal Relationships",
    subtitle = "Two variables plotted against population",
    x = "Variable Value",
    y = "Population (thousands)"
  ) +
  facet_wrap(~ variable, scales = "free_x")
print(p)

# ==============================================================================
# 3. 2D DENSITY CHARTS
# ==============================================================================

# -----------------------------------------------------------------------------
# 3.1 Basic 2D Density Contours
# -----------------------------------------------------------------------------
viz_density2d(iris,
              x = Sepal.Length,
              y = Sepal.Width,
              title = "2D Density: Sepal Dimensions",
              subtitle = "Contour lines show density concentration",
              xlab = "Sepal Length (cm)",
              ylab = "Sepal Width (cm)")

# -----------------------------------------------------------------------------
# 3.2 Filled 2D Density (Heatmap Style)
# -----------------------------------------------------------------------------
p <- ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) +
  stat_density_2d(aes(fill = after_stat(level)), geom = "polygon", alpha = 0.8) +
  scale_fill_viridis_c(option = "inferno", name = "Density") +
  geom_point(alpha = 0.3, size = 1, color = "white") +
  theme_publication() +
  labs(
    title = "2D Density Heatmap with Points",
    subtitle = "Darker areas = higher concentration",
    x = "Sepal Length (cm)",
    y = "Sepal Width (cm)"
  )
print(p)

# -----------------------------------------------------------------------------
# 3.3 2D Density by Groups
# -----------------------------------------------------------------------------
p <- ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, color = Species)) +
  geom_density_2d(linewidth = 1) +
  geom_point(alpha = 0.4, size = 2) +
  scale_color_publication() +
  theme_publication() +
  labs(
    title = "2D Density Contours by Species",
    subtitle = "Each species shows distinct clustering",
    x = "Sepal Length (cm)",
    y = "Sepal Width (cm)"
  )
print(p)

# -----------------------------------------------------------------------------
# 3.4 Hexbin Plot (2D Density for Large Data)
# -----------------------------------------------------------------------------
# Using diamonds dataset (large dataset)
data(diamonds)
diamonds_sample <- diamonds %>% sample_n(5000)

viz_hexbin(diamonds_sample,
           x = carat,
           y = price,
           title = "Diamond Price vs Carat (Hexbin)",
           subtitle = "Hexagons show density for 5000 diamonds",
           xlab = "Carat",
           ylab = "Price ($)")

# Alternative: filled hexbin with custom colors
p <- ggplot(diamonds_sample, aes(x = carat, y = price)) +
  geom_hex(bins = 30) +
  scale_fill_viridis_c(option = "magma", name = "Count",
                       trans = "log10") +
  theme_publication() +
  labs(
    title = "High-Density Scatter Alternative",
    subtitle = "Log-scaled colors for better visibility",
    x = "Carat",
    y = "Price ($)"
  )
print(p)

# ==============================================================================
# 4. POLYNOMIAL CURVE FITTING
# ==============================================================================

# -----------------------------------------------------------------------------
# 4.1 Linear vs Polynomial Fit
# -----------------------------------------------------------------------------
p <- ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point(size = 3, alpha = 0.6, color = "#0072B2") +
  geom_smooth(method = "lm", se = TRUE, color = "#D55E00",
              fill = "#D55E00", alpha = 0.2, linewidth = 1) +
  theme_publication() +
  labs(
    title = "Linear Regression Fit",
    subtitle = "method = 'lm' with 95% confidence interval",
    x = "Weight (1000 lbs)",
    y = "Miles per Gallon"
  )
print(p)

# -----------------------------------------------------------------------------
# 4.2 Polynomial Fit (Degree 2)
# -----------------------------------------------------------------------------
p <- ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point(size = 3, alpha = 0.6, color = "#0072B2") +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = TRUE,
              color = "#D55E00", fill = "#D55E00", alpha = 0.2,
              linewidth = 1.2) +
  theme_publication() +
  labs(
    title = "Polynomial Regression (Degree 2)",
    subtitle = "Quadratic fit captures curvature",
    x = "Weight (1000 lbs)",
    y = "Miles per Gallon"
  )
print(p)

# -----------------------------------------------------------------------------
# 4.3 Compare Multiple Polynomial Degrees
# -----------------------------------------------------------------------------
# Fit different polynomial degrees
fit_data <- data.frame(
  wt = seq(min(mtcars$wt), max(mtcars$wt), length.out = 100)
)

# Degree 1 (linear)
model1 <- lm(mpg ~ wt, data = mtcars)
fit_data$linear <- predict(model1, newdata = fit_data)

# Degree 2
model2 <- lm(mpg ~ poly(wt, 2), data = mtcars)
fit_data$poly2 <- predict(model2, newdata = fit_data)

# Degree 3
model3 <- lm(mpg ~ poly(wt, 3), data = mtcars)
fit_data$poly3 <- predict(model3, newdata = fit_data)

# Convert to long format
fit_long <- fit_data %>%
  tidyr::pivot_longer(cols = c(linear, poly2, poly3),
                      names_to = "model",
                      values_to = "prediction")

p <- ggplot() +
  geom_point(data = mtcars, aes(x = wt, y = mpg),
             size = 3, alpha = 0.5, color = "gray40") +
  geom_line(data = fit_long, aes(x = wt, y = prediction, color = model),
            linewidth = 1.2) +
  scale_color_manual(
    values = c("#0072B2", "#D55E00", "#009E73"),
    labels = c("Linear", "Polynomial (degree 2)", "Polynomial (degree 3)"),
    name = "Fit Type"
  ) +
  theme_publication() +
  labs(
    title = "Comparing Polynomial Fits",
    subtitle = "Higher degrees capture more complexity",
    x = "Weight (1000 lbs)",
    y = "Miles per Gallon"
  )
print(p)

# -----------------------------------------------------------------------------
# 4.4 LOESS Smoothing (Non-parametric)
# -----------------------------------------------------------------------------
p <- ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point(size = 3, alpha = 0.6, color = "#0072B2") +
  geom_smooth(method = "loess", span = 0.75, se = TRUE,
              color = "#D55E00", fill = "#D55E00", alpha = 0.2,
              linewidth = 1.2) +
  theme_publication() +
  labs(
    title = "LOESS Smoothing",
    subtitle = "Flexible non-parametric fit",
    x = "Weight (1000 lbs)",
    y = "Miles per Gallon"
  )
print(p)

# ==============================================================================
# 5. CORRELATION AND DEVELOPMENT ANALYSIS
# ==============================================================================

# -----------------------------------------------------------------------------
# 5.1 Correlation Matrix Heatmap
# -----------------------------------------------------------------------------
# Select numeric columns from mtcars
mtcars_numeric <- mtcars %>%
  select(mpg, cyl, disp, hp, drat, wt, qsec)

viz_correlation(mtcars_numeric,
                method = "pearson",
                show_values = TRUE,
                title = "Car Metrics Correlation Matrix",
                subtitle = "Pearson correlation coefficients")

# -----------------------------------------------------------------------------
# 5.2 Custom Correlation Scatter Plot Matrix
# -----------------------------------------------------------------------------
# Using GGally for pairs plot
if (requireNamespace("GGally", quietly = TRUE)) {
  p <- GGally::ggpairs(
    iris,
    columns = 1:4,
    mapping = aes(color = Species, alpha = 0.6),
    upper = list(continuous = GGally::wrap("cor", size = 3)),
    lower = list(continuous = GGally::wrap("points", alpha = 0.5, size = 0.8)),
    diag = list(continuous = GGally::wrap("densityDiag", alpha = 0.6))
  ) +
    theme_publication() +
    labs(title = "Iris Correlation Matrix")
  print(p)
}

# -----------------------------------------------------------------------------
# 5.3 Human Development Index Style Plot
# -----------------------------------------------------------------------------
# Simulate HDI-style data (corruption vs development)
set.seed(123)
hdi_data <- data.frame(
  country = paste("Country", 1:50),
  corruption_index = runif(50, 20, 90),
  human_development = runif(50, 0.4, 0.95),
  region = sample(c("Africa", "Americas", "Asia", "Europe", "Oceania"),
                  50, replace = TRUE),
  population = exp(rnorm(50, 15, 2))
)

p <- ggplot(hdi_data, aes(x = corruption_index, y = human_development)) +
  geom_point(aes(size = population, color = region), alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "gray30",
              fill = "gray70", alpha = 0.3, linewidth = 1) +
  scale_size_continuous(range = c(3, 15), name = "Population",
                        labels = scales::comma) +
  scale_color_publication() +
  theme_publication() +
  labs(
    title = "Corruption vs Human Development",
    subtitle = "Bubble size = population, line = linear trend",
    x = "Corruption Perception Index (higher = less corrupt)",
    y = "Human Development Index"
  )
print(p)

# -----------------------------------------------------------------------------
# 5.4 Annotated Correlation Plot
# -----------------------------------------------------------------------------
# Highlight specific countries
hdi_data_annotated <- hdi_data %>%
  mutate(
    label = ifelse(row_number() %in% c(1, 10, 25, 40, 50), country, "")
  )

p <- ggplot(hdi_data_annotated,
            aes(x = corruption_index, y = human_development)) +
  geom_point(aes(size = population, color = region), alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "gray30",
              linetype = "dashed", linewidth = 1) +
  ggrepel::geom_text_repel(
    aes(label = label),
    size = 3,
    box.padding = 0.5,
    segment.color = "gray50"
  ) +
  scale_size_continuous(range = c(3, 15), name = "Population") +
  scale_color_publication() +
  theme_publication() +
  labs(
    title = "Global Development Indicators",
    subtitle = "Selected countries labeled for reference",
    x = "Corruption Perception Index",
    y = "Human Development Index",
    caption = "Bubble size represents population"
  )
print(p)

# ==============================================================================
# 6. ADVANCED SCATTER TECHNIQUES
# ==============================================================================

# -----------------------------------------------------------------------------
# 6.1 Marginal Distributions
# -----------------------------------------------------------------------------
if (requireNamespace("ggExtra", quietly = TRUE)) {
  p <- ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, color = Species)) +
    geom_point(size = 2.5, alpha = 0.6) +
    scale_color_publication() +
    theme_publication() +
    labs(
      title = "Scatter with Marginal Distributions",
      x = "Sepal Length (cm)",
      y = "Sepal Width (cm)"
    )

  # Add marginal histograms
  p_marginal <- ggExtra::ggMarginal(p, type = "histogram",
                                    groupFill = TRUE, alpha = 0.6)
  print(p_marginal)

  # Alternative: marginal density plots
  p_marginal2 <- ggExtra::ggMarginal(p, type = "density",
                                     groupFill = TRUE, alpha = 0.4)
  print(p_marginal2)
}

# -----------------------------------------------------------------------------
# 6.2 Ellipse Confidence Regions
# -----------------------------------------------------------------------------
p <- ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, color = Species)) +
  geom_point(size = 2.5, alpha = 0.6) +
  stat_ellipse(level = 0.95, linewidth = 1.2) +
  scale_color_publication() +
  theme_publication() +
  labs(
    title = "Scatter with 95% Confidence Ellipses",
    subtitle = "Ellipses show bivariate normal distribution",
    x = "Sepal Length (cm)",
    y = "Sepal Width (cm)"
  )
print(p)

# ==============================================================================
# 7. SAVING EXAMPLES
# ==============================================================================

# Save any plot for publication
# save_publication("bubble_chart.png", p, width = 8, height = 6, dpi = 300)
# save_publication("correlation_plot.pdf", p, width = 10, height = 8)

message("✓ All advanced scatter plot examples completed successfully!")
message("  - Interactive bubble charts with plotly")
message("  - Connected scatter plots (temporal paths)")
message("  - 2D density: contours, filled, hexbin")
message("  - Polynomial fitting: linear, quadratic, cubic, LOESS")
message("  - Correlation analysis and HDI-style plots")
message("  - Marginal distributions and confidence ellipses")
