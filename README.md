# ChartKit

> **Publication-Ready Data Visualization for R**

ChartKit is a comprehensive R package providing 48 chart types optimized for scientific publications. Built on ggplot2 with colorblind-friendly palettes and publication-ready themes based on best practices from leading scientific journals.

[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](https://opensource.org/licenses/MIT)

## Features

- **48 Chart Types** - Basic, intermediate, and advanced visualizations
- **Publication-Ready** - Theme based on theme_bw() with refined typography
- **Colorblind-Friendly** - Wong (2011) palette optimized for accessibility
- **High-Quality Output** - 300+ DPI export for journals
- **Consistent API** - All functions follow `viz_*()` convention
- **Well-Documented** - Comprehensive examples with real data

## Installation

```r
# Install from GitHub
install.packages("devtools")
devtools::install_github("DataConceptz/ChartKit")

# Or using remotes
install.packages("remotes")
remotes::install_github("DataConceptz/ChartKit")
```

## Quick Start

```r
library(ChartKit)
library(dplyr)

# View all 48 available charts
print_chart_catalog()
```

## Working Examples

### Scatter Plot

```r
library(ChartKit)
library(dplyr)

# IMPORTANT: Convert categorical variables to factors
mtcars_clean <- mtcars %>%
  mutate(cyl_factor = factor(cyl, labels = paste(unique(cyl), "cylinders")))

# Publication-ready scatter plot
viz_scatter(mtcars_clean,
            x = wt,
            y = mpg,
            color = cyl_factor,
            title = "Fuel Efficiency vs. Vehicle Weight",
            subtitle = "Heavier vehicles with more cylinders show lower MPG",
            xlab = "Weight (1000 lbs)",
            ylab = "Miles per Gallon")
```

### Bar Chart

```r
# Bar charts require aggregated data
mpg_summary <- mtcars %>%
  mutate(cyl_factor = factor(cyl)) %>%
  group_by(cyl_factor) %>%
  summarise(avg_mpg = mean(mpg),
            count = n())

viz_bar(mpg_summary,
        x = cyl_factor,
        y = avg_mpg,
        title = "Average Fuel Efficiency by Engine Configuration",
        xlab = "Number of Cylinders",
        ylab = "Average MPG")
```

### Grouped Bar Chart

```r
grouped_summary <- mtcars %>%
  mutate(
    cyl_factor = factor(cyl),
    transmission = factor(am, labels = c("Automatic", "Manual"))
  ) %>%
  group_by(cyl_factor, transmission) %>%
  summarise(avg_mpg = mean(mpg), .groups = "drop")

viz_bar(grouped_summary,
        x = cyl_factor,
        y = avg_mpg,
        fill = transmission,
        position = "dodge",
        title = "Fuel Efficiency: Cylinders and Transmission",
        xlab = "Number of Cylinders",
        ylab = "Average MPG")
```

### Box Plot

```r
mtcars_factor <- mtcars %>%
  mutate(cyl_factor = factor(cyl))

viz_boxplot(mtcars_factor,
            x = cyl_factor,
            y = mpg,
            fill = cyl_factor,
            title = "MPG Distribution by Cylinder Count",
            xlab = "Number of Cylinders",
            ylab = "Miles per Gallon")
```

### Histogram

```r
viz_histogram(mtcars,
              x = mpg,
              bins = 15,
              title = "Distribution of Fuel Efficiency",
              subtitle = "Sample of 32 car models",
              xlab = "Miles per Gallon")
```

### Violin Plot

```r
viz_violin(mtcars_factor,
           x = cyl_factor,
           y = mpg,
           fill = cyl_factor,
           title = "MPG Distribution Shape by Cylinders",
           xlab = "Number of Cylinders",
           ylab = "Miles per Gallon")
```

### Line Plot (Time Series)

```r
# Using built-in economics dataset
library(ggplot2)  # for economics data

economics_recent <- economics %>%
  filter(date >= as.Date("2010-01-01"))

viz_line(economics_recent,
         x = date,
         y = unemploy,
         title = "US Unemployment Over Time",
         subtitle = "Thousands of persons, seasonally adjusted",
         xlab = "Year",
         ylab = "Unemployment (thousands)")
```

## Saving Publication-Quality Figures

```r
# Create plot
p <- viz_scatter(mtcars_clean, x = wt, y = mpg, color = cyl_factor,
                title = "Fuel Efficiency Analysis",
                xlab = "Weight (1000 lbs)",
                ylab = "Miles per Gallon")

# Save at journal quality (300 DPI)
save_publication("figure1.png", p, width = 7, height = 5, dpi = 300)

# High resolution for print
save_publication("figure1.tiff", p, width = 7, height = 5, dpi = 600)

# Vector format for ultimate quality
save_publication("figure1.pdf", p, width = 7, height = 5)
```

## Chart Categories

### Basic Charts (16 types)
- **viz_scatter** - Relationships between variables
- **viz_line** - Time series and trends
- **viz_bar** - Categorical comparisons
- **viz_histogram** - Distributions
- **viz_boxplot** - Distribution and outliers
- **viz_violin** - Distribution shape
- **viz_density** - Smoothed distributions
- **viz_pie** / **viz_donut** - Proportions
- **viz_area** - Cumulative data
- **viz_ribbon** - Confidence intervals
- **viz_step** - Discrete changes
- **viz_errorbar** - Uncertainty
- **viz_jitter** - Categorical scatter
- **viz_qq** - Distribution comparison
- **viz_ecdf** - Cumulative distribution

### Intermediate Charts (15 types)
- **viz_heatmap** - Matrix visualization
- **viz_correlation** - Correlation matrices
- **viz_ridgeline** - Distribution comparison
- **viz_waterfall** - Cumulative effects
- **viz_radar** - Multivariate comparison
- **viz_bubble** - Three-variable relationships
- **viz_lollipop** - Ranked values
- **viz_dumbbell** - Before/after comparison
- And more...

### Advanced Charts (17 types)
- **viz_sankey** - Flow visualization
- **viz_treemap** - Hierarchical data
- **viz_network** - Relationship networks
- **viz_parallel** - Multivariate analysis
- **viz_gantt** - Project timelines
- And more...

## Themes

ChartKit provides three publication-ready themes:

```r
# Default: clean with subtle grid (recommended)
p + theme_publication()

# Minimal: no grid lines
p + theme_minimal_pub()

# Classic: strong borders, no grid
p + theme_classic_pub()
```

All themes are based on `theme_bw()` with:
- Optimized font sizes (11pt base, suitable for most journals)
- Clean typography hierarchy
- Subtle grid lines (removable)
- Professional spacing and margins
- High-contrast, readable text

## Color Palettes

All palettes are research-based and colorblind-friendly:

```r
# Default (Wong 2011 - optimal for colorblind readers)
p + scale_color_publication()

# Vibrant (high contrast)
p + scale_color_publication(palette = "vibrant")

# Muted (Paul Tol scheme)
p + scale_color_publication(palette = "muted")

# Dark (for light backgrounds)
p + scale_color_publication(palette = "dark")
```

**Reference:** Wong, B. (2011). Points of view: Color blindness. *Nature Methods* 8(6), 441.

## Important Notes

### Data Preparation

1. **Convert categorical variables to factors**:
   ```r
   data %>% mutate(category = factor(category))
   ```

2. **Aggregate data for bar charts**:
   ```r
   data %>% group_by(group) %>% summarise(mean_value = mean(value))
   ```

3. **Filter time series to relevant range**:
   ```r
   data %>% filter(date >= as.Date("2010-01-01"))
   ```

### Best Practices

- Use descriptive titles and axis labels
- Specify units in labels (e.g., "Weight (kg)", "Time (seconds)")
- Convert continuous variables to factors for discrete color scales
- Save at 300 DPI minimum for publications
- Use 7" × 5" as default figure size (adjustable)
- Include subtitles to explain patterns or provide context

### Known Issues

- Some functions may show warnings about deprecated parameters in ggplot2 >= 3.4.0 (safe to ignore)
- Advanced charts (Sankey, Chord, Network) require specific data formats - see examples
- Calendar heatmap requires lubridate package

## Comprehensive Example Files

ChartKit includes three complete example files with tested, production-ready code:

### Basic Charts (`inst/examples/basic_examples.R`)
- Scatter plots with proper factor conversion
- Bar charts with data aggregation
- Line plots for time series
- Box plots, violin plots, histograms
- Density plots and statistical charts
- Proper data preparation for each chart type

### Intermediate Charts (`inst/examples/intermediate_examples.R`)
- Heatmaps and correlation matrices
- Bubble charts and lollipop plots
- Dumbbell charts for comparisons
- Waterfall and funnel charts
- Ridgeline and violin-box combinations
- Raincloud plots (ultimate distribution viz)

### Advanced Charts (`inst/examples/advanced_examples.R`)
**Special focus on network visualizations:**
- **Sankey diagrams** - Customer journeys, budget flows, energy flows
- **Network graphs** - Social networks, citations, collaborations
- **Chord diagrams** - Trade flows, migration, information exchange
- Treemaps for hierarchical data
- Parallel coordinates for multivariate analysis
- Gantt charts for project management
- Hexbin plots for large datasets

Each example file includes:
- ✅ Working code you can copy-paste
- ✅ Real data or realistic simulated data
- ✅ Detailed comments explaining each step
- ✅ Best practices and gotchas
- ✅ Data preparation steps

## Citation

If you use ChartKit in your research, please cite:

```
DataConceptz R-Vis Team (2024). ChartKit: Publication-Ready Data Visualization for R.
R package version 0.1.0. https://github.com/DataConceptz/ChartKit
```

## License

MIT License - see [LICENSE](LICENSE) file for details.

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request.

## Support

- **Email**: contact@dataconceptz.com
- **Issues**: https://github.com/DataConceptz/ChartKit/issues
- **Documentation**: See examples in `inst/examples/`

## Acknowledgments

- Color palettes based on research by Wong (2011) and Paul Tol
- Theme design inspired by best practices from Nature, Science, and PLOS journals
- Built on the excellent ggplot2 package by Hadley Wickham
