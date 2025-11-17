# ChartKit

> **Unified Data Visualization Package for Publication-Ready Charts**

ChartKit is a comprehensive R package that provides 47+ chart types ranging from basic to advanced visualizations. It unifies multiple visualization packages (ggplot2, plotly, ggridges, treemapify, ggalluvial, etc.) into a consistent, easy-to-use API designed for creating publication-ready charts.

## Features

- **47+ Chart Types**: From basic scatter plots to advanced Sankey diagrams
- **Consistent API**: All visualization functions follow the `viz_*()` naming convention
- **Publication-Ready**: Built-in themes optimized for academic publications and reports
- **Colorblind-Friendly**: Multiple color palettes designed for accessibility
- **High-Quality Output**: Export functionality supporting PNG, PDF, and TIFF at 300+ DPI

## Installation

You can install ChartKit from GitHub using:

```r
# install.packages("devtools")
devtools::install_github("DataConceptz/ChartKit")

# Or using remotes
# install.packages("remotes")
remotes::install_github("DataConceptz/ChartKit")
```

## Quick Start

```r
library(ChartKit)

# Create a scatter plot
viz_scatter(mtcars, x = wt, y = mpg, color = cyl,
            title = "Car Weight vs MPG",
            subtitle = "Colored by number of cylinders")

# Create a bar chart
viz_bar(mtcars, x = cyl, y = mpg,
        title = "Average MPG by Cylinders")

# View all available charts
print_chart_catalog()
```

## Chart Categories

### Basic Charts (16 types)
- Scatter, Line, Bar, Histogram, Boxplot, Violin
- Density, Pie, Donut, Area, Ribbon, Step
- Error Bar, Jitter, Q-Q, ECDF

### Intermediate Charts (15 types)
- Heatmap, Correlation, Contour, 2D Density, Ridgeline
- Waterfall, Funnel, Radar, Bubble, Lollipop
- Dumbbell, Cleveland Dot, Violin-Box, Raincloud, Polar Bar

### Advanced Charts (16 types)
- Sankey, Treemap, Sunburst, Network, Parallel Coordinates
- Chord, Calendar Heatmap, Hexbin, Stream, Alluvial
- Circular Bar, Bullet, Slope, Wind Rose, Gantt
- Mosaic, Marimekko

## Themes

ChartKit provides three publication-ready themes:

- `theme_publication()`: Clean, professional theme with grid lines
- `theme_minimal_pub()`: Minimalist theme without grid lines
- `theme_classic_pub()`: Classic theme with borders

## Color Palettes

All color palettes are colorblind-friendly:

- `default`: Standard colorblind-safe palette
- `vibrant`: High-contrast vibrant colors
- `muted`: Softer, muted tones
- `dark`: Dark, rich colors

## Saving Plots

```r
p <- viz_scatter(mtcars, x = wt, y = mpg)
save_publication("my_plot.png", p, width = 10, height = 7, dpi = 300)
```

## License

MIT License - see [LICENSE](LICENSE) file for details.

## Author

DataConceptz R-Vis Team
- Email: contact@dataconceptz.com
- GitHub: https://github.com/DataConceptz/ChartKit
- Issues: https://github.com/DataConceptz/ChartKit/issues
