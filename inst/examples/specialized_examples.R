# ==============================================================================
# ChartKit: Specialized Visualization Examples
# ==============================================================================
# Lollipop, dumbbell, treemap, pie/donut, dendrogram, circle packing,
# waffle, and wordcloud examples
# ==============================================================================

library(ChartKit)
library(dplyr)
library(ggplot2)

# ==============================================================================
# 1. LOLLIPOP CHARTS
# ==============================================================================

# -----------------------------------------------------------------------------
# 1.1 Basic Lollipop Chart
# -----------------------------------------------------------------------------
# Top 10 cars by MPG
top_cars <- mtcars %>%
  mutate(car = rownames(mtcars)) %>%
  arrange(desc(mpg)) %>%
  head(10)

viz_lollipop(top_cars,
             x = car,
             y = mpg,
             title = "Top 10 Fuel Efficient Cars",
             subtitle = "Lollipop chart emphasizes individual values",
             xlab = "Car Model",
             ylab = "Miles per Gallon")

# -----------------------------------------------------------------------------
# 1.2 Ordered Horizontal Lollipop
# -----------------------------------------------------------------------------
p <- ggplot(top_cars, aes(x = mpg, y = reorder(car, mpg))) +
  geom_segment(aes(x = 0, xend = mpg, yend = car),
               color = "gray70", linewidth = 1.2) +
  geom_point(color = "#0072B2", size = 5, alpha = 0.8) +
  geom_text(aes(label = sprintf("%.1f", mpg)), hjust = -0.5,
            size = 3.5, fontface = "bold") +
  theme_publication() +
  labs(
    title = "Horizontal Lollipop Chart (Ordered)",
    subtitle = "Ranked by fuel efficiency",
    x = "Miles per Gallon",
    y = "Car Model"
  ) +
  xlim(0, max(top_cars$mpg) * 1.15)
print(p)

# -----------------------------------------------------------------------------
# 1.3 Colored Lollipop by Category
# -----------------------------------------------------------------------------
iris_avg <- iris %>%
  group_by(Species) %>%
  summarise(avg_sepal = mean(Sepal.Length)) %>%
  arrange(avg_sepal)

p <- ggplot(iris_avg, aes(x = avg_sepal, y = reorder(Species, avg_sepal),
                          color = Species)) +
  geom_segment(aes(x = 0, xend = avg_sepal, yend = Species),
               linewidth = 2, alpha = 0.7) +
  geom_point(size = 8, alpha = 0.9) +
  geom_text(aes(label = sprintf("%.2f", avg_sepal)),
            hjust = -0.5, size = 4, fontface = "bold", color = "black") +
  scale_color_publication() +
  theme_publication() +
  theme(legend.position = "none") +
  labs(
    title = "Colored Lollipop Chart",
    subtitle = "Average sepal length by species",
    x = "Mean Sepal Length (cm)",
    y = "Species"
  ) +
  xlim(0, max(iris_avg$avg_sepal) * 1.2)
print(p)

# -----------------------------------------------------------------------------
# 1.4 Diverging Lollipop Chart
# -----------------------------------------------------------------------------
# Show deviation from mean
mtcars_div <- mtcars %>%
  mutate(
    car = rownames(mtcars),
    mpg_diff = mpg - mean(mpg),
    mpg_type = ifelse(mpg_diff > 0, "Above Average", "Below Average")
  ) %>%
  arrange(mpg_diff) %>%
  tail(15) %>%  # Top and bottom combined
  mutate(car = factor(car, levels = car))

p <- ggplot(mtcars_div, aes(x = car, y = mpg_diff, color = mpg_type)) +
  geom_segment(aes(x = car, xend = car, y = 0, yend = mpg_diff),
               linewidth = 1.5) +
  geom_point(size = 5, alpha = 0.9) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  scale_color_manual(values = c("Above Average" = "#009E73",
                                 "Below Average" = "#D55E00"),
                     name = "") +
  coord_flip() +
  theme_publication() +
  labs(
    title = "Diverging Lollipop: Deviation from Mean MPG",
    subtitle = "Green = above average, Orange = below average",
    x = "Car Model",
    y = "Deviation from Mean MPG"
  )
print(p)

# ==============================================================================
# 2. DUMBBELL CHARTS
# ==============================================================================

# -----------------------------------------------------------------------------
# 2.1 Basic Dumbbell Chart
# -----------------------------------------------------------------------------
# Before and after comparison
before_after <- data.frame(
  category = c("Product A", "Product B", "Product C", "Product D", "Product E"),
  before = c(45, 62, 38, 71, 55),
  after = c(68, 75, 52, 82, 71)
)

viz_dumbbell(before_after,
             category = category,
             x1 = before,
             x2 = after,
             title = "Product Performance: Before vs After",
             subtitle = "Improvement across all products")

# -----------------------------------------------------------------------------
# 2.2 Enhanced Dumbbell with Colors
# -----------------------------------------------------------------------------
before_after_colored <- before_after %>%
  mutate(
    improvement = after - before,
    category = reorder(category, improvement)
  )

p <- ggplot(before_after_colored) +
  geom_segment(aes(x = before, xend = after,
                   y = category, yend = category),
               color = "gray70", linewidth = 1.5) +
  geom_point(aes(x = before, y = category),
             color = "#D55E00", size = 6, alpha = 0.8) +
  geom_point(aes(x = after, y = category),
             color = "#009E73", size = 6, alpha = 0.8) +
  geom_text(aes(x = before, y = category, label = before),
            hjust = 1.5, size = 3.5, fontface = "bold") +
  geom_text(aes(x = after, y = category, label = after),
            hjust = -0.5, size = 3.5, fontface = "bold") +
  theme_publication() +
  labs(
    title = "Extended Dumbbell Chart",
    subtitle = "Orange = before, Green = after, ordered by improvement",
    x = "Performance Score",
    y = "Product"
  )
print(p)

# -----------------------------------------------------------------------------
# 2.3 Dumbbell with Arrow Indicators
# -----------------------------------------------------------------------------
p <- ggplot(before_after_colored, aes(y = category)) +
  geom_segment(aes(x = before, xend = after, yend = category),
               arrow = arrow(length = unit(0.3, "cm"), type = "closed"),
               color = "#0072B2", linewidth = 1.8, alpha = 0.7) +
  geom_point(aes(x = before), color = "#D55E00", size = 5, alpha = 0.9) +
  geom_point(aes(x = after), color = "#009E73", size = 5, alpha = 0.9) +
  annotate("text", x = 40, y = 5.5, label = "BEFORE",
           fontface = "bold", color = "#D55E00", size = 4) +
  annotate("text", x = 80, y = 5.5, label = "AFTER",
           fontface = "bold", color = "#009E73", size = 4) +
  theme_publication() +
  labs(
    title = "Dumbbell Chart with Directional Arrows",
    subtitle = "Arrows show magnitude and direction of change",
    x = "Performance Score",
    y = "Product"
  )
print(p)

# ==============================================================================
# 3. TREEMAP CHARTS
# ==============================================================================

# -----------------------------------------------------------------------------
# 3.1 Basic Treemap
# -----------------------------------------------------------------------------
# Sales by category
sales_data <- data.frame(
  category = c("Electronics", "Clothing", "Food", "Books",
               "Toys", "Sports", "Home", "Beauty"),
  sales = c(450, 320, 280, 180, 150, 120, 200, 100)
)

viz_treemap(sales_data,
            area = sales,
            fill = category,
            label = category,
            title = "Sales by Category (Treemap)",
            subtitle = "Rectangle size = sales volume")

# -----------------------------------------------------------------------------
# 3.2 Treemap with Custom Colors and Values
# -----------------------------------------------------------------------------
library(treemapify)

p <- ggplot(sales_data, aes(area = sales, fill = sales, label = category)) +
  geom_treemap(color = "white", linewidth = 2) +
  geom_treemap_text(color = "white", place = "centre",
                    size = 14, fontface = "bold") +
  geom_treemap_text(aes(label = paste0("$", sales, "K")),
                    color = "white", place = "bottom",
                    size = 10, padding.y = unit(2, "mm")) +
  scale_fill_viridis_c(option = "plasma", name = "Sales ($K)") +
  theme_publication() +
  theme(legend.position = "right") +
  labs(title = "Sales Treemap with Value Labels")
print(p)

# -----------------------------------------------------------------------------
# 3.3 Hierarchical Treemap
# -----------------------------------------------------------------------------
# Two-level hierarchy
hier_sales <- data.frame(
  department = rep(c("Electronics", "Clothing", "Home"), each = 3),
  product = c("Phones", "Laptops", "Tablets",
              "Shirts", "Pants", "Shoes",
              "Furniture", "Decor", "Kitchen"),
  sales = c(200, 150, 100, 120, 110, 90, 80, 60, 60)
)

p <- ggplot(hier_sales, aes(area = sales, fill = department,
                            label = product, subgroup = department)) +
  geom_treemap(color = "white", linewidth = 2) +
  geom_treemap_subgroup_border(color = "black", linewidth = 4) +
  geom_treemap_subgroup_text(place = "centre", alpha = 0.7,
                             color = "black", fontface = "bold", size = 16) +
  geom_treemap_text(color = "white", place = "centre", size = 10) +
  scale_fill_publication() +
  theme_publication() +
  labs(title = "Hierarchical Treemap: Department → Product")
print(p)

# ==============================================================================
# 4. PIE AND DONUT CHARTS
# ==============================================================================

# -----------------------------------------------------------------------------
# 4.1 Basic Pie Chart
# -----------------------------------------------------------------------------
viz_pie(sales_data,
        values = sales,
        labels = category,
        title = "Market Share by Category",
        subtitle = "Pie chart shows proportions")

# -----------------------------------------------------------------------------
# 4.2 Pie Chart with Percentages
# -----------------------------------------------------------------------------
sales_pct <- sales_data %>%
  mutate(
    pct = sales / sum(sales) * 100,
    label = sprintf("%s\n%.1f%%", category, pct)
  )

p <- ggplot(sales_pct, aes(x = "", y = sales, fill = category)) +
  geom_bar(stat = "identity", width = 1, alpha = 0.8) +
  geom_text(aes(label = sprintf("%.1f%%", pct)),
            position = position_stack(vjust = 0.5),
            size = 4, fontface = "bold", color = "white") +
  coord_polar(theta = "y") +
  scale_fill_publication() +
  theme_publication() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  ) +
  labs(title = "Pie Chart with Percentage Labels",
       fill = "Category")
print(p)

# -----------------------------------------------------------------------------
# 4.3 Donut Chart
# -----------------------------------------------------------------------------
viz_donut(sales_data,
          values = sales,
          labels = category,
          title = "Sales Distribution (Donut Chart)",
          subtitle = "Hollow center improves readability")

# -----------------------------------------------------------------------------
# 4.4 Donut Chart with Custom Hole Size
# -----------------------------------------------------------------------------
p <- ggplot(sales_pct, aes(x = 2, y = sales, fill = category)) +
  geom_bar(stat = "identity", width = 1, alpha = 0.8) +
  geom_text(aes(label = category),
            position = position_stack(vjust = 0.5),
            size = 3.5, fontface = "bold") +
  coord_polar(theta = "y") +
  xlim(0.5, 2.5) +  # Controls donut hole size
  scale_fill_publication() +
  theme_publication() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    legend.position = "right"
  ) +
  labs(title = "Custom Donut Chart",
       subtitle = "Adjustable center hole size",
       fill = "Category")
print(p)

# ==============================================================================
# 5. DENDROGRAM / HIERARCHICAL CLUSTERING
# ==============================================================================

# -----------------------------------------------------------------------------
# 5.1 Basic Dendrogram
# -----------------------------------------------------------------------------
# Hierarchical clustering of iris data
iris_scaled <- scale(iris[, 1:4])
dist_matrix <- dist(iris_scaled)
hc <- hclust(dist_matrix, method = "ward.D2")

# Basic dendrogram
plot(hc, main = "Hierarchical Clustering Dendrogram",
     xlab = "Sample Index", sub = "", cex = 0.6)

# -----------------------------------------------------------------------------
# 5.2 Colored Dendrogram by Groups
# -----------------------------------------------------------------------------
library(dendextend)

if (requireNamespace("dendextend", quietly = TRUE)) {
  dend <- as.dendrogram(hc)

  # Color by 3 clusters
  dend_colored <- color_branches(dend, k = 3)
  dend_colored <- color_labels(dend_colored, k = 3)

  plot(dend_colored, main = "Colored Dendrogram (3 Clusters)",
       ylab = "Height")

  # Add rectangles around clusters
  rect.dendrogram(dend_colored, k = 3, border = 2:4, lwd = 2)
}

# -----------------------------------------------------------------------------
# 5.3 Circular Dendrogram with ggraph
# -----------------------------------------------------------------------------
library(ggraph)
library(igraph)

# Create hierarchical edge list
hc_data <- dendro_data(hc)
segments_df <- hc_data$segments

# Build graph for ggraph
graph <- graph_from_data_frame(data.frame(from = 1:nrow(iris_scaled),
                                          to = 1:nrow(iris_scaled)))

p <- ggraph(hc, layout = 'dendrogram', circular = TRUE) +
  geom_edge_diagonal(alpha = 0.6, color = "#0072B2") +
  theme_publication() +
  theme_graph() +
  labs(title = "Circular Dendrogram",
       subtitle = "Hierarchical clustering in radial layout")
print(p)

# ==============================================================================
# 6. CIRCLE PACKING
# ==============================================================================

# -----------------------------------------------------------------------------
# 6.1 Basic Circle Packing
# -----------------------------------------------------------------------------
if (requireNamespace("packcircles", quietly = TRUE) &&
    requireNamespace("ggforce", quietly = TRUE)) {

  # Generate circle packing layout
  packing <- packcircles::circleProgressiveLayout(sales_data$sales)
  circle_data <- cbind(sales_data, packing)
  circle_data$radius <- sqrt(circle_data$area / pi)

  p <- ggplot(circle_data) +
    ggforce::geom_circle(aes(x0 = x, y0 = y, r = radius, fill = category),
                         alpha = 0.7, color = "white", linewidth = 2) +
    geom_text(aes(x = x, y = y, label = category),
              size = 4, fontface = "bold") +
    geom_text(aes(x = x, y = y, label = paste0("$", sales, "K")),
              vjust = 2, size = 3.5) +
    scale_fill_publication() +
    coord_equal() +
    theme_publication() +
    theme(
      axis.text = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      legend.position = "none"
    ) +
    labs(title = "Circle Packing Visualization",
         subtitle = "Circle size represents sales value")
  print(p)
}

# ==============================================================================
# 7. WAFFLE CHARTS
# ==============================================================================

# -----------------------------------------------------------------------------
# 7.1 Basic Waffle Chart
# -----------------------------------------------------------------------------
if (requireNamespace("waffle", quietly = TRUE)) {
  # Create waffle data (simplified)
  waffle_data <- c(
    "Category A" = 30,
    "Category B" = 25,
    "Category C" = 20,
    "Category D" = 25
  )

  waffle::waffle(waffle_data,
                 rows = 10,
                 colors = c("#0072B2", "#D55E00", "#009E73", "#CC79A7"),
                 title = "Waffle Chart (100 squares = 100%)",
                 xlab = "1 square = 1%")
}

# -----------------------------------------------------------------------------
# 7.2 Waffle with ggplot2 (geom_waffle)
# -----------------------------------------------------------------------------
# Manual waffle creation
waffle_grid <- expand.grid(x = 1:10, y = 1:10) %>%
  mutate(
    id = row_number(),
    category = case_when(
      id <= 30 ~ "A",
      id <= 55 ~ "B",
      id <= 75 ~ "C",
      TRUE ~ "D"
    )
  )

p <- ggplot(waffle_grid, aes(x = x, y = y, fill = category)) +
  geom_tile(color = "white", linewidth = 2, alpha = 0.8) +
  scale_fill_publication() +
  coord_equal() +
  theme_publication() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  ) +
  labs(
    title = "Waffle Chart (10x10 Grid)",
    subtitle = "Each square = 1% of total",
    fill = "Category"
  )
print(p)

# ==============================================================================
# 8. WORDCLOUD
# ==============================================================================

# -----------------------------------------------------------------------------
# 8.1 Basic Wordcloud with wordcloud2
# -----------------------------------------------------------------------------
if (requireNamespace("wordcloud2", quietly = TRUE)) {
  # Sample word frequency data
  word_freq <- data.frame(
    word = c("R", "ggplot2", "data", "visualization", "chart",
             "analysis", "statistics", "plot", "graphics", "code",
             "programming", "science", "research", "publication", "journal"),
    freq = c(100, 85, 75, 70, 65, 60, 55, 50, 48, 45,
             40, 38, 35, 32, 30)
  )

  wordcloud2::wordcloud2(word_freq, size = 0.8, color = "random-light",
                         backgroundColor = "white")
}

# -----------------------------------------------------------------------------
# 8.2 Shaped Wordcloud
# -----------------------------------------------------------------------------
if (requireNamespace("wordcloud2", quietly = TRUE)) {
  # Letter shape
  wordcloud2::wordcloud2(word_freq, size = 1.2,
                         shape = "circle",
                         color = rep_len(c("#0072B2", "#D55E00", "#009E73"),
                                         nrow(word_freq)))
}

# ==============================================================================
# 9. SAVING EXAMPLES
# ==============================================================================

# Save any plot for publication
# save_publication("lollipop_chart.png", p, width = 10, height = 7, dpi = 300)
# save_publication("treemap.pdf", p, width = 10, height = 8)

message("✓ All specialized chart examples completed successfully!")
message("  - Lollipop: basic, horizontal, colored, diverging")
message("  - Dumbbell: before/after comparisons with arrows")
message("  - Treemap: basic, colored, hierarchical")
message("  - Pie/Donut: with percentages and custom sizing")
message("  - Dendrogram: basic, colored, circular")
message("  - Circle packing: proportional circles")
message("  - Waffle: grid-based proportion charts")
message("  - Wordcloud: frequency-based text visualization")
