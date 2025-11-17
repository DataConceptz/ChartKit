# ==============================================================================
# ChartKit: Advanced Charts - Working Examples
# ==============================================================================
# Focus on Sankey, Network, and Chord diagrams with real-world examples
# All code is tested and production-ready

library(ChartKit)
library(dplyr)
library(tidyr)

# ==============================================================================
# 1. SANKEY DIAGRAM - Flow Visualization
# ==============================================================================

# Example 1: Customer Journey Flow
customer_flow <- data.frame(
  source = c("Website", "Website", "Website",
             "Social Media", "Social Media", "Social Media",
             "Email", "Email", "Email"),
  target = c("Product Page", "Blog", "Exit",
             "Product Page", "Blog", "Exit",
             "Product Page", "Blog", "Exit"),
  value = c(1200, 800, 2000,
            600, 300, 400,
            900, 200, 300)
)

viz_sankey(customer_flow,
           source = source,
           target = target,
           value = value,
           title = "Customer Journey Flow",
           subtitle = "Traffic sources to landing pages")

# Example 2: Energy Flow
energy_flow <- data.frame(
  source = c("Coal", "Coal", "Natural Gas", "Natural Gas",
             "Nuclear", "Nuclear", "Renewables", "Renewables"),
  target = c("Electricity", "Industrial", "Electricity", "Heating",
             "Electricity", "Industrial", "Electricity", "Heating"),
  value = c(400, 200, 350, 150, 300, 50, 200, 100)
)

viz_sankey(energy_flow,
           source = source,
           target = target,
           value = value,
           title = "Energy Production and Consumption",
           subtitle = "Source to end use (TWh)")

# Example 3: Budget Allocation
budget_flow <- data.frame(
  source = c(rep("Total Budget", 4),
             "Operations", "Operations", "Operations",
             "Marketing", "Marketing",
             "R&D", "R&D"),
  target = c("Operations", "Marketing", "R&D", "Admin",
             "Salaries", "Equipment", "Supplies",
             "Digital", "Events",
             "Research", "Development"),
  value = c(500, 300, 250, 150,
            300, 150, 50,
            200, 100,
            150, 100)
)

viz_sankey(budget_flow,
           source = source,
           target = target,
           value = value,
           title = "Annual Budget Allocation",
           subtitle = "Department to program breakdown ($thousands)")

# ==============================================================================
# 2. NETWORK GRAPH - Relationship Visualization
# ==============================================================================

# Example 1: Social Network
# Create nodes (people)
people_nodes <- data.frame(
  id = c("Alice", "Bob", "Charlie", "Diana", "Eve",
         "Frank", "Grace", "Henry"),
  stringsAsFactors = FALSE
)

# Create edges (relationships)
relationships <- data.frame(
  from = c("Alice", "Alice", "Bob", "Bob", "Charlie",
           "Diana", "Eve", "Frank", "Grace"),
  to = c("Bob", "Charlie", "Charlie", "Diana", "Diana",
         "Eve", "Frank", "Grace", "Henry"),
  stringsAsFactors = FALSE
)

viz_network(nodes = people_nodes,
            edges = relationships,
            node_id = id,
            edge_from = from,
            edge_to = to,
            title = "Social Network Structure",
            subtitle = "Connections between individuals",
            layout = "fr")  # Fruchterman-Reingold layout

# Example 2: Collaboration Network (with different layout)
collab_nodes <- data.frame(
  id = c("Dept A", "Dept B", "Dept C", "Dept D", "Dept E")
)

collab_edges <- data.frame(
  from = c("Dept A", "Dept A", "Dept A", "Dept B", "Dept B",
           "Dept C", "Dept D"),
  to = c("Dept B", "Dept C", "Dept D", "Dept C", "Dept E",
         "Dept D", "Dept E")
)

viz_network(nodes = collab_nodes,
            edges = collab_edges,
            node_id = id,
            edge_from = from,
            edge_to = to,
            title = "Inter-Departmental Collaboration Network",
            subtitle = "Project partnerships",
            layout = "circle")

# Example 3: Citation Network
citation_nodes <- data.frame(
  id = c("Paper1", "Paper2", "Paper3", "Paper4",
         "Paper5", "Paper6", "Paper7")
)

citation_edges <- data.frame(
  from = c("Paper1", "Paper1", "Paper2", "Paper3",
           "Paper3", "Paper4", "Paper5", "Paper6"),
  to = c("Paper4", "Paper5", "Paper5", "Paper6",
         "Paper7", "Paper7", "Paper7", "Paper7")
)

viz_network(nodes = citation_nodes,
            edges = citation_edges,
            node_id = id,
            edge_from = from,
            edge_to = to,
            title = "Citation Network",
            subtitle = "Papers citing other papers",
            layout = "kk")  # Kamada-Kawai layout

# ==============================================================================
# 3. CHORD DIAGRAM - Inter-relationships
# ==============================================================================

# Example 1: Trade Flows Between Countries
trade_data <- data.frame(
  from = c("USA", "USA", "USA", "China", "China", "China",
           "EU", "EU", "EU", "Japan", "Japan", "India"),
  to = c("China", "EU", "Mexico", "USA", "EU", "Japan",
         "USA", "China", "UK", "USA", "China", "USA"),
  value = c(120, 150, 80, 130, 100, 60,
            140, 90, 70, 95, 75, 50)
)

viz_chord(trade_data,
          from = from,
          to = to,
          value = value,
          title = "International Trade Flows",
          subtitle = "Bilateral trade volume ($billions)")

# Example 2: Migration Patterns
migration_data <- data.frame(
  from = c("Region A", "Region A", "Region B", "Region B",
           "Region C", "Region C", "Region D"),
  to = c("Region B", "Region C", "Region A", "Region D",
         "Region A", "Region D", "Region B"),
  value = c(250, 180, 220, 150, 200, 130, 170)
)

viz_chord(migration_data,
          from = from,
          to = to,
          value = value,
          title = "Inter-Regional Migration Patterns",
          subtitle = "Number of relocations (thousands)")

# Example 3: Information Flow
info_flow <- data.frame(
  from = c("Research", "Research", "Development", "Development",
           "Marketing", "Marketing", "Sales"),
  to = c("Development", "Marketing", "Marketing", "Sales",
         "Sales", "Research", "Research"),
  value = c(80, 60, 70, 90, 85, 40, 50)
)

viz_chord(info_flow,
          from = from,
          to = to,
          value = value,
          title = "Organizational Information Flow",
          subtitle = "Communication frequency (interactions/week)")

# ==============================================================================
# 4. TREEMAP - Hierarchical Data
# ==============================================================================

# Market share example
market_data <- data.frame(
  company = c("Company A", "Company B", "Company C", "Company D",
              "Company E", "Company F", "Company G", "Company H"),
  revenue = c(450, 320, 280, 210, 180, 150, 120, 90),
  sector = c("Tech", "Tech", "Finance", "Finance",
             "Healthcare", "Healthcare", "Energy", "Energy")
)

viz_treemap(market_data,
            area = revenue,
            fill = sector,
            label = company,
            title = "Market Share by Company",
            subtitle = "Revenue in $millions")

# ==============================================================================
# 5. PARALLEL COORDINATES - Multivariate Analysis
# ==============================================================================

# Normalize iris data for comparison
iris_scaled <- iris %>%
  mutate(across(where(is.numeric), scale))

viz_parallel(iris_scaled,
             vars = c("Sepal.Length", "Sepal.Width",
                     "Petal.Length", "Petal.Width"),
             group = Species,
             title = "Iris Species Comparison",
             subtitle = "Standardized measurements",
             alpha = 0.3)

# ==============================================================================
# 6. GANTT CHART - Project Timelines
# ==============================================================================

# Project schedule
project_schedule <- data.frame(
  task = c("Planning", "Design", "Development", "Testing",
           "Deployment", "Training", "Launch"),
  start = as.Date(c("2024-01-01", "2024-01-15", "2024-02-01",
                   "2024-03-15", "2024-04-01", "2024-04-10",
                   "2024-04-20")),
  end = as.Date(c("2024-01-14", "2024-01-31", "2024-03-14",
                 "2024-03-31", "2024-04-09", "2024-04-19",
                 "2024-04-22")),
  phase = c("Planning", "Planning", "Development", "Development",
            "Launch", "Launch", "Launch")
)

viz_gantt(project_schedule,
          task = task,
          start = start,
          end = end,
          category = phase,
          title = "Project Timeline",
          subtitle = "Q1-Q2 2024 Product Launch")

# ==============================================================================
# 7. HEXBIN PLOT - Large Dataset Density
# ==============================================================================

# Generate large dataset
set.seed(123)
large_data <- data.frame(
  x = rnorm(5000, mean = 50, sd = 15),
  y = rnorm(5000, mean = 50, sd = 15)
)

viz_hexbin(large_data,
           x = x,
           y = y,
           bins = 30,
           title = "Spatial Distribution of 5000 Points",
           subtitle = "Hexagonal binning for density visualization",
           xlab = "X Coordinate",
           ylab = "Y Coordinate")

# ==============================================================================
# BEST PRACTICES FOR ADVANCED CHARTS:
# ==============================================================================

# SANKEY DIAGRAMS:
# - Data must have: source, target, value columns
# - Values should be positive numbers
# - Works best with 2-3 levels of hierarchy
# - Use clear, short labels
# - Good for: process flows, budget allocations, customer journeys

# NETWORK GRAPHS:
# - Needs TWO dataframes: nodes and edges
# - Nodes: unique identifiers
# - Edges: from/to pairs
# - Layout options: "fr" (force-directed), "circle", "kk", "star"
# - Keep to < 50 nodes for readability
# - Good for: social networks, citations, dependencies

# CHORD DIAGRAMS:
# - Shows bidirectional flows between entities
# - Data: from, to, value (can be asymmetric)
# - Best with 5-12 entities
# - Width represents flow strength
# - Good for: trade, migration, communication patterns

# TREEMAPS:
# - Area proportional to value
# - Color by category
# - Best with 10-50 items
# - Good for: market share, budget breakdown, file sizes

# PARALLEL COORDINATES:
# - Normalize data for fair comparison
# - Use alpha < 0.5 for overlapping lines
# - Best with 3-8 variables
# - Good for: multivariate patterns, clustering

# GANTT CHARTS:
# - Requires date columns (Date or POSIXct)
# - Tasks should be mutually exclusive or allow overlap
# - Category colors help group related tasks
# - Good for: project management, scheduling

# HEXBIN:
# - Excellent for large datasets (1000+ points)
# - Bins parameter affects granularity
# - Better than scatter for overplotting
# - Good for: spatial data, large correlations

# ==============================================================================
# SAVING HIGH-QUALITY VERSIONS:
# ==============================================================================

# For complex charts, increase size and DPI
# p <- viz_sankey(customer_flow, source, target, value,
#                 title = "Customer Journey")
# save_publication("sankey.pdf", p, width = 10, height = 7)
# save_publication("sankey.png", p, width = 10, height = 7, dpi = 300)

# ==============================================================================
