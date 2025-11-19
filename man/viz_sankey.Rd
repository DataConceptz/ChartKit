\name{viz_sankey}
\alias{viz_sankey}
\title{Sankey Diagram}
\description{
  Create publication-ready Sankey diagrams for visualizing flows between
  categories. Sankey diagrams show the magnitude of flows from source to target
  nodes, with ribbon width proportional to flow quantity.
}
\usage{
viz_sankey(data, source, target, value,
           title = NULL, subtitle = NULL)
}
\arguments{
  \item{data}{Data frame with source, target, and value columns}
  \item{source}{Column name for source nodes (unquoted)}
  \item{target}{Column name for target nodes (unquoted)}
  \item{value}{Column name for flow values (unquoted)}
  \item{title}{Plot title (character string)}
  \item{subtitle}{Plot subtitle (character string)}
}
\value{
  A ggplot2 object representing the Sankey diagram using ggalluvial.
}
\details{
  \strong{Data Structure:}

  Sankey diagrams require edge-list format:
  \preformatted{
   source      target       value
   Website     Checkout      1200
   Website     Exit           400
   Email       Checkout       800
   Email       Exit           200
  }

  Each row represents a flow from source to target with specified magnitude.

  \strong{When to Use Sankey Diagrams:}
  \itemize{
    \item Customer journey mapping (website → product → checkout)
    \item Budget allocation flows (department → project → expense)
    \item Energy or resource flows (source → conversion → end-use)
    \item Migration patterns (origin → destination)
    \item Process flows in manufacturing or logistics
    \item Information or material flows in systems
  }

  \strong{Design Principles:}
  \itemize{
    \item Ribbon width is proportional to flow magnitude
    \item Colors distinguish different sources or categories
    \item Nodes represent states or categories
    \item Left-to-right flow represents progression
    \item Splits show where flows diverge
    \item Joins show where flows converge
  }

  \strong{Interpretation:}
  \itemize{
    \item Wider ribbons = larger flows
    \item Thick nodes = large throughput
    \item Diverging flows = distribution to multiple targets
    \item Converging flows = consolidation from multiple sources
  }
}
\examples{
library(dplyr)

# Customer journey example
customer_flow <- data.frame(
  source = c("Website", "Website", "Social Media",
             "Social Media", "Email", "Email"),
  target = c("Product Page", "Blog", "Product Page",
             "Blog", "Product Page", "Exit"),
  value = c(1200, 800, 600, 300, 400, 200)
)

viz_sankey(customer_flow, source, target, value,
           title = "Customer Journey Analysis",
           subtitle = "Traffic flow through website sections")

# Budget allocation example
budget_flow <- data.frame(
  source = rep(c("Total Budget"), 3),
  target = c("Research", "Development", "Marketing"),
  value = c(500000, 800000, 400000)
) \%>\%
  bind_rows(
    data.frame(
      source = rep(c("Research", "Development", "Marketing"), each = 2),
      target = rep(c("Personnel", "Equipment"), 3),
      value = c(300000, 200000,  # Research
                500000, 300000,  # Development
                250000, 150000)  # Marketing
    )
  )

viz_sankey(budget_flow, source, target, value,
           title = "Annual Budget Allocation",
           subtitle = "From departments to expense categories")

# Energy flow example
energy_flow <- data.frame(
  source = c("Coal", "Natural Gas", "Nuclear", "Renewables",
             "Coal", "Natural Gas", "Nuclear", "Renewables"),
  target = c(rep("Electricity", 4), rep("Heat", 4)),
  value = c(450, 320, 280, 150,   # Electricity
            200, 180, 50, 80)     # Heat
)

viz_sankey(energy_flow, source, target, value,
           title = "Energy Source to End Use",
           subtitle = "Distribution of energy sources (TWh)")

# Multi-stage flow (3+ levels)
process_flow <- data.frame(
  source = c("Raw Material A", "Raw Material B",
             "Processing", "Processing",
             "Product X", "Product Y"),
  target = c("Processing", "Processing",
             "Product X", "Product Y",
             "Market 1", "Market 2"),
  value = c(1000, 800,
            600, 400,
            600, 400)
)

viz_sankey(process_flow, source, target, value,
           title = "Manufacturing Process Flow",
           subtitle = "From raw materials to markets")

# Migration pattern
migration <- data.frame(
  source = c("Rural North", "Rural South", "Small Cities"),
  target = c("Major City", "Major City", "Major City"),
  value = c(50000, 75000, 100000)
)

viz_sankey(migration, source, target, value,
           title = "Population Migration Patterns",
           subtitle = "Flows to major metropolitan area")

# Information flow in organization
info_flow <- data.frame(
  source = c("Customer Feedback", "Sales Data", "Market Research",
             "Analysis", "Analysis", "Analysis"),
  target = c("Analysis", "Analysis", "Analysis",
             "Product Team", "Marketing Team", "Executive Team"),
  value = c(100, 150, 80,
            120, 110, 100)
)

viz_sankey(info_flow, source, target, value,
           title = "Information Flow in Organization",
           subtitle = "From data sources to decision makers")
}
\note{
  \strong{Data Requirements:}
  \itemize{
    \item Source and target must be character or factor
    \item Values must be positive numbers
    \item Cannot have circular flows (A→B→A)
    \item Node names must be unique across levels
  }

  \strong{Best Practices:}
  \itemize{
    \item Limit to 3-5 levels for readability
    \item Keep number of nodes per level manageable (<10)
    \item Use consistent units across all flows
    \item Ensure all flows are accounted for (conservation of mass)
    \item Order nodes logically (by size or category)
    \item Add value labels for precise quantities when needed
  }

  \strong{Common Issues:}
  \itemize{
    \item Overlapping ribbons: Reduce number of flows or increase plot size
    \item Unclear labels: Use horizontal = TRUE for better readability
    \item Lost flows: Ensure all flow stages are represented in data
    \item Inconsistent totals: Verify conservation between levels
  }

  \strong{Design Tips:}
  \itemize{
    \item Use colors to highlight specific flows or categories
    \item Add annotations for key insights
    \item Consider interactive version (plotly) for complex diagrams
    \item Export at high resolution (600 DPI) for clarity
  }

  \strong{Alternatives:}
  Consider these alternatives for specific use cases:
  \itemize{
    \item \strong{Alluvial plot}: For categorical changes over time
    \item \strong{Chord diagram}: For bidirectional flows
    \item \strong{Network graph}: For complex interconnections
    \item \strong{Tree diagram}: For hierarchical relationships
  }
}
\seealso{
  Related functions:
  \itemize{
    \item \code{\link{viz_alluvial}} - For categorical flows over time
    \item \code{\link{viz_chord}} - For circular flow diagrams
    \item \code{\link{viz_network}} - For network relationships
  }

  Examples:
  \itemize{
    \item See \code{inst/examples/advanced_examples.R} for comprehensive Sankey examples
  }

  Underlying packages:
  \itemize{
    \item \code{\link[ggalluvial]{geom_alluvium}} - Alluvial geometry used
  }
}
\references{
  Schmidt, M. (2008). The Sankey Diagram in Energy and Material Flow Management.
  \emph{Journal of Industrial Ecology} 12(1), 82-94.

  Riehmann, P., Hanfler, M., & Froehlich, B. (2005). Interactive Sankey diagrams.
  \emph{IEEE Symposium on Information Visualization}.
}
