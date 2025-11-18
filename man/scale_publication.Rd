\name{scale_publication}
\alias{scale_color_publication}
\alias{scale_fill_publication}
\title{Publication Color and Fill Scales}
\description{
  Colorblind-friendly discrete color and fill scales for publication-quality
  figures. Based on research by Wong (2011) and other color perception studies.
}
\usage{
scale_color_publication(palette = "default", ...)
scale_fill_publication(palette = "default", ...)
}
\arguments{
  \item{palette}{Palette name. Options:
    \itemize{
      \item \code{"default"}: Wong (2011) palette, optimized for colorblind readers (8 colors)
      \item \code{"vibrant"}: High-contrast vibrant colors for impact (7 colors)
      \item \code{"muted"}: Paul Tol muted scheme for subtle distinctions (9 colors)
      \item \code{"dark"}: Dark colors for light backgrounds (8 colors)
    }
  }
  \item{...}{Additional arguments passed to \code{scale_color_manual} or \code{scale_fill_manual}}
}
\value{
  A ggplot2 scale object that can be added to plots.
}
\details{
  All palettes are designed to be distinguishable by people with color vision
  deficiencies (deuteranopia, protanopia, tritanopia).

  \strong{Default Palette (Wong 2011):}
  \itemize{
    \item Based on Wong, B. (2011) Nature Methods
    \item Tested for all types of colorblindness
    \item 8 colors: blue, orange, green, pink, yellow, light blue, red-orange, gray
    \item Best for most scientific publications
    \item Maximum of 8 categories
  }

  \strong{Vibrant Palette:}
  \itemize{
    \item High contrast for presentations
    \item 7 colors with strong saturation
    \item Good for posters and slides
    \item May be too intense for print
  }

  \strong{Muted Palette (Paul Tol):}
  \itemize{
    \item Softer, more subtle colors
    \item 9 colors for complex categorizations
    \item Professional appearance
    \item Best for multi-category data
  }

  \strong{Dark Palette:}
  \itemize{
    \item Dark, rich colors
    \item 8 colors with high saturation
    \item Good for light backgrounds
    \item Strong visual impact
  }

  \strong{Colorblind Testing:}
  All palettes have been tested using colorblindness simulation tools to ensure
  that colors remain distinguishable under deuteranopia (most common, 6\% of males),
  protanopia (1\% of males), and tritanopia (rare).
}
\examples{
library(ggplot2)
library(dplyr)

# Basic usage with default palette
mtcars_clean <- mtcars \%>\%
  mutate(cyl_factor = factor(cyl))

ggplot(mtcars_clean, aes(x = wt, y = mpg, color = cyl_factor)) +
  geom_point(size = 3) +
  scale_color_publication() +
  theme_publication()

# Using fill scale for bar charts
iris_summary <- iris \%>\%
  group_by(Species) \%>\%
  summarise(mean_length = mean(Sepal.Length))

ggplot(iris_summary, aes(x = Species, y = mean_length, fill = Species)) +
  geom_bar(stat = "identity") +
  scale_fill_publication() +
  theme_publication() +
  theme(legend.position = "none")

# Vibrant palette for presentations
ggplot(mtcars_clean, aes(x = factor(cyl), fill = factor(cyl))) +
  geom_bar() +
  scale_fill_publication(palette = "vibrant") +
  theme_publication(base_size = 14)

# Muted palette for many categories
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, color = Species)) +
  geom_point(size = 2) +
  scale_color_publication(palette = "muted") +
  theme_publication()

# Dark palette on light background
ggplot(mtcars_clean, aes(x = wt, y = mpg, color = cyl_factor)) +
  geom_point(size = 4) +
  scale_color_publication(palette = "dark") +
  theme_publication() +
  theme(panel.background = element_rect(fill = "white"))

# Custom labels with publication colors
ggplot(mtcars_clean, aes(x = wt, y = mpg, color = cyl_factor)) +
  geom_point(size = 3) +
  scale_color_publication(labels = c("4 cyl", "6 cyl", "8 cyl"),
                         name = "Engine") +
  theme_publication()
}
\note{
  \strong{Important Notes:}
  \itemize{
    \item Default palette supports maximum 8 categories
    \item For more than 8-9 categories, consider grouping or using viridis scales
    \item Always test your figures in grayscale to ensure clarity
    \item Some journals require specific color palettes - check guidelines
    \item For continuous data, use viridis scales instead
  }

  \strong{Testing for Colorblindness:}
  You can test your figures using online tools:
  \itemize{
    \item Coblis - Color Blindness Simulator
    \item Adobe Color - Accessibility Tools
    \item Colorblind Web Page Filter
  }
}
\references{
  Wong, B. (2011). Points of view: Color blindness. \emph{Nature Methods} 8(6), 441.
  https://doi.org/10.1038/nmeth.1618

  Tol, P. (2012). Colour Schemes. Technical Note SRON/EPS/TN/09-002.
}
\seealso{
  \code{\link{theme_publication}} for publication-ready themes,
  \code{\link[viridis]{scale_color_viridis}} for continuous color scales,
  \code{\link[ggplot2]{scale_color_manual}} for manual color specification
}
