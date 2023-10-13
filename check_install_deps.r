is_installed <- function(pkg_name) {
  return(pkg_name %in% installed.packages()[, "Package"])
}

# CRAN packages
deps <- c(
  "nlme",
  "drc",
  "devtools",
  "magrittr",
  "ggplot2",
  "ggprism",
  "ggbeeswarm",
  "ggpubr"
)
new <- deps[!(deps %in% installed.packages()[, "Package"])]
if (length(new)) install.packages(new, repo = "https://www.stats.bris.ac.uk/R/")

# GitHub packages
github_deps <- list(
  # need rlang >= 1.1.1; got an error at first & had to manually delete the
  # rlang folder in packages library for this to work
  list(user = "r-lib", repo = "rlang"),
  list(user = "DoseResponse", repo = "drcData"),
  list(user = "DoseResponse", repo = "medrc")
)

for (dep in github_deps) {
  pkg_name <- dep$repo
  if (!is_installed(pkg_name)) {
    devtools::install_github(paste0(dep$user, "/", dep$repo))
  }
}