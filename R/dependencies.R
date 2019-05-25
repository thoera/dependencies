#' Get the dependencies of a package
#'
#' Get the dependencies of a package.
#'
#' @param package A string. The package for which you want to retrieve the
#'   dependencies.
#' @param suggest A boolean (default = FALSE). If TRUE, retrieves `Suggests`
#'   dependencies (non-recursively).
#' @param base_packages A boolean (default = FALSE). If TRUE, includes base R
#'   packages in the dependencies.
#' @return A dataframe with each dependency and the type for each one of them.
#' @seealso \code{\link{plot_dependencies}},
#'   \code{\link{download_dependencies}}, \code{\link{install_dependencies}}
#' @examples
#' \dontrun{
#' get_dependencies(package = "shiny")
#' }
#' @export
get_dependencies <- function(package, suggest = FALSE, base_packages = FALSE) {
  deps <- miniCRAN::makeDepGraph(pkg = package, suggest = suggest,
                                 includeBasePkgs = base_packages)
  return(igraph::as_data_frame(deps))
}

#' Plot a graph of the dependencies
#'
#' Plot a graph of the dependencies.
#'
#' @param dependencies A dataframe. The dependencies of a package.
#' @param colors A vector (default = NULL). Control the colors for each type of
#'   dependency.
#' @param ... Other arguments passed to \code{\link[graphics]{plot}}.
#' @return A plot of the dependencies.
#' @seealso \code{\link{get_dependencies}}, \code{\link{plot_dependencies}},
#'   \code{\link{download_dependencies}}, \code{\link{install_dependencies}}
#' @examples
#' \dontrun{
#' dependencies <- get_dependencies(package = "shiny")
#' plot_graph(dependencies = dependencies)
#' }
plot_graph <- function(dependencies, colors = NULL, ...) {
  if (is.null(colors)) {
    colors <- c("Depends" = "#A885D8", "Imports" = "#4BB4E6",
                "LinkingTo" = "#FFB4E6", "Suggest" = "#50BE87")
  }

  graph <- igraph::graph_from_data_frame(dependencies)
  types <- factor(igraph::E(graph)$type,
                  levels = sort(unique(dependencies[["type"]])))
  igraph::E(graph)$color <- colors[types]

  graphics::plot(graph, edges.color = igraph::E(graph)$color,
                 vertex.shape = "none", vertex.label.color = "#000000",
                 edge.arrow.size = 0.3, ...)
  graphics::legend(x = "bottomleft", legend = levels(types), bty = "n",
                   pch = -9658, col = colors, xpd = TRUE)
}

#' Print a tree of the dependencies
#'
#' Print a tree of the dependencies.
#'
#' @param dependencies A dataframe. The dependencies of a package.
#' @return A tree of the dependencies.
#' @seealso \code{\link{get_dependencies}}, \code{\link{plot_dependencies}},
#'   \code{\link{download_dependencies}}, \code{\link{install_dependencies}}
#' @examples
#' \dontrun{
#' dependencies <- get_dependencies(package = "shiny")
#' tree_dependencies(dependencies = dependencies)
#' }
tree_dependencies <- function(dependencies) {
  data.tree::FromDataFrameNetwork(dependencies)
}

#' Plot a graph or a tree of the dependencies
#'
#' Plot a graph or a tree of the dependencies.
#'
#' @param dependencies A dataframe. The dependencies of a package.
#' @param type A string (default = "network"). Control the type of the output:
#'   "network" or "tree".
#' @param colors A vector (default = NULL). Control the colors for each type of
#'   dependency.
#' @param ... Other arguments passed to \code{\link[graphics]{plot}}.
#' @return A plot or a tree of the dependencies.
#' @seealso \code{\link{get_dependencies}}, \code{\link{plot_dependencies}},
#'   \code{\link{download_dependencies}}, \code{\link{install_dependencies}}
#' @examples
#' \dontrun{
#' dependencies <- get_dependencies(package = "shiny")
#' plot_dependencies(dependencies = dependencies, type = "network")
#' plot_dependencies(dependencies = dependencies, type = "tree")
#' }
#' @export
plot_dependencies <- function(dependencies, type = c("network", "tree"),
                              colors = NULL, ...) {
  type <- match.arg(type)
  if (!inherits(dependencies, "data.frame")) {
    stop("'dependencies' must be a data frame")
  }

  if (nrow(dependencies) == 0L) {
    stop("'dependencies' is an empty data frame")
  }

  if (type == "network") {
    plot_graph(dependencies = dependencies, colors = colors, ...)
  } else {
    tree_dependencies(dependencies = dependencies)
  }
}

#' Get the nodes from the dependencies
#'
#' Get the nodes from the dependencies.
#'
#' @param dependencies A dataframe. The dependencies of a package.
#' @return A vector of the nodes from a dataframe of dependencies.
#' @seealso \code{\link{get_dependencies}}, \code{\link{plot_dependencies}},
#'   \code{\link{download_dependencies}}, \code{\link{install_dependencies}}
#' @examples
#'\dontrun{
#' dependencies <- get_dependencies(package = "shiny")
#' get_nodes(dependencies = dependencies)
#' }
get_nodes <- function(dependencies) {
  tree <- tree_dependencies(dependencies = dependencies)
  nodes <- tree$Get("level", traversal = "post-order")
  nodes <- unique(names(sort(nodes, decreasing = TRUE)))
  return(nodes)
}

#' Download the dependencies of a package
#'
#' Download the dependencies of a package.
#'
#' @param dependencies A dataframe. The dependencies of a package.
#' @param directory A string (default = "network"). The directory where
#'   downloaded packages are to be stored. Creates the directory if it does not
#'   exist.
#' @param replace A boolean (default = FALSE). Control if existing packages in
#'   the directory should be replaced.
#' @return A two-column matrix of names and destination file names of those
#'   packages successfully downloaded.
#' @seealso \code{\link{get_dependencies}}, \code{\link{plot_dependencies}},
#'   \code{\link{download_dependencies}}, \code{\link{install_dependencies}}
#' @examples
#' \dontrun{
#' dependencies <- get_dependencies(package = "shiny")
#' download_dependencies(dependencies = dependencies, directory = "packages")
#' }
#' @export
download_dependencies <- function(dependencies, directory, replace = FALSE) {
  if (!inherits(dependencies, "data.frame")) {
    stop("'dependencies' must be a data frame")
  }

  if (nrow(dependencies) == 0L) {
    stop("'dependencies' is an empty data frame")
  }

  if (!dir.exists(directory)) {
    dir.create(directory)
  }

  nodes <- get_nodes(dependencies = dependencies)

  if (isFALSE(replace)) {
    existing_packages <- list.files("packages", pattern = ".tar.gz$")
    nodes <- nodes[!sapply(nodes, function(x) any(grepl(x, existing_packages)))]
  }

  utils::download.packages(pkgs = nodes, destdir = directory,
                           repos = "https://cran.r-project.org")
}

#' Get the current version on CRAN for a vector of packages
#'
#' Get the current version on CRAN for a vector of packages.
#'
#' @param packages A vector of packages.
#' @return A named vector of packages where the value is the current version on
#'   CRAN.
#' @seealso \code{\link{get_dependencies}}, \code{\link{plot_dependencies}},
#'   \code{\link{download_dependencies}}, \code{\link{install_dependencies}}
#' @examples
#' \dontrun{
#' get_version(packages = c("shiny", "igraph"))
#' }
#' @export
get_version <- function(packages) {
  available_packages <- utils::available.packages()

  versions <- sapply(packages, function(node) {
    available_packages[available_packages[, "Package"] == node, "Version"]
  })
  return(versions)
}

#' Generate the code to install the dependencies of a package
#'
#' Generate the code to install the dependencies of a package.
#'
#' @param dependencies A dataframe. The dependencies of a package.
#' @param installed A vector of already installed packages.
#' @return The code to install the dependencies of a package.
#' @seealso \code{\link{get_dependencies}}, \code{\link{plot_dependencies}},
#'   \code{\link{download_dependencies}}, \code{\link{install_dependencies}}
#' @examples
#' \dontrun{
#' dependencies <- get_dependencies(package = "shiny")
#' install_dependencies(dependencies = dependencies)
#' }
#' @export
install_dependencies <- function(dependencies, installed = NULL) {
  if (!inherits(dependencies, "data.frame")) {
    stop("'dependencies' must be a data frame")
  }

  if (nrow(dependencies) == 0L) {
    stop("'dependencies' is an empty data frame")
  }

  nodes <- get_nodes(dependencies = dependencies)

  if (!is.null(installed)) {
    nodes <- nodes[!nodes %in% installed]
  }

  versions <- get_version(packages = nodes)

  for (node in nodes) {
    pkg_source <- paste0(node, "_", versions[node], ".tar.gz")
    cat(
      'install.packages("', pkg_source, '", repos = NULL, type = "source")',
      '\n', sep = ""
    )
  }
}
