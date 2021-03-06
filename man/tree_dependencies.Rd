% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dependencies.R
\name{tree_dependencies}
\alias{tree_dependencies}
\title{Print a tree of the dependencies}
\usage{
tree_dependencies(dependencies)
}
\arguments{
\item{dependencies}{A dataframe. The dependencies of a package.}
}
\value{
A tree of the dependencies.
}
\description{
Print a tree of the dependencies.
}
\examples{
\dontrun{
dependencies <- get_dependencies(package = "shiny")
tree_dependencies(dependencies = dependencies)
}
}
\seealso{
\code{\link{get_dependencies}}, \code{\link{plot_dependencies}},
  \code{\link{download_dependencies}}, \code{\link{install_dependencies}}
}
