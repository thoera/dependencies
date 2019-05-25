# dependencies

## Overview

This package offers a simple interface to retrieve and show the dependencies of a package. It also offers the possibility to download these dependencies and to generate the code to install them (from source).

## Installation

To install the package, simply run the following from an R console:

```r
# install.packages("remotes")
remotes::install_github("thoera/dependencies")
```

## Usage

```r
library("dependencies")

# retrieve the dependencies for the `shiny` package
dependencies <- get_dependencies(package = "shiny")

# visualize them with a graph
plot_dependencies(dependencies = dependencies, type = "network")

# or a tree
plot_dependencies(dependencies = dependencies, type = "tree")

# download the dependencies in a specific directory called "packages"
download_dependencies(dependencies = dependencies, directory = "packages")

# generate the code to install them
install_dependencies(dependencies = dependencies)
```
