# CRANpiled

A lightweight package for creating and maintaining CRAN repositories of precompiled R packages.

## Usage

The following will compile the latest version of `ggplot2` from cloud.r-project.org, `dplyr` version 1.0.1 from cloud.r-project.org, and a fork of `shiny` found on Github, and add them to a repository found at ~/myCRAN.

```
library(CRANpiled)

options("repos" = "cloud.r-project.org")

repo_dir <- create_repository("~/myCRAN")

packages <- c(
  "ggplot2",
  "http://cloud.r-project.org/src/contrib/Archive/dplyr/dplyr_1.0.1.tar.gz",
  "https://github.com/thomascjohnson/shiny/archive/master.zip"
)

add_packages(packages, repo_dir)
```
