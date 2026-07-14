# Outlier Detection in Sensor Data Using Functional Depth Measures

> **Archive notice.** This is an archived master's-level student project. It is preserved as historical work and as an example for students interested in an applied statistical-computing project. It appears in the [archive of earlier projects](https://jakobjuergens.com/previous-projects/) and is not part of Jakob R. Juergens's [current research portfolio](https://jakobjuergens.com/research/).

## Project context

- **Course:** Final project in Microeconometrics and Scientific Computing
- **Institution:** University of Bonn
- **Completed:** 2022
- **Author:** [Jakob R. Juergens](https://jakobjuergens.com/)
- **Supervisor:** Prof. Dr. Philipp Eisenhauer
- **Cooperation:** Daimler AG

## Start here

- Read and run [`Project_Main.ipynb`](Project_Main.ipynb), the principal project write-up and demonstration notebook.
- Inspect the package-style [`OutDetectR` source](Code/OutDetectR/) and its [function documentation](Code/OutDetectR/man/). The archived source package is available as [`OutDetectR_1.0.tar.gz`](Code/OutDetectR_1.0.tar.gz).
- Launch the local Shiny application from [`app.R`](app.R) to inspect candidate outliers and curves interactively.
- Explore the included [synthetic datasets and saved demonstration results](data/) and the corresponding [data-generation scripts](Code/auxiliary/).
- See the [contextualized project archive](https://jakobjuergens.com/previous-projects/) for a retrospective summary. There is no separate report or presentation in this repository; the notebook is the main narrative artifact.

## Overview

The project is motivated by bolt-tightening sensor recordings collected during production processes. Each recording contains torque measurements observed at a sequence of angles. Because the measurement locations can be irregular and differ between recordings, treating an entire recording as a functional observation preserves the shape information used to identify atypical curves.

The statistical core is adapted from the functional-depth outlier procedure of [Febrero, Galeano, and González-Manteiga (2008)](https://doi.org/10.1002/env.878). Functional depth ranks curves by how central they are within a sample, allowing unusually shallow curves to be considered as candidate outliers. The published procedure supplies the methodological basis; this student project contributes an R/Rcpp implementation and an applied workflow for preparing irregular sensor curves, aligning and stretching observations where appropriate, approximating shared grids, sampling comparable groups, running detection and update procedures, and visualizing results.

[`Project_Main.ipynb`](Project_Main.ipynb) explains the method, motivates the implementation choices, and runs synthetic examples. [`Code/OutDetectR/`](Code/OutDetectR/) contains the package-style implementation used by the notebook. [`app.R`](app.R) reads the included precomputed demonstration results and provides the interactive visualization layer.

## Data availability

The original Daimler bolt-tightening dataset is not distributed with this repository. The notebook sets `real_data <- FALSE` and keeps the industrial-data application in conditional cells that require a separately supplied RDS file. An archived plot in [`material/real_data_plot.png`](material/real_data_plot.png) illustrates that application, but it is not a substitute for the underlying data.

The repository does include three synthetic demonstration datasets under [`data/`](data/), together with their saved detection results and Shiny-ready tables. Their generators are available in [`Code/auxiliary/`](Code/auxiliary/) and in the `OutDetectR` package source. These materials support the notebook's synthetic examples, the package demonstrations, and the local Shiny app without access to the original industrial data. Full reproduction of the industrial application is not possible from the repository alone and would also require the separately supplied data and substantial computation described in the notebook.

## Reproducibility note

R is the principal environment; the notebook metadata records an R 3.6.1 kernel, and parts of `OutDetectR` use Rcpp. Run the notebook from the repository root so its relative paths resolve. The Shiny app can be launched from the same location with `shiny::runApp(".")` or by executing [`app.R`](app.R). Package-style code resides in [`Code/OutDetectR/`](Code/OutDetectR/), and the notebook installs the archived source package from [`Code/OutDetectR_1.0.tar.gz`](Code/OutDetectR_1.0.tar.gz).

Dependencies are listed in [`environment.yml`](environment.yml) and in the package [`DESCRIPTION`](Code/OutDetectR/DESCRIPTION), but their versions are largely unpinned. This archival documentation pass did not comprehensively modernize or revalidate the notebook, package, compiled artifacts, or Shiny app under current R and package versions.

## Acknowledgements

- Dr. Vincent Dekker and Bernhard Häußler, the contacts at Daimler AG who made the project possible.
- Prof. Dr. Philipp Eisenhauer and Manuel Huth, who made the cooperation possible from the university side.
- Marc Kerstan, who allowed the use of his code for preparing sensor data collected in production processes at Daimler.
- Prof. Dr. Dominik Liebl, who introduced the use of functional depth for identifying outliers in functional data.

## Sources

- Cuevas, A., Febrero-Bande, M., and Fraiman, R. (2006). On the use of bootstrap for estimating functions with functional data. *Computational Statistics & Data Analysis*, 51, 1063-1074.
- Febrero-Bande, M., Galeano, P., and González-Manteiga, W. (2008). Outlier detection in functional data by depth measures, with application to identify abnormal NOx levels. *Environmetrics*, 19, 331-345. [https://doi.org/10.1002/env.878](https://doi.org/10.1002/env.878)
- Gijbels, I., and Nagy, S. (2017). On a general definition of depth for functional data. *Statistical Science*, 32, 630-639.

## Retrospective

This repository remains an example of applied functional-data work that combines an industry-motivated problem, statistical implementation, simulation-based demonstrations, and interactive visualization.
