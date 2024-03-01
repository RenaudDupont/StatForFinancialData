## TP1: GARCH(1,1) Model Implementation in R

## Overview
This project implements a GARCH(1,1) model in R to analyze and forecast financial time series volatility. The repository contains scripts to install required R packages, perform the GARCH(1,1) model analysis, and generate a comprehensive report.

### Files Description

- InstallRequiredPackages.R: A script to automatically install the R packages necessary for running the GARCH(1,1) model analysis.
- main.Rmd: An R Markdown file that contains the code and documentation for implementing the GARCH(1,1) model. This file is used to generate the final report.
- main.html: The output report generated from the main.Rmd file, providing insights and results from the GARCH(1,1) model analysis.

## Getting Started

### Prerequisites

Ensure you have R and RStudio installed on your computer.

### Installation

1. Clone this repository to your local machine.
2. Open the InstallRequiredPackages.R script in RStudio and run it. This will install all the required R packages for this project.
3. Open main.Rmd in RStudio to view the code and documentation of the main functions.

### Generating the Report

1. To generate the Report.html from main.Rmd, click on the "Knit" button in RStudio.
2. RStudio will execute the code within main.Rmd and generate the report in HTML format.
3. By doing so, it will generate 3 files, the report.html, the png of the VaR and the returns, and the .rda file containing returns and VaR of the Garch model. 