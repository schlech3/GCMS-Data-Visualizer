# ReadMe.md

## Description
These scripts were initially designed as a straightforward approach to generating publication worthy chromatogram and spectra pictures by having a finer control over the figure generation. This not only includes text size, colors, line widths, alterations to x and y axis ranges, but you can also overlay several chromatograms and control if they are directly on top of one another, have a vertical shift between each, as well as includes horizontal shifts to offset them for a pseudo 3-D view. 

## Necessary Data Prep
Due to the proprietary nature of most GC datasets, you will need to export the data as a **CSV**. This should be done using the free and open-source software [Openchrom](https://www.openchrom.net/). Openchrom is a great tool for quickly looking at any dataset, but it is not suited for generating figures for publication. In openchrom you can export your files as a CSV which can be saved somewhere locally where you can use these scripts to load the data in and either generate your chromatogram(s) individually or in a combined figure, as well as plot any desired mass spectra. 

## Current use case limitations
The current iteration of these scripts are specifically designed for single quadrupole GC-MS data. These scripts utilize R 4.4.1 and the packages tidyverse, ggplot2, and scales. 
