# GCMS-Data-Visualizer
A streamlined way to plot mass spectrometry data. 

There's no manuscript to cite for GCMS-Data-Visualizer. Instead, simply cite the website as: Schlecht, N. https://github.com/schlech3/GCMS-Data-Visualizer and whichever software version you use.


## Table of Contents: 
1. Description
2. Dependencies/Requirements
3. Examples
4. Other Scripts
5. Author Notes
   

## Description
These scripts were designed as a straightforward approach to generating publication worthy chromatograms with fine controls while requiring limited computational expertise. by having a finer control over the figure generation. The users have ready access to adjust text size, colors, line widths, alterations to x and y axis ranges. You can plot not only TICs, but XICs and are capable of overlaying several chromatograms and control the placement of these chromatograms, allowing you to have them directly overlayed, or vertically, and horizontally shifted them. The controlled stacking of them is particularly useful for comparing several chromatograms in a single figure. 

The primary script is GCMS_Data_Visualizer.R . This can input direct data (i.e. Agilent's .D data format), processes them, and then takes your plot parameters and outputs your desired figure. I also have included older scripts that function quite differently  but may still have a niche usecase.

## Dependencies, Requirements, and limitations

### Dependencies 
```
require(chromConverter)
require(tidyverse)
require(scales)
require(ncdf4)
```

A key aspect to streamlining data input was using Ethan Bass' [Chrom Converter](https://github.com/ethanbass/chromConverter/blob/master/README.md#Installation) to directly take mass spec data, as it is capable of converting it into a useful format. Thus far, I have automated .CDF and agilent .D files, however I have not had access to other instrument data, so either you can venture to install the correct parsers and add a new automation line yourself or leave a comment and you can send me the data so I can troubleshoot any new options. These parsers are almost certainly going to be the source of major bugs in the future. Though i can try to help on some of these, Ethan Bass and the other external parsers original creators are more likely able to resolve these issues than I so I encourage you to contact them about issues. 


### Requirements/Limitations
Though this program has no limit in the accuracy (how many decimal points you care to display), time range, or how many chromatograms you stack, RAM limitations will certainly come into play due to large generated tables. This is most relevant for HRMS, which may have issues loading several samples unless you have very impressive RAM. 


For example: a 50-550 m/z with a 200ms scan time, 25 minute GC run with accuracy down to just first decimal point (i.e. table includes 50.0, 50.1, 50.2... 500) will generate a 37.5 million row data table using a minimum of 1Gb RAM. Current iterations actively remove memory as we filter your dataset to the relevant times, ions, etc. the intial loading of files 


## Examples


## Other Scripts

I earlier alluded to my initial scripts, CSV_Chromatogram_generator.R and CSV_Spectra_generator.R

These function quite similarly to the main script, GCMS_Data_Visualizer.R but require a **CSV** file as an input. I would obtain these CSVs by opening the mass spec file in the freely available software [Openchrom](https://www.openchrom.net/). From here you can export your chromatogram as a CSV and input it into these respective programs. 


## Author notes
Though 


## Current use case limitations
The current iteration of these scripts are specifically designed for single quadrupole GC-MS data. These scripts utilize R 4.4.1 and the packages tidyverse, ggplot2, and scales.
