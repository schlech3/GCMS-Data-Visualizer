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

There are a lot of customizable variables, so while I will not be giving examples of everything, these few examples should help illustrate some of the applications of this program. All examples shown will be using the raw data in the example_data folder. These samples were from Agrobacterium-mediated transient expression in N. benthamiana where we expressed terpenoid biosynthesis genes and were subsequently ran on an Agilent GC7890 5975C detector (single quadrupole) and the data was used in the following paper: doi 10.1111/tpj.17031. The files are named by what genes were expressed in planta. 

While there are many variables, most we can leave as the default and we can change these suggested ones to follow along.


```
InputFileFolder <- 'C:/path/to/Example_Data/'

YourPlot <- tribble(~sample,       ~ion,           ~RANGE,                       'DXSGGPPS_CamTPS2.D', 'TIC', 'NA',
                    'DXSGGPPS.D', 'TIC', 'NA'
)

OutputFileFolder <- 'C:/path/to/Example_Data/'
plotname <- 'NoChange_DXSGGPPS_DXSGGPPS_CamTPS2.png'

Plot_Border_Type <- "axes"

BorderLineWidth <- 0.4
LineColors <- c('black', 'black')

VerticalSampleOffset <- 0   
HorizontalSampleOffset <- 0

Relative_OrCounts <- 'relative' 

xrange <- c(7, 20)  
yrange <- c(0,1)

```
![Alt text](https://github.com/schlech3/GCMS-Data-Visualizer/blob/main/Images/Example1_Stacked_TIC_DXSGGPPS_DXSGGPPS_CamTPS2.png)

### second plot

Ok, now let's make a stacked chromatogram of these 2 files, and change ones color. Given we have already run the dataset through, we aren't changing any ions, and are happy with our selection window, we don't need to reprocess everything (the vertical and horizontal offset features happen after data processing). So lets change one of the colors (we can use hexcodes), the Vertical SapmleOffset, and set Process data to false. While were at it, lets draw a box around the plot rather than just have axis lines. 

```
Process_Data <- FALSE
VerticalSampleOffset <- 0.1
LineColors <- c('black', '#C70039')
Plot_Border_Type <- "box"
plotname <- 'Vert_TIC_DXSGGPPS_DXSGGPPS_CamTPS2.png'  
yrange <- c(0,1.3)
```

![Alt text](https://github.com/schlech3/GCMS-Data-Visualizer/blob/main/Images/Example2_Vert_TIC_DXSGGPPS_DXSGGPPS_CamTPS2.png)

Notice that the order of the listed plots in the YourPlot variable  matches the order of the colors and vertical offset you see? writing your variables in the correct order will ensure it pops up the way you wish. You can rearrange as you see fit, but if you change the xrange() values to be outside its initial scope or change the ion you wish to plot (TIC, which XIC etc.) or the +/- range for that value, you will need to reprocess everything. 

Another key thing I'd like to point out here is the yrange. This is plotted as relative data, aka the largest peak in the whole dataset is set to 1 and everything else is proportional to that based on their actual counts. Yet, I am plotting y to go to 1.3? This is due to how I handle the vertical and horizontal shifts. 

All plots are initially overlayed exactly as they are supposed to be, but then are 'shifted'. This means that each chromatogram is shifted by the horizontal and vertical value you set such that the whole trace is moved. As a result, though everything is normalized to "1" if the largest peak size (1) is moved up by the VerticalSampleOffset of 0.1, it is now actually at the 1.1 spot on the figure. 

As a result of this, the yaxis itself tends to not be truly that informative and stacking chromatograms is more about retention times and comparing samples. This is why you tend to see (and I recommend) removing the y axis if you use this feature by cropping it later.

### third plot

Up until now we've been plotting only the total ion chromatograms (TICs). Lets try plotting an XIC. In this example I chose 257.2, as its a large fragment ion of CamTPS2's native product.


```
Process_Data <- TRUE

YourPlot <- tribble(~sample,       ~ion,           ~RANGE,   #Do not edit the  line
                    'DXSGGPPS_CamTPS2.D', '257.2', '0',
                    'DXSGGPPS.D', '257.2', '0',
)

```
![Alt text](https://github.com/schlech3/GCMS-Data-Visualizer/blob/main/Images/Example3_Vert_257_2_DXSGGPPS_DXSGGPPS_CamTPS2.png)

So in this case, we are now looking at exactly 257.2 aka +/- 0. we could adjust these values as we see fit. of note as well, if you don't care about having any decimal points, you could adjust DigitsBelowZero to be 0 or even expand it to have more accurate mass measurements. I would be careful of having too many digits below 0.


### Fourth Plot

Were going to try a harder example here where we want to compare several orthologous enzymes to see if they make similar products. Unfortunately, directly overlaying them on one another, even vertically will get confusing. By introducing a horizontal shift and a vertical shift you can have a pseudo-3D figure where you can trace the diagnol to see the respective traces. I highly suggest if you do this to throw it in biorender and include a semitransparent block highlighting peaks with the same retention time. I typically will crop out the time column as well as though they all share the same timeframe, having them be horizontally shifted can be confusing. 


```
plotname <- 'Example4_VertHoriz_BKs.png'

YourPlot <- tribble(~sample,       ~ion,           ~RANGE,   #Do not edit the  line
'DXSGGPPS_ArTPS2_AjReCYP76BK1.D', 'TIC', 'NA',
'DXSGGPPS_ArTPS2_ClBuCYP76BK1.D', 'TIC', 'NA',
'DXSGGPPS_ArTPS2_CoPy_A_CYP76BK1.D', 'TIC', 'NA',
'DXSGGPPS_ArTPS2_HOSACYP76BK1.D', 'TIC', 'NA',
'DXSGGPPS_ArTPS2_PeBaCYP76BK1.D', 'TIC', 'NA',
'DXSGGPPS_ArTPS2_ScBaCYP76BK1.D', 'TIC', 'NA',
'DXSGGPPS_ArTPS2_TeChCYP76BK1.D', 'TIC', 'NA',
'DXSGPPS_ArTPS2_ViAgCYP76BK1.D', 'TIC', 'NA',
'DXSGGPPS_ArTPS2_CaAmCYP76BK1.D', 'TIC', 'NA',
'DXSGGPPS_ArTPS2.D', 'TIC', 'NA'
)

VerticalSampleOffset <- 0.1   
HorizontalSampleOffset <- 0.16

xrange <- c(11.5, 17)
ChromLineWidth <- 0.3
Plot_Border_Type <- "none"
LineColors <- c('black', 'black', 'black', 'black', 'black', 'black','black','black','black','black')
plotdim <- c(9,5)

```

![Alt text](https://github.com/schlech3/GCMS-Data-Visualizer/blob/main/Images/Example4_VertHoriz_BKs.png)


Hopefully these few examples gave some insight on some relevant use cases for this program

## Other Scripts

I earlier alluded to my initial scripts, CSV_Chromatogram_generator.R and CSV_Spectra_generator.R

These function quite similarly to the main script, GCMS_Data_Visualizer.R but require a **CSV** file as an input. I would obtain these CSVs by opening the mass spec file in the freely available software [Openchrom](https://www.openchrom.net/). 

From here you can export your chromatogram as a CSV and input it into these respective programs. Nearly all of the same features and variables are otherwise present in these CSV scripts. 

## Current use case limitations
The current iteration of these scripts are for MS1 data specifically and have been tested with Agilent_d and CDF files only thus far. You can troubleshoot read_chroms() separately, or if you have the datatype available and can share I may be able to test other proprietary formats.
