require(chromConverter)
require(tidyverse)
require(scales)
require(ncdf4)

# >>>>>>>>>>>>>>>>>>> File inputs and Outputs <<<<<<<<<<<<<<<<<<<<<

InputFileFolder <- 'C:/Path/to/your/data/'    #Folder where .csv's are located

#These are your files and relevant ions to plot
                    #FileName,     #TIC/EIC mz,    +/- value i.e. ion = 40 RANGE = 0.5 aka 40 +/- 0.5. NA if TIC
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

OutputFileFolder <- 'C:/Path/to/where/output/goes/'    #Folder where .csv's are located
plotdim <- c(9,5)                                      # Final Figure dimensions. Printed in R is not final dimensions so please check
plotname <- 'Output_Plot.png'                          #Name the file. please include the type of file (.jpg .png, .svg). It will save in the same  Filefolder as input data.


# >>>>>>>>>>>>>>>>>>>>>   Parameters for analysis <<<<<<<<<<<<<<<<<<<

Process_Data <- TRUE   #If you are just finnicking with Plot parameters, set to FALSE to not have long wait period

DigitsBelowZero <- 1      #Relevant if you want to round up to the nearest m/z i.e. 45, 45.0, 45.00, etc. Stick to 0-3, but even 3 requires a lot of RAM

Relative_OrCounts <- 'relative'     #Unless you want counts, do relative

xrange <- c(11.5, 17)    #Pick x-axes range (minutes)
yrange <- c(0,1.9)    #if relative plot 0-1 or higher if stacking chroms

VerticalSampleOffset <- 0.1     #How big of a gap between
HorizontalSampleOffset <- 0.16      #How much (time) to shift each additional stacked chromatogram by

TickTimeGap <- 1           #Time gap between x-axis ticks
Legend_WrapAmount <- 20    #How many characters before Legend goes to new line

LineColors <- c('black', 'black', 'black', 'black', 'black', 'black','black','black','black','black')    #Put in order of YourPlot variable. For pretty hexadecimals https://coolors.co/156064-00c49a-f8e16c-ffc2b4-fb8f67

ChromLineWidth <- 0.4      #How wide do you want your chromatogram line(s)? 

xaxesLabelSize <- 14
yaxesLabelSize <- 14
xaxesNumberSize <- 14
yaxesNumberSize <- 10
LegendTitleSize <- 15
LegendTextSize <- 12

Plot_Border_Type <- "none" # Options: "none", "axes", "box". This will determine if you want a border/axes.
BorderLineWidth <- 0.4     #Determines how thick of lines you want for axes/box.

############################################################
#                DO NOT EDIT BELOW THIS LINE               #
#     (Internal script logic for data handling/plotting)   #
############################################################

#>>>>>>>>>>>>>>>>>>>>  Functions  <<<<<<<<<<<<<<<<<<<<<<<<<<

get_closest_rt <- function(data, target_rt) {
  actual_rt <- data$rt[which.min(abs(data$rt - target_rt))]
  return(actual_rt)
} #identifies the closest measured ion you ask for

strip_extension <- function(x) {
  sub("\\.[^.]+$", "", x)
}

summarise_mz <- function(data, digits = 0) {
  data %>%
    filter(rt > xrange[1] & rt < xrange[2]) %>%
    filter(intensity != 0) %>%
    mutate(mz = round(mz, digits)) %>%
    group_by(sample, rt, mz) %>%
    summarise(intensity = sum(intensity), .groups = "drop")
} #rounds the data to the closest desired digit and sums up the rounded intensities.

target_mz <- function(data, YourPlot) {
  results <- list()
  
  for (i in seq_len(nrow(YourPlot))) {
    this_sample <- strip_extension(YourPlot$sample[i])
    this_ion <- YourPlot$ion[i]
    this_range <- YourPlot$RANGE[i]
    
    # Filter for this sample
    sample_data <- data %>% filter(this_sample == sample)
    
    # Handle TIC
    if (toupper(this_ion) == 'TIC') {
      result <- sample_data %>%
        group_by(rt) %>%
        summarise(Intensity = sum(intensity, na.rm = TRUE), .groups = "drop")
    } 
    else {
      # If range is NA, default to 0
      if (is.na(this_range) || this_range == "NA") {
        this_range <- 0
      } else {
        this_range <- as.numeric(this_range)
      }
      ion_num <- as.numeric(this_ion)

      result <- sample_data %>%
        filter(mz >= ion_num - this_range, mz <= ion_num + this_range) %>%
        group_by(rt) %>%
        summarise(Intensity := sum(intensity, na.rm = TRUE), .groups = "drop")
    }
    
    # Add metadata
    result <- result %>%
      mutate(sample = this_sample, ion = this_ion)
    
    results[[i]] <- result
  }
  
  bind_rows(results)
} #Pick a TIC or ion (and +/- range surrounding it)

normalize_column <- function(data) {
  col_name <- names(data)[2]  # Get the second column name
  col_sym <- sym(col_name)    # Convert to symbol for tidy evaluation
  
  data %>%
    mutate(
      !!col_sym := !!col_sym / max(!!col_sym, na.rm = TRUE)
    )
}   #Simply alters the column 

chrom_stack_adjuster <- function(data, tbl, VerticalSampleOffset, HorizontalSampleOffset ) {
  data <- data %>%
    mutate(sample = sub("\\.[^.]+$", "", sample))  # remove file extensions
  
  tbl <- tbl %>%
    mutate(sample = sub("\\.[^.]+$", "", sample))  # remove extensions from user input
  
  #lookup order
  sample_order <- tbl %>%
    distinct(sample) %>%
    mutate(offset_index = row_number() - 1)
  
  data %>%
    left_join(sample_order, by = "sample") %>%
    mutate(
      rt = rt + HorizontalSampleOffset * (max(offset_index) - offset_index),
      Intensity = Intensity + VerticalSampleOffset * (max(offset_index) - offset_index)
      ) 
}

#get_closest rt, strip_extension, summarise_mz, target_mz, normalize_column, chrom_stack_adjuster

#>>>>>>>>>>>>>>>>>>>> End of Functions  <<<<<<<<<<<<<<<<<<<<<<<<<<


#If statement allows skipping  data import and initial processing, which is quite slow.
#There is regular clearing of memory and removal of variables at this stage due to table sizes.
  #Initializing "final variable" from processing. (pre-vert/horizontal adjustment and plotting)

if(Process_Data == TRUE){
  gc()
  ms_data3.0 <- tibble()
  #Need to loop through them 1 by 1 as importing all at once RAM intensive.
  for(i in seq_len(nrow(YourPlot))){
    chromatogram <- YourPlot %>% filter(row_number() == i)
    print(paste("Processing", chromatogram$sample))
  
  #initial data import
    if(toupper(strsplit(chromatogram$sample, '\\.')[[1:2]]) == "D"){
      dat <- read_chroms(paste(InputFileFolder, chromatogram$sample, sep = ""), 
                         format_in = "agilent_d",
                         parser = "rainbow",
                         format_out = 'data.table',
                         data_format = "long"
      )
    }
    
    if(toupper(strsplit(chromatogram$sample, '\\.')[[1:2]]) == "CDF"){
      dat <- read_chroms(paste(InputFileFolder, chromatogram$sample, sep = ""), 
                         format_in = "cdf",
                         parser = "ChromConverter",
                         format_out = 'data.table',
                         data_format = "long"
      )
    }
    gc()
    
    #transform into useful tibble
    ms_data <- imap_dfr(dat, ~ {
      .x$MS %>%
        as_tibble() %>%
        mutate(sample = .y)
    })
    
    rm(dat)
    gc()
     
    #limits to your desired range, removes 0 intensity ions, rounds to the desired sigfig.
    ms_data2 <- ms_data %>%
      summarise_mz(. , DigitsBelowZero)
    
    rm(ms_data)
    gc()
    
    # pulls only desired XIC or calculates the TIC you want. 
    ms_data3 <- ms_data2 %>%
      target_mz(. , chromatogram) 
    
    
    rm(ms_data2)
    gc()
    
    ms_data3.0 <- rbind(ms_data3.0, ms_data3)
    
}
gc()


if (tolower(Relative_OrCounts) == 'relative' ){
  ms_data3.0 <- normalize_column(ms_data3.0)
}
# ----------------- End of Data Processing -----------------------

#Converts to relative (1 = largest TIC/XIC in whole dataset)

}





#We don't remove ms_data3 in case we do "no processing"

ms_data4 <- chrom_stack_adjuster(ms_data3.0, YourPlot, VerticalSampleOffset, HorizontalSampleOffset) #Adjusts the files in the order you showed them


ms_data4.0 <- ms_data4
rm(ms_data4)
gc()
ms_data4.0
ms_data4.0$sample <- factor(ms_data4.0$sample, levels = strip_extension(YourPlot$sample)) #Adds factor levels to the samples to put them in order you list them

NamedColors <- setNames(LineColors[1:nrow(YourPlot)], levels(YourPlot$sample))  #ensures that the colors match the above color options
wrapped_labels <- str_wrap(levels(ms_data4.0$sample), Legend_WrapAmount) #So the legend wraps after so many characters

Plot <- ms_data4.0 %>% 
  ggplot() +
  geom_line(mapping = aes(x = rt, y = Intensity, color = sample), lwd = ChromLineWidth) +
  ylim(c(yrange[1], yrange[2])) +
  ylab('Intensity') +
  xlab('minutes') +
  scale_x_continuous(limits = c(xrange[1], max(ms_data4.0$rt), breaks = seq(from = omnibus::roundTo(xrange[1],TickTimeGap), to = omnibus::roundTo(xrange[2],TickTimeGap), by = TickTimeGap))) +
  theme(panel.background = element_blank(),
        axis.text.x = element_text(size = xaxesNumberSize),
        axis.text.y = element_text(size = yaxesNumberSize),
        axis.title.x = element_text(size = xaxesLabelSize),
        axis.title.y = element_text(size = yaxesLabelSize),
        legend.text = element_text(size =LegendTextSize),
        legend.title = element_text(size =LegendTitleSize),
  ) +
  scale_color_manual('sample', values = NamedColors, labels = wrapped_labels)
  # axis.line = element_line(color = "black", linewidth = 0.5)

if(tolower(Plot_Border_Type) == 'box' ){
  Plot <- Plot + 
    theme(panel.border = element_rect(color = 'black', fill = NA, linewidth = BorderLineWidth))
}

if(tolower(Plot_Border_Type) == 'axes' ){
  Plot <- Plot + 
    theme(axis.line = element_line(color = "black", linewidth = BorderLineWidth))
}

suppressWarnings(print(Plot))

OUT <- paste(OutputFileFolder, plotname, sep = '')
ggsave(OUT, plot = Plot, width = plotdim[1], height = plotdim[2], bg = 'transparent' )
rm(Plot)
gc()
