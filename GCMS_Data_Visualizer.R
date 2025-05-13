# install.packages("remotes")
# remotes::install_github("https://github.com/ethanbass/chromConverter/")

require(chromConverter)
require(tidyverse)
require(scales)

Filefolder <- 'C:/MSU/Coding/Stacked_Chromatogram_Generator/20230701/' #Folder where .csv's are located

Process_Data <- TRUE #If you are just finnicking with Plot parameters, set to FALSE to not have long wait period


                    #FileName,     #TIC/EIC mz,    +/- value i.e. ion = 40 RANGE = 0.5 aka 40 +/- 0.5. NA if TIC
YourPlot <- tribble(~sample,       ~ion,           ~RANGE,   #Do not edit the  line
                    '20230701_5-26-23_NBenthi_01.D', 'TIC', 'NA',
                    '20230701_5-26-23_NBenthi_02.D', 'TIC', 'NA',
                    '20230701_5-26-23_NBenthi_03.D', 'TIC', 'NA',
                    '20230701_5-26-23_NBenthi_06.D', 'TIC', 'NA',
                    '20230701_5-26-23_NBenthi_07.D', 'TIC', 'NA',
                    '20230701_5-26-23_NBenthi_08.D', 'TIC', 'NA'
                    
                    
)

DigitsBelowZero <- 1  #Relevant if you want to round up to the nearest m/z i.e. 45, 45.0, 45.00, etc. Stick to 0-3, but even 3 requires a lot of RAM

Relative_OrCounts <- 'relative' #Unless you want counts, do relative

xrange <- c(5, 18) #Pick x-axes range (minutes)
yrange <- c(0,1.3)  #if relative plot 0-1 or higher if stacking chroms



VerticalSampleOffset <- 0.11 #How big of a gap between
HorizontalSampleOffset <- 0 # How much (time) to shift each additional stacked chromatogram by

TickTimeGap <- 1  #Time gap between x-axis ticks
Legend_WrapAmount <- 20  #How many characters before Legend goes to new line

LineColors <- c('black') #Put in order of YourPlot variable. For pretty hexadecimals https://coolors.co/156064-00c49a-f8e16c-ffc2b4-fb8f67

ChromLineWidth <- 0.5 #How wide do you want your line? 

IncludeBorder <- 'no'  #Do you want box around plot?

xaxesLabelSize <- 14
yaxesLabelSize <- 14
xaxesNumberSize <- 14
yaxesNumberSize <- 10
LegendTitleSize <- 15

LegendTextSize <- 12

plotdim <- c(8,5)  # Final Figure dimensions. Printed in R is not final dimensions so please check

plotname <- 'TEST.png' #be sure to include the type of pic (.jpg .png, .svg)

############

#  Functions:

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
  
  data %>%
    left_join(tbl, by = "sample") %>%
    group_by(sample) %>%
    mutate(
      rt = rt + HorizontalSampleOffset * (as.numeric(factor(sample)) - 1),
      Intensity = Intensity + VerticalSampleOffset * (as.numeric(factor(sample)) - 1)
    ) %>%
    ungroup()
}



#get_closest rt, strip_extension, summarise_mz, target_mz, normalize_column 

####################

if(Process_Data == TRUE){
  gc()
  dat <- read_chroms(paste(Filefolder, YourPlot$sample, sep = ""), 
                     format_in = "agilent_d",
                     parser = "rainbow",
                     format_out = 'data.table',
                     data_format = "long"
  )
  
  gc()
  
  ms_data <- imap_dfr(dat, ~ {
    .x$MS %>%
      as_tibble() %>%
      mutate(sample = .y)
  })
  
  rm(dat)
  gc()
   
  
  ms_data2 <- ms_data %>%
    summarise_mz(. , DigitsBelowZero)
  
  rm(ms_data)
  gc()
  
  ms_data3 <- ms_data2 %>%
    target_mz(. , YourPlot)
  
  
  rm(ms_data2)
  gc()
  
  if (tolower(Relative_OrCounts) == 'relative' ){
   ms_data3 <- normalize_column(ms_data3)
  }
}
gc()
ms_data3

ms_data3 %>%
  ggplot(aes(x = rt, y = Intensity, color = sample)) +
  geom_line()



VerticalSampleOffset <- 0.11 #How big of a gap between
HorizontalSampleOffset <- 0 # 


YourPlot$sample <- factor(YourPlot$sample)

VerticalSampleOffset <- 0.11 #How big of a gap between
HorizontalSampleOffset <- 0.4
ms_data4 <- chrom_stack_adjuster(ms_data3, YourPlot, VerticalSampleOffset, HorizontalSampleOffset)

ms_data4 %>%
  ggplot(aes(x = rt, y = Intensity, color = sample)) +
    geom_line()





tempCombinedPlotData <- CombinedPlotData3
for(i in 1:nrow(YourPlot)){
  tempChromName <- paste(YourPlot[i,1], YourPlot[i,2])
  VerticalMathAmount <- (nrow(YourPlot)*VerticalSampleOffset) - (i*VerticalSampleOffset)
  HorizontalMathAmount <- (nrow(YourPlot)*HorizontalSampleOffset) - (i*HorizontalSampleOffset)
  
  tempCombinedPlotData <- tempCombinedPlotData %>% 
    mutate(Relative_Intensity = if_else(Chromatogram == tempChromName, Relative_Intensity + VerticalMathAmount ,Relative_Intensity ) ) %>%
    mutate(Absolute_Intensity = if_else(Chromatogram == tempChromName, Absolute_Intensity + VerticalMathAmount ,Absolute_Intensity ) ) %>%
    mutate(minutes = if_else(Chromatogram == tempChromName, rt + HorizontalMathAmount ,rt ) )

  
}
FinalCombinedPlotData <- tempCombinedPlotData




FinalCombinedPlotData$Chromatogram <- factor(FinalCombinedPlotData$Chromatogram, levels = PlotTbl2$Chromatogram)
NamedColors <- setNames(LineColors[1:nrow(PlotTbl2)], levels(FinalCombinedPlotData$Chromatogram))  


#################################################################
#
#
#
# 
# 
# 
# 
# 


if(Relative_OrAbsolute == 'Relative' | Relative_OrAbsolute == 'relative'){
  Yaxis <- 'Relative_Intensity'
} else{
  Yaxis <- 'Absolute_Intensity'
}

wrapped_labels <- str_wrap(levels(FinalCombinedPlotData$Chromatogram), Legend_WrapAmount)
xrange
Plot <- FinalCombinedPlotData %>% filter(minutes >= xrange[1] & minutes <= xrange[2]) %>%
  ggplot() +
  geom_line(mapping = aes(x = minutes, y = get(Yaxis), color = Chromatogram), lwd = ChromLineWidth) +
  ylim(c(yrange[1], yrange[2])) +
  ylab('Intensity') +
  scale_x_continuous(limits = c(xrange[1], xrange[2]), breaks = seq(from = omnibus::roundTo(xrange[1],TickTimeGap), to = omnibus::roundTo(xrange[2],TickTimeGap), by = TickTimeGap)) +
  theme(panel.background = element_blank(),
        axis.text.x = element_text(size = xaxesNumberSize),
        axis.text.y = element_text(size = yaxesNumberSize),
        axis.title.x = element_text(size = xaxesLabelSize),
        axis.title.y = element_text(size = yaxesLabelSize),
        legend.text = element_text(size =LegendTextSize),
        # # axis.ticks.length.x.bottom = unit(-0.1, 'cm',)
        #  panel.background = element_rect(fill='transparent'), #transparent panel bg
        #  plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
        #  panel.grid.major = element_blank(), #remove major gridlines
        #  panel.grid.minor = element_blank(), #remove minor gridlines
        #  legend.background = element_rect(fill='transparent'), #transparent legend bg
        #  legend.box.background = element_rect(fill='transparent') #transparent legend panel
        legend.title = element_text(size =LegendTitleSize),
  ) +
  scale_color_manual('Chromatogram', values = NamedColors, labels = wrapped_labels)


if(IncludeBorder == 'Yes' | IncludeBorder == 'yes' | IncludeBorder == 'Y'){
  Plot <- Plot + 
    theme(panel.border = element_rect(color = 'black', fill = NA))
}

print(Plot)
ggsave(plotname, plot = Plot, width = plotdim[1], height = plotdim[2], bg = 'transparent' )


