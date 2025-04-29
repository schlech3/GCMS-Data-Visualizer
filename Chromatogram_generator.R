##### CHROMATOGRAM GENERATOR (Split Version - Cleaned Up) #####
{
# Install once if not already:
# install.packages("tidyverse")
# install.packages("ggplot2")
# install.packages("scales")
# install.packages("omnibus")

library(tidyverse)
library(ggplot2)
library(scales)
library(omnibus)
}
#--------------------------- USER INPUT SECTION ---------------------------#

Filefolder <- 'C:/Path/To/Your/Folder/'  # Use forward slashes

# Define files and associated ion channels explicitly (no .csv needed)
YourPlot <- c('sample1', '144',
              'sample2', '144')

Relative_OrAbsolute <- 'Relative'
VerticalSampleOffset <- 0.1
HorizontalSampleOffset <- 0.1
xrange <- c(10, 20)
yrange <- c(0, 1.2)
TickTimeGap <- 1
Legend_WrapAmount <- 20
NewVerticalLine <- c(15, 'dashed', 'red', 1)
LineColors <- c('blue', 'green')
ChromLineWidth <- 0.3
IncludeBorder <- 'Yes'

xaxesLabelSize <- 14
yaxesLabelSize <- 14
xaxesNumberSize <- 12
yaxesNumberSize <- 12
LegendTitleSize <- 14
LegendTextSize <- 12

plotdim <- c(8, 5)
plotname <- 'ChromPlot.png'

#-------------------------- INTERNAL LOGIC ---------------------------#

setwd(Filefolder)

# Organize input filenames and ions
Ion <- tibble(YourPlot) %>% mutate(rownum = row_number()) %>% 
  filter(rownum %% 2 == 0) %>% rename(Ion = YourPlot) %>% select(Ion)
Sample <- tibble(YourPlot) %>% mutate(rownum = row_number()) %>% 
  filter(rownum %% 2 == 1) %>% rename(Sample = YourPlot) %>% select(Sample)
PlotTbl <- cbind(Sample, Ion) %>% mutate(Combo = paste(Sample, Ion))
IonsToPlot <- unique(PlotTbl$Ion)

# Read only necessary files
read_plus <- function(flnm) {
  read_csv(flnm) %>% 
    mutate(filename = tools::file_path_sans_ext(basename(flnm))) %>%
    relocate(filename) %>%
    rename(minutes = `RT(minutes) - NOT USED BY IMPORT`,
           milliseconds = `RT(milliseconds)`) %>%
    mutate(TIC = rowSums(across(where(is.numeric) & !c(minutes, milliseconds))))
}

file_list <- paste0(PlotTbl$Sample, ".csv")
Myfiles <- map_df(file_list, read_plus)

# Filter and format data
DesiredIonFileData <- Myfiles %>%
  filter(filename %in% PlotTbl$Sample) %>%
  group_by(filename) %>%
  select(filename, milliseconds, minutes, all_of(IonsToPlot))

# Create relative abundances
DesiredRelativeIonFileData <- DesiredIonFileData %>% ungroup()
for(i in IonsToPlot){
  tempmax <- max(pull(DesiredRelativeIonFileData, i))
  DesiredRelativeIonFileData[[paste0('relative.', i)]] <- pull(DesiredRelativeIonFileData, i) / tempmax
}

CombinedPlotData <- DesiredRelativeIonFileData %>%
  pivot_longer(cols = starts_with('relative.'), names_to = 'Ion', values_to = 'Relative_Intensity') %>%
  mutate(Ion = str_remove(Ion, 'relative\\.'),
         Chromatogram = paste(filename, Ion)) %>%
  filter(Chromatogram %in% PlotTbl$Combo)

AbsoluteData <- DesiredIonFileData %>%
  pivot_longer(cols = IonsToPlot, names_to = 'Ion', values_to = 'Absolute_Intensity')

CombinedPlotData2 <- left_join(CombinedPlotData, AbsoluteData)

# Add vertical and horizontal offsets
FinalCombinedPlotData <- CombinedPlotData2
for(i in 1:nrow(PlotTbl)){
  tempChromName <- paste(PlotTbl[i,1], PlotTbl[i,2])
  vOffset <- (nrow(PlotTbl)*VerticalSampleOffset) - (i*VerticalSampleOffset)
  hOffset <- (nrow(PlotTbl)*HorizontalSampleOffset) - (i*HorizontalSampleOffset)
  
  FinalCombinedPlotData <- FinalCombinedPlotData %>%
    mutate(Relative_Intensity = if_else(Chromatogram == tempChromName, Relative_Intensity + vOffset, Relative_Intensity),
           Absolute_Intensity = if_else(Chromatogram == tempChromName, Absolute_Intensity + vOffset, Absolute_Intensity),
           minutes = if_else(Chromatogram == tempChromName, minutes + hOffset, minutes),
           milliseconds = if_else(Chromatogram == tempChromName, milliseconds + hOffset, milliseconds))
}

FinalCombinedPlotData$Chromatogram <- factor(FinalCombinedPlotData$Chromatogram, levels = PlotTbl$Combo)

# Final Plotting
Yaxis <- ifelse(tolower(Relative_OrAbsolute) == 'relative', 'Relative_Intensity', 'Absolute_Intensity')

Plot <- ggplot(FinalCombinedPlotData) +
  geom_line(aes(x = minutes, y = .data[[Yaxis]], color = str_wrap(Chromatogram, Legend_WrapAmount)), lwd = ChromLineWidth) +
  ylim(yrange) +
  xlim(xrange) +
  ylab('Intensity') +
  scale_x_continuous(breaks = seq(xrange[1], xrange[2], TickTimeGap)) +
  scale_color_manual('Chromatogram', values = LineColors) +
  theme(panel.background = element_blank(),
        axis.text = element_text(size = xaxesNumberSize),
        axis.title = element_text(size = xaxesLabelSize),
        legend.title = element_text(size = LegendTitleSize),
        legend.text = element_text(size = LegendTextSize))

if(tolower(IncludeBorder) == 'yes'){
  Plot <- Plot + theme(panel.border = element_rect(color = 'black', fill = NA))
}

print(Plot)
ggsave(plotname, plot = Plot, width = plotdim[1], height = plotdim[2])