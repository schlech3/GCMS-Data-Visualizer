#If your first time running this script, ensure the 3 packages 
#below are installed. can uncomment below to install them. 

# install.packages("tidyverse")
# install.packages("ggplot2")
# install.packages("scales")

require("tidyverse")
require("ggplot2")
require('scales')

Filefolder <- 'C:/MSU/Mass Spec and FID/NiceFigures/CYP736_Promiscious_Activities/'

SampleName <- '20240307_8_ArTPS2_SsSS' #Which Sample (no .csv) are you looking to do?

PeakTime <- 15.749 #What time (in minutes) are you trying to plot the spectra for? Be as specific as you can for accuracy

IonRange <- c(40, 320) #X axis to look at
yrange <- c(0,1.25E5)     #Change why  axis ranges

IonsToLabel <- c(41,55,67,79,95,107,123,135,149,163,175,191,205,217,233,247,259,269,287, 302) #Ions aren't labeled at the top by default. Type the ones you want

IonLabelSize <- 4 #The labeles above a given ions respective size

SpectraLineWidth <- 0.6  #If you want thicker or thinner lines change these values

xaxesLabelSize <- 14
yaxesLabelSize <- 14
xaxesNumberSize <- 14
yaxesNumberSize <- 10
LegendTitleSize <- 15
LegendTextSize <- 12

plotdim <- c(8,5) #Plot size/dimension info

plotname <- 'TEST.png' #be sure to include the type of pic (.jpg .png, .svg)




#To run everything now, hit the Source button above, or cntrl + A to highlight everything 
#and then hit run. 

#Hit Source to run everything 

###############################################################################
########################## Internal Code ######################################
###############################################################################

setwd(Filefolder)

#step 2: Description in brackets
{
  #this function will read a csv but also add a column for the filename to it,
  #moved it to the front, and then cut off the ./ for current directory and .csv
  #that are visible in the name normally. This function also will rename
  #the time column(s) from RT(milliseconds) to milliseconds and 
  # RT(minutes)NOT USED BY IMPORT to minutes. 
  # lastly I create a new column that is a sum of the ions aka the TIC. 
}

SampleFile <- paste(SampleName, ".csv", sep = '')

read_plus <- function(flnm) {
  print(flnm)
  suppressMessages(
    read_csv(flnm) %>% 
      mutate(filename = flnm) %>% 
      relocate(filename) %>%
      rename(minutes = `RT(minutes) - NOT USED BY IMPORT`) %>% 
      rename(milliseconds = `RT(milliseconds)`)
  )
}

# Build full paths only for files that match
Myfiles <- 
  list.files(pattern = "*.csv", full.names = TRUE) %>%
  keep(~basename(.) == SampleFile) %>%
  map_df(~read_plus(.))







#Step 3: This pulls out only your Sample and the closest time to what you wanted, then tidy data
Myfiles %>% mutate(filename = str_sub(filename, start = 3L, end = -5L)) %>%
  filter(filename == SampleName) %>%
  filter(abs(PeakTime - minutes) == min(abs(PeakTime - minutes)))


IonPlotData <- Myfiles %>% mutate(filename = str_sub(filename, start = 3L, end = -5L)) %>% filter(filename == SampleName) %>% 
  filter(abs(PeakTime - minutes) == min(abs(PeakTime - minutes)))  %>% 
  pivot_longer(cols = !c(filename, milliseconds, minutes, RI) , names_to = 'm/z', values_to = 'Intensity') %>%
  mutate(`m/z` = as.numeric(`m/z`))


Plot <- IonPlotData %>% ggplot(mapping = aes(x = `m/z`, y = Intensity) ) +
  geom_bar(stat = 'identity', width = SpectraLineWidth) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), panel.background = element_blank(),
        axis.title.x = element_text(size = xaxesLabelSize),
        axis.title.y = element_text(size = yaxesLabelSize))+
  xlim(IonRange) +
  ylim(yrange[1], yrange[2]) +
  geom_text(aes(label=ifelse( `m/z` %in% IonsToLabel, `m/z`, NA ) ), position=position_dodge(width=0.9), vjust=-1.2, size = IonLabelSize)

print(Plot)
ggsave(plotname, plot = Plot, width = plotdim[1], height = plotdim[2] )
