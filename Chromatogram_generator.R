#Before you can run this script you must open your files in openchrom, right 
#click on chromatogram, and export chromatogram as a .csv with a name you'll know 
#Put all relevant ones in a folder. That will be part of the input. 



#General notes / descrigption in the brackets below (Probably read if new to R):
{
  # The point of this script is to automate the hard part of coding out publication
  # quality chromatography and spectra figures. To use this you only need to fill out
  # the variables before the 'ignorable part'.
                                                             
}


#If your first time running this script you need to run the following lines..
#only first time though. To run them remove the # symbol in front, go to the 
#line and hit run above. After that I recommend adding the # back to comment code lines out again
# install.packages("tidyverse")
# install.packages("ggplot2")
# install.packages("scales")

#Before you start: 
#Take your desired GCMS data youd like to plot and open it in OpenChrom
# right click on the individual chromatogram > export chromatogram > export as .csv
# Transfer your relevant file(s) to a folder and be sure to name the files  
# something that you want it to be indicated as on the plot

###Criteria for you to input

# Where is your file located pathwise? Be sure path names have a /  and not  \
Filefolder <- 'C:/MSU/Mass Spec and FID/NiceFigures/CYP736_Promiscious_Activities/CfTPS2Tests/'

#Using the files.csv within your Filefolder input the file(s) you want to plot (no extension)
#After each file (- extension) separate by comma, then do the ion (or TIC). You can add other 
#chromatograms by repeating this. The First listed/top in the list is the top chromatogrma listed. 

YourPlot <- c('20240809_4-12-24_Deriv21','144',
              '20240809_4-12-24_Deriv11','144',
              '20240809_4-12-24_Deriv7','144',
              '20240809_7-18-24_Deriv1','144'
)


#If you are plotting the Raw data (absolute intensity)
#Or are you plotting the relative abundances 
Relative_OrAbsolute <- 'Relative'

#On stacked chromatograms can add separation with this number. 
#Note: if you do relative the y axis scale is 0-1, but absolute intensity is 
#the counts of each ion. That can easily be 10000000 to 1000000000
#At least put 0 for this value. 

VerticalSampleOffset <- 0.11

#This is useful for if you are trying to offset chromatograms for a stacked one. 
HorizontalSampleOffset <- 0.15



#The window range you are plotting.
#Like before, Relative or absolute makes yrange height vary a lot more. 

xrange <- c(13, 15.75) #13.75
yrange <- c(0,1.3)

#Time Gap between your tick lines.. aka if you want a line every minute, set it to 1. if you want every half a minute set to 0.5
TickTimeGap <- 0.5

#If you have a long file/condition name, you can have it go to new lines
#this variable is an integer that says when you want it to go to new line
Legend_WrapAmount <- 20

#Where you want vertical line, What kind, what color, and thickness
#line options are twodash, longdash, dotted, dotdash, dashed, blank
NewVerticalLine <- c(15, 'blank', 'black', 2)


#Put in order of black chromatogram. For pretty hexadecimals https://coolors.co/156064-00c49a-f8e16c-ffc2b4-fb8f67
LineColors <- c('black','black','black','black','black', 'black','black','black','black','black','black','black','black','black','black','black','black','black','black','black','black','black','black','black','black','black','black')

#How wide do you want your line? 
ChromLineWidth <- 0.3

#Do you want a border around your plot? if you do answer Yes. if not put No
IncludeBorder <- 'No'

#---------- Use for both ------------------
#axes size stuff
xaxesLabelSize <- 14
yaxesLabelSize <- 14
xaxesNumberSize <- 14
yaxesNumberSize <- 10
LegendTitleSize <- 15
LegendTextSize <- 12


#Plot size/dimension info
plotdim <- c(8,5)

#be sure to include the type of pic (.jpg .png, .svg)
plotname <- 'TEST.png'



#To run everything now, hit the Source button above, or cntrl + A to highlight everything 
#and then hit run. 



###############################################################################
########################## Internal Code ######################################
###############################################################################


require("tidyverse")
require("ggplot2")
require('scales')

#This is for Plotting Chromatograms.

  #Step 1. taking the Filefolder you set above and reading those individually into R
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
  SampleNames <- YourPlot[seq(1, length(YourPlot), by = 2)]
  SampleFiles <- paste0(SampleNames, ".csv")
  
  read_plus <- function(flnm) {
    print(flnm)
    suppressMessages(
    read_csv(flnm) %>% 
      mutate(filename = flnm, show_col_types = FALSE) %>% 
      relocate(filename) %>%
      rename(minutes = `RT(minutes) - NOT USED BY IMPORT`) %>% 
      rename(milliseconds = `RT(milliseconds)`) %>%
      mutate(TIC = rowSums(across(!minutes & !milliseconds & !filename & !RI))) 
    )
  }
  
  # Build full paths only for files that match
  Myfiles <- 
    list.files(pattern = "*.csv", full.names = TRUE) %>%
    keep(~basename(.) %in% SampleFiles) %>%
    map_df(~read_plus(.))
  
  
  #Step 3: Reorganizing YourPlot list to prep for structure making
  {
    #The below takes the yourPlot value, makes it a datatable that is organized like the 
    # following:   Sample    Ion
    #              Samp1     Ion1
    #              Samp2     Ion2
    }
  
  Ion <- tibble(YourPlot) %>% mutate(rownum = row_number()) %>% filter(rownum%%2 == 0) %>% rename(Ion = YourPlot) %>% select(Ion)
  Sample <- tibble(YourPlot) %>% mutate(rownum = row_number()) %>% filter(rownum%%2 == 1) %>% rename(Sample = YourPlot) %>% select(Sample)
  PlotTbl <- cbind(Sample, Ion) %>% mutate(Combo = paste(Sample, Ion))
  
  IonsToPlot <- unique(PlotTbl$Ion) 
  #Step 4: Getting DesiredIonFileData organized (description below)
  {
    #now I am going to group the different chromatograms by their filenames
    #so that future code knows that all rows that are a given filename belong together
    #this is also pull out only the files and ions you chose at the top along with filename and time columns
  }
  
  
  DesiredIonFileData <- Myfiles %>% mutate(filename = str_sub(filename, start = 3L, end = -5L)) %>%
    filter(filename %in% PlotTbl$Sample ) %>%
    group_by(filename) %>%
    select(filename, milliseconds, minutes ,IonsToPlot)
  
  #Step 5: Getting Relative abundances: This version sets biggest of each respective Ion to 1.
  {
    #in order to plot different ions against one another we need
    #to think about relative abundance not absolute.
    #to do this going to find the max for each ion and then
    #divide everything else by that and multiply by 100 to get % abundance
    
    #First I assign a new DesiredIonFileData before the loop just because it has to overwrite itself,
    #and I'd prefer not to overwrite the original DesiredIonFileData
    }
  DesiredRelativeIonFileData <- DesiredIonFileData %>% ungroup(filename)
  
  for(i in IonsToPlot){
    tempiondata <- DesiredRelativeIonFileData %>% pull(i)
    tempmax <- max(tempiondata)
    tempcolname <- paste('relative', i, sep = '.')
    
    TempRelative <- DesiredRelativeIonFileData %>% select(i) %>%
      mutate(./tempmax) %>% 
      rename_with(.fn = ~paste('relative', i, sep = '.'), .cols = i)
    
    DesiredRelativeIonFileData <- cbind(TempRelative, DesiredRelativeIonFileData)
  }
  
  
  
  #######################
  
  
  #Step 6: Using YourPlot to pull out the exact data to plot
  {
    #Using Pivot_longer you can take the relative.rows and instead make a categorical variable Ion
    # and then split up the data such that it shows off Ion.. .then relative intensity for THAT ion
    # this is better for ggplot
    # Using Mutate to remove the relative. in relative.ion
    # using filter from the PlotTbl from YourPlot to remove the Samples and Ions we don't care about
  }
  
  CombinedPlotData <- DesiredRelativeIonFileData %>% 
    pivot_longer(cols = c(paste('relative',IonsToPlot, sep ='.')), names_to = 'Ion', values_to = 'Relative_Intensity' ) %>%
    mutate(Ion = str_sub(Ion,start = 10L )) %>% 
    mutate(Chromatogram = paste(filename, Ion)) %>%
    filter(Chromatogram %in% PlotTbl$Combo) %>%
    #filter(Ion %in% PlotTbl$Ion) %>%
    select(!IonsToPlot)
  
  
  #Step 7: 
  DesiredIonFileDataLong <- DesiredIonFileData %>% pivot_longer(cols = c(IonsToPlot), names_to = 'Ion', values_to = 'Absolute_Intensity' )
  
  CombinedPlotData2 <- left_join(CombinedPlotData,DesiredIonFileDataLong) 
  
  
  
  
  #Step 8: Adjusting the % relative to offset the data. 
  {
    #The top variable VerticalSampleOffset will adjust how far apart the different plotted chromatograms are.
    #This works by taking the PlotTbl file, which should be tbl in same order as YourFile at the top
    # and reading in that order adds the VerticalSampleOffset value consecutively to each new file and 
    # then saves it as a new final file. To get it to go up by consistent amount its a weird subtraction instance
  }
  
  CombinedPlotData3 <- CombinedPlotData2 #%>% mutate(Chromatogram = paste(filename, Ion))
  tempCombinedPlotData <- CombinedPlotData3
  for(i in 1:nrow(PlotTbl)){
    tempChromName <- paste(PlotTbl[i,1], PlotTbl[i,2])
    VerticalMathAmount <- (nrow(PlotTbl)*VerticalSampleOffset) - (i*VerticalSampleOffset)
    HorizontalMathAmount <- (nrow(PlotTbl)*HorizontalSampleOffset) - (i*HorizontalSampleOffset)
    
    tempCombinedPlotData <- tempCombinedPlotData %>% 
      mutate(Relative_Intensity = if_else(Chromatogram == tempChromName, Relative_Intensity + VerticalMathAmount ,Relative_Intensity ) ) %>%
      mutate(Absolute_Intensity = if_else(Chromatogram == tempChromName, Absolute_Intensity + VerticalMathAmount ,Absolute_Intensity ) ) %>%
      mutate(minutes = if_else(Chromatogram == tempChromName, minutes + HorizontalMathAmount ,minutes ) ) %>%
      mutate(milliseconds = if_else(Chromatogram == tempChromName, milliseconds + HorizontalMathAmount ,milliseconds ) )
    
    
  }
  FinalCombinedPlotData <- tempCombinedPlotData
  tempCombinedPlotData <- tibble()
  
  #
  FinalCombinedPlotData$Chromatogram <- factor(FinalCombinedPlotData$Chromatogram, levels = PlotTbl$Combo)
  
  #################################################################################
  #################################################################################
  ##############################FINAL PLOTTING BELOW! #############################
  #################################################################################
  
  
  
  if(Relative_OrAbsolute == 'Relative' | Relative_OrAbsolute == 'relative'){
    Yaxis <- 'Relative_Intensity'
  } else{
    Yaxis <- 'Absolute_Intensity'
  }
  
  
  xrange
  Plot <- FinalCombinedPlotData %>% filter(minutes >= xrange[1] & minutes <= xrange[2]) %>%
    ggplot() +
    geom_line(mapping = aes(x = minutes, y = get(Yaxis), color = str_wrap(Chromatogram, Legend_WrapAmount)), lwd = ChromLineWidth) +
    ylim(c(yrange[1], yrange[2])) +
    ylab('Intensity')+
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
    scale_color_manual('Chromatogram', values = LineColors) +

    
    if(IncludeBorder == 'Yes' | IncludeBorder == 'yes' | IncludeBorder == 'Y'){
      Plot <- Plot + 
        theme(panel.border = element_rect(color = 'black', fill = NA))
    }
  
  print(Plot)
  ggsave(plotname, plot = Plot, width = plotdim[1], height = plotdim[2], bg = 'transparent' )
  
  

