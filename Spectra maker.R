#Before you can run this script you must open your files in openchrom, right 
#click on chromatogram, and export chromatogram as a .csv with a name you'll know 
#Put all relevant ones in a folder. That will be part of the input. 



#General notes / description in the brackets below (Probably read if new to R):
{
  # The point of this script is to automate the hard part of coding out publication
  # quality chromatography and spectra figures. To use this you only need to fill out
  # the variables before the 'ignorable part'.
  # 
  # Some general notes about coding and these variables. 
  # X <- Y indicates that We are assigning the value Y to X. Do not change the left
  # sides of the line as thats a variable name that is hard coded into the ignorable part. 
  # 
  # Some of the variables used are strings, which are variables that have to be in quotes ''
  # Basically anything not a number is going to be in quotes, but each variable below should be an example.
  # 
  # additionally, we occasionally have vectors. those are going to be denoted by having c()
  # and are filled with a comma separated list. i.e. c('Apple','bannana','etc.') or c(1,2,3)
  #                                                                
}


#If your first time running this script you need to run the following lines..
#only first time though. To run them remove the # symbol in front, go to the 
#line and hit run above. After that I recommend adding the # back to comment code lines out again
# install.packages("tidyverse")
# install.packages("ggplot2")
# install.packages("scales")
# install.packages("omnibus")
#Before you start: 
#Take your desired GCMS data youd like to plot and open it in OpenChrom
# right click on the individual chromatogram > export chromatogram > export as .csv
# Transfer your relevant file(s) to a folder and be sure to name the files  
# something that you want it to be indicated as on the plot

###Criteria for you to input

# Where is your file located pathwise? Be sure path names have a /  and not  \
Filefolder <- 'C:/MSU/Mass Spec and FID/NiceFigures/CYP736_Promiscious_Activities/CfTPS2Tests/'


file.choose()
#Try File.choose()

#If you filled out Spectra do the below otherwise you can leave as is:
#--------------------------------------------------------------------------------
#Which Sample (no .csv) are you looking to do?
SampleName <- '20240307_1_GGPPS'

#What time (in minutes) are you trying to plot the spectra for? Be as specific as you can for accuracy
PeakTime <- 15.749

#X axis to look at
IonRange <- c(40, 320)
#Ions aren't labeled at the top by default. Type the ones you want
IonsToLabel <- c(41,55,67,79,95,107,123,135,149,163,175,191,205,217,233,247,259,269,287, 302)
#The labeles above a given ions respective size
IonLabelSize <- 4
#If you want thicker or thinner lines change these values
SpectraLineWidth <- 0.6
#----------------------------------------------

#If you are doing Chromatograms fill out the below:

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
read_plus <- function(flnm) {
  read_csv(flnm) %>% 
    mutate(filename = flnm) %>% 
    relocate(filename) %>% 
    rename(minutes = `RT(minutes) - NOT USED BY IMPORT`) %>% 
    rename(milliseconds = `RT(milliseconds)`)
  
  
}


#this will then look in the folder you set and then using the made function above make 1 big dataframe
Myfiles <-
  list.files(pattern = "*.csv", 
             full.names = T) %>% 
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
  geom_text(aes(label=ifelse( `m/z` %in% IonsToLabel, `m/z`, NA ) ), position=position_dodge(width=0.9), vjust=-1.2, size = IonLabelSize)
Plot
ggsave(plotname, plot = Plot, width = plotdim[1], height = plotdim[2] )
}