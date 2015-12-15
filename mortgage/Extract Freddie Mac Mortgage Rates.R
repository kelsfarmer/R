##Script to download and visualize archived Freddie Mac mortgage rates
##Current rates can be accessed at http://www.freddiemac.com/ through
##a download link at the bottom of the page that is stored sequentially by year
getwd()
dir.create("./mortgage_rates")
setwd("./mortgage_rates")

file1url<- "http://www.freddiemac.com/pmms/2015/historicalweeklydata.xls"
file2url<- "http://www.freddiemac.com/pmms/docs/30-yr_historics.xls"

download.file(file1url, "current_rates.xls", mode="wb")
download.file(file2url,"archive_rates.xls", mode="wb")

##Upon opening and inspecting both files, each had explanatory rows at the top.  
##These need to be removed to combine both files, they are not relevant to the analysis

library(readxl)
#import archive rates
archives <- read_excel("archive_rates.xls", 
                    sheet = 1, 
                    skip=7,
                    col_names=TRUE,
                    col_types=NULL
)
##Name columns
colnames(archives)<- c("Year", "Date", "Base.Rate", "Points")

#dates are split into 2 columns and year only listed once
#create a variable with the first year and loop through to fill in all NA rates
missing<- archives[1,1]
for(i in 1:nrow(archives)){
if (is.na(archives[i,1])) {archives[i,1]<- missing} else
{missing<- archives[i,1]}
}

#Some dates are randomly formatted as numbers
#Loop through to evaluate dirty dates and correct by adding 7 days to last entry as new date
bad.dates<- c()
for(i in 1:nrow(archives)){
  if (nchar(archives[i,2])>5) {archives[i,2]<- as.character(format(as.Date(paste(archives[(i-1),1], "/",archives[(i-1),2], sep="")) + 7, "%m/%d"))
                               } else
  {bad.dates<- rbind(bad.dates, archives[i,2])}
}

#Some dates have NA values for interest rate, so remove those
archives[which(is.na(archives$Date)),]
archives<- na.omit(archives)

#One additional date wasn't formatted consistently.  1984 was a strange year so this made sense to me
archives[nchar(archives$Date2)<10,]
archives[685,2]<- "05/11"

#current rates spreadsheet has 3 relevant columns, including combined dates.
#re-format dates and columns to be consistent with current rates
archives$Date<- as.Date(paste(archives$Year,"/", archives$Date, sep=""))
archives<- archives[2:4]
#import current rates
current <- read_excel("current_rates.xls", 
                       sheet = 1, 
                       skip=6,
                       col_names=TRUE,
                       col_types=NULL
)
##Spreadsheet includes all survey rates
##This analysis focuses on 30 year rates, so strip others out next
current<- current[,1:3]
current<- na.omit(current)

##Name columns, and note that this spreadsheet doesn't have year, which will be added later
colnames(current)<- c("Date", "Base.Rate", "Points")
current$Date<- as.Date(current$Date)


#Combine rates into single data frame
rates<- rbind(archives, current)


#Create a new column with combined Base+points
rates$Base.Rate<- as.numeric(rates$Base.Rate)
rates$Points<- as.numeric(rates$Points)
rates$Loaded.Rate<- rates$Base.Rate+rates$Points

##Note that these rates are an average of market rates, so they blend customers at all tiers of credit scores

library(ggplot2)
library(scales)
graph<- ggplot(rates, aes(x=Date, y=Loaded.Rate))
graph<- graph+geom_area()+geom_line(aes(y=Base.Rate), color=c("Base.Rate"="blue"))
graph +scale_x_date()+xlab("")+ylab("Weekly Average Mortgage Rate Per Freddie Mac")