#Activity 2 Chengzhe Li (Richard)

#make a vector of tree heights in meters
heights <- c(30,41,20,22)
#convert to cm
heights_cm <- heights*100
heights_cm
heights[1]
heights[2:3]

#set up a matrix with 2 columns and fill in by rows
#first argument is the vector of numbers to fill in the matrix
Mat<-matrix(c(1,2,3,4,5,6), ncol=2, byrow=TRUE)
Mat
#set up a matrix that fills in by columns
#first argument is the vector of numbers to fill in the matrix
Mat.bycol<-matrix(c(1,2,3,4,5,6), ncol=2, byrow=FALSE)
Mat.bycol
#subset the matrix to look at row 1, column2
Mat.bycol[1,2]
#look at all values in row 1
Mat.bycol[1,]
#look at all values in column 2
Mat.bycol[,2]

#read in weather station file from your data folder

datW <- read.csv("Z:\\cli2\\noaa_weather\\2011124.csv",
                 stringsAsFactors = T)
str(datW)
#row(datW)
#specify a column with a proper date format
#note the format here dataframe$column
datW$dateF <- as.Date(datW$DATE, "%Y-%m-%d")
#google date formatting in r to find more options and learn more
#create a date column by reformatting the date to only include years
#and indicating that it should be treated as numeric data
datW$year <- as.numeric(format(datW$dateF,"%Y"))

##### Q2 #####
q2 <- c('a','b','c','d','e',1,2,3,4,5,0.1,0.2,0.3,0.4,0.5,"Hamilton","Earlville","Madison","Oriskany Falls","Morrisville")
##### Q2 #####
