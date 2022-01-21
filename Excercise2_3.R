library(lubridate)
library(stringr)
txt <- readLines("example.txt")
txt
cmnt <- grepl("^/",txt)
cmntVec <- txt[cmnt]
cmntVec
#Extracting date from comment line 1
dateStr1 <- cmntVec[1]
dateStr1
dateStr2 <- dmy(dateStr1)
dateStr2
#Extracting column names from comment lines
colNmstr1 <- cmntVec[2]
colNmstr1
colNmstr2 <- cmntVec[3]
colNmstr2
colNmstr3 <- cmntVec[4]
colNmstr3
# using strsplit
#gender
colNm1a <- unlist(strsplit(colNmstr1,split = ":"))
colNm1a
colNm1b <- str_trim(colNm1a[2])
colNm1b
#age
colNm2a <- unlist(strsplit(colNmstr2,split = ":"))
colNm2a
colNm2b <- str_trim(colNm2a[2])
colNm2b
#weight
colNm3a <- unlist(strsplit(colNmstr3,split = ":"))
colNm3a
colNm3b <- str_trim(colNm3a[2])
colNm3b
#using substr 
#we need to know the integer position 
#start and stop of required string
#gender
colNm1 <- str_trim(substr(colNmstr1,12,18))
colNm1
#age
colNm2 <- str_trim(substr(colNmstr2,12,16))
colNm2
#weight
colNm3 <- str_trim(substr(colNmstr3,12,18))
colNm3
#creating a single column vector
colNms <- c(colNm1b,colNm2b,colNm3b)
colNms
#Extracting Data from File
datVec <- txt[!cmnt]
datVec
#Splitting each line into separate field
fieldList <- strsplit(datVec,split = ";")
fieldList
dim(fieldList)
length(fieldList)
#Standardizing rows
assignFields <- function(x){
  out <- character(3)
  # get gender vector
  i <- grepl("[[:alpha:]]",x)
  out[1] <- x[i]
  # get weight (if any)
  i <- grepl(",", x)
  x[i] <- gsub(",",".", x[i])
  i <- grepl("[[:punct:]]",as.numeric(x))
  out[3] <- ifelse(length(i)>0, x[i], NA)
  # get age (in years) if any
  i <- grepl("[[:digit:]]",as.integer(x))
  out[2] <- ifelse(length(i)>0, x[i], NA)
  out
}
standardFields <- lapply(fieldList,assignFields)
standardFields
#Transform to Matrix
M <- matrix(
  unlist(standardFields)
  , nrow=length(standardFields)
  , byrow=TRUE)
colnames(M) <- colNms
M
# Transform Matrix to data.frame
weightDat <- as.data.frame(M)
weightDat
# All columns are character data
sapply(weightDat,class)
View(weightDat)
#Transform the Gender column into a factor variable 
#with labels man and woman 
#using string distance technique
gender <- weightDat$Gender
gender
#class(gender)
#typeof(gender)
#gender <- as.factor(gender)
#gender
#class(gender)
#typeof(gender)
#using adist
#first run gave a tie 
#for distance with female and fem.
#therfore first code is made woman
codes <- c("woman", "man")
codes[1]
#uncomment next for adist
D <- adist(gender, codes)
#using agrep
#uncomment the next for agrep
#D <- character(4)
#D
#D[1] <- agrep(codes[1],gender[1])
#D[1]
colnames(D) <- codes
rownames(D) <- gender
D
i <- apply(D, 1, which.min)
data.frame(rawtext = gender, coded = codes[i])
#uncomment following 2 lines 
#for testing each option
weightDat$Gender = codes[i]
weightDat$Gender <- as.factor(weightDat$Gender)
weightDat$Gender
weightDat
class(weightDat$Gender)
levels(weightDat$Gender)
typeof(weightDat$Gender)
#Coerce the Age column to integer.
weightDat$`Age (in years)` <- as.integer(weightDat$`Age (in years)`)
class(weightDat$`Age (in years)`)
typeof(weightDat$`Age (in years)`)
#Coerce the weight column to numeric.
weightDat$`Weight (in kg)` <- as.numeric(weightDat$`Weight (in kg)`)
class(weightDat$`Weight (in kg)`)
typeof(weightDat$`Weight (in kg)`)
weightDat