#This file is to run the randomization for the 3 states.
#New Jersey was run on 21 November 2016

#Load packages
require("openxlsx")
require("knitr")

 filepath1 <- "C:/Users/Owner/Google Drive/ISCS _ One Thousand Voices/Consulting Projects/SBST/Homeless Education/Data/HOMELESS 2015-2016 - 2016-10-27 (1).xlsx" #file path to district list with number of homeless
#filepath1 <- "HOMELESS 2015-2016 - 2016-10-27.xlsx" #file path to district list with number of homeless
 filepath2 <- "C:/Users/Owner/Google Drive/ISCS _ One Thousand Voices/Consulting Projects/SBST/Homeless Education/Data/Homeless Free Lunch Request 2015-16.xlsx" #file path to district list with free school lunches
#filepath2 <- "Homeless Free Lunch Request 2015-16.xlsx" #file path to district list with free school lunches

homeless.df <- read.xlsx(filepath1)
lunches.df <- read.xlsx(filepath2)

#Merge the two datasets
homeless.df2 <- merge(homeless.df, lunches.df, by="DISTRICT_ID", all.x = TRUE)
View(homeless.df2)
homeless.df2$HOMELESS_COUNT_1516.x <- as.numeric(homeless.df2$HOMELESS_COUNT_1516.x)

#Tables to show size
attach(homeless.df2)
table(county = COUNTY_NAME.x, homeless.reported = (HOMELESS_COUNT_1516.x>0))
table(charter = COUNTY_NAME.x=="CHARTERS", homeless.reported = (HOMELESS_COUNT_1516.x>0))

#----Many to One Analysis----
#There are LEAs with more than one HL and HL with more than one LEA
homeless.df2$email <- homeless.df2$X8 #Relable HL email address variable name
attach(homeless.df2)
naEmails <- length(which(is.na(email))) #How many emails are missing
naLEAEmails <- length(unique(homeless.df2[which(is.na(email)), "DISTRICT_ID"]))
naLEAs <- length(which(is.na(DISTRICT_ID)))
#homeless.df2[which(is.na(DISTRICT_ID)), ] #View the NA LEA Row to make sure it is fully NA
homeless.df2 <- homeless.df2[-which(is.na(DISTRICT_ID)), ]#Remove the NA row
kable(homeless.df2[which(is.na(email)), c("DISTRICT_ID", "DISTRICT_NAME.x", "X6", "X7", "email")])#Provide districts that we are missing emails for to NJ
#Remove the missing emails for the many to one analysis
homeless.df2.noNA <- homeless.df2[-which(is.na(homeless.df2$email)), ]
#Identify the duplicate emails -- HL assigned to >1 location
homeless.df2.noNA[duplicated(homeless.df2.noNA$email), c("DISTRICT_ID", "DISTRICT_NAME.x", "X6", "X7", "email")] #7, need to merge back in after randomized
duplicateEmails <- homeless.df2.noNA[duplicated(homeless.df2.noNA$email), c("DISTRICT_ID", "email", "X6", "X7")] #create vector storing duplicate emails / leas etc
#Identify how many districts are duplicated (>1 HL for the district)
stopifnot(nrow(homeless.df2.noNA[duplicated(homeless.df2.noNA$DISTRICT_ID), ])==0) #NONE!
#Remove the duplicate rows from the DF for randomization
homeless.df2.noNA.noDup <- homeless.df2.noNA[-which(duplicated(homeless.df2.noNA$email)), ]

#View for any odd rows
View(homeless.df2.noNA.noDup)
#Remove other missing values (non-named or with same name yet different emails)
homeless.df2.noNA.noDup[which(homeless.df2.noNA.noDup$X6=="abc"),]#Row with name as 'abc abc' but has a legitimate District ID and email, ask Tony
#duplicateEmails <- rbind(duplicateEmails, homeless.df2.noNA.noDup[which(homeless.df2.noNA.noDup$X6=="abc"), c("DISTRICT_ID", "email", "X6", "X7")])#Append to list, -- CORRECTION DO NOT APPEND, TREAT AS NA
homeless.df2.noNA.noDup <- homeless.df2.noNA.noDup[-which(homeless.df2.noNA.noDup$X6=="abc"),]#Remove that row
homeless.df2.noNA.noDup[which(homeless.df2.noNA.noDup$X6=="None"),]#Row with name as 'abc abc' but has a legitimate District ID and email, ask Tony
#duplicateEmails <- rbind(duplicateEmails, homeless.df2.noNA.noDup[which(homeless.df2.noNA.noDup$X6=="None"), c("DISTRICT_ID", "email", "X6", "X7")])#Append to list, -- CORRECTION DO NOT APPEND, TREAT AS NA
homeless.df2.noNA.noDup <- homeless.df2.noNA.noDup[-which(homeless.df2.noNA.noDup$X6=="None"),]#Remove that row
homeless.df2.noNA.noDup[which(duplicated(paste(homeless.df2.noNA.noDup$X6, homeless.df2.noNA.noDup$X7))), ] #Rows with same First & Last names but different emails
duplicateEmails <- rbind(duplicateEmails, homeless.df2.noNA.noDup[which(duplicated(paste(homeless.df2.noNA.noDup$X6, homeless.df2.noNA.noDup$X7))), 
                                                                  c("DISTRICT_ID", "email", "X6", "X7")])#Append to list 
homeless.df2.noNA.noDup <- homeless.df2.noNA.noDup[-which(duplicated(paste(homeless.df2.noNA.noDup$X6, homeless.df2.noNA.noDup$X7))), ] #Remove those rows

#Rename DF for ease of use
x <- homeless.df2.noNA.noDup
dim(x)[1] #how many LEAs

#When stratified by any homeless and charters
#Blocking variable models
m0a <- lm(HOMELESS_COUNT_1516.x ~ (HOMELESS_COUNT_1516.x>0),data = x)
summary(m0a) #determine amount of variance explained by reporting / not
m0b <- lm(HOMELESS_COUNT_1516.x ~ (COUNTY_NAME.x=="CHARTERS"), data = x)
summary(m0b) #determine amount of variance explained by charter / not
m0c <- lm(HOMELESS_COUNT_1516.x ~ (HOMELESS_COUNT_1516.x>0) + (COUNTY_NAME.x=="CHARTERS"), data = x)
summary(m0c)

anova(m0a, m0c) # does adding charter explain more variance than reporting --> no
anova(m0b, m0c) # does adding reporting to charter explain more variance --> yes
#Still using Charter as a variable b/c of the difference in the manner of operation

## Add a block variable
x$block <- interaction(x$COUNTY_NAME.x=="CHARTERS",x$HOMELESS_COUNT_1516.x>0)
table(x$block,exclude=c())
levels(x$block) <- c("No Charter, No Report","Charter, No Report","No Charter, Report","Charter, Report")
## Check block coding
with(x,ftable(block,x$COUNTY_NAME.x=="CHARTERS"))
with(x,ftable(block,x$HOMELESS_COUNT_1516.x>0))
with(x,ftable(block,x$COUNTY_NAME.x=="CHARTERS",x$HOMELESS_COUNT_1516.x>0))

## Random assignment
#install.packages("randomizr")
library(randomizr)
set.seed(20161118)
x$treatment <- block_ra(x$block)
## Check to ensure equal numbers assigned
table(x$treatment,x$block)

#Add the individuals earlier removed for duplicates (to ensure they get counted in balance check and future analysis)
duplicateEmails$treatment <- NA
for(i in 1:7)
{
  print(i)
  print(x[x$email==duplicateEmails$email[i], c("email", "treatment", "block")])
  print(duplicateEmails[i,])
  duplicateEmails$treatment[i] <- x[which(x$email==duplicateEmails$email[i]), "treatment"]
  #duplicateEmails$treatment <- x[which(paste(x$X6, x$X7)==
  #                                       paste(duplicateEmails$X6[i], duplicateEmails$X7[i])), "treatment"]
  print("-----------------------")
}
#Do some by hand, Umbach and Erety
duplicateEmails[duplicateEmails$X7=="Umbach", "treatment"] <-  x[x$X7=="Umbach", "treatment"]
duplicateEmails[duplicateEmails$X6=="Erety", "treatment"] <-  x[x$X6=="Erety", "treatment"]

#Extract the rows from the original DF, give them the correct treatment status, and append to treatment
homeless.df2.noNA$block <- interaction(homeless.df2.noNA$COUNTY_NAME.x=="CHARTERS",homeless.df2.noNA$HOMELESS_COUNT_1516.x>0)
levels(homeless.df2.noNA$block) <- c("No Charter, No Report","Charter, No Report","No Charter, Report","Charter, Report")
homeless.df2.noNA$treatment <- NA
y <- homeless.df2.noNA
for(i in duplicateEmails[duplicateEmails$treatment==1, "DISTRICT_ID"])
{
  print(i)
  y[which(y$DISTRICT_ID==i), "treatment"] <- 1
}
treatmentAppend <- y[which(y$treatment==1), ]
for(i in duplicateEmails[duplicateEmails$treatment==0, "DISTRICT_ID"])
{
  print(i)
  y[which(y$DISTRICT_ID==i), "treatment"] <- 0
}
controlAppend <- y[which(y$treatment==0), ]

#Append rows
dim(x)
x2 <- rbind(x, treatmentAppend)
x2 <- rbind(x2, controlAppend)
dim(x2)

## Look for equal numbers of treated and controls in each block
table(x.nocharter.noreport$treatment)
with(x,table(treatment,block))
with(x2, table(treatment,block))

# Balance check (DS: how about now)
m1 <- lm(treatment ~ block,data = x2)
summary(m1) # treatment should not be related to block at baseline
m2 <- lm(HOMELESS_COUNT_1516.x ~ treatment*block, x2)
summary(m2) # treatment should not be related to count at baseline
m3 <- lm(FREE_LUNCH ~ treatment*block, x2)
summary(m3) # treatment should not be related to number receiving free lunches at baseline

## I would do this for a balance assessment
#install.packages("RItools")
x2$freelunchn <- as.numeric(x2$FREE_LUNCH)
library(RItools)
xb1 <- xBalance(treatment~freelunchn+HOMELESS_COUNT_1516.x,strata=list(block=~block),data=x2,report="all")
xb1
xb2 <- xBalance(treatment~HOMELESS_COUNT_1516.x,strata=list(block=~block),data=x2,report="all")
xb2

#Write the files
write.csv(x2, "ehcy.randomized.nj.csv")
write.csv(x2[x2$treatment==1,], "ehcy.treatment.nj.csv")
write.csv(x2[x2$treatment==0, ], "ehcy.control.nj.csv")
