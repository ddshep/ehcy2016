#This file is to run the randomization for the 3 states.
#New Jersey was run on 21 November 2016

#Load packages
require("openxlsx")
require("knitr")

filepath1 <- "C:/Users/Owner/Google Drive/ISCS _ One Thousand Voices/Consulting Projects/SBST/Homeless Education/Data/HOMELESS 2015-2016 - 2016-10-27 (1).xlsx" #file path to district list with number of homeless
filepath2 <- "C:/Users/Owner/Google Drive/ISCS _ One Thousand Voices/Consulting Projects/SBST/Homeless Education/Data/Homeless Free Lunch Request 2015-16.xlsx" #file path to district list with free school lunches

homeless.df <- read.xlsx(filepath1)
lunches.df <- read.xlsx(filepath2)

#Merge the two datasets
homeless.df2 <- merge(homeless.df, lunches.df, by="DISTRICT_ID", all.x = TRUE)
#View(homeless.df2)
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
homeless.df2 <- homeless.df2[-708, ]#Remove the NA row
kable(homeless.df2[which(is.na(email)), c("DISTRICT_ID", "DISTRICT_NAME.x")])#Provide districts that we are missing emails for to NJ
#Remove the missing emails for the many to one analysis
homeless.df2.noNA <- homeless.df2[-which(is.na(homeless.df2$email)), ]
#Identify the duplicate emails -- HL assigned to >1 location
homeless.df2.noNA[duplicated(homeless.df2.noNA$email), ] #7
#Identify how many districts are duplicated (>1 HL for the district)
homeless.df2.noNA[duplicated(homeless.df2.noNA$DISTRICT_ID), ] #NONE!
#Remove the duplicate rows from the DF for randomization
homeless.df2.noNA.noDup <- homeless.df2.noNA[-which(duplicated(homeless.df2.noNA$email)), ]


#When stratified by any homeless and charters
#Blocking variable models 
m0a <- lm(HOMELESS_COUNT_1516.x ~ (HOMELESS_COUNT_1516.x>0),data = homeless.df2)
summary(m0a) #determine amount of variance explained by reporting / not 
m0b <- lm(HOMELESS_COUNT_1516.x ~ (COUNTY_NAME.x=="CHARTERS"), data = homeless.df2)
summary(m0b) #determine amount of variance explained by charter / not
m0c <- lm(HOMELESS_COUNT_1516.x ~ (HOMELESS_COUNT_1516.x>0) + (COUNTY_NAME.x=="CHARTERS"), data = homeless.df2)
summary(m0c)

anova(m0a, m0c) # does adding charter explain more variance than reporting --> no
anova(m0b, m0c) # does adding reporting to charter explain more variance --> yes

#Subset the dataframe into blocks
x <- homeless.df2.noNA.noDup
x.charter.report <- x[x$COUNTY_NAME.x=="CHARTERS" & x$HOMELESS_COUNT_1516.x>0, ]
x.charter.noreport <- x[x$COUNTY_NAME.x=="CHARTERS" & !(x$HOMELESS_COUNT_1516.x>0), ]
x.nocharter.report <- x[!(x$COUNTY_NAME.x=="CHARTERS") & (x$HOMELESS_COUNT_1516.x>0), ]
x.nocharter.noreport <- x[!(x$COUNTY_NAME.x=="CHARTERS") & !(x$HOMELESS_COUNT_1516.x>0), ]
table(charter = x$COUNTY_NAME.x=="CHARTERS", homeless.reported = (x$HOMELESS_COUNT_1516.x>0))

#Randomize 
set.seed(20161118)
x.charter.report$ran <- runif(length(x.charter.report$email), 0, 1)
x.charter.noreport$ran <- runif(length(x.charter.noreport$email), 0, 1)
x.nocharter.report$ran <- runif(length(x.nocharter.report$email), 0, 1)
x.nocharter.noreport$ran <- runif(length(x.nocharter.noreport$email), 0, 1)
#If the random number is >= median then assign to treatment (1), else control (0)
x.charter.report$treatment <- 0 #first assign all 0 
x.charter.report[which(x.charter.report$ran >= median(x.charter.report$ran)), "treatment"] <- 1#second, assign treatment
x.charter.noreport$treatment <- 0 #first assign all 0 
x.charter.noreport[which(x.charter.noreport$ran >= median(x.charter.noreport$ran)), "treatment"] <- 1#second, assign treatment
x.nocharter.report$treatment <- 0 #first assign all 0 
x.nocharter.report[which(x.nocharter.report$ran >= median(x.nocharter.report$ran)), "treatment"] <- 1#second, assign treatment
x.nocharter.noreport$treatment <- 0 #first assign all 0 
x.nocharter.noreport[which(x.nocharter.noreport$ran >= median(x.nocharter.noreport$ran)), "treatment"] <- 1#second, assign treatment
#Combine back into one dataframe
x.randomized <- rbind(x.charter.report, x.charter.noreport, x.nocharter.report, x.nocharter.noreport)
x.treat <- x.randomized[x.randomized$treatment==1, ]
x.control <- x.randomized[x.randomized$treatment==0, ]


#Balance check
m1 <- lm(HOMELESS_COUNT_1516.x ~ treatment + (COUNTY_NAME.x=="CHARTERS") + 
           (HOMELESS_COUNT_1516.x>0),data = x.randomized)
summary(m1) #treatment and control balanced on # of homeless reported

m2 <- lm(FREE_LUNCH ~ treatment + (COUNTY_NAME.x=="CHARTERS") + 
           (HOMELESS_COUNT_1516.x>0),data = x.randomized)
summary(m2) #balanced on number receiving free lunches


#Write the files
write.csv(x.randomized, "ehcy.randomized.nj.csv")
write.csv(x.treat, "ehcy.treatment.nj.csv")
write.csv(x.control, "ehcy.control.nj.csv")
