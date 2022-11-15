##here I will operate data cleansing to the dataset

#import data
vdata <- readxl::read_excel(
  'file location'
)

#import libraries
library(plyr)
library(dplyr)
library(tidyr)
library(countrycode)
require(data.table)
require(ISOweek)

#The data structure is already known so I just check for null values to have an overview
colSums(is.na(vdata))

#filter unnecessary target groups and subregions
b <- vdata %>%
  filter(TargetGroup=="ALL" & 
           nchar(Region)<=2)

colSums(is.na(b))

#look into na values
a <- b %>%
  filter(is.na(NumberDosesReceived))

#complete dataset with missing categories rows
b <- b %>%
  complete(YearWeekISO, 
           ReportingCountry, 
           Vaccine, 
           fill = list(
             NumberDosesReceived = 0,
             NumberDosesExported = 0,
             FirstDose = 0,
             FirstDoseRefused = 0,
             SecondDose = 0,
             DoseAdditional1 = 0,
             UnknownDose = 0,
             DoseAdditional2 = 0
             )
           )

#delete target group column
b = subset(b, 
           select = -TargetGroup )

colSums(is.na(b))

#Add missing values in Region category
b<-  ddply(b, 
           .(YearWeekISO, 
             ReportingCountry, 
             Vaccine), 
           transform, 
           Region = ReportingCountry)

#Transform country code in region to country name
b$Region<-countrycode(b$Region, 
                      'iso2c', 
                      'country.name', 
                      warn = TRUE,
                      nomatch = 'Greece'
                      ) #it does not recognize only Greece

colSums(is.na(b))

#order dataframe
b <- b[order(b$ReportingCountry, 
             b$YearWeekISO, 
             b$Vaccine),]

#add year and week columns
b$Year <- substr(b$YearWeekISO , 
                 start=1, 
                 stop=4)

b$Week <- substr(b$YearWeekISO, 
                 start=7, 
                 stop=8)

#fill NA values with Country and year reference
b<- b %>% 
  group_by(ReportingCountry, 
           Year
           ) %>% 
  tidyr::fill(c(Denominator, Population), 
              .direction = "downup"
              ) %>% 
  ungroup()

colSums(is.na(b))

#Better understand why there are NA values
a <- b %>%
  filter(is.na(Population))

a <- vdata %>%
  filter(ReportingCountry=="LI")

#modify week iso for further transformations
b$YearWeekISO <- paste(b$YearWeekISO, 
                       "4", 
                       sep = "-")

b$thursday_ISO <- ISOweek2date(b$YearWeekISO)

b<- as.data.table(b) 

#add yearmonth column
b[, YearMonth := format(thursday_ISO, 
                        "%Y-%m")]

#aggregate monthly
b<- b %>% 
  group_by(ReportingCountry, 
           YearMonth, 
           Vaccine, 
           Region
           ) %>% 
  summarise(Denominator=mean(Denominator),
            Population=mean(Population),
            NumberDosesReceived=sum(NumberDosesReceived),
            NumberDosesExported=sum(NumberDosesExported),
            FirstDose=sum(FirstDose),
            FirstDoseRefused=sum(FirstDoseRefused),
            SecondDose=sum(SecondDose),
            DoseAdditional1=sum(DoseAdditional1),
            DoseAdditional2=sum(DoseAdditional2),
            UnknownDose= sum(UnknownDose)
            )

#calculate cumulative doses received and administrated
b<-  ddply(b, 
           .(YearMonth, 
             ReportingCountry, 
             Vaccine
             ), 
           transform, 
           tot_administrated = sum(c(FirstDose, 
                                     SecondDose, 
                                     DoseAdditional1, 
                                     UnknownDose, 
                                     DoseAdditional2
                                     )
                                   )
           )

colSums(is.na(b))

b<-  ddply(b, 
           .(YearMonth, 
             ReportingCountry, 
             Vaccine
             ), 
           transform, 
           Cumulative_Monthly_A = cumsum(tot_administrated))

b<-  ddply(b, 
           .(YearMonth, 
             ReportingCountry, 
             Vaccine
             ), 
           transform, 
           Cumulative_Monthly_R = cumsum(NumberDosesReceived))

b<-  ddply(b, 
           .(ReportingCountry, 
             Vaccine
             ), 
           transform, 
           Cumulative_A = cumsum(tot_administrated))

colSums(is.na(b))

b<-  ddply(b, 
           .(ReportingCountry, 
             Vaccine
             ), 
           transform, 
           Cumulative_R = cumsum(NumberDosesReceived))


#export as csv
write.csv(b,"new file location", row.names = FALSE)
