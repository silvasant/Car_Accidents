## Data input

library(readr)
Acc <- read_csv("Project Data/Acc.csv", col_types = cols(Accident_Index = col_character(), 
      Date = col_date(format = "%d/%m/%Y"), 
      Time = col_time(format = "%H:%M")))
Acc<-Acc[!is.na(Acc$Latitude)&!is.na(Acc$Longitude),]
Cas <- read_csv("Project Data/Cas.csv", col_types = cols(Accident_Index = col_character()))
Veh <- read_csv("Project Data/Veh.csv", col_types = cols(Accident_Index = col_character()))

