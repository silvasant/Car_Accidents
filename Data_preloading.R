## Data input

library(readr)
Acc <- read_csv("Project Data/Acc.csv", col_types = cols(Accident_Index = col_character(), 
      Date = col_date(format = "%d/%m/%Y"), 
      Time = col_time(format = "%H:%M")))
Acc<-Acc[!is.na(Acc$Latitude)&!is.na(Acc$Longitude),]
Cas <- read_csv("Project Data/Cas.csv", col_types = cols(Accident_Index = col_character()))
Veh <- read_csv("Project Data/Veh.csv", col_types = cols(Accident_Index = col_character()))
Veh_df<-Veh[Veh$Vehicle_Reference==1,]
Veh_df$Latitude<-as.numeric(Acc$Latitude[match(Veh_df$Accident_Index,Acc$Accident_Index)])
Veh_df$Longitude<-as.numeric(Acc$Longitude[match(Veh_df$Accident_Index,Acc$Accident_Index)])
Veh_df$Date<-Acc$Date[match(Veh_df$Accident_Index,Acc$Accident_Index)]
Veh_df$District<-Acc$`Local_Authority_(District)`[match(Veh_df$Accident_Index,Acc$Accident_Index)]
Veh_df$GeoCode<-Acc$LSOA_of_Accident_Location[match(Veh_df$Accident_Index,Acc$Accident_Index)]

