## Data input

library(readr)
#library(sf)
#source('API_info.R')
Acc <- read_csv("Project Data/Acc.csv", col_types = cols(Accident_Index = col_character(), 
      Date = col_date(format = "%d/%m/%Y"), 
      Time = col_time(format = "%H:%M")))
Acc<-Acc[!is.na(Acc$Latitude)&!is.na(Acc$Longitude),]
Cas <- read_csv("Project Data/Cas.csv", col_types = cols(Accident_Index = col_character()))
Veh <- read_csv("Project Data/Veh.csv", col_types = cols(Accident_Index = col_character()))
Vehicle_types<-read_csv("Project Data/Vehicle_Type.csv")
Veh_df<-Veh[Veh$Vehicle_Reference==1,]
Veh_df$Latitude<-as.numeric(Acc$Latitude[match(Veh_df$Accident_Index,Acc$Accident_Index)])
Veh_df$Longitude<-as.numeric(Acc$Longitude[match(Veh_df$Accident_Index,Acc$Accident_Index)])
Veh_df$Date<-Acc$Date[match(Veh_df$Accident_Index,Acc$Accident_Index)]
Veh_df$District<-Acc$`Local_Authority_(District)`[match(Veh_df$Accident_Index,Acc$Accident_Index)]
Veh_df$GeoCode<-Acc$LSOA_of_Accident_Location[match(Veh_df$Accident_Index,Acc$Accident_Index)]
Veh_df$Vehicle_Type_categorical<-Vehicle_types$label[match(Veh_df$Vehicle_Type,Vehicle_types$code)]

Veh_df$Vehicle_Type_categorical_abbreviated<-Veh_df$Vehicle_Type
Veh_df$Vehicle_Type_categorical_abbreviated<-ifelse(Veh_df$Vehicle_Type_categorical_abbreviated %in% c(1,16,22,17,90),'Otros',Veh_df$Vehicle_Type_categorical_abbreviated)
Veh_df$Vehicle_Type_categorical_abbreviated<-ifelse(Veh_df$Vehicle_Type_categorical_abbreviated %in% c(2:5,18,23,97),'Motos',Veh_df$Vehicle_Type_categorical_abbreviated)
Veh_df$Vehicle_Type_categorical_abbreviated<-ifelse(Veh_df$Vehicle_Type_categorical_abbreviated %in% c(8,9),'Autos',Veh_df$Vehicle_Type_categorical_abbreviated)
Veh_df$Vehicle_Type_categorical_abbreviated<-ifelse(Veh_df$Vehicle_Type_categorical_abbreviated %in% c(10,11),'Transporte de pasajeros',Veh_df$Vehicle_Type_categorical_abbreviated)
Veh_df$Vehicle_Type_categorical_abbreviated<-ifelse(Veh_df$Vehicle_Type_categorical_abbreviated %in% c(20,21,19,98),'Transporte de bienes',Veh_df$Vehicle_Type_categorical_abbreviated)
Veh_df$Vehicle_Type_categorical_abbreviated<-ifelse(Veh_df$Vehicle_Type_categorical_abbreviated %in% c(-1),'S/D',Veh_df$Vehicle_Type_categorical_abbreviated)
Veh_df$Month<-str_to_sentence(month(Veh_df$Date,label = T,abbr = F))
Veh_df$Amort<-(1-.2)^Veh_df$Age_of_Vehicle

