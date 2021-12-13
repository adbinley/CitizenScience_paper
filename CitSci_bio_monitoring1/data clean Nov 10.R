# data cleaning NOV 10

#### threats/responses ####


data_clean$PredictorCause_Metric <- data_clean$PredictorCause_Metric %>%
  tolower() 
data_clean$Response_Metric <- data_clean$Response_Metric%>%
  tolower() 

data_clean1 <- separate_rows(data_clean, PredictorCause_Metric, sep=";\\s+")
data_clean1 <- separate_rows(data_clean1, Response_Metric, sep=";\\s+")

#gsub method - other method seems fishy

data_clean1$threat1 <- data_clean1$PredictorCause_Metric
data_clean1$threat1 <- gsub("habitat loss and fragmentation", "habitat loss", data_clean1$threat1)
data_clean1$threat1 <- gsub("habitat", "habitat loss", data_clean1$threat1) 
data_clean1$threat1 <- gsub("none/habitat", "habitat loss", data_clean1$threat1) 
data_clean1$threat1 <- gsub("hunting", "harvesting", data_clean1$threat1) 
data_clean1$threat1 <- gsub("habitat loss ", "habitat loss", data_clean1$threat1)
data_clean1$threat1 <- gsub("climate change ", "climate change", data_clean1$threat1)
data_clean1$threat1 <- gsub("habitat loss loss", "habitat loss", data_clean1$threat1)
data_clean1$threat1 <- gsub("invasives ", "invasives", data_clean1$threat1)
data_clean1$threat1 <- gsub("habitat lossloss", "habitat loss", data_clean1$threat1)
data_clean1$threat1 <- gsub("none", "no threat", data_clean1$threat1)

unique(data_clean1$threat1)

# responses
unique(data_clean1$Response_Metric)

data_clean1$response1 <- data_clean1$Response_Metric
data_clean1$response1 <- gsub("abundance and trend" , "abundance and trends" , data_clean1$response1)
data_clean1$response1 <- gsub("distribution and range shifts " , "distribution and range shifts" , data_clean1$response1)
data_clean1$response1 <- gsub("distrubution and range shifts" , "distribution and range shifts" , data_clean1$response1)
data_clean1$response1 <- gsub("abundance, trends", "abundance and trends" , data_clean1$response1)
data_clean1$response1 <- gsub("life history evolution", "life-history evolution", data_clean1$response1)
data_clean1$response1 <- gsub("richness, diversity, community composition ", "richness, diversity, community composition", data_clean1$response1)
data_clean1$response1 <- gsub("abundance. trends" , "abundance and trends" , data_clean1$response1)
data_clean1$response1 <- gsub("adundance, trends" , "abundance and trends" , data_clean1$response1)
data_clean1$response1 <- gsub("phenology " , "phenology" , data_clean1$response1)
data_clean1$response1 <- gsub("abundance and trendss" , "abundance and trends" , data_clean1$response1)
data_clean1$response1 <- gsub("abundance,trends" , "abundance and trends" , data_clean1$response1)
#data_clean1$response1 <- gsub("", "no response" , data_clean1$response1)

unique(data_clean1$response1)

save(data_clean1, file="data/data_clean_nov10.RData")
write.csv(data_clean1, file = "data/data_clean_nov10.csv")


