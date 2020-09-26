library(rworldmap)

theCountries <- c("SYR","IND","BGD","CHN","PAK","IDN","PHL","SDN","ESP","LKA",
                  "MMR","LBY","IRN","NPL","MAR","GEO","MLI","MEX","NGA","ROU",
                  "EGY","ARE","QAT","OMN","CHE","AFG","AUS","CAN","DEU","GBR",
                  "USA","IRQ","JOR","KWT","LBN","MYS","RUS","SAU","SDN","TUR",
                  "FRA","JPN","SGP")

malDF <- data.frame(country = c("SYR","IND","BGD","CHN","PAK","IDN","PHL","SDN","ESP","LKA",
                                "MMR","LBY","IRN","NPL","MAR","GEO","MLI","MEX","NGA","ROU",
                                "EGY","ARE","QAT","OMN","CHE","AFG","AUS","CAN","DEU","GBR",
                                "USA","IRQ","JOR","KWT","LBN","MYS","RUS","SAU","SDN","TUR",
                                "FRA","JPN","SGP"), 
                    Languages = c("Arabic","Hindi","Bengali","Mandarin","Punjabi","Indonesian",
                                  "English","Arabic","Spanish","English","Burmese","Arabic",
                                  "Persian","Hindi","Arabic","Georgian","French","Spanish",
                                  "English","Romanian","Arabic","Arabic","Arabic","Arabic",
                                  "German","Persian","English","English","German","English",
                                  "English","Arabic","Arabic","Arabic","Arabic","Indonesian",
                                  "Russian","Arabic","Arabic","Turkish","French","Japanese",
                                  "Mandarin"))

malMap <- joinCountryData2Map(malDF, joinCode = "ISO3", nameJoinColumn = "country")

mapParams <- mapCountryData(malMap, nameColumnToPlot=c("Languages"), catMethod = "categorical",
               numCats = 15, missingCountryCol = gray(.8),addLegend = T,xlim = c(-300,200))