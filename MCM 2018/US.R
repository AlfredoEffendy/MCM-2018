Migration = read.csv("Migration.csv")
names(Migration) = c("HDI Rank","Country",1990,1995,2000,2005,2010,2015)
Population = read.csv("Population.csv")
names(Population) = c("HDI Rank", "Country", 1990,1995,2000,2005,2010,2015)
HDI = read.csv("HDI.csv")
names(HDI) = c("HDI Rank", "Country", 1990, 1995, 2000, 2005, 2010, 2015)


US = rep("United States",6)
Year = c(1990,1995,2000,2005,2010,2015)
USmigration = rep(0,6)
USpopulation = rep(0,6)
USHDI = rep(0,6)

USMindex = 1
for(country in Migration$Country){
  if(country == " United States"){
    USmigration[1] = Migration[USMindex,3]
    USmigration[2] = Migration[USMindex,4]
    USmigration[3] = Migration[USMindex,5]
    USmigration[4] = Migration[USMindex,6]
    USmigration[5] = Migration[USMindex,7]
    USmigration[6] = Migration[USMindex,8]
  }
  USMindex = USMindex + 1
} 

USPindex = 1
for(country in Population$Country){
  if(country == " United States"){
    USpopulation[1] = Population[USPindex,3]
    USpopulation[2] = Population[USPindex,4]
    USpopulation[3] = Population[USPindex,5]
    USpopulation[4] = Population[USPindex,6]
    USpopulation[5] = Population[USPindex,7]
    USpopulation[6] = Population[USPindex,8]
  }
  USPindex = USPindex + 1
} 


USHindex = 1
for(country in HDI$Country){
  if(country == " United States"){
    USHDI[1] = HDI[USHindex,3]
    USHDI[2] = HDI[USHindex,4]
    USHDI[3] = HDI[USHindex,5]
    USHDI[4] = HDI[USHindex,6]
    USHDI[5] = HDI[USHindex,7]
    USHDI[6] = HDI[USHindex,8]
  }
  USHindex = USHindex + 1
} 

USA = data.frame(US, Year, USmigration, USpopulation, USHDI)