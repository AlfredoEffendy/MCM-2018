setwd("C://Education/University of California Irvine/MCM 2018")

GDP_Growth = read.csv2("GDP_Growth.csv", sep = "\t")
GDP_Data = read.csv("GDP_Growth_Data.csv",header = F)
Population_Data = read.csv("Population.csv",header = F)
Country_List = GDP_Growth$Country.Name
Country = rep(character(264*56))
Year = integer(264*56)
GDP = rep(0,264*56)
Population = rep(0,264*56)
index = 1
cindex = 1
for(country in Country_List){
  for(time in (1:56)){
    Country[index] = country
    Year[index] = time + 1960
    if(is.null(GDP_Data[cindex,index])){
      GDP[index] = 0
      }
      else{
        GDP[index] = GDP_Data[cindex,index]
      }
    index = index + 1
    if(is.null(Population_Data[cindex,index])){
      Population[index] = 0
    }
    else{
      Population[index] = Population_Data[cindex,index]
    }
  }
  cindex = cindex + 1
}

Data = data.frame(Country, Year, GDP, Population)