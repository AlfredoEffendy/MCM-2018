Languages = c("ARABIC","CHINESE","FRENCH","GERMAN","ITALIAN","JAPANESE","RUSSIAN","SPANISH")
Learners = rep(0,56*8)

Enrollment = read.csv("Ed_Data.csv",header=F)
names(Enrollment) = c("Year","Language","Learners")
Listoflanguage = Enrollment$Language
count = 1
index = rep(0,length(Listoflanguage))
for(l in Listoflanguage){
  index[count] = count
  count = count + 1
}
lang = data.frame(Listoflanguage,index)

result = matrix(integer(56*8),nrow = 56, ncol = 8)



for(i in (1:length(Listoflanguage))){
  if(Enrollment$Language[i] %in% Languages){
    if(Enrollment$Language[i] == "ARABIC"){
      result[Enrollment$Year[i]-1957,1] = result[Enrollment$Year[i]-1957,1] + Enrollment$Learners[i]
    }
    if(Enrollment$Language[i] == "CHINESE"){
      result[Enrollment$Year[i]-1957,2] = result[Enrollment$Year[i]-1957,2] + Enrollment$Learners[i]
    }
    if(Enrollment$Language[i] == "FRENCH"){
      result[Enrollment$Year[i]-1957,3] = result[Enrollment$Year[i]-1957,3] + Enrollment$Learners[i]
    }
    if(Enrollment$Language[i] == "GERMAN"){
      result[Enrollment$Year[i]-1957,4] = result[Enrollment$Year[i]-1957,4] + Enrollment$Learners[i]
    }
    if(Enrollment$Language[i] == "ITALIAN"){
      result[Enrollment$Year[i]-1957,5] = result[Enrollment$Year[i]-1957,5] + Enrollment$Learners[i]
    }
    if(Enrollment$Language[i] == "JAPANESE"){
      result[Enrollment$Year[i]-1957,6] = result[Enrollment$Year[i]-1957,6] + Enrollment$Learners[i]
    }
    if(Enrollment$Language[i] == "RUSSIAN"){
      result[Enrollment$Year[i]-1957,7] = result[Enrollment$Year[i]-1957,7] + Enrollment$Learners[i]
    }
    if(Enrollment$Language[i] == "SPANISH"){
      result[Enrollment$Year[i]-1957,8] = result[Enrollment$Year[i]-1957,8] + Enrollment$Learners[i]
    }
  }
}

lang_year = matrix(integer(23*8),nrow = 23, ncol = 8)
year = c()
exist = 1
for(i in (1:56)){
  if(sum(result[i,] != 0)){
    lang_year[exist,] = result[i,]
    year[exist] = i+1957
    exist = exist + 1
  }
}

final = data.frame(year,lang_year)

names(final) = c("YEAR", "ARABIC","CHINESE","FRENCH","GERMAN","ITALIAN","JAPANESE","RUSSIAN","SPANISH")
year1980 = year[14:23] - 1980

Arabic = lm(lang_year[14:23,1]  ~ year1980, data = final)
Chinese = lm(lang_year[14:23,2]  ~ year1980, data = final)
French = lm(lang_year[14:23,3]  ~ year1980, data = final)
German = lm(lang_year[14:23,4]  ~ year1980, data = final)
Italian = lm(lang_year[14:23,5]  ~ year1980, data = final)
Japanese = lm(lang_year[14:23,6]  ~ year1980, data = final)
Russian = lm(lang_year[14:23,7]  ~ year1980, data = final)
Spanish = lm(lang_year[14:23,8]  ~ year1980, data = final)

Growth_Arabic = rep(0,10)
Growth_Chinese = rep(0,10)
Growth_French = rep(0,10)
Growth_German = rep(0,10)
Growth_Italian = rep(0,10)
Growth_Japanese = rep(0,10)
Growth_Russian = rep(0,10)
Growth_Spanish = rep(0,10)


for(i in (14:23)){
  Growth_Arabic[i-13] = (lang_year[i,1] - lang_year[i-1,1])/(year[i] - year[i-1]) *100 / lang_year[i,1]
  Growth_Chinese[i-13] = (lang_year[i,2] - lang_year[i-1,2])/(year[i] - year[i-1]) *100 / lang_year[i,2]
  Growth_French[i-13] = (lang_year[i,3] - lang_year[i-1,3])/(year[i] - year[i-1]) *100 / lang_year[i,3]
  Growth_German[i-13] = (lang_year[i,4] - lang_year[i-1,4])/(year[i] - year[i-1]) *100 / lang_year[i,4]
  Growth_Italian[i-13] = (lang_year[i,5] - lang_year[i-1,5])/(year[i] - year[i-1]) *100 / lang_year[i,5]
  Growth_Japanese[i-13] = (lang_year[i,6] - lang_year[i-1,6])/(year[i] - year[i-1]) *100 / lang_year[i,6]
  Growth_Russian[i-13] = (lang_year[i,7] - lang_year[i-1,7])/(year[i] - year[i-1]) *100 / lang_year[i,7]
  Growth_Spanish[i-13] = (lang_year[i,8] - lang_year[i-1,8])/(year[i] - year[i-1]) *100 / lang_year[i,8]
}

Rate_Arabic = lm(Growth_Arabic  ~ exp(-year1980))
Rate_Chinese = lm(Growth_Chinese  ~ year1980)
Rate_French = lm(Growth_French  ~ year1980)
Rate_German = lm(Growth_German  ~ year1980)
Rate_Italian = lm(Growth_Italian  ~ year1980)
Rate_Japanese = lm(Growth_Japanese  ~ year1980)
Rate_Russian = lm(Growth_Russian  ~ year1980)
Rate_Spanish = lm(Growth_Spanish  ~ year1980)
