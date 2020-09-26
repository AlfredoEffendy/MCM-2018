library(rworldmap)

#Migration Data
Bilateral = read.csv("BiMigra.csv", sep = ",")
names(Bilateral) = c("Origin","Destination",1960,1970,1980,1990,2000,"Include")

#Percentage of Language Spoken
table_test = read.csv("Language_Index_Test.csv", sep = ",", header = F)
actual_table = as.matrix(table_test)
actual_table[1,1] = 0.66
colnames(actual_table) = c("Mandarin", "English", "Hindi", "Spanish", "Arabic",
                   "Malay","Russian","Bengali","Portuguese","French",
                   "Hausa","Punjabi","Japanese","German","Persian")
rownames(actual_table) = List_Of_Countries


Origin = c("Syrian Arab Republic","India","Bangladesh","China","Pakistan",
           "Indonesia","Philippines","Sudan","Spain","Sri Langka","Myanmar",
           "Libya","Iran, Islamic Rep.","Nepal","Morocco","Georgia","Mali",
           "Mexico","Nigeria","Romania","Egypt, Arab Rep.")

Destination = c("United Arab Emirates","Qatar","Oman","Switzerland","Afghanistan",
                "Australia","Canada","Germany","United Kingdom","United States",
                "Iraq","Jordan","Kuwait","Lebanon","Malaysia","Russia",
                "Saudi Arabia","Sudan","Turkey","France","Japan","Singapore")

result = matrix(rep(0,5*462),ncol=5)
for(i in (1:462)){
  result[i,1] = Bilateral$`1960`[i]
  result[i,2] = Bilateral$`1970`[i]
  result[i,3] = Bilateral$`1980`[i]
  result[i,4] = Bilateral$`1990`[i]
  result[i,5] = Bilateral$`2000`[i]
}

time = c(0,10,20,30,40)

regression = c()

indexdes = 1
for(i in Origin){
  for(j in Destination){
    regression = append(regression,i)
    regression = append(regression,j)
    regression = append(regression,coef(lm(result[indexdes,]~time))[1])
    regression = append(regression,coef(lm(result[indexdes,]~time))[2])
    r = summary(lm(result[indexdes,]~time))$r.squared
    f = summary(lm(result[indexdes,]~time))$fstatistic
    p = pf(f[1],f[2],f[3],lower.tail=F)
    #pwer = pwr.f2.test(u = f[2], v = f[3], f2 = r/(1-r))
    regression = append(regression,p)
    regression = append(regression,r)
    #regression = append(regression,pwer)
    indexdes = indexdes + 1
  }
}

sequence = seq(from = 1, to = 462*6, by = 6)

finalresult = matrix(character(6*462), ncol = 6, nrow = 462)


for(i in (1:462)){
  a = sequence[i]
  b = sequence[i] + 5
  finalresult[i,] = regression[a:b]
}

#Exclude Insignificant Migrations
include = Bilateral$Include
finalresult = finalresult[include,]

included_languages = c("Mandarin","English","Hindi","Spanish","Arabic","Malay",
                       "Russian","Bengali","Portuguese","French","Hausa","Punjabi",
                       "Japanese","German","Persian")

#Prediction the Migration 
prediction = matrix(0,nrow = length(finalresult)/6,ncol  = 6)

for(i in (1:length(finalresult)/6)){
  for(t in (6:11)){
    prediction[i,t-5] = as.numeric(finalresult[i,3]) + as.numeric(finalresult[i,4])*t*10
  }
}

predicted_table = data.frame(finalresult,prediction)
names(predicted_table) = c("Origin","Destination","Intercept","Slope","p-value",
                           "R^2",2020,2030,2040,2050,2060,2070)

#Compute the Change in Languages
change_languages_2020 = matrix(0,nrow = length(finalresult)/6, ncol = 17)

for(i in (1:length(finalresult)/6)){
  change_languages_2020[i,1] = finalresult[i,1]
  change_languages_2020[i,2] = finalresult[i,2]
  change_languages_2020[i,3:17] = as.numeric(actual_table[finalresult[i,1],])*prediction[i,1]
}

ImGer = c(4,7,20,24,41,45,56,59,72,76,90,93,107,110,134,137,146,150,171,178,181,202,
          205,213,218,221,228,231,239,242,252,255) #16
ImEng = c(5,6,8,9,22,23,25,26,37,43,44,46,47,55,57,58,60,61,68,74,75,77,78,87,91,92,94,
          95,103,108,109,111,112,121,125:128,135,136,138,139,148,149,151,152,160,
          161:164,169,170,172,173,179,180,182,183,193:195,199,203,204,206,207,214,215,
          219,220,222,227,240,241,243,244,253,254,256,257) #4
ImFre = c(16,53,66,85,101,119,141,158,166,190,212,225,236,247,265) #12
ImArab = c(17,18,19,21,27:30,32,33,38:40,42,48:50,52,62,69:71,73,79:81,83,88,89,96:98,
           100,104:106,113:115,118,143:145,147,153:155,157,176,177,184:186,216,234,235,
           238,245) #7
ImMalay = c(31,51,63,82,116,156,165,196,223,192,197) #8
ImJapan = c(36,54,67,86,102,120,142,159,167,191,198,226,237,248,266) #15
ImRus = c(64,117,187,224) #9

# 0.43 Factor of Immigrant Learning New Languages
change_languages_2020[ImGer,16] = as.numeric(change_languages_2020[ImGer,16]) + 
                                  0.43*prediction[ImGer,1]
change_languages_2020[ImEng,4] = as.numeric(change_languages_2020[ImEng,4]) + 
                                  0.43*prediction[ImEng,1]
change_languages_2020[ImFre,12] = as.numeric(change_languages_2020[ImFre,12]) + 
                                  0.43*prediction[ImFre,1]
change_languages_2020[ImArab,7] = as.numeric(change_languages_2020[ImArab,7]) + 
                                  0.43*prediction[ImArab,1]
change_languages_2020[ImMalay,8] = as.numeric(change_languages_2020[ImMalay,8]) + 
                                  0.43*prediction[ImMalay,1]
change_languages_2020[ImJapan,15] = as.numeric(change_languages_2020[ImJapan,15]) + 
                                  0.43*prediction[ImJapan,1]
change_languages_2020[ImRus,9] = as.numeric(change_languages_2020[ImRus,9]) + 
                                  0.43*prediction[ImRus,1]

change_languages_2030 = matrix(0,nrow = length(finalresult)/6, ncol = 17)

for(i in (1:length(finalresult)/6)){
  change_languages_2030[i,1] = finalresult[i,1]
  change_languages_2030[i,2] = finalresult[i,2]
  change_languages_2030[i,3:17] = as.numeric(actual_table[finalresult[i,1],])*prediction[i,2]
}

change_languages_2040 = matrix(0,nrow = length(finalresult)/6, ncol = 17)

for(i in (1:length(finalresult)/6)){
  change_languages_2040[i,1] = finalresult[i,1]
  change_languages_2040[i,2] = finalresult[i,2]
  change_languages_2040[i,3:17] = as.numeric(actual_table[finalresult[i,1],])*prediction[i,3]
}

change_languages_2050 = matrix(0,nrow = length(finalresult)/6, ncol = 17)

for(i in (1:length(finalresult)/6)){
  change_languages_2050[i,1] = finalresult[i,1]
  change_languages_2050[i,2] = finalresult[i,2]
  change_languages_2050[i,3:17] = as.numeric(actual_table[finalresult[i,1],])*prediction[i,4]
}

change_languages_2060 = matrix(0,nrow = length(finalresult)/6, ncol = 17)

for(i in (1:length(finalresult)/6)){
  change_languages_2060[i,1] = finalresult[i,1]
  change_languages_2060[i,2] = finalresult[i,2]
  change_languages_2060[i,3:17] = as.numeric(actual_table[finalresult[i,1],])*prediction[i,5]
}

change_languages_2070 = matrix(0,nrow = length(finalresult)/6, ncol = 17)

for(i in (1:length(finalresult)/6)){
  change_languages_2070[i,1] = finalresult[i,1]
  change_languages_2070[i,2] = finalresult[i,2]
  change_languages_2070[i,3:17] = as.numeric(actual_table[finalresult[i,1],])*prediction[i,6]
}

change_languages_2070[ImGer,16] = as.numeric(change_languages_2070[ImGer,16]) + 
  0.43*prediction[ImGer,6]
change_languages_2070[ImEng,4] = as.numeric(change_languages_2070[ImEng,4]) + 
  0.43*prediction[ImEng,6]
change_languages_2070[ImFre,12] = as.numeric(change_languages_2070[ImFre,12]) + 
  0.43*prediction[ImFre,6]
change_languages_2070[ImArab,7] = as.numeric(change_languages_2070[ImArab,7]) + 
  0.43*prediction[ImArab,6]
change_languages_2070[ImMalay,8] = as.numeric(change_languages_2070[ImMalay,8]) + 
  0.43*prediction[ImMalay,6]
change_languages_2070[ImJapan,15] = as.numeric(change_languages_2070[ImJapan,15]) + 
  0.43*prediction[ImJapan,6]
change_languages_2070[ImRus,9] = as.numeric(change_languages_2070[ImRus,9]) + 
  0.43*prediction[ImRus,6]



change_2020 = data.frame(change_languages_2020)
names(change_2020) = c("Origin","Destination","Mandarin","English","Hindi","Spanish",
                       "Arabic","Malay","Russian","Bengali","Portuguese","French",
                       "Hausa","Punjabi","Japanese","German","Persian")

change_2030 = data.frame(change_languages_2030)
names(change_2030) = c("Origin","Destination","Mandarin","English","Hindi","Spanish",
                       "Arabic","Malay","Russian","Bengali","Portuguese","French",
                       "Hausa","Punjabi","Japanese","German","Persian")

change_2040 = data.frame(change_languages_2040)
names(change_2040) = c("Origin","Destination","Mandarin","English","Hindi","Spanish",
                       "Arabic","Malay","Russian","Bengali","Portuguese","French",
                       "Hausa","Punjabi","Japanese","German","Persian")

change_2050 = data.frame(change_languages_2050)
names(change_2050) = c("Origin","Destination","Mandarin","English","Hindi","Spanish",
                       "Arabic","Malay","Russian","Bengali","Portuguese","French",
                       "Hausa","Punjabi","Japanese","German","Persian")

change_2060 = data.frame(change_languages_2060)
names(change_2060) = c("Origin","Destination","Mandarin","English","Hindi","Spanish",
                       "Arabic","Malay","Russian","Bengali","Portuguese","French",
                       "Hausa","Punjabi","Japanese","German","Persian")

change_2070 = data.frame(change_languages_2070)
names(change_2070) = c("Origin","Destination","Mandarin","English","Hindi","Spanish",
                       "Arabic","Malay","Russian","Bengali","Portuguese","French",
                       "Hausa","Punjabi","Japanese","German","Persian")

#2020 for Each Country

IndexUAE = c(1,17,38,69,88,104,122,143,176,200,249)
IndexQatar = c(2,18,39,70,89,105,123,144,168,177,201,238,250)
IndexOman = c(3,19,40,71,106,124,145,251)
IndexSwitzerland = c(4,20,41,56,72,90,107,134,146,178,202,218,228,239,252)
IndexAustralia = c(5,22,43,57,74,91,108,125,135,148,161,169,179,193,203,219,229,240,253)
IndexCanada = c(6,23,44,58,75,92,109,126,136,149,162,170,180,194,204,220,230,241,254)
IndexGermany = c(7,24,45,59,76,93,110,137,150,171,181,205,213,221,231)
IndexUK = c(8,25,46,60,77,94,111,127,138,151,163,172,182,206,214,232,243,256)
IndexUS = c(9,26,47,61,78,95,112,128,139,152,164,173,183,195,207,215)
IndexIraq = c(10,27,129,184,258)
IndexJordan = c(11,28,48,62,79,96,113,130,153,259)
IndexKuwait = c(12,29,49,80,97,114,131,154,185,260)
IndexLebanon = c(13,30,50,81,98,115,132,155,186,208,245,261)
IndexSaudi = c(14,32,52,83,100,118,133,157,188,197,209,234,262)
IndexTurkey = c(15,34,65,84,140,175,189,211,246,264)
IndexFrance = c(16,35,53,66,85,101,119,141,158,166,190,212,217,225,236,247,265)
IndexRussia = c(64,117,187,224)
IndexAfghan = c(21,42,73,147,192)
IndexMalay = c(31,51,63,82,99,116,156,165,196,223)
IndexSudan = c(33,174,210,216,235,263)
IndexJapan = c(36,54,67,86,102,120,142,159,167,191,198,226,237,248,266)
IndexSingapore = c(37,55,68,87,103,121,160,199,227)

Change_Syria_2020 = matrix(0,ncol=15,nrow=1)
for(i in (1:15)){
  Change_Syria_2020[,i] = (-1)*sum(as.numeric(change_languages_2020[1:16,i+2]))
}

Change_India_2020 = matrix(0,ncol=15,nrow=1)
for(i in (1:15)){
  Change_India_2020[,i] = (-1)*sum(as.numeric(change_languages_2020[17:37,i+2]))
}

Change_Bangladesh_2020 = matrix(0,ncol=15,nrow=1)
for(i in (1:15)){
  Change_Bangladesh_2020[,i] = (-1)*sum(as.numeric(change_languages_2020[38:55,i+2]))
}

Change_China_2020 = matrix(0,ncol=15,nrow=1)
for(i in (1:15)){
  Change_China_2020[,i] = (-1)*sum(as.numeric(change_languages_2020[56:68,i+2]))
}

Change_Pakistan_2020 = matrix(0,ncol=15,nrow=1)
for(i in (1:15)){
  Change_Pakistan_2020[,i] = (-1)*sum(as.numeric(change_languages_2020[69:87,i+2]))
}

Change_Indonesia_2020 = matrix(0,ncol=15,nrow=1)
for(i in (1:15)){
  Change_Indonesia_2020[,i] = (-1)*sum(as.numeric(change_languages_2020[88:103,i+2]))
}

Change_Philippines_2020 = matrix(0,ncol=15,nrow=1)
for(i in (1:15)){
  Change_Philippines_2020[,i] = (-1)*sum(as.numeric(change_languages_2020[104:121,i+2]))
}

Change_Sudan_2020 = matrix(0,ncol=15,nrow=1)
for(i in (1:15)){
  Change_Sudan_2020[,i] = (-1)*sum(as.numeric(change_languages_2020[122:133,i+2])) + 
                            sum(as.numeric(change_languages_2020[IndexSudan,i+2]))
}

Change_Spain_2020 = matrix(0,ncol=15,nrow=1)
for(i in (1:15)){
  Change_Spain_2020[,i] = (-1)*sum(as.numeric(change_languages_2020[134:142,i+2]))
}

Change_SriLangka_2020 = matrix(0,ncol=15,nrow=1)
for(i in (1:15)){
  Change_SriLangka_2020[,i] = (-1)*sum(as.numeric(change_languages_2020[143:160,i+2]))
}

Change_Libya_2020 = matrix(0,ncol=15,nrow=1)
for(i in (1:15)){
  Change_Libya_2020[,i] = (-1)*sum(as.numeric(change_languages_2020[168:175,i+2]))
}

Change_Myanmar_2020 = matrix(0,ncol=15,nrow=1)
for(i in (1:15)){
  Change_Myanmar_2020[,i] = (-1)*sum(as.numeric(change_languages_2020[161:167,i+2]))
}

Change_Iran_2020 = matrix(0,ncol=15,nrow=1)
for(i in (1:15)){
  Change_Iran_2020[,i] = (-1)*sum(as.numeric(change_languages_2020[176:191,i+2]))
}

Change_Nepal_2020 = matrix(0,ncol=15,nrow=1)
for(i in (1:15)){
  Change_Nepal_2020[,i] = (-1)*sum(as.numeric(change_languages_2020[192:199,i+2]))
}

Change_Morocco_2020 = matrix(0,ncol=15,nrow=1)
for(i in (1:15)){
  Change_Morocco_2020[,i] = (-1)*sum(as.numeric(change_languages_2020[200:212,i+2]))
}

Change_Mali_2020 = matrix(0,ncol=15,nrow=1)
for(i in (1:15)){
  Change_Mali_2020[,i] = (-1)*sum(as.numeric(change_languages_2020[213:217,i+2]))
}

Change_Mexico_2020 = matrix(0,ncol=15,nrow=1)
for(i in (1:15)){
  Change_Mexico_2020[,i] = (-1)*sum(as.numeric(change_languages_2020[218:227,i+2]))
}

Change_Nigeria_2020 = matrix(0,ncol=15,nrow=1)
for(i in (1:15)){
  Change_Nigeria_2020[,i] = (-1)*sum(as.numeric(change_languages_2020[228:237,i+2]))
}

Change_Romania_2020 = matrix(0,ncol=15,nrow=1)
for(i in (1:15)){
  Change_Romania_2020[,i] = (-1)*sum(as.numeric(change_languages_2020[238:248,i+2]))
}

Change_Egypt_2020 = matrix(0,ncol=15,nrow=1)
for(i in (1:15)){
  Change_Egypt_2020[,i] = (-1)*sum(as.numeric(change_languages_2020[249:266,i+2]))
}

Change_UAE_2020 = matrix(0,ncol=15,nrow=1)
for(i in (1:15)){
  Change_UAE_2020[,i] = sum(as.numeric(change_languages_2020[IndexUAE,i+2]))
}

Change_Qatar_2020 = matrix(0,ncol=15,nrow=1)
for(i in (1:15)){
  Change_Qatar_2020[,i] = sum(as.numeric(change_languages_2020[IndexQatar,i+2]))
}

Change_Oman_2020 = matrix(0,ncol=15,nrow=1)
for(i in (1:15)){
  Change_Oman_2020[,i] = sum(as.numeric(change_languages_2020[IndexOman,i+2]))
}

Change_Switzerland_2020 = matrix(0,ncol=15,nrow=1)
for(i in (1:15)){
  Change_Switzerland_2020[,i] = sum(as.numeric(change_languages_2020[IndexSwitzerland,i+2]))
}

Change_Australia_2020 = matrix(0,ncol=15,nrow=1)
for(i in (1:15)){
  Change_Australia_2020[,i] = sum(as.numeric(change_languages_2020[IndexAustralia,i+2]))
}

Change_Canada_2020 = matrix(0,ncol=15,nrow=1)
for(i in (1:15)){
  Change_Canada_2020[,i] = sum(as.numeric(change_languages_2020[IndexCanada,i+2]))
}

Change_Germany_2020 = matrix(0,ncol=15,nrow=1)
for(i in (1:15)){
  Change_Germany_2020[,i] = sum(as.numeric(change_languages_2020[IndexGermany,i+2]))
}

Change_UK_2020 = matrix(0,ncol=15,nrow=1)
for(i in (1:15)){
  Change_UK_2020[,i] = sum(as.numeric(change_languages_2020[IndexUK,i+2]))
}

Change_US_2020 = matrix(0,ncol=15,nrow=1)
for(i in (1:15)){
  Change_US_2020[,i] = sum(as.numeric(change_languages_2020[IndexUS,i+2]))
}

Change_Iraq_2020 = matrix(0,ncol=15,nrow=1)
for(i in (1:15)){
  Change_Iraq_2020[,i] = sum(as.numeric(change_languages_2020[IndexIraq,i+2]))
}

Change_Jordan_2020 = matrix(0,ncol=15,nrow=1)
for(i in (1:15)){
  Change_Jordan_2020[,i] = sum(as.numeric(change_languages_2020[IndexJordan,i+2]))
}

Change_Kuwait_2020 = matrix(0,ncol=15,nrow=1)
for(i in (1:15)){
  Change_Kuwait_2020[,i] = sum(as.numeric(change_languages_2020[IndexKuwait,i+2]))
}

Change_Lebanon_2020 = matrix(0,ncol=15,nrow=1)
for(i in (1:15)){
  Change_Lebanon_2020[,i] = sum(as.numeric(change_languages_2020[IndexLebanon,i+2]))
}

Change_Saudi_2020 = matrix(0,ncol=15,nrow=1)
for(i in (1:15)){
  Change_Saudi_2020[,i] = sum(as.numeric(change_languages_2020[IndexSaudi,i+2]))
}

Change_Turkey_2020 = matrix(0,ncol=15,nrow=1)
for(i in (1:15)){
  Change_Turkey_2020[,i] = sum(as.numeric(change_languages_2020[IndexTurkey,i+2]))
}

Change_France_2020 = matrix(0,ncol=15,nrow=1)
for(i in (1:15)){
  Change_France_2020[,i] = sum(as.numeric(change_languages_2020[IndexFrance,i+2]))
}

Change_Afghan_2020 = matrix(0,ncol=15,nrow=1)
for(i in (1:15)){
  Change_Afghan_2020[,i] = sum(as.numeric(change_languages_2020[IndexAfghan,i+2]))
}

Change_Malay_2020 = matrix(0,ncol=15,nrow=1)
for(i in (1:15)){
  Change_Malay_2020[,i] = sum(as.numeric(change_languages_2020[IndexMalay,i+2]))
}

Change_Japan_2020 = matrix(0,ncol=15,nrow=1)
for(i in (1:15)){
  Change_Japan_2020[,i] = sum(as.numeric(change_languages_2020[IndexJapan,i+2]))
}

Change_Singapore_2020 = matrix(0,ncol=15,nrow=1)
for(i in (1:15)){
  Change_Singapore_2020[,i] = sum(as.numeric(change_languages_2020[IndexSingapore,i+2]))
}

Change_Russia_2020 = matrix(0,ncol=15,nrow=1)
for(i in (1:15)){
  Change_Russia_2020[,i] = sum(as.numeric(change_languages_2020[IndexRussia,i+2]))
}

ImmigrationCountry = c("Syrian Arab Republic","India","Bangladesh","China","Pakistan",
                       "Indonesia","Philippines","Sudan","Spain","Sri Langka","Myanmar",
                       "Libya","Iran, Islamic Rep.","Nepal","Morocco","Mali",
                       "Mexico","Nigeria","Romania","Egypt, Arab Rep.",
                       "United Arab Emirates","Qatar","Oman","Switzerland","Afghanistan",
                       "Australia","Canada","Germany","United Kingdom","United States",
                       "Iraq","Jordan","Kuwait","Lebanon","Malaysia","Russia",
                       "Saudi Arabia","Turkey","France","Japan","Singapore")

total2020 = matrix(0,ncol=16,nrow=length(ImmigrationCountry))

total2020[,1] = ImmigrationCountry
total2020[1,2:16] = Change_Syria_2020
total2020[2,2:16] = Change_India_2020
total2020[3,2:16] = Change_Bangladesh_2020
total2020[4,2:16] = Change_China_2020
total2020[5,2:16] = Change_Pakistan_2020
total2020[6,2:16] = Change_Indonesia_2020
total2020[7,2:16] = Change_Philippines_2020
total2020[8,2:16] = Change_Sudan_2020
total2020[9,2:16] = Change_Spain_2020
total2020[10,2:16] = Change_SriLangka_2020
total2020[11,2:16] = Change_Myanmar_2020
total2020[12,2:16] = Change_Libya_2020
total2020[13,2:16] = Change_Iran_2020
total2020[14,2:16] = Change_Nepal_2020
total2020[15,2:16] = Change_Morocco_2020
total2020[16,2:16] = Change_Mali_2020
total2020[17,2:16] = Change_Mexico_2020
total2020[18,2:16] = Change_Nigeria_2020
total2020[19,2:16] = Change_Romania_2020
total2020[20,2:16] = Change_Egypt_2020
total2020[21,2:16] = Change_UAE_2020
total2020[22,2:16] = Change_Qatar_2020
total2020[23,2:16] = Change_Oman_2020
total2020[24,2:16] = Change_Switzerland_2020
total2020[25,2:16] = Change_Afghan_2020
total2020[26,2:16] = Change_Australia_2020
total2020[27,2:16] = Change_Canada_2020
total2020[28,2:16] = Change_Germany_2020
total2020[29,2:16] = Change_UK_2020
total2020[30,2:16] = Change_US_2020
total2020[31,2:16] = Change_Iraq_2020
total2020[32,2:16] = Change_Jordan_2020
total2020[33,2:16] = Change_Kuwait_2020
total2020[34,2:16] = Change_Lebanon_2020
total2020[35,2:16] = Change_Malay_2020
total2020[36,2:16] = Change_Russia_2020
total2020[37,2:16] = Change_Saudi_2020
total2020[38,2:16] = Change_Turkey_2020
total2020[39,2:16] = Change_France_2020
total2020[40,2:16] = Change_Japan_2020
total2020[41,2:16] = Change_Singapore_2020

data_2020 = data.frame(total2020)
names(data_2020) = c("Country","Mandarin","English","Hindi","Spanish",
                     "Arabic","Malay","Russian","Bengali","Portuguese","French",
                     "Hausa","Punjabi","Japanese","German","Persian")

#2070 For Each Country

Change_Syria_2070 = matrix(0,ncol=15,nrow=1)
for(i in (1:15)){
  Change_Syria_2070[,i] = (-1)*sum(as.numeric(change_languages_2070[1:16,i+2]))
}

Change_India_2070 = matrix(0,ncol=15,nrow=1)
for(i in (1:15)){
  Change_India_2070[,i] = (-1)*sum(as.numeric(change_languages_2070[17:37,i+2]))
}

Change_Bangladesh_2070 = matrix(0,ncol=15,nrow=1)
for(i in (1:15)){
  Change_Bangladesh_2070[,i] = (-1)*sum(as.numeric(change_languages_2070[38:55,i+2]))
}

Change_China_2070 = matrix(0,ncol=15,nrow=1)
for(i in (1:15)){
  Change_China_2070[,i] = (-1)*sum(as.numeric(change_languages_2070[56:68,i+2]))
}

Change_Pakistan_2070 = matrix(0,ncol=15,nrow=1)
for(i in (1:15)){
  Change_Pakistan_2070[,i] = (-1)*sum(as.numeric(change_languages_2070[69:87,i+2]))
}

Change_Indonesia_2070 = matrix(0,ncol=15,nrow=1)
for(i in (1:15)){
  Change_Indonesia_2070[,i] = (-1)*sum(as.numeric(change_languages_2070[88:103,i+2]))
}

Change_Philippines_2070 = matrix(0,ncol=15,nrow=1)
for(i in (1:15)){
  Change_Philippines_2070[,i] = (-1)*sum(as.numeric(change_languages_2070[104:121,i+2]))
}

Change_Sudan_2070 = matrix(0,ncol=15,nrow=1)
for(i in (1:15)){
  Change_Sudan_2070[,i] = (-1)*sum(as.numeric(change_languages_2070[122:133,i+2])) + 
    sum(as.numeric(change_languages_2070[IndexSudan,i+2]))
}

Change_Spain_2070 = matrix(0,ncol=15,nrow=1)
for(i in (1:15)){
  Change_Spain_2070[,i] = (-1)*sum(as.numeric(change_languages_2070[134:142,i+2]))
}

Change_SriLangka_2070 = matrix(0,ncol=15,nrow=1)
for(i in (1:15)){
  Change_SriLangka_2070[,i] = (-1)*sum(as.numeric(change_languages_2070[143:160,i+2]))
}

Change_Libya_2070 = matrix(0,ncol=15,nrow=1)
for(i in (1:15)){
  Change_Libya_2070[,i] = (-1)*sum(as.numeric(change_languages_2070[168:175,i+2]))
}

Change_Myanmar_2070 = matrix(0,ncol=15,nrow=1)
for(i in (1:15)){
  Change_Myanmar_2070[,i] = (-1)*sum(as.numeric(change_languages_2070[161:167,i+2]))
}

Change_Iran_2070 = matrix(0,ncol=15,nrow=1)
for(i in (1:15)){
  Change_Iran_2070[,i] = (-1)*sum(as.numeric(change_languages_2070[176:191,i+2]))
}

Change_Nepal_2070 = matrix(0,ncol=15,nrow=1)
for(i in (1:15)){
  Change_Nepal_2070[,i] = (-1)*sum(as.numeric(change_languages_2070[192:199,i+2]))
}

Change_Morocco_2070 = matrix(0,ncol=15,nrow=1)
for(i in (1:15)){
  Change_Morocco_2070[,i] = (-1)*sum(as.numeric(change_languages_2070[200:212,i+2]))
}

Change_Mali_2070 = matrix(0,ncol=15,nrow=1)
for(i in (1:15)){
  Change_Mali_2070[,i] = (-1)*sum(as.numeric(change_languages_2070[213:217,i+2]))
}

Change_Mexico_2070 = matrix(0,ncol=15,nrow=1)
for(i in (1:15)){
  Change_Mexico_2070[,i] = (-1)*sum(as.numeric(change_languages_2070[218:227,i+2]))
}

Change_Nigeria_2070 = matrix(0,ncol=15,nrow=1)
for(i in (1:15)){
  Change_Nigeria_2070[,i] = (-1)*sum(as.numeric(change_languages_2070[228:237,i+2]))
}

Change_Romania_2070 = matrix(0,ncol=15,nrow=1)
for(i in (1:15)){
  Change_Romania_2070[,i] = (-1)*sum(as.numeric(change_languages_2070[238:248,i+2]))
}

Change_Egypt_2070 = matrix(0,ncol=15,nrow=1)
for(i in (1:15)){
  Change_Egypt_2070[,i] = (-1)*sum(as.numeric(change_languages_2070[249:266,i+2]))
}

Change_UAE_2070 = matrix(0,ncol=15,nrow=1)
for(i in (1:15)){
  Change_UAE_2070[,i] = sum(as.numeric(change_languages_2070[IndexUAE,i+2]))
}

Change_Qatar_2070 = matrix(0,ncol=15,nrow=1)
for(i in (1:15)){
  Change_Qatar_2070[,i] = sum(as.numeric(change_languages_2070[IndexQatar,i+2]))
}

Change_Oman_2070 = matrix(0,ncol=15,nrow=1)
for(i in (1:15)){
  Change_Oman_2070[,i] = sum(as.numeric(change_languages_2070[IndexOman,i+2]))
}

Change_Switzerland_2070 = matrix(0,ncol=15,nrow=1)
for(i in (1:15)){
  Change_Switzerland_2070[,i] = sum(as.numeric(change_languages_2070[IndexSwitzerland,i+2]))
}

Change_Australia_2070 = matrix(0,ncol=15,nrow=1)
for(i in (1:15)){
  Change_Australia_2070[,i] = sum(as.numeric(change_languages_2070[IndexAustralia,i+2]))
}

Change_Canada_2070 = matrix(0,ncol=15,nrow=1)
for(i in (1:15)){
  Change_Canada_2070[,i] = sum(as.numeric(change_languages_2070[IndexCanada,i+2]))
}

Change_Germany_2070 = matrix(0,ncol=15,nrow=1)
for(i in (1:15)){
  Change_Germany_2070[,i] = sum(as.numeric(change_languages_2070[IndexGermany,i+2]))
}

Change_UK_2070 = matrix(0,ncol=15,nrow=1)
for(i in (1:15)){
  Change_UK_2070[,i] = sum(as.numeric(change_languages_2070[IndexUK,i+2]))
}

Change_US_2070 = matrix(0,ncol=15,nrow=1)
for(i in (1:15)){
  Change_US_2070[,i] = sum(as.numeric(change_languages_2070[IndexUS,i+2]))
}

Change_Iraq_2070 = matrix(0,ncol=15,nrow=1)
for(i in (1:15)){
  Change_Iraq_2070[,i] = sum(as.numeric(change_languages_2070[IndexIraq,i+2]))
}

Change_Jordan_2070 = matrix(0,ncol=15,nrow=1)
for(i in (1:15)){
  Change_Jordan_2070[,i] = sum(as.numeric(change_languages_2070[IndexJordan,i+2]))
}

Change_Kuwait_2070 = matrix(0,ncol=15,nrow=1)
for(i in (1:15)){
  Change_Kuwait_2070[,i] = sum(as.numeric(change_languages_2070[IndexKuwait,i+2]))
}

Change_Lebanon_2070 = matrix(0,ncol=15,nrow=1)
for(i in (1:15)){
  Change_Lebanon_2070[,i] = sum(as.numeric(change_languages_2070[IndexLebanon,i+2]))
}

Change_Saudi_2070 = matrix(0,ncol=15,nrow=1)
for(i in (1:15)){
  Change_Saudi_2070[,i] = sum(as.numeric(change_languages_2070[IndexSaudi,i+2]))
}

Change_Turkey_2070 = matrix(0,ncol=15,nrow=1)
for(i in (1:15)){
  Change_Turkey_2070[,i] = sum(as.numeric(change_languages_2070[IndexTurkey,i+2]))
}

Change_France_2070 = matrix(0,ncol=15,nrow=1)
for(i in (1:15)){
  Change_France_2070[,i] = sum(as.numeric(change_languages_2070[IndexFrance,i+2]))
}

Change_Afghan_2070 = matrix(0,ncol=15,nrow=1)
for(i in (1:15)){
  Change_Afghan_2070[,i] = sum(as.numeric(change_languages_2070[IndexAfghan,i+2]))
}

Change_Malay_2070 = matrix(0,ncol=15,nrow=1)
for(i in (1:15)){
  Change_Malay_2070[,i] = sum(as.numeric(change_languages_2070[IndexMalay,i+2]))
}

Change_Japan_2070 = matrix(0,ncol=15,nrow=1)
for(i in (1:15)){
  Change_Japan_2070[,i] = sum(as.numeric(change_languages_2070[IndexJapan,i+2]))
}

Change_Singapore_2070 = matrix(0,ncol=15,nrow=1)
for(i in (1:15)){
  Change_Singapore_2070[,i] = sum(as.numeric(change_languages_2070[IndexSingapore,i+2]))
}

Change_Russia_2070 = matrix(0,ncol=15,nrow=1)
for(i in (1:15)){
  Change_Russia_2070[,i] = sum(as.numeric(change_languages_2070[IndexRussia,i+2]))
}

total2070 = matrix(0,ncol=16,nrow=length(ImmigrationCountry))

total2070[,1] = ImmigrationCountry
total2070[1,2:16] = Change_Syria_2070
total2070[2,2:16] = Change_India_2070
total2070[3,2:16] = Change_Bangladesh_2070
total2070[4,2:16] = Change_China_2070
total2070[5,2:16] = Change_Pakistan_2070
total2070[6,2:16] = Change_Indonesia_2070
total2070[7,2:16] = Change_Philippines_2070
total2070[8,2:16] = Change_Sudan_2070
total2070[9,2:16] = Change_Spain_2070
total2070[10,2:16] = Change_SriLangka_2070
total2070[11,2:16] = Change_Myanmar_2070
total2070[12,2:16] = Change_Libya_2070
total2070[13,2:16] = Change_Iran_2070
total2070[14,2:16] = Change_Nepal_2070
total2070[15,2:16] = Change_Morocco_2070
total2070[16,2:16] = Change_Mali_2070
total2070[17,2:16] = Change_Mexico_2070
total2070[18,2:16] = Change_Nigeria_2070
total2070[19,2:16] = Change_Romania_2070
total2070[20,2:16] = Change_Egypt_2070
total2070[21,2:16] = Change_UAE_2070
total2070[22,2:16] = Change_Qatar_2070
total2070[23,2:16] = Change_Oman_2070
total2070[24,2:16] = Change_Switzerland_2070
total2070[25,2:16] = Change_Afghan_2070
total2070[26,2:16] = Change_Australia_2070
total2070[27,2:16] = Change_Canada_2070
total2070[28,2:16] = Change_Germany_2070
total2070[29,2:16] = Change_UK_2070
total2070[30,2:16] = Change_US_2070
total2070[31,2:16] = Change_Iraq_2070
total2070[32,2:16] = Change_Jordan_2070
total2070[33,2:16] = Change_Kuwait_2070
total2070[34,2:16] = Change_Lebanon_2070
total2070[35,2:16] = Change_Malay_2070
total2070[36,2:16] = Change_Russia_2070
total2070[37,2:16] = Change_Saudi_2070
total2070[38,2:16] = Change_Turkey_2070
total2070[39,2:16] = Change_France_2070
total2070[40,2:16] = Change_Japan_2070
total2070[41,2:16] = Change_Singapore_2070

data_2070 = data.frame(total2070)
names(data_2070) = c("Country","Mandarin","English","Hindi","Spanish",
                     "Arabic","Malay","Russian","Bengali","Portuguese","French",
                     "Hausa","Punjabi","Japanese","German","Persian")

#Predicting the Growth
population_16 = read.csv("Population2070.csv",header=F)

country_16 = as.character(population_16$V1)
data_20 = as.numeric(population_16$V2)*1000
data_30 = as.numeric(population_16$V3)*1000
data_40 = as.numeric(population_16$V4)*1000
data_50 = as.numeric(population_16$V5)*1000
data_60 = as.numeric(population_16$V6)*1000
data_70 = as.numeric(population_16$V7)*1000

data_20 = matrix(data_20,ncol=1)
data_30 = matrix(data_30,ncol=1)
data_40 = matrix(data_40,ncol=1)
data_50 = matrix(data_50,ncol=1)
data_60 = matrix(data_60,ncol=1)
data_70 = matrix(data_70,ncol=1)
rownames(data_20) = country_16
colnames(data_20) = c(2020)
rownames(data_30) = country_16
colnames(data_30) = c(2030)
rownames(data_40) = country_16
colnames(data_40) = c(2040)
rownames(data_50) = country_16
colnames(data_50) = c(2050)
rownames(data_60) = country_16
colnames(data_60) = c(2060)
rownames(data_70) = country_16
colnames(data_70) = c(2070)
data_20 = as.table(data_20)
data_30 = as.table(data_30)
data_40 = as.table(data_40)
data_50 = as.table(data_50)
data_60 = as.table(data_60)
data_70 = as.table(data_70)


initial_population = actual_table
for(country in List_Of_Countries){
  initial_population[country,] = data_20[country,]*initial_population[country,]
}


population2030 = actual_table
for(country in List_Of_Countries){
  population2030[country,] = data_30[country,]*population2030[country,]
}

dataframe_2030 = data.frame(population2030)


population2040 = actual_table
for(country in List_Of_Countries){
  population2040[country,] = data_40[country,]*population2040[country,]
}

dataframe_2040 = data.frame(population2040)


population2050 = actual_table
for(country in List_Of_Countries){
  population2050[country,] = data_50[country,]*population2050[country,]
}

dataframe_2050 = data.frame(population2050)


population2060 = actual_table
for(country in List_Of_Countries){
  population2060[country,] = data_60[country,]*population2060[country,]
}

dataframe_2060 = data.frame(population2060)


final_population = actual_table
for(country in List_Of_Countries){
  final_population[country,] = data_70[country,]*final_population[country,]
}

final_dataframe = data.frame(final_population)

languages_used = c("Mandarin","English","Hindi","Spanish",
                   "Arabic","Malay","Russian","Bengali","Portuguese","French",
                   "Hausa","Punjabi","Japanese","German","Persian")

#Calculating the Growth+Migration Effect

immigration_2020 = total2020[,2:16]
country_2020 = total2020[,1]
rownames(immigration_2020) = country_2020
colnames(immigration_2020) = languages_used
immigration_2020 = as.table(immigration_2020)

initial_immigration = initial_population
for(country in country_2020){
  initial_immigration[country,] = initial_immigration[country,] + 
    as.numeric(immigration_2020[country,])
}

initial_immigration[initial_immigration<0] <- 0
 

immigration_2070 = total2070[,2:16]
country_2070 = total2070[,1]
rownames(immigration_2070) = country_2070
colnames(immigration_2070) = c("Mandarin","English","Hindi","Spanish",
                               "Arabic","Malay","Russian","Bengali","Portuguese","French",
                               "Hausa","Punjabi","Japanese","German","Persian")
immigration_2070 = as.table(immigration_2070)

final_immigration = final_population
for(country in country_2070){
  final_immigration[country,] = final_immigration[country,] + 
    as.numeric(immigration_2070[country,])
}

final_immigration[final_immigration<0] <- 0

growth = matrix(0,nrow = 6, ncol = 15)
rownames(growth) = c(2020,2030,2040,2050,2060,2070)
colnames(growth) = c("Mandarin","English","Hindi","Spanish",
                     "Arabic","Malay","Russian","Bengali","Portuguese","French",
                     "Hausa","Punjabi","Japanese","German","Persian")

growth["2020","Mandarin"] = sum(initial_dataframe$Mandarin)
growth["2020","English"] = sum(initial_dataframe$English)
growth["2020","Hindi"] = sum(initial_dataframe$Hindi)
growth["2020","Spanish"] = sum(initial_dataframe$Spanish)
growth["2020","Arabic"] = sum(initial_dataframe$Arabic)
growth["2020","Malay"] = sum(initial_dataframe$Malay)
growth["2020","Russian"] = sum(initial_dataframe$Russian)
growth["2020","Bengali"] = sum(initial_dataframe$Bengali)
growth["2020","Portuguese"] = sum(initial_dataframe$Portuguese)
growth["2020","French"] = sum(initial_dataframe$French)
growth["2020","Hausa"] = sum(initial_dataframe$Hausa)
growth["2020","Punjabi"] = sum(initial_dataframe$Punjabi)
growth["2020","Japanese"] = sum(initial_dataframe$Japanese)
growth["2020","German"] = sum(initial_dataframe$German)
growth["2020","Persian"] = sum(initial_dataframe$Persian)
growth["2030","Mandarin"] = sum(dataframe_2030$Mandarin)
growth["2030","English"] = sum(dataframe_2030$English)
growth["2030","Hindi"] = sum(dataframe_2030$Hindi)
growth["2030","Spanish"] = sum(dataframe_2030$Spanish)
growth["2030","Arabic"] = sum(dataframe_2030$Arabic)
growth["2030","Malay"] = sum(dataframe_2030$Malay)
growth["2030","Russian"] = sum(dataframe_2030$Russian)
growth["2030","Bengali"] = sum(dataframe_2030$Bengali)
growth["2030","Portuguese"] = sum(dataframe_2030$Portuguese)
growth["2030","French"] = sum(dataframe_2030$French)
growth["2030","Hausa"] = sum(dataframe_2030$Hausa)
growth["2030","Punjabi"] = sum(dataframe_2030$Punjabi)
growth["2030","Japanese"] = sum(dataframe_2030$Japanese)
growth["2030","German"] = sum(dataframe_2030$German)
growth["2030","Persian"] = sum(dataframe_2030$Persian)
growth["2040","Mandarin"] = sum(dataframe_2040$Mandarin)
growth["2040","English"] = sum(dataframe_2040$English)
growth["2040","Hindi"] = sum(dataframe_2040$Hindi)
growth["2040","Spanish"] = sum(dataframe_2040$Spanish)
growth["2040","Arabic"] = sum(dataframe_2040$Arabic)
growth["2040","Malay"] = sum(dataframe_2040$Malay)
growth["2040","Russian"] = sum(dataframe_2040$Russian)
growth["2040","Bengali"] = sum(dataframe_2040$Bengali)
growth["2040","Portuguese"] = sum(dataframe_2040$Portuguese)
growth["2040","French"] = sum(dataframe_2040$French)
growth["2040","Hausa"] = sum(dataframe_2040$Hausa)
growth["2040","Punjabi"] = sum(dataframe_2040$Punjabi)
growth["2040","Japanese"] = sum(dataframe_2040$Japanese)
growth["2040","German"] = sum(dataframe_2040$German)
growth["2040","Persian"] = sum(dataframe_2040$Persian)
growth["2050","Mandarin"] = sum(dataframe_2050$Mandarin)
growth["2050","English"] = sum(dataframe_2050$English)
growth["2050","Hindi"] = sum(dataframe_2050$Hindi)
growth["2050","Spanish"] = sum(dataframe_2050$Spanish)
growth["2050","Arabic"] = sum(dataframe_2050$Arabic)
growth["2050","Malay"] = sum(dataframe_2050$Malay)
growth["2050","Russian"] = sum(dataframe_2050$Russian)
growth["2050","Bengali"] = sum(dataframe_2050$Bengali)
growth["2050","Portuguese"] = sum(dataframe_2050$Portuguese)
growth["2050","French"] = sum(dataframe_2050$French)
growth["2050","Hausa"] = sum(dataframe_2050$Hausa)
growth["2050","Punjabi"] = sum(dataframe_2050$Punjabi)
growth["2050","Japanese"] = sum(dataframe_2050$Japanese)
growth["2050","German"] = sum(dataframe_2050$German)
growth["2050","Persian"] = sum(dataframe_2050$Persian)
growth["2060","Mandarin"] = sum(dataframe_2060$Mandarin)
growth["2060","English"] = sum(dataframe_2060$English)
growth["2060","Hindi"] = sum(dataframe_2060$Hindi)
growth["2060","Spanish"] = sum(dataframe_2060$Spanish)
growth["2060","Arabic"] = sum(dataframe_2060$Arabic)
growth["2060","Malay"] = sum(dataframe_2060$Malay)
growth["2060","Russian"] = sum(dataframe_2060$Russian)
growth["2060","Bengali"] = sum(dataframe_2060$Bengali)
growth["2060","Portuguese"] = sum(dataframe_2060$Portuguese)
growth["2060","French"] = sum(dataframe_2060$French)
growth["2060","Hausa"] = sum(dataframe_2060$Hausa)
growth["2060","Punjabi"] = sum(dataframe_2060$Punjabi)
growth["2060","Japanese"] = sum(dataframe_2060$Japanese)
growth["2060","German"] = sum(dataframe_2060$German)
growth["2060","Persian"] = sum(dataframe_2060$Persian)
growth["2070","Mandarin"] = sum(final_dataframe$Mandarin)
growth["2070","English"] = sum(final_dataframe$English)
growth["2070","Hindi"] = sum(final_dataframe$Hindi)
growth["2070","Spanish"] = sum(final_dataframe$Spanish)
growth["2070","Arabic"] = sum(final_dataframe$Arabic)
growth["2070","Malay"] = sum(final_dataframe$Malay)
growth["2070","Russian"] = sum(final_dataframe$Russian)
growth["2070","Bengali"] = sum(final_dataframe$Bengali)
growth["2070","Portuguese"] = sum(final_dataframe$Portuguese)
growth["2070","French"] = sum(final_dataframe$French)
growth["2070","Hausa"] = sum(final_dataframe$Hausa)
growth["2070","Punjabi"] = sum(final_dataframe$Punjabi)
growth["2070","Japanese"] = sum(final_dataframe$Japanese)
growth["2070","German"] = sum(final_dataframe$German)
growth["2070","Persian"] = sum(final_dataframe$Persian)

initial_immigration_df = data.frame(initial_immigration)
final_immigration_df = data.frame(final_immigration)

#Percentage 2020

English_2020 = initial_immigration_df$English / sum(initial_immigration_df$English) *100
Mandarin_2020 = initial_immigration_df$Mandarin / sum(initial_immigration_df$Mandarin) *100
French_2020 = initial_immigration_df$French / sum(initial_immigration_df$French) * 100
Hindi_2020 = initial_immigration_df$Hindi / sum(initial_immigration_df$Hindi) * 100
Bengali_2020 = initial_immigration_df$Bengali / sum(initial_immigration_df$Bengali) *100
Malay_2020 = initial_immigration_df$Malay / sum(initial_immigration_df$Malay) *100
German_2020 = initial_immigration_df$German / sum(initial_immigration_df$German) *100
Arabic_2020 = initial_immigration_df$Arabic / sum(initial_immigration_df$Arabic) *100
Portuguese_2020 = initial_immigration_df$Portuguese / sum(initial_immigration_df$Portuguese) *100
Russian_2020 = initial_immigration_df$Russian / sum(initial_immigration_df$Russian) *100
Japanese_2020 = initial_immigration_df$Japanese / sum(initial_immigration_df$Japanese) *100
Punjabi_2020 = initial_immigration_df$Punjabi / sum(initial_immigration_df$Punjabi) *100
Spanish_2020 = initial_immigration_df$Spanish / sum(initial_immigration_df$Spanish) *100
Persian_2020 = initial_immigration_df$Persian / sum(initial_immigration_df$Persian) *100
Hausa_2020 = initial_immigration_df$Hausa / sum(initial_immigration_df$Hausa) *100

#Percentage 2070

English_2070 = final_immigration_df$English / sum(final_immigration_df$English) *100
Mandarin_2070 = final_immigration_df$Mandarin / sum(final_immigration_df$Mandarin) *100
French_2070 = final_immigration_df$French / sum(final_immigration_df$French) * 100
Hindi_2070 = final_immigration_df$Hindi / sum(final_immigration_df$Hindi) * 100
Bengali_2070 = final_immigration_df$Bengali / sum(final_immigration_df$Bengali) *100
Malay_2070 = final_immigration_df$Malay / sum(final_immigration_df$Malay) *100
German_2070 = final_immigration_df$German / sum(final_immigration_df$German) *100
Arabic_2070 = final_immigration_df$Arabic / sum(final_immigration_df$Arabic) *100
Portuguese_2070 = final_immigration_df$Portuguese / sum(final_immigration_df$Portuguese) *100
Russian_2070 = final_immigration_df$Russian / sum(final_immigration_df$Russian) *100
Japanese_2070 = final_immigration_df$Japanese / sum(final_immigration_df$Japanese) *100
Punjabi_2070 = final_immigration_df$Punjabi / sum(final_immigration_df$Punjabi) *100
Spanish_2070 = final_immigration_df$Spanish / sum(final_immigration_df$Spanish) *100
Persian_2070 = final_immigration_df$Persian / sum(final_immigration_df$Persian) *100
Hausa_2070 = final_immigration_df$Hausa / sum(final_immigration_df$Hausa) *100



#ISO3 Country Code
ISO3 = read.csv("List_Of_Countries.csv")
ISO3 = as.character(ISO3$ISO3)

#Generating Map 2020
mapEnglish20DF = data.frame(Countries = ISO3,English_2020)
MapEnglish20 <- joinCountryData2Map(mapEnglish20DF, joinCode = "ISO3", nameJoinColumn = "Countries")
par(mai=c(0,0,0.2,0),xaxs="i",yaxs="i")
mapParamsEnglish20 <- mapCountryData(MapEnglish20, nameColumnToPlot= "English_2020", catMethod = c(0:25),
                            missingCountryCol = gray(.8))

mapMD20DF = data.frame(Countries = ISO3,Mandarin_2020 = Mandarin_2020)
MapMD20 <- joinCountryData2Map(mapMD20DF, joinCode = "ISO3", nameJoinColumn = "Countries")
mapParamsMD20 <- mapCountryData(MapMD20, nameColumnToPlot= "Mandarin_2020", catMethod = c(0:100),
                                     missingCountryCol = gray(.8))

mapFR20DF = data.frame(Countries = ISO3,French_2020)
MapFR20 <- joinCountryData2Map(mapFR20DF, joinCode = "ISO3", nameJoinColumn = "Countries")
mapParamsFR20 <- mapCountryData(MapFR20, nameColumnToPlot= "French_2020", catMethod = c(0:28),
                                missingCountryCol = gray(.8))

mapHI20DF = data.frame(Countries = ISO3,Hindi_2020)
MapHI20 <- joinCountryData2Map(mapHI20DF, joinCode = "ISO3", nameJoinColumn = "Countries")
mapParamsHI20 <- mapCountryData(MapHI20, nameColumnToPlot= "Hindi_2020", catMethod = c(0:100),
                                missingCountryCol = gray(.8))

mapBE20DF = data.frame(Countries = ISO3,Bengali_2020)
MapBE20 <- joinCountryData2Map(mapBE20DF, joinCode = "ISO3", nameJoinColumn = "Countries")
mapParamsBE20 <- mapCountryData(MapBE20, nameColumnToPlot= "Bengali_2020", catMethod = c(0:60),
                                missingCountryCol = gray(.8))

mapMA20DF = data.frame(Countries = ISO3,Malay_2020)
MapMA20 <- joinCountryData2Map(mapMA20DF, joinCode = "ISO3", nameJoinColumn = "Countries")
mapParamsMA20 <- mapCountryData(MapMA20, nameColumnToPlot= "Malay_2020", catMethod = c(0:95),
                                missingCountryCol = gray(.8))

mapGE20DF = data.frame(Countries = ISO3,German_2020)
MapGE20 <- joinCountryData2Map(mapGE20DF, joinCode = "ISO3", nameJoinColumn = "Countries")
mapParamsGE20 <- mapCountryData(MapGE20, nameColumnToPlot= "German_2020", catMethod = c(0:85),
                                missingCountryCol = gray(.8))

mapAR20DF = data.frame(Countries = ISO3,Arabic_2020)
MapAR20 <- joinCountryData2Map(mapAR20DF, joinCode = "ISO3", nameJoinColumn = "Countries")
mapParamsAR20 <- mapCountryData(MapAR20, nameColumnToPlot= "Arabic_2020", catMethod = c(0:35),
                                missingCountryCol = gray(.8))

mapPO20DF = data.frame(Countries = ISO3,Portuguese_2020)
MapPO20 <- joinCountryData2Map(mapPO20DF, joinCode = "ISO3", nameJoinColumn = "Countries")
mapParamsPO20 <- mapCountryData(MapPO20, nameColumnToPlot= "Portuguese_2020", catMethod = c(0:90),
                                missingCountryCol = gray(.8))

mapRU20DF = data.frame(Countries = ISO3,Russian_2020)
MapRU20 <- joinCountryData2Map(mapRU20DF, joinCode = "ISO3", nameJoinColumn = "Countries")
mapParamsRU20 <- mapCountryData(MapRU20, nameColumnToPlot= "Russian_2020", catMethod = c(0:90),
                                missingCountryCol = gray(.8))

mapJP20DF = data.frame(Countries = ISO3,Japanese_2020)
MapJP20 <- joinCountryData2Map(mapJP20DF, joinCode = "ISO3", nameJoinColumn = "Countries")
mapParamsJP20 <- mapCountryData(MapJP20, nameColumnToPlot= "Japanese_2020", catMethod = c(0:100),
                                missingCountryCol = gray(.8))

mapPU20DF = data.frame(Countries = ISO3,Punjabi_2020)
MapPU20 <- joinCountryData2Map(mapPU20DF, joinCode = "ISO3", nameJoinColumn = "Countries")
mapParamsPU20 <- mapCountryData(MapPU20, nameColumnToPlot= "Punjabi_2020", catMethod = c(0:80),
                                missingCountryCol = gray(.8))

mapSP20DF = data.frame(Countries = ISO3,Spanish_2020)
MapSP20 <- joinCountryData2Map(mapSP20DF, joinCode = "ISO3", nameJoinColumn = "Countries")
mapParamsSP20 <- mapCountryData(MapSP20, nameColumnToPlot= "Spanish_2020", catMethod = c(0:25),
                                missingCountryCol = gray(.8))

mapPE20DF = data.frame(Countries = ISO3,Persian_2020)
MapPE20 <- joinCountryData2Map(mapPE20DF, joinCode = "ISO3", nameJoinColumn = "Countries")
mapParamsPE20 <- mapCountryData(MapPE20, nameColumnToPlot= "Persian_2020", catMethod = c(0:75),
                                missingCountryCol = gray(.8))

mapHA20DF = data.frame(Countries = ISO3,Hausa_2020)
MapHA20 <- joinCountryData2Map(mapHA20DF, joinCode = "ISO3", nameJoinColumn = "Countries")
mapParamsHA20 <- mapCountryData(MapHA20, nameColumnToPlot= "Hausa_2020", catMethod = c(0:80),
                                missingCountryCol = gray(.8))


#Generating Map 2070
mapEnglish70DF = data.frame(Countries = ISO3,English_2070)
MapEnglish70 <- joinCountryData2Map(mapEnglish70DF, joinCode = "ISO3", nameJoinColumn = "Countries")
mapParamsEnglish70 <- mapCountryData(MapEnglish70, nameColumnToPlot= "English_2070", catMethod = c(0:25),
                                     missingCountryCol = gray(.8))

mapMD70DF = data.frame(Countries = ISO3,Mandarin_2070)
MapMD70 <- joinCountryData2Map(mapMD70DF, joinCode = "ISO3", nameJoinColumn = "Countries")
mapParamsMD70 <- mapCountryData(MapMD70, nameColumnToPlot= "Mandarin_2070", catMethod = c(0:100),
                                missingCountryCol = gray(.8))

mapFR70DF = data.frame(Countries = ISO3,French_2070)
MapFR70 <- joinCountryData2Map(mapFR70DF, joinCode = "ISO3", nameJoinColumn = "Countries")
mapParamsFR70 <- mapCountryData(MapFR70, nameColumnToPlot= "French_2070", catMethod = c(0:28),
                                missingCountryCol = gray(.8))

mapHI70DF = data.frame(Countries = ISO3,Hindi_2070)
MapHI70 <- joinCountryData2Map(mapHI70DF, joinCode = "ISO3", nameJoinColumn = "Countries")
mapParamsHI70 <- mapCountryData(MapHI70, nameColumnToPlot= "Hindi_2070", catMethod = c(0:100),
                                missingCountryCol = gray(.8))

mapBE70DF = data.frame(Countries = ISO3,Bengali_2070)
MapBE70 <- joinCountryData2Map(mapBE70DF, joinCode = "ISO3", nameJoinColumn = "Countries")
mapParamsBE70 <- mapCountryData(MapBE70, nameColumnToPlot= "Bengali_2070", catMethod = c(0:60),
                                missingCountryCol = gray(.8))

mapMA70DF = data.frame(Countries = ISO3,Malay_2070)
MapMA70 <- joinCountryData2Map(mapMA70DF, joinCode = "ISO3", nameJoinColumn = "Countries")
mapParamsMA70 <- mapCountryData(MapMA70, nameColumnToPlot= "Malay_2070", catMethod = c(0:95),
                                missingCountryCol = gray(.8))

mapGE70DF = data.frame(Countries = ISO3,German_2070)
MapGE70 <- joinCountryData2Map(mapGE70DF, joinCode = "ISO3", nameJoinColumn = "Countries")
mapParamsGE70 <- mapCountryData(MapGE70, nameColumnToPlot= "German_2070", catMethod = c(0:85),
                                missingCountryCol = gray(.8))

mapAR70DF = data.frame(Countries = ISO3,Arabic_2070)
MapAR70 <- joinCountryData2Map(mapAR70DF, joinCode = "ISO3", nameJoinColumn = "Countries")
mapParamsAR20 <- mapCountryData(MapAR70, nameColumnToPlot= "Arabic_2070", catMethod = c(0:35),
                                missingCountryCol = gray(.8))

mapPO70DF = data.frame(Countries = ISO3,Portuguese_2070)
MapPO70 <- joinCountryData2Map(mapPO70DF, joinCode = "ISO3", nameJoinColumn = "Countries")
mapParamsPO70 <- mapCountryData(MapPO70, nameColumnToPlot= "Portuguese_2070", catMethod = c(0:90),
                                missingCountryCol = gray(.8))

mapRU70DF = data.frame(Countries = ISO3,Russian_2070)
MapRU70 <- joinCountryData2Map(mapRU70DF, joinCode = "ISO3", nameJoinColumn = "Countries")
mapParamsRU70 <- mapCountryData(MapRU70, nameColumnToPlot= "Russian_2070", catMethod = c(0:90),
                                missingCountryCol = gray(.8))

mapJP70DF = data.frame(Countries = ISO3,Japanese_2070)
MapJP70 <- joinCountryData2Map(mapJP70DF, joinCode = "ISO3", nameJoinColumn = "Countries")
mapParamsJP70 <- mapCountryData(MapJP70, nameColumnToPlot= "Japanese_2070", catMethod = c(0:100),
                                missingCountryCol = gray(.8))

mapPU70DF = data.frame(Countries = ISO3,Punjabi_2070)
MapPU70 <- joinCountryData2Map(mapPU70DF, joinCode = "ISO3", nameJoinColumn = "Countries")
mapParamsPU70 <- mapCountryData(MapPU70, nameColumnToPlot= "Punjabi_2070", catMethod = c(0:80),
                                missingCountryCol = gray(.8))

mapSP70DF = data.frame(Countries = ISO3,Spanish_2070)
MapSP70 <- joinCountryData2Map(mapSP70DF, joinCode = "ISO3", nameJoinColumn = "Countries")
mapParamsSP70 <- mapCountryData(MapSP70, nameColumnToPlot= "Spanish_2070", catMethod = c(0:25),
                                missingCountryCol = gray(.8))

mapPE70DF = data.frame(Countries = ISO3,Persian_2070)
MapPE70 <- joinCountryData2Map(mapPE70DF, joinCode = "ISO3", nameJoinColumn = "Countries")
mapParamsPE70 <- mapCountryData(MapPE70, nameColumnToPlot= "Persian_2070", catMethod = c(0:75),
                                missingCountryCol = gray(.8))

mapHA70DF = data.frame(Countries = ISO3,Hausa_2070)
MapHA70 <- joinCountryData2Map(mapHA70DF, joinCode = "ISO3", nameJoinColumn = "Countries")
mapParamsHA70 <- mapCountryData(MapHA70, nameColumnToPlot= "Hausa_2070", catMethod = c(0:80),
                                missingCountryCol = gray(.8))


Percentage2020 = data.frame(List_Of_Countries,Mandarin_2020,English_2020,Hindi_2020,
                            Spanish_2020,Arabic_2020,Malay_2020,Russian_2020,Bengali_2020,
                            Portuguese_2020,French_2020,Hausa_2020,Punjabi_2020,
                            Japanese_2020,German_2020,Persian_2020)

Percentage2070 = data.frame(List_Of_Countries,Mandarin_2070,English_2070,Hindi_2070,
                            Spanish_2070,Arabic_2070,Malay_2070,Russian_2070,Bengali_2070,
                            Portuguese_2070,French_2070,Hausa_2070,Punjabi_2070,
                            Japanese_2070,German_2070,Persian_2070)