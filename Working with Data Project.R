require(lme4)
require(lmerTest)
require(tidyverse)
require(readxl)
require(magrittr)
require(sjPlot)     
require(sjmisc)     
require(sjstats)    
require(arm)
require(compare)
require(mice)
require(VIM)
require(dplyr)
require(rworldmap)
require(RColorBrewer)

#DATA WRANGLING

#Read excel file
alcoholConsumptionByCapita = read_xlsx(path = "Alcohol Consumption by GDP Per Capita.xlsx")

#Rename appropriate Variable Names
newAlcoholConsumptionByCapita = rename(alcoholConsumptionByCapita,
                                       Region = "ParentLocation",
                                       CountryCode = "SpatialDimValueCode",
                                       Country = "Location",
                                       Year = "Period",
                                       Sex = "Dim1",
                                       MeanConsumption = "FactValueNumeric",
                                       CILowerBoundConsumption = "FactValueNumericLow",
                                       CIUpperBoundConsumption = "FactValueNumericHigh",
                                       )
#Eliminating unnecessary columns by selecting the ones we wish to keep
newAlcoholConsumptionByCapita = newAlcoholConsumptionByCapita[,c("Region", 
                                                                 "CountryCode", 
                                                                 "Country", 
                                                                 "Year", 
                                                                 "Sex", 
                                                                 "MeanConsumption", 
                                                                 "CILowerBoundConsumption", 
                                                                 "CIUpperBoundConsumption")]
#Changing variable types 
newAlcoholConsumptionByCapita = mutate(newAlcoholConsumptionByCapita, Region = as.factor(Region),
                                                                      CountryCode = as.factor(CountryCode),
                                                                      Country = as.factor(Country),                               
                                                                      Sex = as.factor(Sex))
#Made assumption that all values of 0 meant missing values
newAlcoholConsumptionByCapita[newAlcoholConsumptionByCapita == 0] = NA

#Removed the years 2000, 2005 and 2010 and female and male from sex 
#as this data isn't applicable to happiness report
#Then sorted Country column alphabetically 
finalAlcoholConsumptionByCapita = subset(newAlcoholConsumptionByCapita, 
                                         Year != 2000 &  
                                         Year != 2005 &  
                                         Year != 2010)
finalAlcoholConsumptionByCapita = subset(finalAlcoholConsumptionByCapita, Sex != "Female" & Sex != "Male")
finalAlcoholConsumptionByCapita = finalAlcoholConsumptionByCapita[order(finalAlcoholConsumptionByCapita$Country),]




#Read excel file
happinessIndex2015 = read_csv(file = "2015.csv")

#Rename appropriate Variable Names
newHappinessIndex2015 = rename(happinessIndex2015,
                               Rank = "Happiness Rank",
                               GDPperCapita = "Economy (GDP per Capita)",
                               lifeExpectancy = "Health (Life Expectancy)",
                               happinessScore = "Happiness Score",
                               )

#Changing variable types 
newHappinessIndex2015 = mutate(newHappinessIndex2015, Country = as.factor(Country))


#Eliminating unnecessary columns by selecting the ones we wish to keep
newHappinessIndex2015 = newHappinessIndex2015[,c("Country", 
                                                 "happinessScore",
                                                 "GDPperCapita",
                                                 "lifeExpectancy",
                                                 "Freedom",
                                                 "Generosity"
                                                 )]



#Read excel file
happinessIndex2019 = read_csv(file = "2019.csv")

#Rename appropriate Variable Names
newHappinessIndex2019 = rename(happinessIndex2019,
                               Rank = "Overall rank",
                               happinessScore = "Score",
                               lifeExpectancy = "Healthy life expectancy",
                               Freedom = "Freedom to make life choices",
                               GDPperCapita = "GDP per capita",
                               Country = "Country or region",
                               )

#Eliminating unnecessary columns by selecting the ones we wish to keep
newHappinessIndex2019 = newHappinessIndex2019[,c("Country", 
                                                 "happinessScore",
                                                 "GDPperCapita",
                                                 "lifeExpectancy",
                                                 "Freedom",
                                                 "Generosity"
                                                  )]
#Changing variable types 
newHappinessIndex2019 = mutate(newHappinessIndex2019, Country = as.factor(Country))


#Used anti_join to find if there were any discrepancies between datasets and removed them using dplpyr's setdiff 
print(anti_join(newHappinessIndex2015, newHappinessIndex2019, by="Country"))
df1 = anti_join(newHappinessIndex2015, newHappinessIndex2019, by="Country")
finalNewHappinessIndex2015 = dplyr::setdiff(newHappinessIndex2015, df1)


print(anti_join(newHappinessIndex2019, newHappinessIndex2015, by="Country"))
df2 = anti_join(newHappinessIndex2019, newHappinessIndex2015, by="Country")
finalNewHappinessIndex2019 = dplyr::setdiff(newHappinessIndex2019, df2)

#Used anti_join to find if there were any discrepancies between datasets and removed them using dplpyr's setdiff
#Then renamed country names such that format became standard throughout all datasets 
print(anti_join(finalAlcoholConsumptionByCapita, newHappinessIndex2015, by="Country"), n=112)
mismatchedCountries2015 = anti_join(finalAlcoholConsumptionByCapita, newHappinessIndex2015, by="Country")

print(anti_join(finalAlcoholConsumptionByCapita, newHappinessIndex2019, by="Country"), n=86)
mismatchedCountries2019 = anti_join(finalAlcoholConsumptionByCapita, newHappinessIndex2019, by="Country")


levels(finalAlcoholConsumptionByCapita$Country)[match("Bolivia (Plurinational State of)",
                                                      levels(finalAlcoholConsumptionByCapita$Country))] = "Bolivia"
levels(finalAlcoholConsumptionByCapita$Country)[match("Brunei Darussalam",
                                                      levels(finalAlcoholConsumptionByCapita$Country))] = "Brunei"
levels(finalAlcoholConsumptionByCapita$Country)[match("CÃ´te dâ€™Ivoire",
                                                      levels(finalAlcoholConsumptionByCapita$Country))] = "Ivory Coast"
levels(finalAlcoholConsumptionByCapita$Country)[match("Democratic People's Republic of Korea",
                                                      levels(finalAlcoholConsumptionByCapita$Country))] = "North Korea"
levels(finalAlcoholConsumptionByCapita$Country)[match("Democratic Republic of the Congo",
                                                      levels(finalAlcoholConsumptionByCapita$Country))] = "Congo (Kinshasa)"
levels(finalAlcoholConsumptionByCapita$Country)[match("Congo",
                                                      levels(finalAlcoholConsumptionByCapita$Country))] = "Congo (Brazzaville)"
levels(finalAlcoholConsumptionByCapita$Country)[match("Iran (Islamic Republic of)",
                                                      levels(finalAlcoholConsumptionByCapita$Country))] = "Iran"
levels(finalAlcoholConsumptionByCapita$Country)[match("Lao People's Democratic Republic",
                                                      levels(finalAlcoholConsumptionByCapita$Country))] = "Laos"
levels(finalAlcoholConsumptionByCapita$Country)[match("Micronesia (Federated States of)",
                                                      levels(finalAlcoholConsumptionByCapita$Country))] = "Micronesia"
levels(finalAlcoholConsumptionByCapita$Country)[match("Republic of Korea",
                                                      levels(finalAlcoholConsumptionByCapita$Country))] = "South Korea"
levels(finalAlcoholConsumptionByCapita$Country)[match("Republic of Moldova",
                                                      levels(finalAlcoholConsumptionByCapita$Country))] = "Moldova"
levels(finalAlcoholConsumptionByCapita$Country)[match("Russian Federation",
                                                      levels(finalAlcoholConsumptionByCapita$Country))] = "Russia"
levels(finalAlcoholConsumptionByCapita$Country)[match("Syrian Arab Republic",
                                                      levels(finalAlcoholConsumptionByCapita$Country))] = "Syria"
levels(finalAlcoholConsumptionByCapita$Country)[match("The former Yugoslav Republic of Macedonia",
                                                      levels(finalAlcoholConsumptionByCapita$Country))] = "Macedonia"
levels(finalAlcoholConsumptionByCapita$Country)[match("Trinidad and Tobago",
                                                      levels(finalAlcoholConsumptionByCapita$Country))] = "Trinidad & Tobago"
levels(finalAlcoholConsumptionByCapita$Country)[match("United Kingdom of Great Britain and Northern Ireland",
                                                      levels(finalAlcoholConsumptionByCapita$Country))] = "United Kingdom"
levels(finalAlcoholConsumptionByCapita$Country)[match("United Republic of Tanzania",
                                                      levels(finalAlcoholConsumptionByCapita$Country))] = "Tanzania"
levels(finalAlcoholConsumptionByCapita$Country)[match("United States of America",
                                                      levels(finalAlcoholConsumptionByCapita$Country))] = "United States"
levels(finalAlcoholConsumptionByCapita$Country)[match("Venezuela (Bolivarian Republic of)",
                                                      levels(finalAlcoholConsumptionByCapita$Country))] = "Venezuela"
levels(finalAlcoholConsumptionByCapita$Country)[match("Viet Nam",
                                                      levels(finalAlcoholConsumptionByCapita$Country))] = "Viet Nam"

finalFinalAlcoholComsumptionByCapita = dplyr::setdiff(finalAlcoholConsumptionByCapita, mismatchedCountries2015)
finalFinalAlcoholComsumptionByCapita = dplyr::setdiff(finalAlcoholConsumptionByCapita, mismatchedCountries2019)

#Sort Countries Alphabetically 2015 and 2019 Happiness Data
finalNewHappinessIndex2015 = finalNewHappinessIndex2015[order(finalNewHappinessIndex2015$Country),]
finalNewHappinessIndex2019 = finalNewHappinessIndex2019[order(finalNewHappinessIndex2019$Country),]

#HANDLING MISSING DATA
#Visualizing Missing Data
missingDataPlot = aggr(finalFinalAlcoholComsumptionByCapita, col=c("navyblue", "yellow"),
                       numbers=TRUE, sortVars=TRUE,
                       label=names(finalFinalAlcoholComsumptionByCapita), cex.axis=1,
                       gap=3, ylab=c("Missing Data", "Pattern"))
#Simple Imputation
imputationData = finalFinalAlcoholComsumptionByCapita
imputationData$MeanConsumption[which(is.na(imputationData$MeanConsumption))] = mean(imputationData$MeanConsumption, na.rm =TRUE)
imputationData$CILowerBoundConsumption[which(is.na(imputationData$CILowerBoundConsumption))] = mean(imputationData$CILowerBoundConsumption, na.rm =TRUE)
imputationData$CIUpperBoundConsumption[which(is.na(imputationData$CIUpperBoundConsumption))] = mean(imputationData$CIUpperBoundConsumption, na.rm =TRUE)

#MICE Imputation
#CIUpperbound is collinear to CILowerbound hence MICE cant impute Upperbound
my_imp = mice(finalFinalAlcoholComsumptionByCapita, m=5, method = "pmm", maxit = 20)

my_imp$imp$MeanConsumption
my_imp$imp$CILowerBoundConsumption
my_imp$imp$CIUpperBoundConsumption

summary(finalFinalAlcoholComsumptionByCapita$MeanConsumption)
summary(finalFinalAlcoholComsumptionByCapita$CILowerBoundConsumption)
cleanDataSet = complete(my_imp, 1)


#HYPOTHESIS TESTING
data2015 = cleanDataSet[,c("Country", "Year", "MeanConsumption")]
data2015 = subset(data2015, Year != 2019)
data2015$happinessScore = finalNewHappinessIndex2015$happinessScore

data2019 = cleanDataSet[,c("Country", "Year", "MeanConsumption")]
data2019 = subset(data2019, Year != 2015)
data2019$happinessScore = finalNewHappinessIndex2019$happinessScore

cor.test(x = data2015$MeanConsumption, y = data2015$happinessScore)
cov(x = data2015$MeanConsumption, y = data2015$happinessScore)
t.test(x = data2015$MeanConsumption, y = data2015$happinessScore)

cor.test(x = data2019$MeanConsumption, y = data2019$happinessScore)
cov(x = data2019$MeanConsumption, y = data2019$happinessScore)
t.test(x = data2019$MeanConsumption, y = data2019$happinessScore)

#PLOTS
missingDataPlot = aggr(finalFinalAlcoholComsumptionByCapita, col =c ("navyblue", "yellow"),
                       numbers = TRUE, sortVars = TRUE,
                       label = names(finalFinalAlcoholComsumptionByCapita), cex.axis = 1,
                       gap = 3, ylab = c("Missing Data", "Pattern"))

#Map Plot 
map1 = aggregate(data2015$MeanConsumption, by = list(data2015$Country), FUN=sum)
colnames(map1)[colnames(map1)=="x"] = "Mean Consumption" 
map1 = joinCountryData2Map(data2015, nameJoinColumn = "Country", joinCode = "NAME")
ColorPalette = RColorBrewer::brewer.pal(9, "Purples")
mapCountryData(map1, nameColumnToPlot = "MeanConsumption", 
               catMethod = "fixedwidth", 
               colourPalette = ColorPalette,
               numCats = 10)

map2 = aggregate(data2019$MeanConsumption, by = list(data2019$Country), FUN=sum)
colnames(map2)[colnames(map2)=="x"] = "Mean Consumption" 
map2 = joinCountryData2Map(data2019, nameJoinColumn = "Country", joinCode = "NAME")
ColorPalette = RColorBrewer::brewer.pal(9, "Purples")
mapCountryData(map2, nameColumnToPlot = "MeanConsumption", 
               catMethod = "fixedwidth", 
               colourPalette = ColorPalette,
               numCats = 10)

map3 = aggregate(data2015$happinessScore, by = list(data2015$Country), FUN=sum)
colnames(map3)[colnames(map3)=="x"] = "Happiness Score" 
map3 = joinCountryData2Map(data2015, nameJoinColumn = "Country", joinCode = "NAME")
ColorPalette = RColorBrewer::brewer.pal(9, "Purples")
mapCountryData(map3, nameColumnToPlot = "happinessScore", 
               catMethod = "fixedwidth", 
               colourPalette = ColorPalette,
               numCats = 10)

map4 = aggregate(data2019$happinessScore, by = list(data2019$Country), FUN=sum)
colnames(map4)[colnames(map4)=="x"] = "Happiness Score" 
map4 = joinCountryData2Map(data2019, nameJoinColumn = "Country", joinCode = "NAME")
ColorPalette = RColorBrewer::brewer.pal(9, "Purples")
mapCountryData(map4, nameColumnToPlot = "happinessScore", 
               catMethod = "fixedwidth", 
               colourPalette = ColorPalette,
               numCats = 10)
#Scatter Plots
Scatter1 = ggplot(data2015, aes(x=happinessScore, y=MeanConsumption)) +
  geom_point() + 
  geom_text(label=data2015$Country) +
  geom_smooth(method=lm, se=FALSE) +
  ggtitle("Average Alcohol Consumption and a Nation's Happiness 2015")
Scatter1
  
Scatter2 = ggplot(data2019, aes(x=happinessScore, y=MeanConsumption)) +
  geom_point() + 
  geom_text(label=data2019$Country) +
  geom_smooth(method=lm, se=FALSE) +
  ggtitle("Average Alcohol Consumption and a Nation's Happiness 2019")
Scatter2
