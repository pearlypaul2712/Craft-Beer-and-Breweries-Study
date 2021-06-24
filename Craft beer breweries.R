install.packages("tidyverse")
install.packages("usdata")
install.packages("ggthemes")
install.packages("viridis")
install.packages("class")
install.packages("caret")

library(class)
library(caret)
library(e1071)
library(viridis)
library(ggthemes)
library(usdata)
library(tidyverse)
library(dplyr)
library(sqldf)

#Read in Core Data
Beers = read.csv("/Users/Kevin/Desktop/School/Doing Data Science/Project 1/Beers.csv", header = TRUE)
Breweries = read.csv("/Users/Kevin/Desktop/School/Doing Data Science/Project 1/Breweries.csv", header = TRUE)

#Bring in Region Data
RegionData = data.frame(State = state.abb, Region = state.region)
Breweries = left_join(Breweries,RegionData,by = "State")

#Handle NA and missing data by replacing with Regional Means
Breweries = sqldf('
      select
      "Brew_ID","Name","City","State",
      
      case when "State"  = "DC" then "South" else "Region" end as Region
      
      from
      Breweries
                 ')

Distilled_Data = merge(Beers,Breweries, by.x = "Brewery_id", by.y = "Brew_ID")

#Create table for IBU and ABV means by State and Region - excluding missing data 
State_Means = 
Distilled_Data[!is.na(Distilled_Data$ABV) & !is.na(Distilled_Data$IBU),] %>% 
  group_by(State) %>%
  summarize(Mean_IBU_by_State = mean(IBU),
            Mean_ABV_by_State = mean(ABV))
Regional_Means = 
  Distilled_Data[!is.na(Distilled_Data$ABV) & !is.na(Distilled_Data$IBU),] %>% 
  group_by(Region) %>%
  summarize(Mean_IBU_by_Region = mean(IBU),
            Mean_ABV_by_Region = mean(ABV))

Distilled_Data = left_join(Distilled_Data,State_Means,by = "State")
Distilled_Data = left_join(Distilled_Data,Regional_Means,by = "Region")


#Brewery Count by State
Breweries %>% 
  group_by(State,Region) %>%
  summarize(Count = n()) %>%
  ggplot(aes(x = reorder(State,-Count), y = Count,fill=Region)) + 
  geom_bar(stat = "identity") + 
  geom_text(aes(label=Count),vjust=2) + 
  ggtitle("Brewery Count by State",) + xlab("State") + ylab("Count") + 
  facet_wrap(~Region, scales="free") + 
  theme(plot.title=element_text(hjust=0.5)) 

#Median Alcohol By Volume - did a double coalesce to pull from both State and Regional Means 
#- return values of state and regional means where NA exist for both ABV and IBU
Distilled_Data2 = Distilled_Data

Distilled_Data2 = 
Distilled_Data2 %>%
  mutate(ABV = coalesce(Distilled_Data2$ABV,Distilled_Data2$Mean_ABV_by_State),
         IBU = coalesce(coalesce(Distilled_Data2$IBU,Distilled_Data2$Mean_IBU_by_State),Distilled_Data2$Mean_IBU_by_Region))
#Median ABV                        
Distilled_Data2 %>%
  group_by(State,Region) %>%
  summarize(
    Median_Alcohol_Content = round(median(ABV,na.rm=TRUE),digits=5), 
    Median_Bitterness = median(IBU,na.rm=TRUE)
  ) %>%
  ggplot(aes(reorder(x = State,Median_Alcohol_Content), y = Median_Alcohol_Content,fill=Region)) + geom_bar(stat="identity") + 
  xlab("State") + ylab("ABV") + ggtitle("Median ABV by State") + facet_wrap(~Region, scales="free") + 
  geom_text(aes(label=round(Median_Alcohol_Content,digits=6)),hjust=1) + 
  coord_flip()

#Median Bitterness
Distilled_Data2 %>%
  group_by(State,Region) %>%
  summarize(
    Median_Bitterness = median(IBU,na.rm=TRUE),
    Median_Alcohol_Content = median(ABV,na.rm=TRUE) 
  ) %>%
  ggplot(aes(reorder(x = State,Median_Bitterness), y = Median_Bitterness,fill=Region)) + geom_bar(stat="identity") + 
  xlab("State") + ylab("Bitterness") + ggtitle("Median Bitterness by State") + coord_flip() +
  geom_text(aes(label=round(Median_Bitterness,digits=2)),hjust=1) + 
  facet_wrap(~Region, scales="free") 

#Breweries By Region Heat Map
map_data = map_data('state')
map_data = map_data %>% mutate(State = state2abbr(map_data$region))

Final_Breweries = left_join(Breweries,map_data, by = 'State')
Count_Breweries = Breweries %>%
  group_by(State) %>%
  summarize(Tally = n())

New_Breweries = left_join(Final_Breweries,Count_Breweries,by = 'State')
p0 <- ggplot(data = New_Breweries,
             mapping = aes(x = long, y = lat,
                           group = group,fill=Tally))
p1 <- p0 + geom_polygon(color = "black", size = 0.1) + 
  theme_map() +
  scale_fill_gradient2(low = "green",
                       mid = "yellow",
                       high = "red") +
  ggtitle("Breweries By Region") + 
  facet_grid(~Region, scales="free")
p1  
#Relationship Between IBU and ABV
Distilled_Data2 %>% ggplot(aes(x=IBU, y=ABV)) + geom_point(color="red") + geom_smooth(method="lm") + 
  ggtitle("Relationship Between ABV and IBU")

#Relationship Between IBU and ABV by Type
IPA_ALE = sqldf('
  select
  "Name","Style" as Style,"ABV","IBU","State","Region","City",
  case when "Style" like "%ALE%" then "ALE"
       when "Style" like "%IPA%" then "IPA" end as Type
 from
  Distilled_Data2
 where
  "Style" like "%ALE%" OR "Style" like "%IPA%"
 ')

IPA_ALE %>% ggplot(aes(x=IBU, y=ABV,color=Type)) + geom_point() + geom_smooth(method="lm") + 
  ggtitle("Relationship Between ABV and IBU") + 
  facet_wrap(~Region,scales="free")

#Which State has the maximum alcoholic ABV beer?
Top_ABV = Distilled_Data2[order(-Distilled_Data2$ABV),] 
head(Top_ABV,1) 
#Minnesota

#Which State has the most bitter beer
Most_Bitter = Distilled_Data2[order(-Distilled_Data2$IBU),] 
head(Most_Bitter,1)

#Distribution of ABV
summary(Distilled_Data2$ABV)
boxplot(Distilled_Data2$ABV,main="ABV")

#KNN
  #Test for Optimal K
splitPerc = .70
trainBeer = sample(1:dim(IPA_ALE)[1],round(splitPerc * dim(IPA_ALE)[1]))
train = IPA_ALE[trainBeer,]
test = IPA_ALE[-trainBeer,]

accs = data.frame(accuracy = numeric(90), k = numeric(90))

for(i in 1:90)
{
  classifications = knn(train[,c(3,4)],test[,c(3,4)],train$Type, prob = TRUE, k = i)
  table(test$Type,classifications)
  CM = confusionMatrix(table(test$Type,classifications))
  accs$accuracy[i] = CM$overall[1]
  accs$k[i] = i
}
plot(accs$k,accs$accuracy, type = "l", xlab = "k") 
abline(v=accs$k[which.max(accs$accuracy)], col="red")
accs$k[which.max(accs$accuracy)]
  #Use K to train a 70/30 train-test KNN model
splitPerc = .70
trainIndices = sample(1:dim(IPA_ALE)[1],round(splitPerc * dim(IPA_ALE)[1]))
train = IPA_ALE[trainIndices,]
test = IPA_ALE[-trainIndices,]

classification = knn(IPA_ALE[,c(3,4)],IPA_ALE[,c(3,4)],IPA_ALE$Type,prob = TRUE, k = 5)
table(classification,IPA_ALE$Type)
confusionMatrix(table(classification,IPA_ALE$Type))
###############

Distilled_Data2 %>%
  group_by(Region) %>%
  summarize(MedianABV = median(ABV)) %>%
  arrange(-MedianABV)

Distilled_Data2 %>%
  group_by(Region) %>%
  summarize(MedianIBU = median(IBU)) %>%
  arrange(-MedianIBU)


############3



