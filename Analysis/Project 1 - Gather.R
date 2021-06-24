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