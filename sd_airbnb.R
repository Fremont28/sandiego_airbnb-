# import dataset 
sd<-read.csv(file.choose(),header=TRUE)

#libraries
install.packages("rworldmap")
library(rworldmap)
library(ggmap)
library(ggplot)
library(plyr)
library(dplyr)

# 1. Converting the date into year-month-day format using lubridate
# 2. Creating a new date variable and transformaing the date into a numeric variable
# 3. Calculating the difference between the Airbnb host's first and last review

library(lubridate)
sd$new_first_review<-sd$first_review
sd$new_last_review<-sd$last_review
sd$new_first_review<-ymd(sd$new_first_review)
sd$new_last_review<-ymd(sd$new_last_review)
sd$new_first_review<-as.numeric(sd$new_first_review)
sd$new_last_review<-as.numeric(sd$new_last_review)
sd$time<-sd$new_last_review-sd$new_first_review # The time that the host has been active on Airbnb

# Scatterplot between the host's active time on Airbnb and the median price per night
ggplot(sd,aes(x=time,y=price))+geom_point()
# Reviews between July 7, 2015 and July 7,2016 (365 days)
one_year<-subset(sd,new_last_review<16999 & new_first_review>16623)
# Hosts with at least one active year on Airbnb
two_years<-subset(sd,new_last_review<16999 & new_first_review>16258)
# Host with 2+ years on Airbnb
three_years<-subset(sd,new_last_review<16999 & new_first_review>15893)
summary(three_years)

# Do more expensive Airbnb rentals have higher availability, higher reviews?
price_cheap<-subset(sd,price<109,select=c(price,review_scores_value,availability_30))
price_mid<-subset(sd,price>109 & price<259,select=c(price,review_scores_value,availability_30))
price_exp<-subset(sd,price>258,select=c(price,review_scores_value,availability_30))

#reviews vs. active time on Airbnb
time_sub<-subset(sd,time>90) # Hosts active for a mimimum of 90 days
ggplot(time_sub,aes(x=time,y=reviews_per_month))+geom_point()

# Expensive San Diego neighborhoods
sd_neighborhoods<-ddply(sd,~neighbourhood,summarise,mean=mean(price))
# Popular neighborhoods in San Diego
count_neighborhoods<-sd %>%
  group_by(neighbourhood) %>%
  summarise(n=n()) %>%
  mutate(freq=n/sum(n))

# merging the average price and count of San Diego neighborhoods into one dataframe
merge_df<-merge(sd_neighborhoods,count_neighborhoods,by="neighbourhood")
merge_df1<-subset(merge_df,n<1000)

# Finding the average longitude and latitude of San Diego neighborhoods
sd_long<-ddply(sd,~neighbourhood,summarise,mean=mean(longitude))
sd_lat<- ddply(sd,~neighbourhood,summarise,mean=mean(latitude))

# merging data frames
merge1<-merge(sd_long,sd_lat,by="neighbourhood")
merge2<-merge(merge1,sd_neighborhoods,by="neighbourhood")
merge3<-subset(merge2,n>100) #neighborhoods with greater than 100 listings

## Plot density of Airbnb listings in San Diego

map <- get_map(location = c(-117.1626,32.70867), zoom = 10)
ggmap(map)+geom_point(data=sd,aes(x=sd$longitude,y=sd$latitude,alpha=0.5))

# Plot listings using ggmap (neighborhoods with greater than 100 listings)
p<-ggmap(map)+geom_point(data=merge3,aes(x=merge3$mean.x,y=merge3$mean.y,size=merge3$freq,color=merge3$neighbourhood))+
  labs(color='Neighbourhood')+labs(size='Percent of Listings')

# Are more expensive neighborhoods less popular?
ggplot(merge_df1,aes(x=n,y=mean))+geom_point()+geom_text(aes(label=ifelse(n>250,as.character(neighbourhood),'')),hjust=0,vjust=0)+
  xlab("Number of Listings")+ylab("Average Price")

# Listings within the last year (by neighborhood)
pacific_beach1<-subset(sd,new_first_review>16624 & neighbourhood=="Pacific Beach")
View(pacific_beach1)

# Percent of Airbnb listings over the last 365 days
ggplot(neighborhood_df, aes(x = Neighborhood, y = New_Listings,label=New_Listings)) +
  geom_point(aes(size = New_Listings, colour = Neighborhood)) + coord_flip()+
  geom_text(hjust = 1, size = 2) +
  scale_size(range = c(1,15)) +
  theme_bw()+xlab("Neighborhood")+ylab("Percent of Listings Over Last 365 Days")

# Find the percentage of hosts with more than one active listing
count_double<-count(sd$host_id)
count_double1<-subset(count_double,freq>1,select=c(1,2))

##import zillow rental dataset 
# Zillow 2 bedroom apartment rental prices in San Diego
sd_rents<-read.csv(file.choose(),header=TRUE)

#convert long to wide format
median_rents<-melt(sd_rents,id.vars=c("RegionName","City","State","Metro","CountyName","SizeRank"))

#subset San Diego neighborhoods
median_rents_sd <-subset(median_rents,City=="San Diego")
#Use grep() to retrieve the average 2 bedroom rental price over 2013,2014,2015,2016,2017 (example for 2016)
recent_rents_sd<-median_rents_sd[grep("X2016",median_rents_sd$variable),]
#2 bedroom rental prices by year in San Diego Neighborhoods
sd_rent_prices<-ddply(recent_rents_sd,~RegionName,summarise,mean=mean(value))

# New dataframe with the average price per year (2 bedroom apartments) and the geographic coordinates for
# the neighborhoods
neighborhood_df<-read.csv(file.choose(),header=TRUE)
colnames(neighborhood_df)[2] <- "New_Listings"

# Percent change in price for 2 bedroom rentals year to year
ggplot(neighborhood_df, aes(x = reorder(Neighborhood.1, -increase_percent_15.16_rent), y = increase_percent_15.16_rent)) +
  geom_bar(stat = "identity")+xlab("Neighborhood")+ylab("Percent Rent Increase (2015 to 2016)")+ggtitle("San Diego Neighborhoods Are Getting More Expensive")+
  theme(plot.title = element_text(hjust = 0.5))