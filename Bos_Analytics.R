getwd()
setwd("C:/Users/rasim/Desktop/MPP1/Internships/Boston_Analytics")
getwd()
library(dplyr)
install.packages("ggmap")
library(ggmap)

rm(list=ls())

bos19 <- read.csv("2019.csv")
bos <- read.csv("2020.csv")
register_google("AIzaSyAQJwF-cZx5mrNda-EstycTVEmkjiERscQ")
has_google_account()

locations <- c("Boston, Massachusetts") %>% geocode()

library(ggplot2)
install.packages("mapproj")
library(mapproj)
install.packages("sf")
library(sf)

#Making Maps of Boston
open_case <- subset(bos, case_status=="Open" & type == "Graffiti Removal")
case_lat_long <- data.frame(longitude = open_case$longitude, latitude = open_case$latitude)


bos_plot <- ggmap(get_map('Boston, Massachusetts',
                       zoom=12,
                       scale = 2,
                       legend = "right",
                       source='google',
                       maptype='terrain'))

bos_plot +
  geom_point(data = fire_ll, aes(x = longitude, y = latitude), size = 3, 
           shape = 21, fill = "darkred", alpha = 0.4) +
  geom_point(data = fire19_ll, aes(x = longitude, y = latitude), size = 2, 
             shape = 22, fill = "yellow", alpha = 0.4)


  coord_sf(xlim = c(-71.115, -71.0), ylim = c(42.32, 42.40), expand = FALSE)
  

nb <- function(city, place) {
  place <- subset(city, neighborhood == as.character(place))
  neighbor <- table(place$type)
  neighbor <- data.frame(neighbor)
  neighbor <- neighbor[order(-neighbor$Freq),]
  neighbor <- neighbor[1:20,]
  neighbor$Var1 <- factor(neighbor$Var1, levels = neighbor$Var1[order(-neighbor$Freq)])
  return(neighbor)
}


#Barplots

library(ggplot2)
# Basic barplot
nb_barplot <- function(hood) {
  q <- ggplot(data=hood, aes(x=Var1, y=Freq, color = Var1)) +
    geom_bar(stat="identity", fill = "white") +
    geom_text(aes(label=Freq), vjust=-0.3, size=3.5) +
    ylab("Count") +
    xlab("") +
    theme(axis.text.x = element_text(angle = 90, hjust = 0.95)) +
    theme(legend.position = "none") +
    coord_cartesian(ylim=c(0,hood[1,2]+20)) +
    ggtitle(paste(deparse(substitute(hood)),"'s Service Priorities"))
  return(q)
}

sb_barplot <- function(hood) {
  s <- ggplot(data=hood, aes(x=Var1, y=Freq)) +
    geom_bar(stat="identity", fill = "pink") +
    geom_text(aes(label=Freq), vjust=-0.3, size=3.5) +
    ylab("Count") +
    xlab("") +
    theme(axis.text.x = element_text(angle = 90, hjust = 0.95)) +
    theme(legend.position = "none") +
    coord_cartesian(ylim=c(0,hood[1,2]+20)) +
    ggtitle("Constituent Calls")
  return(s)
}


for (i in unique(bos$neighborhood)) {
  if (!(i == " " | i == "")) {
    assign(as.character(i),nb(bos, as.character(i)))
  } else {
    next
  }
  print(i)
} 

case19 <- bos19[bos19$ontime == "OVERDUE",]
ll_case19 <- data.frame(longitude = case19$longitude, latitude = case19$latitude)

#Creating an income table with number of tickets
nbhood <- c("Allston","Back Bay", "Beacon Hill", "Brighton", "Charlestown", "Dorchester",
            "Downtown / Financial District", "East Boston", "Fenway", "Hyde Park", "Jamaica Plain", "Longwood",
            "Mattapan", "Mission Hill", "North End", "Roslindale", "Roxbury", 
            "South Boston", "South Boston / South Boston Waterfront", "South End", "West Roxbury")
income <- c(39717, 88469, 93033, 50110, 91067, 47200, 65090, 51549, 34483, 65260, 76968, 38235,
            43256, 32103, 82965, 68209, 25937, 77223, 111518, 77161, 79424)
ticekts <- c(19894, 13199, 7730, 20718, 8879, 36069, 17296, 16149, 5919, 10986, 15778, 5919, 398, 4857, 0, 
             9312, 22852, 1810, 19142, 16789, 9100)

income_data <- data.frame(neighborhood = nbhood, income = income)
income_data[,"2019_cases"] <- ticekts

#test: nrow(subset(bos,type=="Request for Pothole Repair" & neighborhood=="Jamaica Plain" ))

#Makign Maps of Incidents of Fire
fire <- bos[bos$case_title == "Fire",]
fire_ll <- data.frame(longitude = fire$longitude, latitude = fire$latitude)

fire19 <- bos19[bos19$case_title=="Fire",]
fire19_ll <- data.frame(longitude = fire19$longitude, latitude = fire19$latitude)

apps <- bos[bos$source=="Constituent Call",]
ap_t <- table(apps$type)
ap_t <- data.frame(ap_t)
ap_t <- ap_t[order(-ap_t$Freq),]
ap_t <- ap_t[ap_t$Freq >0,]
ap_t <- ap_t[order(-ap_t$Freq),]
ap_t$Var1 <- factor(ap_t$Var1, levels = ap_t$Var1[order(-ap_t$Freq)])
nb_barplot(ap_t)


require(ggmap)
map.center <- geocode("Boston, MA")
Bos_map <- qmap(c(lon=map.center$lon, lat=map.center$lat), zoom=12)
g <- Bos_map + geom_point(aes(x=longitude, y=latitude), data=fire19_ll, size=2, alpha=0.2, color="red") + 
  ggtitle("Fires in Boston (2019)")

needle <- bos[bos$type == "Needle Pickup",]
n <- Bos_map + geom_point(aes(x=longitude, y=latitude), data=needle, size=1, alpha=0.2, color="blue") + 
  ggtitle("Needle Pickup in Boston (2020)")

clean <- bos[bos$type == "Requests for Street Cleaning" & bos$case_status=="Open",]
c <- Bos_map + geom_point(aes(x=longitude, y=latitude), data=clean, size=3, alpha=0.2, color="blue") + 
  ggtitle("Requests for Street Cleaning (2020)")


install.packages("lubridate")
install.packages("tidyverse")

bos$closed_dt <- ymd_hms(as.character(bos$closed_dt))
bos$open_dt <- ymd_hms(as.character(bos$open_dt))
bos19$closed_dt <- ymd_hms(as.character(bos19$closed_dt))
bos19$open_dt <- ymd_hms(as.character(bos19$open_dt))


diffs <- round(difftime(bos$closed_dt,bos$open_dt,units="hours"),3)
bos$diffs <- diffs

diffs19 <- round(difftime(bos19$closed_dt,bos19$open_dt, units = "hours"),3)
bos19$diffs <- diffs19

diff_by_type <- aggregate(cbind(diffs)~type,bos, FUN = median)
colnames(diff_by_type) <- c("Type of Request", "Median Time Taken to Complete (Hours)")
form_diff <- diff_by_type[order(-diff_by_type$`Median Time Taken to Complete (Hours)`),]

diff_by_neighborhood <- aggregate(cbind(diffs)~neighborhood, bos, FUN=median)
diff_by_neighborhood$diffs <- round(diff_by_neighborhood$diffs,3)
colnames(diff_by_neighborhood) <- c("Neighborhood", "Median Time Taken to Complete (Hours)")
neigh_diff <- diff_by_neighborhood[order(-diff_by_neighborhood$`Median Time Taken to Complete (Hours)`),]
neigh_diff <- neigh_diff[-c(3,11),]



diff19_by_type <- aggregate(cbind(diffs)~type,bos19, FUN = median)
diff19_by_neighborhood <- aggregate(cbind(diffs)~neighborhood, bos19, FUN=median)
diff19_by_type[order(-diff19_by_type$diffs),]
diff_by_neighborhood[order(-diff_by_neighborhood$diffs),]

compa <- merge(diff_by_type, diff19_by_type, by="type")
colnames(compa) <- c("type","2020","2019")
compa[order(-compa$`2019`),]

neihba <- merge(diff_by_neighborhood, diff19_by_neighborhood, by="neighborhood")
colnames(neihba) <- c("type","2020","2019")

install.packages("formattable")
library(formattable)

Dorch <- bos[bos$neighborhood=="Dorchester",]
aggregate(diffs~type,Dorch,FUN = median)

formattable(neigh_diff, align = c("l","l"),
            list(`Type of Request` = formatter("span", style = ~ style(color = "grey", font.weight = "bold")), 
                 `Median Time Taken to Complete (Hours)` = color_bar("#FA614B66")))

            