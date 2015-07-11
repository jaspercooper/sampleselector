
rm(list = ls())
setwd("~/Dropbox/Wellspring Research/Intervention planning/Bibanda census/")

install.packages(c("devtools", "roxygen2", "testthat", "knitr"))

library(ggmap)
library(ggplot2)
library(xtable)
library(foreign)
library(plyr)
library(dplyr)
library(reshape2)
library(ggthemes)



multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
     require(grid)

     # Make a list from the ... arguments and plotlist
     plots <- c(list(...), plotlist)

     numPlots = length(plots)

     # If layout is NULL, then use 'cols' to determine layout
     if (is.null(layout)) {
          # Make the panel
          # ncol: Number of columns of plots
          # nrow: Number of rows needed, calculated from # of cols
          layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                           ncol = cols, nrow = ceiling(numPlots/cols))
     }

     if (numPlots==1) {
          print(plots[[1]])

     } else {
          # Set up the page
          grid.newpage()
          pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

          # Make each plot, in the correct location
          for (i in 1:numPlots) {
               # Get the i,j matrix positions of the regions that contain this subplot
               matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

               print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                               layout.pos.col = matchidx$col))
          }
     }
}

bibanda <- read.dta("bibanda_census.dta",
                    convert.dates = T,
                    convert.factors = T,
                    convert.underscore = T)

bibanda <- within(bibanda,{
     village <- as.character(village)
     # village.correct[village.correct==""] <- NA
})



bibanda <- within(bibanda,{
     # village[village.correct!="99"&!is.na(village.correct)] <- village.correct[village.correct!="99"&!is.na(village.correct)]
     time.week[time.week%in%c("99","")] <- NA
     time.week[time.week== "5:00:00 AM"] <- "5:00:00 PM"
     time.week[time.week== "7:00:00 AM"] <- "7:00:00 PM"
     time.week[time.week== "9:00:00 AM"] <- "9:00:00 PM"
     time.week[time.week== "8:00:00 AM"] <- "8:00:00 PM"
     time.week[time.week== "3:12:00 AM"] <- "3:12:00 PM"
     time.week[time.week== "3:00:00 AM"] <- "3:00:00 PM"
     time.week[time.week== "10:00:00 AM"] <- "10:00:00 PM"
     time.week[time.week== "9:48:00 AM"] <- "9:48:00 PM"
     time.weekend[time.weekend== "7:00:00 AM"] <- "7:00:00 PM"
     time.weekend[time.weekend== "9:00:00 AM"] <- "9:00:00 PM"
     catch.area[as.character(catch.area)=="99.0"] <- NA
     catch.travel[as.character(catch.travel)=="99.0"] <- NA
     catch.travel.we[as.character(catch.travel.we)=="99.0"] <- NA

})



bibanda$village <- toupper(bibanda$village)

village <- ddply(.data = bibanda, .variables = .(village),.fun = summarize,
                 n.viable.bib = length(village),
                 longitude = longitude[1],
                 latitude = latitude[1],
                 catch.area = mean(as.numeric(catch.area),na.rm = T),
                 catch.travel = mean(as.numeric(catch.travel),na.rm = T),
                 catch.travel.we = mean(as.numeric(catch.travel.we),na.rm = T)
)



bibanda$screen.time <- format( strptime(x = bibanda$time.week,format = "%I:%M:%S %p"),"%H:%M")
bibanda$wknd.screen.time <- format( strptime(x = bibanda$time.weekend,format = "%I:%M:%S %p"),"%H:%M")

bibanda$soccer.time <- format( strptime(x = bibanda$football.time,format = "%I:%M:%S %p"),"%H:%M")

# Create distance matrix functions

# Convert degrees to radians
deg.to.rad <- function(x){
     return(x * pi / 180)
}

# Calculate spherical distance between two points on earth
haversine <- function(long1, lat1, long2, lat2) {
     R <- 6371 # Earth mean radius in km
     delta.long <- deg.to.rad(long2 - long1)
     delta.lat <- deg.to.rad(lat2 - lat1)
     a <- sin(delta.lat/2)^2 + cos(deg.to.rad(lat1)) * cos(deg.to.rad(lat2)) * sin(delta.long/2)^2
     c <- 2 * atan2(sqrt(a),sqrt(1-a))
     d = R * c*1000
     return(c(distance = d)) # Distance in meters
}

dist.gen <- function(latlon.org,latlon.dests){

     dists <- adply(.data = latlon.dests,
                    .margins = 1,
                    .fun = function(i){
                         return(haversine(lat1  = latlon.org[1],
                                          long1 = latlon.org[2],
                                          lat2  = i[1],
                                          long2 = i[2]))
                    })[,2]

     return(dists)

}


dist.mat.gen <- function(latlon.orgs,latlon.dests){

     dist.mat <- adply(.data = latlon.orgs,.margins = 1,.fun = function(row.slice){
          dist.gen(latlon.org = row.slice,
                   latlon.dests = latlon.dests
          )
     })[,-1]

     return(dist.mat)

}


latlon.dests <- latlon.org <- cbind(village$latitude,village$longitude)

dist.mat <- dist.mat.gen(latlon.orgs = latlon.dests,latlon.dests = latlon.dests)
village$av.dist <- rowMeans(dist.mat,na.rm = T)/1000

village$min.dist <- apply(dist.mat,1,function(dists)min(dists[dists!=0],na.rm = T))
village$min.dist[is.infinite(village$min.dist)] <- NA


km <- 5

# Generate list of villages within 5km radius
exclude <- sapply(1:(dim(dist.mat)[1]-1),function(i){
     which(dist.mat[i,]/1000<km)
})
# Remove each village from its own list
exclude <- sapply(1:(dim(dist.mat)[1]-1),function(i){
     exclude[[i]][-which(exclude[[i]]==i)]
})
# Create list to sample from
sample.list <- 1:length(exclude)


# Probability weights

dist_mat <- dist.mat[-1,]

sims <- 1000

threshold <- 5000

create_set <- function(dist_mat,sims,threshold){

     exclude_set <- sapply(1:(dim(dist_mat)[1]),function(i){
          which(dist_mat[i,]<threshold)
     })
     # Remove each village from its own list
     exclude_set <- sapply(1:(dim(dist_mat)[1]),function(i){
          exclude_set[[i]][-which(exclude_set[[i]]==i)]
     })


     sapply()

     sapply(exclude,function(neighbors)length(neighbors)/length(exclude))

     # While it is true that the list is not either all 0's or NAs
     while(all(!is.na(unlist(exclude)))){
          # Randomly sample one of the trading centers (TCs)
          keep <- sample(subset(sample.list,is.na(sample.list)==F),1,F)
          # If that TC has other TCs in its proximity, and has not been removed,
          if(length(exclude[[keep]])==0){}else{if(is.na(exclude[[keep]])){}else{
               # then randomly sample one of its villages to remove from the sample
               remove <- sample(exclude[[keep]],1,F)
               # Replace the remove village in sample.list with an NA
               sample.list[sample.list==remove] <- NA
               # Now go through and remove the remove village from each village's
               # list of proximate villages
               for(i in 1:length(exclude)){
                    if(remove%in%exclude[[i]]){
                         exclude[[i]] <- exclude[[i]][-which(exclude[[i]]==remove)]
                    }else{}
               }
               # And also remove the remove village from the list of
               # lists of proximate villages
               exclude[remove] <- NA}
          }
     }
     # Return the villages that made it through
     return(which(sapply(exclude,class)=="integer"))

}

create.set()






# Takes distmat, threshold and sims as argument
# Firstly converts distmat to binary
# Then calculates probability weights
# Then does optimized sims permutations





























