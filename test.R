What is the mean total number of steps per day?

1 Make a histogram of total number of steps per day.


#1 load the data activity.csv R
   Steps
   date
   interval 

setwd("C:/ReprodResearch/PA1")
pa1 <- read.csv("activity.csv")
library(dplyr)

#remove the NA
pa11 <- pa1[!is.na(pa1$step),]

#group by date and sum the steps
pa1sumbd <- group_by(pa11,date) %>% summarize(sum(steps))
names(pa1sumbd)[2] <- 'total_steps'


#totalpd <- aggregate(steps~date,pa11,sum)

#histogram 
library(ggplot2)

g <- ggplot(pa1sumbd,aes(x=date ,y=total_steps))

plot1 <- 
g + 
geom_bar(stat="identity", position="identity",fill="pink", colour="white") +
theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
ggtitle("Total of steps over date \n")



#group by date and median the steps
pa1mdbd <- group_by(pa11,date) %>% summarize(median(steps))
names(pa1mdbd)[2] <- 'median_steps'

g <- ggplot(pa1mdbd,aes(x=date ,y=median_steps))

plot2 <- 
g + 
geom_bar(stat="identity", position="identity",fill="blue", colour="white") +
theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
ggtitle("Median steps over date \n")


#group by date and mean the steps
pa1mnbd <- group_by(pa11,date) %>% summarize(mean(steps))
names(pa1mnbd)[2] <- 'mean_steps'

g <- ggplot(pa1mnbd,aes(x=date ,y=mean_steps))

plot3 <- 
g + 
geom_bar(stat="identity", position="identity",fill="green", colour="white") +
theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
ggtitle("Mean steps over date \n")



# Time series plots
pa11 <- pa1[!is.na(pa1$step),]

# pa11: steps, date, interval
# transform [date,interval] into datetime



# The highest number of interval is 2355, this means a day is divided into 2360 intervals
# In a day there is 86400 seconds (24*60*60) 
# Each interval last for 86440/2360 seconds

b <- pa11$interval * 86440/2360
s <- (pa11$interval * 86440/2360)%%60
m <- floor((pa11$interval * 86440/2360)/60)%%60
h <- floor(floor((pa11$interval * 86440/2360)/60)/60)

pa11$s <- (pa11$interval * 86440/2360)%%60
pa11$m <- floor((pa11$interval * 86440/2360)/60)%%60
pa11$h <- floor(floor((pa11$interval * 86440/2360)/60)/60)

# pa11: steps  date         interval   s   m   h   datetime
#              %Y-%m-%d                            "%Y-%m-%d:%H:%M:%OS"

paste(pa11$date,pa11$h,pa11$m,pa11$s, sep=":")
pa11$datetime <- strptime(paste(pa11$date,pa11$h,pa11$m,pa11$s, sep=":"),"%Y-%m-%d:%H:%M:%OS")


# Plot time series
with(pa11,plot(datetime,steps, type="l",ylab="Steps",xlab="Intervals"))



# id = interval     vars = steps
#pa11$date <- as.Date(pa11$date)
#int_steps <- melt(pa11, id="steps", measure.vars="interval")
#dcast(int_steps, interval~variable

# average by interval across all date

pa11 <- pa1[!is.na(pa1$step),]
pa1avebi <- group_by(pa11,interval) %>% summarize(mean(steps))
names(pa1avebi)[2] <- 'average_steps'
with(pa1avebi,plot(interval,average_steps, type="l",ylab="Average of steps across dates",xlab="Intervals"))
pa1avebi[(pa1avebi$average_steps == max(pa1avebi$average_steps)),pa1avebi$interval]







# Missing values NA

nb <- pa1[(is.na(pa1$steps)),]
nrow(nb)
[1] 2304



# replace na with averge of interval across all dates
pa1mg  <- merge(pa1,pa1avebi,by.x="interval",by.y="interval", all=TRUE)

# replace the steps NA by average_steps
pa1mg$nsteps <- ifelse(is.na(pa1mg$steps),pa1mg$average_steps,pa1mg$steps)
pa1mg$steps <- pa1mg$nsteps


#group by date and sum the steps
pa1mgsumbd <- group_by(pa1mg,date) %>% summarize(sum(steps))
names(pa1mgsumbd)[2] <- 'total_steps'

#histogram 
library(ggplot2)

g <- ggplot(pa1mgsumbd,aes(x=date ,y=total_steps))

plot10 <- 
g + 
geom_bar(stat="identity", position="identity",fill="pink", colour="white") +
theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
ggtitle("Total of steps over date \n")


#group by date and median the steps
pa1mgmdbd <- group_by(pa1mg,date) %>% summarize(median(steps))
names(pa1mgmdbd)[2] <- 'median_steps'

g <- ggplot(pa1mgmdbd,aes(x=date ,y=median_steps))

plot11 <- 
g + 
geom_bar(stat="identity", position="identity",fill="blue", colour="white") +
theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
ggtitle("Median steps over date \n")


#group by date and mean the steps
pa1mgmnbd <- group_by(pa1mg,date) %>% summarize(mean(steps))
names(pa1mgmnbd)[2] <- 'mean_steps'

g <- ggplot(pa1mgmnbd,aes(x=date ,y=mean_steps))

plot12 <- 
g + 
geom_bar(stat="identity", position="identity",fill="green", colour="white") +
theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
ggtitle("Mean steps over date \n")


library(gridExtra)
library(grid)

grid.arrange(plot2, plot11, nrow=2)

grid.arrange(plot3, plot12, nrow=2)




library(data.table)

names(pa1sumbd)[2] <- 'na_total_steps'
names(pa1mgsumbd)[2] <- 'total_steps'

nasumbd <- data.table(pa1sumbd)
mgsumbd <- data.table(pa1mgsumbd)

setkey(nasumbd,date)
setkey(mgsumbd,date)

dagreg <- merge(nasumbd, mgsumbd, all=TRUE)

# melting dagreg into melted
# date    
# variable in total_steps, mean_steps, median_steps
# value

library(reshape2)
melted <- melt(dagreg,id="date", measure.vars=c("na_total_steps","total_steps"))

# plot melted
g <- ggplot(melted,aes(x=date,y=value,fill=variable))
g <- g + geom_bar(stat="identity",position = "identity", alpha=.3) +
theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
ggtitle("Comparison sum of steps over date w/wo NAs \n")





# Create the variable "day" for Weekdays 

pa1mg$day <- weekdays(as.Date(pa1mg$date))


# Create the the variable "wdwe" for "weekday" or "weekend"

wdwe <- function(a) {
  res <<- ""
  for (i in 1:length(a)) { 
        if (a[i] %in% c("Monday","Tuesday","Wednesday","Thursday", "Friday")) {
           res[i] <<- "weekday" 
	    }
	    else {
	       res[i] <<- "weekend"
	    }
    }
   res
}

pa1mg$wdwe <- wdwe(pa1mg$day)

# Compute the average steps for each interval across all weekday from Monday to Friday

pa1mgmnwd <- filter(pa1mg,wdwe=="weekday") %>%  group_by(interval) %>% summarize(mean(steps))
names(pa1mgmnwd)[2] <- 'average_steps'
pa1mgmnwd$wdwe <- rep("weekday",nrow(pa1mgmnwd))


# Compute the average steps for each interval across all weekend for Saturday and Sunday

pa1mgmnwe <- filter(pa1mg,wdwe=="weekend") %>%  group_by(interval) %>% summarize(mean(steps))
names(pa1mgmnwe)[2] <- 'average_steps'
pa1mgmnwe$wdwe <- rep("weekend",nrow(pa1mgmnwe))

# Combine the 2 sets of data and transform the variable wdwe into factor for lattice panel plots

pa1mgmn <- rbind(pa1mgmnwd,pa1mgmnwe)
pa1mgmn$wdwe <- as.factor(pa1mgmn$wdwe)


# Lattice panel plot
library(lattice)
xyplot(pa1mgmn$average_steps~pa1mgmn$interval | pa1mgmn$wdwe, type="l", layout=c(1,2),
       ylab="Number of steps",
	   xlab="Interval"
)

xyplot(pa1mgmn$average_steps~pa1mgmn$interval | pa1mgmn$wdwe,
       ylab="Number of steps",
	   xlab="Interval",
       type="l", layout=c(1,2),
	   panel= function(x, y, ...){
          panel.xyplot(x,y,...)
		  panel.lmline(x,y,col=2)
		 }
 )
























#######################################################




pa1mg[is.na(pa1mg$mean_steps),]


pa1mg[!is.na(pa1mg$mean_steps) & is.na(pa1mg$steps),]

pa1[(pa1$date=="2012-10-01" & !is.NA(pa1$steps)),]



for (i in 1:

pa1mg  <- merge(pa1,pa1mnbd,by.x="date",by.y="data")


nad <- l$date
d <- pa11$date
tv1 <- table(nad)
tv2 <- table(d)
intersect(names(tv1),names(tv2))














#######################################
# The number of interval varies range from 0 to 2355
# The number of minutes in a day is 1440 (24 * 60)
# The time stamp added to the date will use interval times 1440 divided by 2360 to have base of 1440 minutes
# The maximum value of interval is 2360, the 1440 minutes of a day has been divided by 2360 intervals of a certain value of minutes
b <- pa11$interval * 1440 /2360
h <- floor((pa11$interval*1440/2360)/60)
m <- floor((pa11$interval*1440/2360) %% 60)
######################################
# Merge the 3 sets together 
# pa1sumbd pa1mdbd  pa1mnbd into dagreg

library(data.table)

dsumbd <- data.table(pa1sumbd)
dmdbd <- data.table(pa1mdbd)
dmnbd <- data.table(pa1mnbd)

setkey(dsumbd,date)
setkey(dmdbd,date)
setkey(dmnbd,date)

dagreg <- merge(dsumbd, dmnbd)
dagreg <- merge(dagreg, dmdbd)

# melting dagreg into melted
# date    
# variable in total_steps, mean_steps, median_steps
# value

library(reshape2)
melted <- melt(dagreg,id="date", measure.vars=c("total_steps","mean_steps","median_steps"))

# plot melted
g <- ggplot(melted,aes(x=date,y=value,fill=variable))

g <- g + geom_bar(stat="identity",position = "identity", alpha=.3)


names(pa1sumbd)[2] <- 'total_steps'

+
scale_fill_manual(values=c("#CCEEFF","#FFDDDD"),guide=FALSE)


 
             position=position_dodge(),
stat="identity") +
labs(x="Year", y="PM2.5 Emission ( Thousands of Tons)") +
ggtitle("PM 2.5 Emission from Coal combustion across US \n")



#2 
   Total_Steps  date
   -----------  -----     