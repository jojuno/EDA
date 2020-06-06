#delete setwd() for plots
#upload dataset
#covid19.csv
#US_COVID19_combo_v1.csv

###################################################################
#time series line plots of new cases and change in residential stay
###################################################################

#setwd()
library(ggplot2)
library(data.table)
library(lubridate)
library(dplyr)
covid19_dataset <- read.csv("covid19.csv")
covid19_dataset$X <- NULL

ny_data <- subset(covid19_dataset[covid19_dataset$state == "New York",])
ny_data <- ny_data[order(ny_data$date),]

#time lag to account for the time for disease to manifest
ny_data$growthld <- lead(ny_data$newcase,7)

#lead new cases by 7 days
p <- ggplot() +
  geom_line(data=ny_data, aes(y=growthld, x=as.Date(date), colour="red")) + 
  geom_line(data=ny_data, aes(y=residential * coeff, x=as.Date(date), colour="blue")) +
  xlab("Date")  +
  scale_y_continuous(
    name="New Cases",
    sec.axis=sec_axis(~./coeff, name="Change in Residential Stay in %")
  ) +
  scale_color_discrete(name = "Type", labels = c("Residential", "New Cases")) +
  ggtitle("New Cases of COVID 19 and Change in Residential Stay in New York by Date")
p



#############################################
#dewpoint vs. number of new cases scatterplot
#############################################

#setwd()

health_data <- read.csv("C:/Users/Moonsoo Jo/Desktop/moonsoo jo/covid ucla hackathon/US_COVID19_combo_v1.csv")

cali_data <- subset(cali_data[cali_data$state == "California",])
later_period_cases <- subset(cali_data[mdy(cali_data$date) > mdy("4/20/2020") &
                                               mdy(cali_data$date) < mdy("4/25/2020"),])
earlier_period_cases <- subset(cali_data[mdy(cali_data$date) > mdy("4/5/2020") &
                                                 mdy(cali_data$date) < mdy("4/10/2020"),])

#offset data to account for manifestation of the disease
cases_combo <- data.frame(cbind(earlier_period_cases$dewpoint, later_period_cases$cases))

ggplot(cases_combo, aes(X1, X2) )+
  geom_point(color = "darkblue") +
  labs(y="Confirmed Cases", x="Dewpoint in Fahrenheit", title = "Confirmed Cases by Dewpoint in California") +
  geom_smooth(method = "lm", se = FALSE)



#############################################################
#boxplots of change in residential stay by states in the U.S.
#before and after the stay-at-home order
#############################################################

#setwd()

library("lubridate")
options(scipen = 999)

covid19_dataset <- read.csv("covid19.csv")
covid19_dataset$X <- NULL

covid19_before_order <- subset(covid19_dataset[covid19_dataset$order_placed == "N",])
covid19_after_order <- subset(covid19_dataset[covid19_dataset$order_placed == "Y",])

states_and_regions <- subset(covid19_dataset[,c(1, 17)]) 
states_and_regions <- unique(states_and_regions[,1:2])
states_and_regions_ordered<-states_and_regions[order(states_and_regions$region),]

#residential
#before order
boxplot(covid19_before_order$residential ~ covid19_before_order$state, las = 2,
        col="pink", xlab="", 
        ylab="Change in Visits by %",
        main="Change in Residential Stay by State in the U.S. Before the Order Was Placed")
mtext(text="States", side=1, line=7)

#after order
boxplot(covid19_after_order$residential ~ covid19_after_order$state, las = 2,
        col="pink", xlab="", 
        ylab="Change in Visits by %",
        main="Change in Residential Stay by State in the U.S. After the Order Was Placed")
mtext(text="States", side=1, line=7)


#############################################################
#barplots of aggregated number of cases by states and region
#in the U.S. over time before and after the stay-at-home order
#############################################################

#before order
#set new directory to save the plots that will be generated
setwd(".../aggregated_case_before_order_plots")
date = mdy("2/27/2020")
while (date < mdy("3/31/2020")) {
  #list of states
  state = data.frame(states_and_regions_ordered)
  state$aggcase = 0
  #subset data by a day
  day_data <- subset(covid19_dataset[as.character(covid19_dataset$date) == date,])
  day_data <- subset(day_data[,c(1, 4)])
  #join day data to list of states
  for (row in 1:nrow(state)) {
    for (row2 in 1:nrow(day_data)) {
      if (as.character(state[row,]$state) == as.character(day_data[row2,]$state)) {
        state[row,]$aggcase = day_data[row2,]$aggcase
      }
    } 
  }
  #open a file to save a plot
  png(sprintf('Aggregated Cases by State Before the Stay-At-Home Order Was Placed, on %s.jpg', date), width = 1200, height = 800)
  par(mar=c(15, 9, 9, 2.1))
  #colors for regions
  palette <- RColorBrewer::brewer.pal(length(unique(state$region)),name = 'Set1')
  state$color <- palette[as.factor(state$region)]   
  #generate a plot
  barplot(state$aggcase, names.arg=state$state, las=2,
          col=state$color, ylim=c(0, 100000),
          main=sprintf('Aggregated Cases by State Before the Stay-At-Home Order Was Placed, on %s', date),
          col.main="#55BB77", cex.main=2, cex.names=1.5, cex.axis=1.5)
  title(ylab="Aggregated Cases", cex.lab=2, line = 7, col.lab="#55BB77")
  title(xlab = "States", cex.lab=2,line = 13, col.lab="#55BB77")
  legend("topright", cex=1.5,ncol=2,text.width = 6,bty = 'n',
         x.intersp = .7,y.intersp = .7,box.lwd = 1,
         legend = state$region[!duplicated(state$region)],
         fill = state$color[!duplicated(state$region)])
  #save the plot
  dev.off()
  #update date for the loop
  date = date + 1
}
#this will generate a number of plots
#play the plots in the order of days, and it will display the data in a time series


#after order
#set new directory to save the plots that will be generated
setwd(".../aggregated_case_after_order_plots")
date = mdy("3/31/2020")
while (date < mdy("5/3/2020")) {
  #list of states
  state = data.frame(states_and_regions_ordered)
  state$aggcase = 0
  #subset data by a day
  day_data <- subset(covid19_dataset[as.character(covid19_dataset$date) == date,])
  day_data <- subset(day_data[,c(1, 4)])
  #join day data to list of states
  for (row in 1:nrow(state)) {
    for (row2 in 1:nrow(day_data)) {
      if (as.character(state[row,]$state) == as.character(day_data[row2,]$state)) {
        state[row,]$aggcase = day_data[row2,]$aggcase
      }
    }
  }
  #open a file to save a plot
  png(sprintf('Aggregated Cases by State After the Stay-At-Home Order Was Placed, on %s.jpg', date), width = 1200, height = 800)
  par(mar=c(15, 9, 9, 2.1))
  #colors for regions
  palette <- RColorBrewer::brewer.pal(length(unique(state$region)),name = 'Set1')
  state$color <- palette[as.factor(state$region)]   
  #generate a plot
  barplot(state$aggcase, names.arg=state$state, las=2,
          col=state$color, ylim=c(0, 100000),
          main=sprintf('Aggregated Cases by State After the Stay-At-Home Order Was Placed, on %s', date),
          col.main="#55BB77", cex.main=2, cex.names=1.5, cex.axis=1.5)
  title(ylab="Aggregated Cases", cex.lab=2, line = 7, col.lab="#55BB77")
  title(xlab = "States", cex.lab=2,line = 13, col.lab="#55BB77")
  #labels for states with overflowing data
  if (date > mdy("4/1/2020")) {
    text(23,70000, labels =sprintf('New York, %s', state[18,3]), adj=0, cex=1.5)
    if (date > mdy("4/21/2020")) {
      text(8,70000, labels =sprintf('New Jersey, %s', state[17,3]), adj=0, cex=1.5)
    }
  }
  legend("topright", cex=1.5,ncol=2,text.width = 6,bty = 'n',
         x.intersp = .7,y.intersp = .7,box.lwd = 1,
         legend = state$region[!duplicated(state$region)],
         fill = state$color[!duplicated(state$region)])
  #save the plot
  dev.off()
  #update date for the loop
  date = date + 1
}



