# THIS CODE WAS USED TO CREATE PROJECT #2

ny = read.csv('new_york_city.csv')
wash = read.csv('washington.csv')
chi = read.csv('chicago.csv')

#Explore the data to see what is available
head(ny)

by(ny$Trip.Duration,ny$Gender,summary)

head(wash)
head(chi)

#The code below is used to explore the data and provide
#Visual feedback.

# Your solution code goes here
# Do different cities have different volumes of bike share?

# Load Library
library(ggplot2)

ggplot(aes( x= Trip.Duration), data = ny) +
      geom_histogram(binwidth = 50, color = 'blue') +
      ggtitle ('Trip Duration Histogram') +
      labs( x = 'Trip Duration', y = 'Count')

# Limit duration to 4000 (bulk of the data is from 0 to 3000)
ggplot(aes( x= Trip.Duration), data = ny) +
      geom_histogram(binwidth = 5, color = 'blue', fill = 'black') +
      ggtitle (' Trip Duration Histogram') +
      labs( x = 'Trip Duration', y = 'Count') +
      scale_x_continuous(limits=c(0,4000))

# Add other dataset
# Limit duration to 4000 (bulk of the data is from 0 to 3000)

ggplot() +

      geom_histogram( data = wash, aes( x= Trip.Duration, color = "P"), alpha = .4, fill = "Purple", binwidth=5) +
      geom_histogram( data = ny, aes( x= Trip.Duration, color = "Y"), alpha = 0.1, fill = "Yellow", binwidth=5) +
      geom_histogram( data = chi, aes( x= Trip.Duration, color = "LG"), alpha = 0.1, fill = "LightGreen", binwidth=5) +

      ggtitle (' Trip Duration Histogram') +
      labs( x = 'Trip Duration', y = 'Count') +
      scale_x_continuous(limits=c(0,4000)) +

      scale_colour_manual(name="City",values=c("P"="Purple","Y"="Yellow","LG"="LightGreen"),labels=c("Washington","New York","Chicago"))


summary(wash$Trip.Duration)



summary(ny$Trip.Duration)

summary(chi$Trip.Duration)

#Washigton definitely has higher volume of bikeshare data when compared to New York and Chicago.
#The distribution of the data seems to be even accross all cities, the median for all three cities is fairly close.
#It also seems that the minimum trip duration for all cities is 60 minutes (this may be a minimum required by the bike share company)
#The usage of bike share my be caused by weather, chicago with the lowest usage also has the coldest weather of all the cities,
#followed by New York, then Washington

# How are the bikeshare used between man and women, who uses it more and how

# Your solution code goes here

# Look at the data for each city
print('New York')
by(ny$User.Type,ny$Gender,summary)
# by(wash$Trip.Duration,wash$Gender,summary) #### GENDER DATA NOT AVAILABLE ####
print('Chicago')
by(chi$User.Type,chi$Gender,summary)

ggplot (aes(x=Trip.Duration), data=ny) +
    geom_histogram(binwidth=10) +
    scale_x_continuous(limits=c(0,3000)) +
    ggtitle('Histogram of Trip Duration Per User') +
    facet_wrap(~Gender)


#Same data, but remove if not Female or male
TripDurationPlot <- function (DataSubset, CityName)
    {
    ggplot (aes(x=Trip.Duration), data=subset(DataSubset, Gender != "")) +
    geom_histogram(binwidth=10) +
    scale_x_continuous(limits=c(0,3000)) +
    ggtitle('Histogram of Trip Duration Per User - ', CityName ) +
    labs (x = 'Trip Duration', y = 'Count') +
    facet_wrap(~Gender)
    }

TripDurationPlot (ny, "New York")
TripDurationPlot (chi, "Chicago")

TripDurationGender <- function (DataSubset, CityName)
    {
    qplot (x=Gender, y=Trip.Duration, data=subset(DataSubset, Gender !=''), geom = 'boxplot', ylim = c(0,3000)) +
    ggtitle('Trip Duration per Gender - ', CityName)
    }

TripDurationGender (ny, "New York")
TripDurationGender (chi, "Chicago")

# When looking at the trip duration per gender, although Males have a higher volume,
# Females actually use the bikes for a longer period of tim

# When do most bikeshare user have the highest use, does it change by location?

# Your solution code goes here

library(scales)

New_NY = ny
New_NY$Month_Yr <- format(as.Date(ny$Start.Time), "%Y-%m")

New_Chi = chi
New_Chi$Month_Yr <- format(as.Date(chi$Start.Time), "%Y-%m")

New_Wash = wash
New_Wash$Month_Yr <- format(as.Date(wash$Start.Time), "%Y-%m")


head(ny)
head(New_NY)

summary (wash)
summary (New_Wash)

DurationByTimeYear <- function (DataSubset, CityName, DotColor, LineColor)
    {
    ggplot(aes(x=Month_Yr, y=Trip.Duration, group = 1), data = subset(DataSubset, !is.na(Trip.Duration)) ) +
        ylim(0,4000) +
        geom_point(alpha = 1/15, position = position_jitter(h=0), color = DotColor) +
        coord_trans(y='sqrt') +
        geom_line( stat = 'summary', fun.y=mean, color = LineColor) +
        labs( x = "Time of Year", y="Trip Duration") +
        ggtitle(CityName, " Trip Duration by time of year")
    }

DurationByTimeYear (New_NY, "New York", 'orange', '#CC8400')

DurationByTimeYear (New_Wash, "Washington", 'blue', '#0000B3')

DurationByTimeYear (New_Chi, "Chicago", 'light green', '#00B300')

# The Bike Share Data shows that as the weather gets warmer, the number of use goes up as well as the duration.

system('python -m nbconvert Explore_bikeshare_data.ipynb')
