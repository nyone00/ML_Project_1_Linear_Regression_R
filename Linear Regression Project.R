
# Read in bikeshare.csv

bike <- read.csv('bikeshare.csv')
summary(bike)
# Check head of df

str(df)
head(bike)

library(ggplot2)
library(ggthemes)
library(dplyr)

# Create a scatter plot of count vs temp. 

ggplot(bike,aes(x= temp,y= count)) + geom_point(aes(color=temp),alpha=0.2) + theme_bw()

# CONVERT TO POSIXct()
bike$datetime <- as.POSIXct(bike$datetime)
head(bike)

help("as.POSIXct")

# Plot count versus datetime as a scatterplot with a color gradient based on temperature. 
ggplot(bike,aes(x=datetime, y=count)) + geom_point(aes(color=temp),alpha = 0.5) + scale_color_continuous(low = 'Green', high='Red' )+ theme_bw()


library(corrplot)
library(corrgram)

# Create a correlation between temp and count

tvc <- subset(bike, select=c('temp','count'))
cor(bike[,c('temp','count')])

num.cols <- sapply(tvc,is.numeric)     
cor.data <- cor(tvc[,num.cols])

print(cor.data)


# Explore season data. Create a boxplot, with the y axis indicating ocunt and the x axis
# begin a box for each season

ggplot(bike,aes(x=factor(season),y=count)) + geom_boxplot(aes(color=factor(season)))

bike$hour <- sapply(bike$datetime, function(x){format(x,"%H")})

# From the boxplot, a line can't capture a non-linear relationship
# there are more rentals in winter than in spring due to grwoth of rental count

# Create new features from the datetime column. 

#Working day
ggplot(subset(bike,workingday==1),aes(x=hour,y=count)) + geom_point(position = position_jitter(w=1, h=0), aes(color=temp),
                                                                    alpha = 0.5) + scale_color_gradientn(colours = c('dark blue', 'blue','light blue','light green','yellow','orange','red'))

#non-working day
ggplot(subset(bike,workingday==0),aes(x=hour,y=count)) + geom_point(position = position_jitter(w=1, h=0),aes(color=temp),alpha = 0.5) + scale_color_gradient(
  low = "Blue", high = "Red")


# Building the Model

library(caTools)
set.seed(101)


# Build lm() Model

temp.model <- lm(count~ temp, bike)

summary(temp.model)

# How many bike rental counts at 25C ?

# Method 1
#3.0462 + 9.1705*25

#Method 2
predict(temp.model, data.frame(temp=c(25)) , interval = "confidence")

# Build a model that attemps to predict count based off of the following features. 
# season, holiday, workingday, weather, temp, humidity, windspeed, hour

bike$hour <- sapply(bike$hour,as.numeric)

# substract columns i dont want 
model <- lm(count~. -casual - registered - datetime -atemp,bike)

# From summary
# Multiple R-squared: 0.3344 which is not close to 1,
# so I confirmed this lm model does not perform well on the training data.  
summary(model)
