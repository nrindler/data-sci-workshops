### Fourier Transform ###

#install.packages("TSA")
library(TSA)

# Generate data
period1 = 5
period2 = 1
x = seq(0,20,len=1000)
y = sin((1/period1)*(2*pi)*x) + 0.5*sin((1/period2)*(2*pi)*x) + 0.1*rnorm(100)

    # Does this data have trend?
    # What are the periods of the seasonality?
    # Which term in the formula above specifies the noise (random variation)?

plot(x,y, pch = 16, cex=0.5)

# With the TSA package:

#Plot periodogram
p = periodogram(y)

#Get frequency and its associated spectrum 
periods_data = data.frame(freq=p$freq, spec=p$spec)

#Sort by declining spec
periods_data = periods_data[order(-periods_data$spec),]
head(periods_data)

#Data is shown in units of frequency, but we want to view the period length in units of X
x_width = x[2]-x[1]
periods_data$seasonality = x_width / periods_data$freq

head(periods_data)
