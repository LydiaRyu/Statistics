data("AirPassengers")
AirPassengers

class(AirPassengers)

frequency(AirPassengers)

plot(AirPassengers)

# Remove sensonal effect

plot(aggregate(AirPassengers))

# Monthly/ Annually plot of time series

AP.month.ts <- ts(AirPassengers, start = c(1949, 1), freq = 12)
AP.annual.ts <- aggregate(AP.month.ts)/12

plot(AP.month.ts)
plot(AP.annual.ts)

# Factors of time series

plot(AP.decom$trend, ylab = "Passengers(1000's)", main = "Trend Factor")
plot(AP.decom$seasonal, ylab = "Passengers(1000's)", main = "Seasonal Factor")
plot(AP.decom$random, ylab = "Passengers(1000's)", main = "Irregular Factor")

# Decomposition of time series

plot(decompose(AirPassengers))

plot(decompose(AirPassengers, type = "mult"))


AP.decom <- decompose(AirPassengers, type = "mult")
trend <- AP.decom$trend
seasonal <- AP.decom$seasonal
ts.plot(cbind(trend, trend*seasonal), lty = 1:2)
        

