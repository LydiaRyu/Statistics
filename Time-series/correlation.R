# Auto-Correlation-Functiom

plot(acf(AirPassengers))

AP.decom <- decompose(AirPassengers, "multiplicative")
plot(ts(AP.decom$random[7:138]))

acf(AP.decom$random[7:138])
