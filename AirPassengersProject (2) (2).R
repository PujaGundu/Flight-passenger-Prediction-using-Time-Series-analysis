library(forecast)
library(ggplot2)
library(zoo)

## CREATE DATA FRAME. 


# Set working directory for locating files.
setwd("/Users/vishalreddywudaru/Downloads")
getwd()

# Create data frame.
AirPassengers.data <- read.csv("AirPassengers.csv")
# See the first 6 records of the file.
head(AirPassengers.data)
tail(AirPassengers.data)

# Check for missing values in the entire dataset
any_missing <- any(is.na(AirPassengers.data))
print(any_missing)  # Returns TRUE if any missing values are present

# Count missing values in each column
colSums(is.na(AirPassengers.data))

## USE ts() FUNCTION TO CREATE TIME SERIES DATA SET.
## USE stl() FUNCTION TO PLOT TIME SERIES COMPONENTS 
## USE Acf() FUNCTION TO IDENTIFY AUTOCORRELATION

# Function ts() takes three arguments: start, end, and freq.
# With monthly data, frequency (freq) of periods in a season (year) is 12. 
# With quarterly data, frequency in a season (year) is equal to 4.
# Arguments start and end are pairs: (season number, period number).
Passengers.ts <- ts(AirPassengers.data$Passengers, 
                   start = c(1949, 1), end = c(1960, 12), freq = 12)
Passengers.ts

# Use stl() function to plot times series components of the original data. 
# The plot includes original data, trend, seasonal, and reminder 
# (level and noise component).
passenger.stl <- stl(Passengers.ts, s.window = "periodic")
autoplot(passenger.stl, main = "AirPassenger Time Series Components")

# Use Acf() function to identify autocorrelation and plot autocorrelation
# for different lags.
autocor <- Acf(Passengers.ts, lag.max = 12, 
               main = "Autocorrelation for AirPassengers Data")

## Use plot() to plot time series data  
plot(Passengers.ts, 
     xlab = "Time", ylab = "Passengers (in 90s)", 
     ylim = c(100, 630), xaxt = 'n',
     main = "AirPassengers ")

# Establish x-axis scale interval for time in months.
axis(1, at = seq(1949, 1960, 1), labels = format(seq(1949, 1960, 1)))

# Define the numbers of months in the training and validation sets,
# nTrain and nValid, respectively.
nValid <- 24
nTrain <- length(Passengers.ts) - nValid
train.ts <- window(Passengers.ts, start = c(1949, 1), end = c(1949, nTrain))
valid.ts <- window(Passengers.ts, start = c(1949, nTrain + 1), end = c(1949, nTrain + nValid))


# Use Acf() function to identify autocorrelation for training and validation
# data sets, and plot autocorrelation for different lags (up to maximum of 12)
Acf(train.ts, lag.max = 12, main = "Autocorrelation for AirPassengers Training Data Set")
Acf(valid.ts, lag.max = 12, main = "Autocorrelation for AirPassengers Validation Data Set")

# Use Arima() function to fit AR(1) model for S&P500 close prices.
# The ARIMA model of order = c(1,0,0) gives an AR(1) model.
close.passenger.ar1<- Arima(Passengers.ts, order = c(1,0,0))
summary(close.passenger.ar1)


# Apply z-test to test the null hypothesis that beta 
# coefficient of AR(1) is equal to 1.
ar1 <- 0.9646
s.e. <- 0.0214
null_mean <- 1
alpha <- 0.05
z.stat <- (ar1-null_mean)/s.e.
z.stat
p.value <- pnorm(z.stat)
p.value
if (p.value<alpha) {
  "Reject null hypothesis"
} else {
  "Accept null hypothesis"
}

# Create first difference of ClosePrice data using diff() function.
diff.Passenger <- diff(Passengers.ts, lag = 1)
diff.Passenger

# Develop data frame with Close Price, Close Price lag 1, and first
# differenced data.
diff.df <- data.frame(Passengers.ts, c("", round(Passengers.ts[1:143],2)), 
                      c("", round(diff.Passenger,2)))

names(diff.df) <- c("Passengers", "Passengers Lag-1", 
                    "First Difference")
diff.df 
# Use Acf() function to identify autocorrealtion for first differenced
# ClosePrice and plot autocorrelation for different lags 
# (up to maximum of 12).
Acf(diff.Passenger, lag.max = 12, 
    main = "Autocorrelation for Air passengers Data")


###Model 1 ARIMA Model
## FIT ARIMA(2,1,2)(1,1,2) MODEL.

# Use Arima() function to fit ARIMA(2,1,2)(1,1,2) model for 
# trend and seasonality.
# Use summary() to show ARIMA model and its parameters.
train.arima.seas <- Arima(train.ts, order = c(2,1,2), 
                          seasonal = c(1,1,2)) 
summary(train.arima.seas)

# Apply forecast() function to make predictions for ts with 
# ARIMA model in validation set.    
train.arima.seas.pred <- forecast(train.arima.seas, h = nValid, level = 0)
train.arima.seas.pred

# Use Acf() function to create autocorrelation chart of ARIMA(2,1,2)(1,1,2) 
# model residuals.
Acf(train.arima.seas$residuals, lag.max = 12, 
    main = "Autocorrelations of ARIMA(2,1,2)(1,1,2) Model Residuals")

# Plot historical data, predictions for historical data, and seasonal 
# ARIMA forecast for 12 future periods.
plot(train.arima.seas.pred, 
     xlab = "Time", ylab = "Passenger (in 90s)", 
     ylim = c(100, 630), xaxt = "n",
     bty = "l", xlim = c(1949, 1962.25), lwd = 2,
     main = "Seasonal ARIMA(2,1,2)(1,1,2)[12] Model for Training Data Set") 
axis(1, at = seq(1949, 1962, 1), labels = format(seq(1949, 1962, 1)))
lines(train.arima.seas.pred$fitted, col = "blue", lwd = 2)
lines(valid.ts, col = "blue", lty = 5, lwd = 2)
legend(1950,400, legend = c("Passenger Data", 
                            "Seasonal ARIMA Forecast", 
                            "Seasonal ARIMA Forecast for 24 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")


# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(1959, 1959), c(0, 630))
lines(c(1961, 1961), c(0, 630))
text(1952, 610, "Training")
text(1960.5, 610, "Validation")
text(1961.8, 610, "Future")
arrows(1949, 600, 1959, 600, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(1959.1, 600, 1961, 600, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(1961.1, 600, 1962.3, 600, code = 3, length = 0.1,
       lwd = 1, angle = 30)


## FIT AUTO ARIMA MODEL.

# Use auto.arima() function to fit ARIMA model.
# Use summary() to show auto ARIMA model and its parameters.
train.auto.arima <- auto.arima(train.ts)
summary(train.auto.arima)

# Apply forecast() function to make predictions for ts with 
# auto ARIMA model in validation set.  
train.auto.arima.pred <- forecast(train.auto.arima, h = nValid, level = 0)
train.auto.arima.pred

# Using Acf() function, create autocorrelation chart of auto ARIMA 
# model residuals.
Acf(train.auto.arima$residuals, lag.max = 12, 
    main = "Autocorrelations of Auto ARIMA Model Residuals")

# Plot historical data, predictions for historical data, and seasonal 
# ARIMA forecast for 12 future periods.
plot(train.auto.arima.pred, 
     xlab = "Time", ylab = "Passenger (in 90s)", 
     ylim = c(100, 630), xaxt = "n",
     bty = "l", xlim = c(1949, 1962.25), lwd = 2,
     main = "Auto ARIMA Modelfor Training Data Set") 
axis(1, at = seq(1949, 1962, 1), labels = format(seq(1949, 1962, 1)))
lines(train.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(valid.ts, col = "blue", lty = 5, lwd = 2)
legend(1950,400, legend = c("Passenger Data", 
                            "Auto ARIMA Forecast", 
                            "Auto ARIMA Forecast for 24 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")


# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(1959, 1959), c(0, 630))
lines(c(1961, 1961), c(0, 630))
text(1952, 610, "Training")
text(1960.5, 610, "Validation")
text(1961.8, 610, "Future")
arrows(1949, 600, 1959, 600, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(1959.1, 600, 1961, 600, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(1961.1, 600, 1962.3, 600, code = 3, length = 0.1,
       lwd = 1, angle = 30)

### FIT SEASONAL ARIMA AND AUTO ARIMA MODELS FOR ENTIRE DATA SET. 
## FORECAST AND PLOT DATA, AND MEASURE ACCURACY.


# Use arima() function to fit seasonal ARIMA(2,1,2)(1,1,2) model 
# for entire data set.
# use summary() to show auto ARIMA model and its parameters for entire data set.
arima.seas <- Arima(Passengers.ts, order = c(2,1,2), 
                    seasonal = c(1,1,2)) 
summary(arima.seas)

# Apply forecast() function to make predictions for ts with 
# seasonal ARIMA model for the future 12 periods. 
arima.seas.pred <- forecast(arima.seas, h = 12, level = 0)
arima.seas.pred

# Use Acf() function to create autocorrelation chart of seasonal ARIMA 
# model residuals.
Acf(arima.seas$residuals, lag.max = 12, 
    main = "Autocorrelations of Seasonal ARIMA (2,1,2)(1,1,2) Model Residuals")

# Plot historical data, predictions for historical data, and seasonal 
# ARIMA forecast for 12 future periods.
plot(arima.seas.pred, 
     xlab = "Time", ylab = "Passenger (in 90s)", 
     ylim = c(100, 630), xaxt = "n",
     bty = "l", xlim = c(1949, 1962.25), lwd = 2,
     main = "Seasonal ARIMA(2,1,2)(1,1,2)[12] Model for Training Data Set") 
axis(1, at = seq(1949, 1962, 1), labels = format(seq(1949, 1962, 1)))
lines(arima.seas$fitted, col = "blue", lwd = 2)
lines(arima.seas.pred$mean, col = "blue", lty = 5, lwd = 2)
legend(1950,400, legend = c("Passenger Data", 
                            "Auto ARIMA Forecast", 
                            "Auto ARIMA Forecast for 24 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")


# plot on the chart vertical lines and horizontal arrows
# describing training and future prediction intervals.
# lines(c(2004.25 - 3, 2004.25 - 3), c(0, 2600))
lines(c(1961, 1961), c(0, 630))
text(1955, 630, "Data Set")
text(1961.2, 630, "Future")
arrows(1949, 600, 1961, 600, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(1961.1, 600, 1962.3, 600, code = 3, length = 0.1,
       lwd = 1, angle = 30)

# Use auto.arima() function to fit ARIMA model for entire data set.
# use summary() to show auto ARIMA model and its parameters for entire data set.
auto.arima <- auto.arima(Passengers.ts)
summary(auto.arima)

# Apply forecast() function to make predictions for ts with 
# auto ARIMA model for the future 12 periods. 
auto.arima.pred <- forecast(auto.arima, h = 12, level = 0)
auto.arima.pred

# Use Acf() function to create autocorrelation chart of auto ARIMA 
# model residuals.
Acf(auto.arima$residuals, lag.max = 12, 
    main = "Autocorrelations of Auto ARIMA Model Residuals")

# Plot historical data, predictions for historical data, and seasonal 
# ARIMA forecast for 12 future periods.
plot(auto.arima.pred, 
     xlab = "Time", ylab = "Passenger (in 90s)", 
     ylim = c(100, 730), xaxt = "n",
     bty = "l", xlim = c(1949, 1964.25), lwd = 2,
     main = "Auto ARIMA Model for Entire Data Set") 
axis(1, at = seq(1949, 1962, 1), labels = format(seq(1949, 1962, 1)))
lines(auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(auto.arima.pred$mean, col = "blue", lty = 5, lwd = 2)
legend(1950,400, legend = c("Passenger Data", 
                            "Auto ARIMA Forecast", 
                            "Auto ARIMA Forecast for 24 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

# plot on the chart vertical lines and horizontal arrows
# describing training and future prediction intervals.
# lines(c(2004.25 - 3, 2004.25 - 3), c(0, 2600))
lines(c(1961, 1961), c(0, 680))
text(1955, 730, "Data Set")
text(1961.2, 730, "Future")
arrows(1949, 680, 1961, 680, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(1961.1, 680, 1962.3, 680, code = 3, length = 0.1,
       lwd = 1, angle = 30)




# MEASURE FORECAST ACCURACY FOR ENTIRE DATA SET.

# Use accuracy() function to identify common accuracy measures for:
# (1) Seasonal ARIMA (2,1,2)(1,1,2) Modelfor entire data set
# (2) Auto ARIMA Model,
# (3) Seasonal naive forecast, and
# (4) Naive forecast.
round(accuracy(arima.seas.pred$fitted, Passengers.ts), 3)#seasonal Arima
round(accuracy(auto.arima.pred$fitted, Passengers.ts), 3)#Auto arima
round(accuracy((snaive(Passengers.ts))$fitted, Passengers.ts), 3)#seasonal Naive
round(accuracy((naive(Passengers.ts))$fitted, Passengers.ts), 3)#Naive

#2nd Model
## HOLT-WINTER'S (HW) EXPONENTIAL SMOOTHING WITH PARTITIONED DATA, AUTOMATIC
## ERROR, TREND and SEASONALITY (ZZZ) OPTIONS, AND OPTIMAL PARAMETERS
## ALPHA, BETA, AND GAMMA.

# Create Holt-Winter's (HW) exponential smoothing for partitioned data.
# Use ets() function with model = "ZZZ", i.e., automatic selection of
# error, trend, and seasonality options.
# Use optimal alpha, beta, & gamma to fit HW over the training period.
hw.ZZZ <- ets(train.ts, model = "ZZZ")
hw.ZZZ # Model appears to be (M, N, M), with alpha = 0.5156 and gamma = 0.158.

# Use forecast() function to make predictions using this HW model with 
# validation period (nValid). 
# Show predictions in tabular format.
hw.ZZZ.pred <- forecast(hw.ZZZ, h = nValid, level = 0)
hw.ZZZ.pred


# Plot HW predictions for original data, with correct training and validation partitioning
plot(hw.ZZZ.pred$mean, 
     xlab = "Time", 
     ylab = "Passengers", 
     ylim = c(100, 700), 
     bty = "l", 
     xlim = c(1949, 1961), 
     xaxt = "n",
     main = "Holt-Winters Model: Training (1949-1958) vs Validation (1959-1960)",
     lty = 5, 
     col = "blue", 
     lwd = 2)
# Custom x-axis: years 1949 to 1960
axis(1, at = seq(1949, 1961, 1), labels = seq(1949, 1961, 1))
# Add fitted values (training period)
lines(hw.ZZZ.pred$fitted, col = "blue", lwd = 2)
# Add original data (black line)
lines(Passengers.ts, col = "black", lwd = 1)
legend(1949, 650, 
       legend = c("Actual Passengers", 
                  "HW Model (Training)", 
                  "HW Model (Validation Forecast)"), 
       col = c("black", "blue", "blue"), 
       lty = c(1, 1, 2), 
       lwd = c(1, 2, 2), 
       bty = "n")

# Vertical lines to separate training and validation
lines(c(1949, 1949), c(0, 700), lty = 3)     # Start of data
lines(c(1959, 1959), c(0, 700), lty = 3)     # Start of validation
lines(c(1961, 1961), c(0, 700), lty = 3)     # End of validation
text(1954, 680, "Training (1949-1958)")
text(1960, 680, "Validation (1959-1960)")
arrows(1949, 670, 1958.98, 670, code = 3, length = 0.1, lwd = 1, angle = 30)
arrows(1959.02, 670, 1960.98, 670, code = 3, length = 0.1, lwd = 1, angle = 30)



## FORECAST WITH HOLT-WINTER'S MODEL USING ENTIRE DATA SET INTO
## THE FUTURE FOR 12 PERIODS.

# Create Holt-Winter's (HW) exponential smoothing for full  data set. 
# Use ets() function with model = "ZZZ", to identify the best HW option
# and optimal alpha, beta, & gamma to fit HW for the entire data period.
HW.ZZZ <- ets(Passengers.ts, model = "ZZZ")
HW.ZZZ # Model appears to be (M, Ad, M), with alpha = 0.5334, beta = 0.0014,
# gamma = 0.1441, and phi = 0.9698.

# Use forecast() function to make predictions using this HW model for
# 12 month into the future.
HW.ZZZ.pred <- forecast(HW.ZZZ, h = 12 , level = 0)
HW.ZZZ.pred

# Plot Holt-Winter's Automated Model for Entire Data Set and Forecast for Future 12 Periods
plot(HW.ZZZ.pred$mean, 
     xlab = "Time", ylab = "Passengers", 
     ylim = c(min(Passengers.ts) * 0.9, max(Passengers.ts, HW.ZZZ.pred$mean) * 1.1), 
     bty = "l", 
     xlim = c(start(Passengers.ts)[1], end(HW.ZZZ.pred$mean)[1] + 0.25), 
     xaxt = "n",
     main = "Holt-Winter's Automated Model for Entire Data Set and Forecast for Future 12 Periods", 
     lty = 2, col = "blue", lwd = 2)

years <- seq(start(Passengers.ts)[1], end(HW.ZZZ.pred$mean)[1] + 1, 1)
axis(1, at = years, labels = years)
# Add the fitted values from the model (solid blue line)
lines(HW.ZZZ.pred$fitted, col = "blue", lwd = 2)
# Add the original data (black line)
lines(Passengers.ts, col = "black", lwd = 1)
# Add a vertical line to separate historical data from forecasts
forecast_start <- end(Passengers.ts)[1] + (end(Passengers.ts)[2])/12
lines(c(forecast_start, forecast_start), 
      c(par("usr")[3], par("usr")[4]),  # Use the actual plot limits
      lty = 1, lwd = 1)

# Calculate y-positions for text and arrows (near the top of the plot)
y_max <- par("usr")[4]
y_min <- par("usr")[3]
y_text <- y_max - (y_max - y_min) * 0.05
y_arrow <- y_max - (y_max - y_min) * 0.1
# Add text labels for Data Set and Future
text(mean(c(start(Passengers.ts)[1], forecast_start)), y_text, "Data Set")
text(mean(c(forecast_start, end(HW.ZZZ.pred$mean)[1])), y_text, "Future")
# Add bidirectional arrows
arrows(start(Passengers.ts)[1], y_arrow, 
       forecast_start - 0.01, y_arrow, 
       code = 3, length = 0.1, lwd = 1, angle = 30)
arrows(forecast_start + 0.01, y_arrow, 
       end(HW.ZZZ.pred$mean)[1], y_arrow, 
       code = 3, length = 0.1, lwd = 1, angle = 30)
# Add legend
legend("topleft", 
       legend = c("Passengers", 
                  "Holt-Winter's Model for Entire Data Set", 
                  "Holt-Winter's Model Forecast, Future 12 Periods"), 
       col = c("black", "blue", "blue"), 
       lty = c(1, 1, 2), lwd = c(1, 2, 2), bty = "n")

# Identify performance measure for HW forecast.
round(accuracy(HW.ZZZ.pred$fitted, Passengers.ts), 3)

#compare Accuracies for Auto Arima and Holts-Winter Model
# MEASURE FORECAST ACCURACY FOR ENTIRE DATA SET.

# Use accuracy() function to identify common accuracy measures for:
# (1) Seasonal ARIMA (2,1,2)(1,1,2) Modelfor entire data set
# (2) Auto ARIMA Model,
#(3) HW Forecast
# (4) Seasonal naive forecast, and
# (5) Naive forecast.
round(accuracy(arima.seas.pred$fitted, Passengers.ts), 3)#seasonal Arima
round(accuracy(auto.arima.pred$fitted, Passengers.ts), 3)#Auto arima
round(accuracy(HW.ZZZ.pred$fitted, Passengers.ts), 3)#HW Model
round(accuracy((snaive(Passengers.ts))$fitted, Passengers.ts), 3)#seasonal Naive
round(accuracy((naive(Passengers.ts))$fitted, Passengers.ts), 3)#Naive




