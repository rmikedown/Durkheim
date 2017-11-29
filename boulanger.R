# Setting up a Time Series with Control
suicide <- read.csv("suicide038.csv")

# Just the boulanger
bou <- suicide[suicide$Event == "Boulanger Agitation",]

# Time markers (these are the months of the year)
bou$time <- rep(seq(1:8), 3)

# Creating the anomie variable
bou$anomie <- c(rep(0, 8), rep(0, 3), 1, 1, rep(0,3), rep(0,8))

# Create a time variable for both the Agitation Year and the Control
bou$time <- c(seq(1:8), seq(1:8), seq(1:8))

# Create a level variable for both WV and the control group. 
# The intervention is at 4
bou$level <- rep(c(0,1,0,1,0,1), times = c(3, 5, 3, 5, 3, 5))

# Creating variable for the Boulanger year
bou$yr1889 <- rep(c(0,1,0), times = c(8, 8, 8))

# Creating a variable for trend (5 months each)
bou$trend <- c(rep(0,3), 1:5, rep(0,3), 1:5, rep(0,3), 1:5)

# Creating Boulanger-specific variables
bou$setime <- c(rep(0, 8), 1:8, rep(0,8))
bou$selevel <- c(rep(0,8), rep(0,3), rep(1, 5), rep(0,8))
bou$setrend <- c(rep(0,8), rep (0,3), 1:5, rep(0,8))

# Comparing only 1888 and 1889
bou2 <- bou[1:16,]

# Plot the time series for the Boulanger Agitation
plot(bou2$time[9:16], bou2$SP1M[9:16],
     ylab="Suicides Per Million Inhabitants",
     ylim=c(400, 1000),
     main="How the 1889 Election Influenced Suicide Rates in France",
     xlab="Month",
     type="l",
     col="red",
     xaxt="n")

# Add in the other year as a comparison
points(bou2$time[1:8], bou2$SP1M[1:8],
       type='l',
       col="blue")

# Some aesthetics
points(bou$time[17:24], bou$SP1M[17:24],
       type = 'l',
       col="darkgreen")

points(bou$time[17:24], bou$SP1M[17:24],
       col="darkgreen",
       pch=20)

points(bou$time[17:24], bou$SP1M[17:24],
       col="black",
       pch=21)

# Add x-axis year labels
axis(1, at=1:8, labels= bou2$Month[1:8])

# Add in the points for the figure
points(bou2$time[9:16], bou2$SP1M[9:16],
       col="black",
       pch=21)

points(bou2$time[1:8], bou2$SP1M[1:8],
       col="black",
       pch=21)

# For style
points(bou2$time[9:16], bou2$SP1M[9:16],
       col="red",
       pch=20)

points(bou2$time[1:8], bou2$SP1M[1:8],
       col="blue",
       pch=20)

points(bou3$time[9:16], bou3$SP1M[9:16],
       col="darkgreen",
       pch=20)

# Label the Chamber dissolving
abline(v= 4,lty=2)

# Label the Election
abline(v=5.5, lty=2)

# Add in a legend
legend(x = 1, y= 600, legend=c("1888","1889", "1890"),
       col=c("blue","red", "darkgreen"),pch=20)

# The Chamber is dissolved at the beginning of August; the excitement of the election 
# period begins at once and lasts to the end of September, the time of the elections.

# Add a box to show the higher intensity
rect(4, 0, 5, 5000, border = NA, col= '#00000011')

# Add a box to show the lighter intensity
rect(4, 0, 5.5, 5000, border = NA, col= '#00000011')

# Creating a model
model_ols <- lm(SP1M ~ yr1889 + time + trend + level + setime +
                   setrend + anomie, data = bou2)

# Durkheim says the excitement is over at the end of September
first <- bou[1:6,]
second <- bou[9:14,]
third <- bou[17:22,]

# Combine
an <- rbind(first, second, third)
an2 <- rbind(first, second)

# Clean up our work space
rm(first, second, third)

# And now let's construct a new model
model_ols2 <- lm(SP1M ~ yr1889 + time + trend + setime +
                   setrend + anomie + level, data = an2)

summary(model_ols2)

# First plot the raw data points for 1889
plot(an2$time[7:12], an2$SP1M[7:12],
     ylab="Suicides Per Million Inhabitants",
     ylim=c(400, 1000),
     main="How the 1889 Election Influenced Suicide Rates in France",
     xlab="Month",
     pch=20,
     col="red",
     xaxt="n")

# Add x-axis month labels
axis(1, at=1:6, labels= an2$Month[1:6])

# Label the chamber dissolving
abline(v=3.5,lty=2)

# Label the election
abline(v=5.5,lty=2)

# Add in the points for the control
points(an2$time[1:6], an2$SP1M[1:6],
       col="blue",
       pch=20)

points(an2$time[1:6], an2$SP1M[1:6],
       col="black",
       pch=21)

points(an2$time[7:12], an2$SP1M[7:12],
       col="black",
       pch=21)

# Plot the first line segment
lines(an2$time[7:10], fitted(model_ols2)[7:10], col="red",lwd=2)

# Plot the second line segment
lines(an2$time[10:12], fitted(model_ols2)[10:12], col="red",lwd=2)

# Add the counterfactual for the intervention group
segments(3, model_ols2$coef[1] + model_ols2$coef[3] * 3 + model_ols2$coef[2] +
           model_ols2$coef[5] * 3,
         6, model_ols2$coef[1] + model_ols2$coef[3] * 6 + model_ols2$coef[2] +
           model_ols2$coef[5] * 6,
         lty=2,col='red',lwd=2)

# Plot the first line segment for the control group
lines(an2$time[1:6], fitted(model_ols2)[1:6], col="blue",lwd=2)

# Add the counterfactual for the control group
segments(1, model_ols2$coef[1]+ model_ols2$coef[3], 6,
         model_ols2$coef[1] + model_ols2$coef[3] * 6,
         lty=2,col='blue',lwd=2)

# Add in a legend
legend(x=1, y=550, legend=c("1888","1889"), col=c("blue","red"),pch=20)

# Add a box to show the higher intensity
rect(3.5, 0, 5.5, 5000, border = NA, col= '#00000011')

# Add a box to show the lighter intensity
rect(4, 0, 5.5, 5000, border = NA, col= '#00000011')

# Predicted value 2 months after chambers dissolved
pred <- fitted(model_ols2)[11]

# Estimate the counterfactuals at 5 months (2 after chambers dissolved)
cfac <- model_ols2$coef[1] + model_ols2$coef[3] * 5 + model_ols2$coef[2] + 
  model_ols2$coef[5] * 5 + model_ols2$coef[4] * 2 + model_ols2$coef[8]

# Absolute change at 5 months
pred - cfac

# Relative change at 5 months
(pred-cfac) / cfac