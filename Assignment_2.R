### Paul Murrell's R examples (selected)

## Start plotting from basics 
# Note the order
plot(pressure, pch=5)  # Can you change pch? Yes, changing to 5 caused the points to change shape
text(150, 600, 
     "Pressure (mm Hg)\nversus\nTemperature (Celsius)")

#  Examples of standard high-level plots 
#  In each case, extra output is also added using low-level 
#  plotting functions.
# 

# Setting the parameter (3 rows by 2 cols)
par(mfrow=c(3, 2))

# Scatterplot
# Note the incremental additions

x <- c(0.5, 2, 4, 8, 12, 16)
y1 <- c(1, 1.3, 1.9, 3.4, 3.9, 4.8)
y2 <- c(4, .8, .5, .45, .4, .3)

# Setting label orientation, margins c(bottom, left, top, right) & text size
par(las=1, mar=c(4, 4, 2, 4), cex=.7) 
plot.new()
plot.window(range(x), c(0, 6))
lines(x, y1)
lines(x, y2)
points(x, y1, pch=16, cex=3) # Try different cex value? Changing to 3 makes the points larger
points(x, y2, pch=21, bg="white", cex=2)  # Different background color
par(col="gray50", fg="gray50", col.axis="gray50")
axis(1, at=seq(0, 16, 4)) # What is the first number standing for? # The 1 means we are altering the x-axis
axis(2, at=seq(0, 6, 2))
axis(4, at=seq(0, 6, 2))
box(bty="u")
mtext("Travel Time (s)", side=1, line=2, cex=0.8)
mtext("Responses per Travel", side=2, line=2, las=0, cex=0.8)
mtext("Responses per Second", side=4, line=2, las=0, cex=0.8)
text(4, 5, "Bird 131")
par(mar=c(5.1, 4.1, 4.1, 2.1), col="black", fg="black", col.axis="black")

# Histogram
# Random data
Y <- rnorm(50)
# Make sure no Y exceed [-3.5, 3.5]
Y[Y < -3.5 | Y > 3.5] <- NA # Selection/set range
x <- seq(-3.5, 3.5, .1)
dn <- dnorm(x)
par(mar=c(4.5, 4.1, 3.1, 0))
hist(Y, breaks=seq(-3.5, 3.5), ylim=c(0, 0.5), 
     col="gray80", freq=FALSE)
lines(x, dnorm(x), lwd=2)
par(mar=c(5.1, 4.1, 4.1, 2.1))

# Barplot
par(mar=c(2, 3.1, 2, 2.1)) 
midpts <- barplot(VADeaths, 
                  col=gray(0.1 + seq(1, 9, 2)/11), 
                  names=rep("", 4))
mtext(sub(" ", "\n", colnames(VADeaths)),
      at=midpts, side=1, line=0.5, cex=0.5)
text(rep(midpts, each=5), apply(VADeaths, 2, cumsum) - VADeaths/2,
     VADeaths, 
     col=rep(c("white", "black"), times=3:2), 
     cex=0.8)
par(mar=c(5.1, 4.1, 4.1, 2.1))  

# Boxplot
par(mar=c(3, 4.1, 2, 0))
boxplot(len ~ dose, data = ToothGrowth,
        boxwex = 0.25, at = 1:3 - 0.2,
        subset= supp == "VC", col="white",
        xlab="",
        ylab="tooth length", ylim=c(0,35))
mtext("Vitamin C dose (mg)", side=1, line=2.5, cex=0.8)
boxplot(len ~ dose, data = ToothGrowth, add = TRUE,
        boxwex = 0.25, at = 1:3 + 0.2,
        
        subset= supp == "OJ")
legend(1.5, 9, c("Ascorbic acid", "Orange juice"), 
       fill = c("white", "gray"), 
       bty="n")
par(mar=c(5.1, 4.1, 4.1, 2.1))

# Persp
x <- seq(-10, 10, length= 30)
y <- x
f <- function(x,y) { r <- sqrt(x^2+y^2); 10 * sin(r)/r }
z <- outer(x, y, f)
z[is.na(z)] <- 1
# 0.5 to include z axis label
par(mar=c(0, 0.5, 0, 0), lwd=0.5)
persp(x, y, z, theta = 30, phi = 30, 
      expand = 0.5)
par(mar=c(5.1, 4.1, 4.1, 2.1), lwd=1)

# Piechart
par(mar=c(0, 2, 1, 2), xpd=FALSE, cex=0.5)
pie.sales <- c(0.12, 0.3, 0.26, 0.16, 0.04, 0.12)
names(pie.sales) <- c("Blueberry", "Cherry",
                      "Apple", "Boston Cream", "Other", "Vanilla")
pie(pie.sales, col = gray(seq(0.3,1.0,length=6))) 

# Exercise: Can you generate these charts individually?  Try these functions 
# using another dataset. Be sure to work on the layout and margins

# Filler dataset made with chatgpt assistance
# Set seed for reproducibility
set.seed(123)

# Number of observations
n <- 100

# Generate filler data
filler_data <- data.frame(
  ID = 1:n,
  age = sample(18:65, n, replace = TRUE),
  income = round(runif(n, 30000, 100000), 0),
  gender = sample(c("Male", "Female", "Other"), n, replace = TRUE, prob = c(0.45, 0.45, 0.1))
)

# Setting parameters
par(las=1, mar=c(4, 4, 2, 4), cex=.7) 

# Scatterplot
plot.new()
plot.window(range(filler_data$age), range(filler_data$income))
lines(lowess(filler_data$age, filler_data$income), col="gray50")
points(filler_data$age, filler_data$income, pch=16, cex=1.5, col="blue")
points(filler_data$age, filler_data$income, pch=21, bg="white", cex=1.2)
axis(1, at=seq(age_min, age_max, 10)) # X-axis
axis(2, at=seq(income_min, income_max, 20000)) # Y-axis
axis(4, at=seq(income_min, income_max, 20000)) # Right Y-axis
box(bty="u")
mtext("Age (Years)", side=1, line=2, cex=0.8)
mtext("Income (USD)", side=2, line=2, las=0, cex=0.8)
mtext("Income (USD)", side=4, line=2, las=0, cex=0.8)
par(mar=c(5.1, 4.1, 4.1, 2.1), col="black", fg="black", col.axis="black")

# Histogram
# Set up the plot parameters
par(mar=c(4.5, 4.1, 3.1, 0))

# Define breaks using pretty() for better range coverage
breaks <- pretty(filler_data$income, n = 10)  # 10 breaks

# Create histogram for Income
hist(filler_data$income, 
     breaks=breaks,  # Use the defined breaks
     ylim=c(0, max(table(cut(filler_data$income, breaks=breaks))) * 1.1), 
     col="gray80", 
     freq=FALSE, 
     main="Income Distribution", 
     xlab="Income (USD)")

# Overlay normal distribution curve
x <- seq(min(filler_data$income), max(filler_data$income), length.out=100)
lines(x, dnorm(x, mean=mean(filler_data$income), sd=sd(filler_data$income)), lwd=2)

# Reset margins to default
par(mar=c(5.1, 4.1, 4.1, 2.1))

# Summarize the data: Average income by gender
income_summary <- aggregate(income ~ gender, data=filler_data, FUN=mean)

# Barplot
par(mar=c(2, 3.1, 2, 2.1)) 
midpts <- barplot(income_summary$income, 
                  col=gray(0.1 + seq(1, 3, 1)/4), 
                  names.arg=income_summary$gender, 
                  ylim=c(0, max(income_summary$income) * 1.1))

# Adding text on top of bars
text(midpts, income_summary$income, 
     round(income_summary$income, 0), 
     pos=3, cex=0.8, col="black")

# Reset margins to default
par(mar=c(5.1, 4.1, 4.1, 2.1))

# Boxplot
par(mar=c(3, 4.1, 2, 0))

# Create boxplot of income by gender
boxplot(income ~ gender, data=filler_data,
        boxwex = 0.25, col=c("white", "gray"), 
        xlab="", 
        ylab="Income (USD)", ylim=c(0, max(filler_data$income) * 1.1))

# Add labels for gender on the x-axis
mtext("Gender", side=1, line=2.5, cex=0.8)

# Adding a legend
legend("topright", legend=c("Female", "Male"), 
       fill=c("white", "gray"), 
       bty="n")

# Reset margins to default
par(mar=c(5.1, 4.1, 4.1, 2.1))

# Define age and income sequences for the grid
age_seq <- seq(min(filler_data$age), max(filler_data$age), length=30)
income_seq <- seq(min(filler_data$income), max(filler_data$income), length=30)

# Create a function for z values; in this case, let's use a simple density estimate
f <- function(x, y) {
  mean_income <- mean(filler_data$income[filler_data$age == x], na.rm = TRUE)
  return(ifelse(is.na(mean_income), 0, mean_income))
}

# Create a grid of z values using outer
z <- outer(age_seq, income_seq, Vectorize(f))

# Replace NA with a value (for better visualization)
z[is.na(z)] <- 0

# Set up the perspective plot
par(mar=c(0, 0.5, 0, 0), lwd=0.5)
persp(age_seq, income_seq, z, 
      theta = 30, 
      phi = 30, 
      expand = 0.5, 
      col="lightblue", 
      xlab="Age (Years)", 
      ylab="Income (USD)", 
      zlab="Mean Income")

# Reset margins to default
par(mar=c(5.1, 4.1, 4.1, 2.1), lwd=1)

# Summarize the data: Count of individuals by gender
gender_counts <- table(filler_data$gender)

# Create pie chart
par(mar=c(0, 0, 0, 0))  # No margins for a clean look
pie(gender_counts, 
    col=c("lightblue", "lightgreen"), 
    labels=paste(names(gender_counts), "\n", gender_counts, sep=""), 
    main="Gender Distribution in Filler Data")

# Reset margins to default
par(mar=c(5.1, 4.1, 4.1, 2.1))