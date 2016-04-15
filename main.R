
## Let us get started with a few Machine Learning Techniques. 
## We will work on the property sales data from New York City. The dataset can be found here:

# http://www1.nyc.gov/site/finance/taxes/property-rolling-sales-data.page

## Let us just consider Manhattan data for analyses

setwd("~/ML")


### Data Exploration:


# Before we begin applying crazy ML algorithms, it is imperative to explore the data at hand

# Load in and clean up the data. The data will be messy. Once you get it loaded in, conduct
# exploratory data analysis in order to find out where there are outliers or missing values, decide
# how you will treat them, make sure the dates are formatted correctly, making sure values you
# think are numerical are being treated as such, etc.

## a)

# Loading the raw data and exploring the data set at first to find the nature of every variable. 
# Summary function along with the raw data would be a good place to start. 
# Converting all date columns with as.Date and all numerical-like values to numerical. 
# Missing values must be analyzed carefully. 

# However, in a few columns, more htan 80% of the values are either missing or are '0'. 
# Thus we convert all of those values to 'NA'.

# We load in the data using file.choose() and load in the manhattan data.

man <- read.csv(file.choose(), header=TRUE, skip = 4, colClasses = c("NEIGHBORHOOD" = "character", "ADDRESS" = "character","APARTMENT.NUMBER" = "character","LAND.SQUARE.FEET" = "character","GROSS.SQUARE.FEET" = "character","TAX.CLASS.AT.TIME.OF.SALE" = "factor","SALE.PRICE" = "character"))
str(man)

# changing the format of sale date to date format

man$SALE.DATE <- as.Date(man$SALE.DATE, format = "%m/%d/%Y")

# changing the sale price to number format

man$SALE.PRICE <- gsub(",","",man$SALE.PRICE)
man$SALE.PRICE <- as.numeric(man$SALE.PRICE)

# changing the land square feet to number format

man$LAND.SQUARE.FEET <- gsub(",","",man$LAND.SQUARE.FEET)
man$LAND.SQUARE.FEET <- as.numeric(man$LAND.SQUARE.FEET)

# changing gross square feet to number format

man$GROSS.SQUARE.FEET <- gsub(",","",man$GROSS.SQUARE.FEET)
man$GROSS.SQUARE.FEET <- as.numeric(man$GROSS.SQUARE.FEET)

# changing neighbourhood to a factor type variable

man <- man[-which(man$NEIGHBORHOOD == "MANHATTAN-UNKNOWN        "),]
man$NEIGHBORHOOD <- as.factor(man$NEIGHBORHOOD)

# We are only concerened about Manhattan and therfore there is no need for the borough column.

man <- man[,-1]

# Ease.Ment has no non-zero values and therefore we can remove that column

summary(man$EASE.MENT)
man <- man[,-6]

# Find the class of all variables and verify.

str(man)

# We need to remove the NULL values by replacing them with N/A values
# Replacing blank cells in Apartment number with NA values

man$APARTMENT.NUMBER <- data.frame(gsub("            ",NA,man$APARTMENT.NUMBER, fixed = TRUE))
colnames(man[,8]) <- "APARTMENT.NUMBER"
man$APARTMENT.NUMBER <- as.character(man$APARTMENT.NUMBER)

# Replacing Land square feet missing values with NA

man$LAND.SQUARE.FEET[man$LAND.SQUARE.FEET == 0] <-NA

# Replacing gross square feet missing values with NA

man$GROSS.SQUARE.FEET[man$GROSS.SQUARE.FEET == 0] <-NA

# Replacing Year built missing values with NA

man$YEAR.BUILT[man$YEAR.BUILT == 0] <- NA

# We replace total units with 0 value as NA and only replace those values in residential and commercial units corresponding to NA total unit values

man$RESIDENTIAL.UNITS[man$TOTAL.UNITS == 0] <- NA
man$COMMERCIAL.UNITS[man$TOTAL.UNITS == 0] <- NA
man$TOTAL.UNITS[man$TOTAL.UNITS == 0] <- NA


## b)

# Once the data is in good shape. Conduct exploratory data analysis to make comparisons 
# (i)across neighborhoods, and 
# (ii) across time. 
# You can use descriptive statistics and/or visualizations

# Perform exploratory data analysis on the cleaned data.
# Taking unique values from neighborhood column from main dataset and calling summary function 
# on it to get an idea about the mean,median,mode and quantiles.
# I have also plotted the sale price vs sale date and the sale price corresponding 
# to the various neighborhoods as these two predictors are the most important.

neigh<-unique(man$NEIGHBORHOOD)
summary(man$SALE.PRICE[man$NEIGHBORHOOD==neigh[1]]) -> summ
summ
for(i in 1:38){
  plot(man$SALE.PRICE[man$NEIGHBORHOOD==neigh[i]],col=i, pch=20)}
plot(man$SALE.DATE,man$SALE.PRICE,col="steelblue", pch=20)



### Supervised Learning:




## a)

# We will analyze the sales data using regression with predictors we feel are relevant. 

# Implementing linear regression for Sale Price with predictors Gross square feet, 
# land square feet, neighborhood and building class category factors in man dataset. 
# Linear regression can disclose relationships between the listed features and the predictor i.e., Sale Price.

library(caret)
library(dplyr)
model_lm <- lm(SALE.PRICE ~ GROSS.SQUARE.FEET+LAND.SQUARE.FEET+ factor(NEIGHBORHOOD)*factor(BUILDING.CLASS.CATEGORY), data = man)



## b) Visualize the coefficients and fitted regression model.
 

plot(resid(model_lm))


## c) 

# We can predict the neighborhood using a k-nearest neighbors classifier. 
# Use a 10-fold cross validation, and report the error.
# All the values of GROSS SQUARE FEET,LAND SQUARE FEET and SALE PRICE 
# has been assigned to sales_data as we will be needing them for analysis.
# The fit variable can be plotted and therefore we can easily visualize and 
# report the 10-fold cross validation procedure visually and the accuracy 
# comes out to be very less and thus we find out the error.


sales_man <- man[which(man$GROSS.SQUARE.FEET > 0 & man$LAND.SQUARE.FEET >0&man$SALE.PRICE > 0),]
sales_man_1 <- sales_man[,c(1,9,14,13,15)]
control <- trainControl(method="repeatedcv",repeats = 10)      
fit <- train(NEIGHBORHOOD~ ., data = sales_man_1,trControl = control, method = "knn")                                       
plot(fit)


## d) 

# Plotting the 'fit' function and printing it helps us understand better. 
# We find that the accuracy is fairly low, i.e., below 50%
# Therefore, parameters surely need to be tuned to obtain better accuracy.


plot(fit)
print(fit) 




### Unsupervised Learning:




## a)

# We will perform a PCA on the data after scaling the variables to have standard deviation equal to one.

# Here we have used the prcomp function in order to perform the PCA analysis 
# on the sales_man_2 data which we will create now. We have taken the parameters 
# RESIDENTIAL UNITS,GROSS SQUARE FEET and LAND SQUARE FEET.
# We have to use na.action as na.omit because we want to omit the 'N/A' values.
# We have used the summary and predict functions to visualize our answers.

sales_man_2 <- scale(sales_man_1[,c(3,4,5)])
View(sales_man_2)
PCA = prcomp(~SALE.PRICE+GROSS.SQUARE.FEET+LAND.SQUARE.FEET,data=sales_man,na.action = na.omit,scale=T,center=T)
str(PCA)
predict(PCA)
summary(PCA)


## b)

# Plot the first three principal component score vectors in order to visualize the data. One way
# to do this is to generate one plot with the first two vectors as the x and y axes, and a second plot
# with the first and the third

# Using biplot function, PCA1 vs PCA2 and PCA1 vs PCA3 were plotted
# I have used blue and red colors for depicting the two plots to show their difference.

biplot(PCA,cex=1.5)

# now we plot the first and second vector

plot_pca1=as.matrix(sales_man_2 [,c(1:3)])%*%PCA$rot[,c(1,2)]
plot(plot_pca1,col="blue")

# now we plot the first and third vector

plot_pca2=as.matrix(sales_man_2[,c(1:3)])%*%PCA$rot[,c(1,3)]
points(plot_pca2,col="red")
