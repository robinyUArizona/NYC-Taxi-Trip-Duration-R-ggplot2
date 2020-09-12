#### New York Taxi trip duration ####

#### Libraries or Packages #####################################
# install.packages('vcd')
# install.packages('ggplot2')
# install.packages('geosphere')
# install.packages('lubridate')
# install.packages('e1071')
# install.packages('ggcorrplot')
# install.packages('caret')
# install.packages('gridExtra')
# install.packages('dplyr') 
# install.packages('mice')
# install.packages('VIM')
# install.packages('Boruta')
# install.packages('mlbench')
# install.packages('caret')
# install.packages('randomForest')
# install.packages("varImp")
# install.packages("party") 
# install.packages("ROSE")


options(warn=-1)

## load packages ###############################################
library(vcd) # for distribution plot
library(ggplot2) # for visualisation
library(geosphere) # for distance
library(lubridate) # for date-time formatting
library(e1071) # for naive bayes
library(ggcorrplot) # for correlation matrix plot
library(caret) # for confusion matrix
library(gridExtra) # for grid for multiplot
library(dplyr) # for %>%
library(mice) # for md.pattern()
library(VIM) # for visulaization of missing value

library(varImp) # for feature selection
library(party) # for feature selection
library(Boruta) # for feature selection
library(mlbench)
library(randomForest) # for feature selection
library(ROSE) # for ROC curve  
library(neuralnet) # for neural network
library(C50) # for decision tree

library(repr) # for plot margin
options(repr.plot.width=4, repr.plot.height=3)



################################################################
train_data <- read.csv('train.csv')
cat('Dimension of Train Data: \n')
print(dim(train_data))
print(summary(train_data))

# ##### check duplicate rows in train_data #######
# cat('Duplicate rows in train_data: ')
# print(train_data[duplicated(train_data)])
# ##-- Comment: No duplicate rows

# #### Exploring missing value in train_data ####
# cat('\n Exploring missing value in train_data: \n')

# ## Check for missing value "NA"
# cat('Check for missing value: \n')
# print(sum(is.na(train_data)))
# cat('check missing value in each column: \n')
# print(colSums(is.na(train_data)))

# ## Distribution of missing value
# print(md.pattern(train_data))
# ##-- comment: No missing values

### train_data without missing rows -- complete case
train_data <- na.omit(train_data)

set.seed(123)
sample_size <- 50000 
sample_train_data <- train_data[sample(nrow(train_data), size=sample_size,
                            replace=FALSE), ]

## Save sample_train_data as 'sample_train.csv' file
write.csv(sample_train_data, "sample_train.csv", row.names = FALSE)

#####################################################################



## Sample_train.csv
new_train_data <- read.csv('sample_train.csv')
new_train_data <- as_data_frame(new_train_data)

cat('Dimension of Sample Train Data: \n')
print(dim(new_train_data))
cat('\n\nColumn Names: \n')
print(colnames(new_train_data))

# Summary of data
cat('Summary of New Train Data: \n')
print(summary(new_train_data))
cat('\n\nOriginal Data Format: \n')
print(str(new_train_data))

head(new_train_data)

sapply(new_train_data, class)

##### check duplicate rows in sample_train_data #######
# cat('Duplicate rows in train_data: \n')
# print(new_train_data[duplicated(new_train_data)])

#-- Comment: No duplicate rows

#### Exploring missing value in train_data ####
cat('\nExploring missing value in train_data: \n')

## Check for missing value "NA"
cat('Check for missing value: \n')
print(sum(is.na(new_train_data)))
cat('check missing value in each column: \n')
print(colSums(is.na(new_train_data)))

# Visualization of missing value
#  call function aggr (), prop = FALSE convert percentage value into counts
# Function aggr() returns a bar-chart and a heat-map showing the distribution of missing value
options(repr.plot.width = 4, repr.plot.height = 3)
print(aggr(new_train_data, prop = FALSE, numbers = TRUE))

##-- comment: No missing values


### train_data without missing rows -- complete case
## no need for this step
new_train_data <- na.omit(new_train_data)


# Function aggr() returns a bar-chart and a heat-map showing the distribution of missing value
options(repr.plot.width = 7, repr.plot.height = 5)
print(aggr(new_train_data, prop = FALSE, numbers = TRUE))

## formatting target variable 'trip_duration'
# Dr. Zhang suggestion comment, "Since we mainly use classification-
# methods, I suggest you convert your Ride Duration to Long/Short."

############## Summary for target variable "trip_duration"
summary_trip_duration <- summary(new_train_data$trip_duration)
print(summary_trip_duration)
median_trip_duration <- summary_trip_duration['Median']
max_trip_duration <- summary_trip_duration['Max.']
cat('Median of trip_duration in Seconds: \n')
print(median_trip_duration)

#### Some plots to decide where to cutoff for short and long -- 'trip_duration' ####
# distribution plot
options(repr.plot.width = 4, repr.plot.height = 4)
distplot(new_train_data$trip_duration)

# density plot
options(repr.plot.width = 4, repr.plot.height = 3)
dens_dist <- ggplot(new_train_data, aes(x=trip_duration)) +
  geom_density() + 
  ggtitle("Target Variable - Trip Duration Analysis") +
  xlab("Trip Duration") +
  theme(legend.position="none")
dens_dist

# histogram 
options(repr.plot.width = 4, repr.plot.height = 4)
new_train_data %>%
    sample_frac(size = 0.05) %>%
    ggplot() +
    geom_histogram(aes(trip_duration), binwidth = 5) +
    ggtitle("Target Variable - Trip Duration Analysis") +
    xlab("Trip Duration") +
    theme(legend.position="none")

# boxplot
options(repr.plot.width = 4, repr.plot.height = 3)
box_dist <- ggplot(data = new_train_data, aes(x=trip_duration)) +
               geom_boxplot() +
               ggtitle("Target Variable - Trip Duration Analysis") +
               xlab("Trip Duration") +
               theme(legend.position="none")
box_dist

#### Regression problem to Classification problem #####################
cat('\n Changing regression problem to classification problem: \n')
# Assuming trip duration more than 30 minutes is long trip --> 1800 seconds
new_train_data$trip_duration <- cut(new_train_data$trip_duration,
                                    breaks=c(0, 1800, max_trip_duration),
                                    labels=c("Short Trip","Long Trip"))

# changing target variable to factor
new_train_data$trip_duration <- as.factor(new_train_data$trip_duration)


## Analysis of target variable 
# 'trip_duration - short and long trip' feature
## count the number of "Short Trip" and "Long Trip"
print(summary(new_train_data$trip_duration))

# print(str(new_train_data))

trip <- new_train_data %>%
  group_by(trip_duration) %>%
  count() 

options(repr.plot.width = 3, repr.plot.height = 2.5)
trip %>% ggplot(aes(x=factor(trip_duration), y=n))+
  geom_col(aes(fill = factor(trip_duration))) +
  ggtitle("Trip Types") +
  xlab("Trip Types") +
  ylab("No. of Passenger") +
  theme(legend.position = "none")

## baseline accuracy
# Assuming my model will classify all of my observations according to majority class
num_short_trip <- summary(new_train_data$trip_duration)['Short Trip']
num_short_trip
num_long_trip <- summary(new_train_data$trip_duration)['Long Trip']
num_long_trip

base_accuracy <- (num_short_trip / (num_short_trip + num_long_trip)) * 100
cat('Base-line accuracy based on majority class: ')
print(base_accuracy)



### Lets check dataset
head(new_train_data)
print(str(new_train_data))
print(summary(new_train_data))



# 'vender_id' feature
vendorID <- new_train_data %>%
  group_by(vendor_id) %>%
  count() 

options(repr.plot.width = 3, repr.plot.height = 2.5)
vendorID %>% ggplot(aes(x=factor(vendor_id), y=n))+
  geom_col(aes(fill = factor(vendor_id))) +
  ggtitle("Vendor ID Feature") +
  xlab("vendor_id") +
  ylab("No. of Observation") +
  theme(legend.position = "none")

#### Formatting Features: #####################################################
##  “store_and_fwd_flag” variable to numeric, from character.
new_train_data$store_and_fwd_flag <- as.numeric(as.factor(new_train_data$store_and_fwd_flag))
# print(str(new_train_data))

# 'store_and_fwd_flag' feature
store_flag <- new_train_data %>%
  group_by(store_and_fwd_flag) %>%
  count()

options(repr.plot.width = 3, repr.plot.height = 2.5)
store_flag %>% ggplot(aes(x=factor(store_and_fwd_flag), y=n))+
  geom_col(aes(fill = factor(store_and_fwd_flag))) +
  ggtitle("Store and Forward Feature") +
  xlab("store_and_fwd_flag") +
  ylab("No. of Observation") +
  theme(legend.position = "none")


# 'passenger_count' feature
passeng_count <- new_train_data %>%
  group_by(passenger_count) %>%
  count() 

options(repr.plot.width = 3, repr.plot.height = 2.5)
passeng_count %>% ggplot(aes(x=passenger_count, y=n))+
  geom_col(aes(fill = factor(passenger_count)))+
  ggtitle("Passenger Count in Each Pick Up") +
  xlab("passenger_count") +
  ylab("No. of Pick Up") +
  theme(legend.position = "none")





###### Distance ####################################################
## 'Distance Travelled during each Trip' using
# 'pickup_latitude', 'pickup_longitude', 'dropoff_latitude', 'dropoff_longitude' features

i  <- cbind(pick_longitude = new_train_data$pickup_longitude,
            pick_latitude = new_train_data$pickup_latitude)
j <- cbind(drop_longitude = new_train_data$dropoff_longitude,
           drop_latitude = new_train_data$dropoff_latitude)

## computing haversine distance from co-ordinates
# distance column is added at the end
new_train_data$distance <- distHaversine(i, j)

# 'pickup_latitude', pickup_longitude', dropoff_latitude', 'dropoff_longitude' features
options(repr.plot.width = 6, repr.plot.height = 5)
p1 <- new_train_data %>%
  filter(pickup_latitude > min(new_train_data$pickup_latitude) &
          pickup_latitude < max(new_train_data$pickup_latitude)) %>%
  ggplot(aes(x=pickup_latitude)) +
  geom_histogram(bins=40)

p2 <- new_train_data %>%
  filter(pickup_longitude > min(new_train_data$pickup_longitude) &
           pickup_longitude < max(new_train_data$pickup_longitude)) %>%
  ggplot(aes(x=pickup_longitude)) +
  geom_histogram(bins=40)

p3 <- new_train_data %>%
  filter(dropoff_latitude > min(new_train_data$dropoff_latitude) &
           dropoff_latitude < max(new_train_data$dropoff_latitude)) %>%
  ggplot(aes(x=dropoff_latitude)) +
  geom_histogram(bins=40)

p4 <- new_train_data %>%
  filter(dropoff_longitude > min(new_train_data$dropoff_longitude) &
           dropoff_longitude < max(new_train_data$dropoff_longitude)) %>%
  ggplot(aes(x=dropoff_longitude)) +
  geom_histogram(bins=40)

grid.arrange(p1, p2, p3, p4)





## 'pickup_datetime' and 'dropoff_datetime'
# convert 'Factor' into 'data/time' data type
new_train_data$pickup_datetime <- ymd_hms(new_train_data$pickup_datetime)
new_train_data$dropoff_datetime <- ymd_hms(new_train_data$dropoff_datetime)

new_train_data$pickup_month <- month(new_train_data$pickup_datetime)
new_train_data$pickup_day <- day(new_train_data$pickup_datetime)
new_train_data$pickup_hour <- hour(new_train_data$pickup_datetime)
new_train_data$pickup_minutes <- minute(new_train_data$pickup_datetime)


new_train_data$pickup_week <- week(new_train_data$pickup_datetime)
new_train_data$pickup_weekdays <- wday(new_train_data$pickup_datetime)
new_train_data$pickup_weekend <- ifelse(new_train_data$pickup_weekdays==6 | new_train_data$pickup_weekdays==7,
                               "Weekend","not-Weekend")

new_train_data$pickup_weekend <- as.factor(new_train_data$pickup_weekend)
print(str(new_train_data))
print(summary(new_train_data$pickup_weekend))


head(new_train_data)

# install.packages('RColorBrewer')
library(RColorBrewer)

# 'Number of Pickup' in 24-Hours
num_pickup_24 <- new_train_data %>%
  group_by(pickup_hour) %>%
  count()

options(repr.plot.width = 3, repr.plot.height = 2.5)
num_pickup_24 %>%
  ggplot(aes(x=pickup_hour, y=n)) +
  geom_col(aes(fill = factor(pickup_hour))) +
  ggtitle("No. of Pick Up During 24-Hours") +
  xlab("Hours of a Day") +
  ylab("No. of Pickups") +
  theme(legend.position = "none") +
  scale_color_brewer(palette="Dark1")


##### 'pickup_datetime' feature
num_pickup_24hours_day <- new_train_data %>%
  group_by(pickup_hour, pickup_weekdays) %>%
  count()

options(repr.plot.width = 6, repr.plot.height = 3)
num_pickup_24hours_day %>% ggplot(aes(x=pickup_hour, y=n, color= pickup_weekdays)) +
  geom_point(size=1, aes(color = factor(pickup_weekdays))) +
  ggtitle("Pick Up Hours by Day") +
  xlab("Hours of a Day") +
  ylab("No. of pick up") +
  scale_color_brewer(palette="Dark2")

## Sunday is '1'

num_pickup_24hours_month <- new_train_data %>%
  group_by(pickup_hour, pickup_month) %>%
  count()

options(repr.plot.width = 6, repr.plot.height = 3)
num_pickup_24hours_month %>% ggplot(aes(x=pickup_hour, y=n, color=pickup_month)) +
  geom_point(size=1, aes(color = factor(pickup_month))) +
  ggtitle("Pick Up Hours by Month") +
  xlab("Hours of a Day") +
  ylab("No. of pick up") + 
  scale_color_brewer(palette="Dark2")


## Realtionship between distance vs. passenger count
# This is important--> By intuition distance should increase with passenger count

ggplot(new_train_data, aes(distance, passenger_count)) + 
geom_col(aes(fill = factor(passenger_count))) +
ggtitle("No. of Pick Up During 24-Hours") +
  xlab("Distance in meters") +
  ylab("Passenger Count") +
  theme(legend.position = "none") 



## Visualization after changing time format "pickup_datetime", "dropoff_datetime"

## Relationship between "pickup_datetime", "dropoff_datetime" with 'trip_duration'
options(repr.plot.width = 6, repr.plot.height = 5)
b1 <- ggplot(data = new_train_data,
             aes(x=trip_duration, y=pickup_hour,
                 fill=trip_duration)) +
  geom_boxplot() +
  ggtitle("Trip Duration for 24-Hours") +
  xlab("Trip Duration Class") +
  ylab("Hours of the Day") +
  theme(legend.position="none")

b2 <- ggplot(data = new_train_data,
             aes(x=trip_duration, y=pickup_day,
                 fill=trip_duration)) +
  geom_boxplot() +
  ggtitle("Trip Duration for Day") +
  xlab("Trip Duration Class") +
  ylab("Day") +
  theme(legend.position="none")

b3 <- ggplot(data = new_train_data,
             aes(x=trip_duration, y=pickup_month,
                 fill=trip_duration)) +
  geom_boxplot() +
  ggtitle("Trip Duration for Month") +
  xlab("Trip Duration Class") +
  ylab("6-Months") +
  theme(legend.position="none")

grid.arrange(b1, b2, b3, ncol=2)

head(new_train_data)

colnames(new_train_data)

## Lets select attriutes which is important
##### Cleaning Data ############
## Selecting columns or attributes which does not contribute to model learning

## removing "id", "pickup_datetime", "dropoff_datetime", "pickup_longitude", "pickup_latitude", 
## "dropoff_latitude", "dropoff_longitude", "store_and_fwd_flag"
train_data <- new_train_data[, -which(names(new_train_data) 
                                          %in% c("id", "pickup_datetime", "dropoff_datetime", 
                                                 "pickup_longitude", "pickup_latitude", 
                                                 "dropoff_latitude", "dropoff_longitude", 
                                                 "store_and_fwd_flag", "pickup_weekend")) ]

cat("\n After Removal of irrelevant attributes: \n")
print(colnames(train_data))
head(train_data)



### Correlation Matrix & Correlation Plot####
print(str(train_data))

corr_data <- train_data[, which(names(train_data)
                                    %in% c("vendor_id", "passenger_count", "distance",
                                           "pickup_month", "pickup_day", "pickup_hour", 
                                           "pickup_minutes", "pickup_week", "pickup_weekdays"))]

corr_matrix <- round(cor(corr_data), 4)
# print(corr_matrix)

options(repr.plot.width = 6, repr.plot.height = 4)
ggcorrplot(corr_matrix, hc.order = TRUE, type = "lower", lab = TRUE)

# 70 % train_data
# set.seed(123)
sample_size <- floor(0.7 * nrow(train_data))

# randomly select index of observations for training
# training_index <- sample(nrow(train_data), size=sample_size, replace=FALSE)
# train <- train_data[training_index, ]
# print(head(train))
# test <- train_data[-training_index, ]
# print(head(test))

# install.packages('caret')

library(caret)
train.index <- createDataPartition(train_data$trip_duration, p = .7, list = FALSE)
train <- train_data[ train.index,]
test  <- train_data[-train.index,]



print(table(train$trip_duration))
print(table(test$trip_duration))
print(dim(train))
print(dim(test))



## Analyze the distance feature
# boxplot
options(repr.plot.width = 4, repr.plot.height = 3)
box_distance <- ggplot(data = train, aes(x=distance)) +
               geom_boxplot() +
               ggtitle("Distance Analysis") +
               xlab("Distance") +
               theme(legend.position="none")
box_distance 

sum_distance <- summary(train$distance)
third_Q <- sum_distance['3rd Qu.']
print(third_Q)
bench <- third_Q + (1.5* IQR(train$distance))
print(bench)



remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 2 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}
y1 <- remove_outliers(train$distance)
y2 <- remove_outliers(test$distance)

## png()
par(mfrow = c(1, 2))

options(repr.plot.width = 6, repr.plot.height = 3)
box_dis1 <- ggplot(data = train, aes(x=distance)) +
               geom_boxplot() +
               ggtitle("Distance Before") +
               xlab("Distance in meters") +
               theme(legend.position="none")

box_dis2 <- ggplot(data = train, aes(x=y1)) +
               geom_boxplot() +
               ggtitle("Distance After") +
               xlab("Distance in meters") +
               theme(legend.position="none")


grid.arrange(box_dis1, box_dis2, ncol=2)

## dev.off()


train$distance <- y1
test$distance <- y2

## remove NA from both train and test
# ## Check for missing value "NA"
cat('Check for missing value: \n')
print(sum(is.na(train)))
print(sum(is.na(test)))

# we can retain observation thta do not conatain NA(null) value
train <- na.omit(train)
print(head(train))
test <- na.omit(test)
print(head(test))

head(train)
print(dim(train))







train <- train %>% mutate_if(is.numeric, scale)
test <- test %>% mutate_if(is.numeric, scale)
# train <- train %>% mutate_at(c('distance'), funs(c(scale(.))))
# test <- test %>% mutate_at(c('distance'), funs(c(scale(.))))

# check after scaling numeric attributes
head(train)
head(test)





# load the random forest package
# fit the random forest
cf <- cforest(as.factor(trip_duration) ~ . , data= train, 
              control=cforest_unbiased(mtry=2,ntree=50)) 
# more robust towards class imbalance.
varimpAUC(cf) 

## Feature selection - Boruta Algorithm

boruta <- Boruta(trip_duration ~., data=train, doTrace=2, maxRuns = 12)
print(attStats(boruta))

options(repr.plot.width = 7, repr.plot.height = 5)
# print(boruta)
plot(boruta, las=2, cex=1)



## Save clean file 
write.csv(train,"clean_train.csv",row.names = FALSE)
write.csv(test,"clean_test.csv",row.names = FALSE)



## Training Naive Bayes using Train dataset
model <- naiveBayes(as.factor(trip_duration) ~. , data=train)

# Predicting "trip types" on test dataset using trained model
y_predict <- predict(model, test, type = 'class')
print(head(y_predict))

## Confusion matrix
confusionMatrix(y_predict, test$trip_duration, positive = 'Short Trip')

## plot size settings
options(repr.plot.width = 5, repr.plot.height = 5)

## ROC curves - Relationship between Sensitivity and Specificity
roc.curve(test$trip_duration, y_predict, plotit=T)


# Sensitivity = (TP)/(TP+FN) =  43/(43+2625)
# Specificity = (TN)/(TN+FP) = 20/(20+81)

##                      Actual
#                    Short  Long
# Predicted Short    TP     FP
#           Long     FN     TN

# Precision = (TP)/(TP+FP)
# Recall = (TP)/(TP+FN)



6490*2

32334*2


print(table(train$trip_duration))
## oversampling method
over <- ovun.sample(trip_duration ~., data = train, method = "over", N=64668)$data
print(table(over$trip_duration))

# model training
model_over <- naiveBayes(as.factor(trip_duration) ~. , data=over)

# prediction
y_predict_over <- predict(model_over, test, type = 'class')
print(head(y_predict_over))

## Confusion matrix
confusionMatrix(y_predict_over, test$trip_duration, positive = 'Short Trip')

# ROC curve
options(repr.plot.width = 5, repr.plot.height = 5)
roc.curve(test$trip_duration, y_predict_over, plotit=T)

2667*2

print(table(train$trip_duration))
under <- ovun.sample(trip_duration ~., data = train, method = "under", N=5334)$data
print(table(under$trip_duration))


model_under <- naiveBayes(as.factor(trip_duration) ~. , data=under)
# print(model)

# prediction
y_predict_under <- predict(model_under, test, type = 'class')
print(head(y_predict_under))

## Confusion matrix
confusionMatrix(y_predict_under, test$trip_duration, positive = 'Short Trip')

# ROC curve
options(repr.plot.width = 5, repr.plot.height = 5)
roc.curve(test$trip_duration, y_predict_under, plotit=T)

32334+2667

print(table(train$trip_duration))
both <- ovun.sample(trip_duration ~., data = train, method = "both",
                    seed = 222,
                    N=35001)$data
print(table(both$trip_duration))

# model training
model_both <- naiveBayes(as.factor(trip_duration) ~. , data=both)
# print(model)

# prediction
y_predict_both <- predict(model_both, test, type = 'class')
print(head(y_predict_both))

## Confusion matrix
confusionMatrix(y_predict_both, test$trip_duration, positive = 'Short Trip')

# ROC curve
options(repr.plot.width = 5, repr.plot.height = 5)
roc.curve(test$trip_duration, y_predict_both, plotit=T)

# predictors -- independent variables
predictors <- train[, -which(names(train) %in% c('trip_duration')) ]

# Predicting "trip types" on test dataset using trained model
dt_model <- C5.0.default(x = predictors, y = train$trip_duration)

# prediction 
y_predict_dt <- predict(dt_model, test, type = 'class')
# print(head(y_predict_dt))

## Confusion matrix
confusionMatrix(y_predict_dt, test$trip_duration, positive = 'Short Trip')

# ROC curve
options(repr.plot.width = 5, repr.plot.height = 5)
roc.curve(test$trip_duration, y_predict_dt, plotit=T)

# predictors -- independent variables

# over
predictors_over <- over[, -which(names(over) %in% c('trip_duration')) ]

# Predicting "trip types" on test dataset using trained model
dt_model_over <- C5.0.default(x = predictors_over, y = over$trip_duration)

# prediction 
y_predict_dt_over <- predict(dt_model_over, test, type = 'class')
print(head(y_predict_dt_over))

## Confusion matrix
confusionMatrix(y_predict_dt_over, test$trip_duration, positive = 'Short Trip')

# ROC curve
options(repr.plot.width = 5, repr.plot.height = 5)
roc.curve(test$trip_duration, y_predict_dt_over, plotit=T)


# predictors -- independent variables

# down
predictors_under <- under[, -which(names(under) %in% c('trip_duration')) ]

# Predicting "trip types" on test dataset using trained model
dt_model_under <- C5.0.default(x = predictors_under, y = under$trip_duration)

# prediction 
y_predict_dt_under <- predict(dt_model_under, test, type = 'class')
print(head(y_predict_dt_under))

## Confusion matrix
confusionMatrix(y_predict_dt_under, test$trip_duration, positive = 'Short Trip')

# ROC curve
options(repr.plot.width = 5, repr.plot.height = 5)
roc.curve(test$trip_duration, y_predict_dt_under, plotit=T)

# predictors -- independent variables

# both
predictors_both <- both[, -which(names(both) %in% c('trip_duration')) ]

# Predicting "trip types" on test dataset using trained model
dt_model_both <- C5.0.default(x = predictors_both, y = both$trip_duration)

# prediction 
y_predict_dt_both <- predict(dt_model_both, test, type = 'class')
print(head(y_predict_dt_both))

## Confusion matrix
confusionMatrix(y_predict_dt_both, test$trip_duration, positive = 'Short Trip')

# ROC curve
options(repr.plot.width = 5, repr.plot.height = 5)
roc.curve(test$trip_duration, y_predict_dt_both, plotit=T)






# # Features creation
# features <- names(train[, -which(names(train) %in% c('trip_duration'))])
# features

# f <- paste(features, collapse=' + ')
# f <- paste('trip_duration ~', f)
# f

# # Convert to formula
# f <- as.formula(f)
# f

# nn <- neuralnet(f, train, hidden = 5, linear.output = FALSE )


# # install.packages('plyr')
# library(plyr)

# # Compute Predictions off Test Set
# predicted <- compute(nn, test[,-3])

# # Check out net.result
# # head(predicted$net.result)

# predicted$net.result <- sapply(predicted$net.result, round, digits = 0)
# # head(predicted$net.result)

# evaluation <- data.frame(test$trip_duration, predicted$net.result)
# # head(evaluation)
# colnames(evaluation) <- c("actual", "predict")
# head(evaluation)

# evaluation$predict <- mapvalues(evaluation$predict, c(0, 1), c("Short Trip", "Long Trip"))
# evaluation$predict <- as.factor(evaluation$predict)
# head(evaluation)
# table(evaluation$actual, evaluation$predict)

# # Confusion matrix
# confusionMatrix(evaluation$predict, evaluation$actual, positive = 'Short Trip')

# # # ROC curve
# # options(repr.plot.width = 5, repr.plot.height = 5)
# # roc.curve(test$trip_duration, predicted$net.result, plotit=T)



# # Features creation
# features_over <- names(over[, -which(names(over) %in% c('trip_duration'))])
# features_over

# f_over <- paste(features_over, collapse=' + ')
# f_over <- paste('trip_duration ~', f_over)
# f_over

# # Convert to formula
# f_over <- as.formula(f_over)
# f_over

# nn <- neuralnet(f_over, over, hidden = 5, linear.output = FALSE )







set.seed(123)

library(e1071)




## Build model – linear kernel and C-classification (soft margin) 
# with default cost (C=1), all default settings.
svm_model <- svm(trip_duration ~ ., data=train, method="C-classification", 
                 kernel="radial")

# prediction 
y_predict_svm <- predict(svm_model, test, type = 'class')
print(head(y_predict_svm))

## Confusion matrix
confusionMatrix(y_predict_svm, test$trip_duration, positive = 'Short Trip')

# ROC curve
options(repr.plot.width = 5, repr.plot.height = 5)
roc.curve(test$trip_duration, y_predict_svm, plotit=T)

## Build model – linear kernel and C-classification (soft margin) 
# with default cost (C=1), all default settings.
svm_model_over <- svm(trip_duration ~ ., data=over, method="C-classification", 
                      kernel="radial")

# prediction 
y_predict_svm_over <- predict(svm_model_over, test, type = 'class')
print(head(y_predict_svm_over))

## Confusion matrix
confusionMatrix(y_predict_svm_over, test$trip_duration, positive = 'Short Trip')

# ROC curve
options(repr.plot.width = 5, repr.plot.height = 5)
roc.curve(test$trip_duration, y_predict_svm_over, plotit=T)

## Build model – linear kernel and C-classification (soft margin) 
# with default cost (C=1), all default settings.
svm_model_under <- svm(trip_duration ~ ., data=under, method="C-classification", 
                      kernel="radial")

# prediction 
y_predict_svm_under <- predict(svm_model_under, test, type = 'class')
print(head(y_predict_svm_under))

## Confusion matrix
confusionMatrix(y_predict_svm_under, test$trip_duration, positive = 'Short Trip')

# ROC curve
options(repr.plot.width = 5, repr.plot.height = 5)
roc.curve(test$trip_duration, y_predict_svm_under, plotit=T)

## Build model – linear kernel and C-classification (soft margin) 
# with default cost (C=1), all default settings.
svm_model_both <- svm(trip_duration ~ ., data=both, method="C-classification", 
                      kernel="radial")

# prediction 
y_predict_svm_both <- predict(svm_model_both, test, type = 'class')
print(head(y_predict_svm_both))

## Confusion matrix
confusionMatrix(y_predict_svm_both, test$trip_duration, positive = 'Short Trip')

# ROC curve
options(repr.plot.width = 5, repr.plot.height = 5)
roc.curve(test$trip_duration, y_predict_svm_both, plotit=T)












