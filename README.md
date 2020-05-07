# NYC-Taxi-Trip-Duration-R
![](NYC-Taxi-Trip-Duration-Pic.PNG)

The New York Times reported on May 22, 2019 that the NYC Yellow Taxi organization revenue has decreased by about 10 percent per cab since Uber entered New York in 2011. The NYC Yellow Taxi minimum base fare is $5.80 while Uber minimum base fare is $10.75.  And it is found that the Uber is cheap after fare amount $35, which depends on the distance travelled, pickup time, geo-coordinates, number of passengers, and several other variables.

# File descriptions
**NYC-Taxi Trip Duration Analysis and Predictive Modelling in R.ipynb** - Explored and feature engineered the NYC Taxi Trip Duration dataset and built a predictive Machine Learning Model. <br/>
**readme.md** - This file <br/>


# Datasets
The dataset based on the 2016 NYC Yellow Cab trip record data, which was originally published by the NYC Taxi and Limousine Commission (TLC) . The data was generated for the purposes of this kaggle competition, and this dataset can be found on Kaggle link https://www.kaggle.com/c/nyc-taxi-trip-duration/data. It consist of two files i.e. train and test dataset:
• train.csv - the training set (contains 1458644 trip records) 
• test.csv - the testing set (contains 625134 trip records)

# Data fields
**id** -  a unique identifier for each trip <br/>
**vendor_id** - a code indicating the provider associated with the trip record <br/>
**pickup_datetime** - date and time when the meter was engaged <br/>
**dropoff_datetime** - date and time when the meter was disengaged <br/>
**passenger_count** - the number of passengers in the vehicle (driver entered value) <br/>
**pickup_longitude** - the longitude where the meter was engaged <br/>
**pickup_latitude** -  the latitude where the meter was engaged <br/>
**dropoff_longitude** - the longitude where the meter was disengaged <br/>
**dropoff_latitude** - the latitude where the meter was disengaged <br/>
**store_and_fwd_flag** - This flag indicates whether the trip record was held in vehicle memory before sending to the vendor because the vehicle did not have a connection to the server - Y=store and forward; N=not a store and forward trip <br/>
**trip_duration** -duration of the trip in seconds <br/>
