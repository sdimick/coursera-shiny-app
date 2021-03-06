# Chance of Rain

Course project for [Developing Data Products](https://www.coursera.org/learn/data-products)

### [Go to the App!](https://sdimick.shinyapps.io/rain-app/)

### Background

In Seattle, rain is something you learn to live with. I am no weather expert, but for this course project I decided to build a few simplistic models for predicting rainy days in Seattle based on historic data. This [Shiny](https://shiny.rstudio.com/) app is powered by 100 years (1918 to 2017) of weather data from the [National Centers for Environmental Information](https://www.ncdc.noaa.gov/cdo-web/) for the Portage Bay station in Seattle. The historic data is used to train a Logistic Regression model to predict the probability or rain as well as a GLM Classifier for rain versus no rain for a day given the month and whether or not on the previous day.

### How to Use the App

##### User Input

- Select what month it is
- Select if it has rained today

##### Interpret Results

- Headline generated by GLM Classification model for rain versus no rain
- Probability of rain generated by Logistic Regression model
- A visual of the historic data is generated for tomorrow's precipitation status for the selected month and status of today's precipitation
