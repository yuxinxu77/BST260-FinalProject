# BST260-FinalProject - Predicting Risk of Diabetes
### Author: Yuxin Xu, Runting Yang, Junyi Guo

## Overview and Motivation
Diabetes is a group of metabolic disorders characterized by a high blood sugar level over a prolonged period of time. In this project, we mainly focused on the analysis and prediction of the risks of diabetes. Our motivation for this project is: first, to find and analyze possible risk factors of diabetes, such as diet, smoking, health condition, and demographic and socioeconomic factors. Secondly, we would like to study the potential health-related influences of diabetes according to their lab test results and questionnaire answers. Finally, we would like to forecast whether an individual would develop Diabetes given the current living condition. 

We hope that these findings can lead to a better understanding of diabetes and help physicians and patients to prevent, treat, control, and manage diabetes.

Our entire project contains four major sections. We first preprcessed out data, then we conduct exploratory data analysis on the cleaned data, following that we build our data pipeline for preperation of the prediction process, and finally we constructed machine learning models to fulfill the prediction tasks.

## Data Overview
We used the dataset [`National Health and Nutrition Examination Survey`](https://www.cdc.gov/nchs/nhanes/about_nhanes.htm) collection by CDC from 2017 to 2018. It contains more than ten thousand interviewees' data that cover a variety of topics: demographics, examination, health status, diet, lab results, etcetera. We used a specific subset of the data that contains 8897 interviewees who participated in the diabetes survey. 

## Project Structure
### Data Preprocessing
In this section, we perform data cleaning on the raw data collected from CDC. Our dataset contains features from four categories: diet, demographics, examination and lab, and the raw data are stored in .XPT files.

There are a few steps that we performed across all categories of data:
1. Import data from .XPT file.
2. Select features we want and rename their columns from special indices into column names that's more intuitive.
3. Drop rows with NA values and extremely rare values.

### Exploratory Data Analysis adn Visualization
We performed some explorative analysis over the features we selected to examine if they seem to relate to our target variable (diabetes y/n). Since our dataset contains both categorical data and numerical data, we apply different kinds of visualization plot on our data. Here we list the type of graphs we used in this section:
* Pie chart: mainly for categorical data.
![Highest Education Level Among Diabetes Patients]("./image/pie_chart.png")

* Back-to-back histogram: mainly for numerical data.
* Histogram with density plot: mainly for numerical data.

[Probably some examples?]

### Data Pipeline
### Machine Learning Models

## Conclusion

## Reference

## Links to Documents
