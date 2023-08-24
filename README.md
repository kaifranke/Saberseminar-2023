# Saberseminar-2023
Kai Franke and Jack Rogers' Saberseminar 2023 project about catcher blocking with all of the code and data.

blocking.R has all of the code that created everything in our model and research. It includes data scraping, manipulation, cleaning, modeling, and analysis. There are 6 sections in our code.
blockingleaderboard.R includes code to make a leaderboard with data created in blocking.R

* Data Preparation
  * All of the data preparation including scraping the 2022 data for training and making it so that the data is usable to be able to feed into the XGBoost algorithm
* EDA
  * Graphing the data and looking at summaries to understand what is going on with the dataset 
* Baseline Model
  * Building an initial model to build off of and compare to see if we are building a better model than originally created
* Model w/ Grid Search
  * Optimizing hyperparameters to find a good balance of 4 metrics: AUC, Logloss, Precision, and Recall. It is also where we analyze our model to make sure it is quality enough to push through 
* 2023 Data Retrieval
  * Getting 2023 data to test on and ensure the model continues doing well with new data. Also being able to do some analysis on the 2023 season thus far.
* Player Development
  * We looked at Freddy Fermin to see how well he was doing and where there was room for improvement. Used stat_summary_2d to make a heatmap for both xBlock% and BAX 


9-Rogers-Franke (UPDATED) (2).pdf is the presentation from Saberseminar on Saturday, August 12th, 2023
