# NHL-Shot-Quantity-vs-Quality-Analysis---R
Data analysis of shot quantity vs quality in NHL games and what leads to greater player success. Analysis done in R with visualizations in ggplot.


The purpose of this project was to use R to analyze NHL data. I determined a gameplay strategy I would like to investigate was shot quantity vs shot quality.

I first imported the data set into R and removed all columns I deemed irrelevant based on domain knowledge.

I then compared each shot quality (low, medium, high) for each player with he expected goals per shift of a player to normalize.
I now think per 60 for both would normalize better.

I then created a linear regression by using all rows I determined relevant and removing rows to see the effetct on the R^2 value.
Using this reverse technique, I was able to find a linear regression that was accurate. 

I tested the regression and had good accuracy.

I then gave an allaysis of both the regression and the data findings in a report.

This was an undergraduate project for a data science course.
