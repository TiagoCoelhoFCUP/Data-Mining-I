# Data-Mining-I
Projects developed under the Data Mining I college chair during the 2019/2020 school year


# Prediction of Air Pollution

This project provides a study of the data about Air Pollution in Beijing, China. We first analyse and describe the characteristics and relationships between the variables in the Beijing Multi-Site Air-Quality Data Data Set. Then we process the data and build different models to get predictions and review their
performances. 

Our goal was finding a Machine Learning model that was able to make predictions on future values of pollution level based on the given information, that would tell what people need to know about pollution daily (if it is going to be polluted or not) and at the same time that wouldn’t predict days without pollution
when the day was actually polluted.

In the end we reached that goal by making a linear combination of some models we tested, because some of them were better in accuracy and others in not predicting false non polluted days. In fact, we trained some of the models to have better accuracy and others to prevent predictions of false not polluted days. We found out that the best model was the linear combination of the models that were trained to get the best accuracy, since it is the one that predicts the pollution of the days better and the second best at giving false not polluted days. This models gave predictions with 84.4% accuracy and 83.1% sucess in classifying bad
air quality days. 

![image](https://user-images.githubusercontent.com/13381706/163242074-f2e3b71f-2a67-4714-80ac-adcfadaa07ab.png)

![image](https://user-images.githubusercontent.com/13381706/163242265-e3a06ba1-1701-4ee2-af1e-616d477fdca6.png)

![image](https://user-images.githubusercontent.com/13381706/163242143-35433674-83ec-4ddb-b0e0-8cea34f69772.png)


This analysis was done for only one of the 12 stations in Beijing, so it would be interesting in the future to complete our study and predictions with spatial data of them all together.


