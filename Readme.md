#house_prices_advanced_regression_techniques



**Description**

Ask a home buyer to describe their dream house, and they probably won't begin with the height of the basement ceiling or the proximity to an east-west railroad. But this playground competition's dataset proves that much more influences price negotiations than the number of bedrooms or a white-picket fence.

With 79 explanatory variables describing (almost) every aspect of residential homes in Ames, Iowa, this competition challenges you to predict the final price of each home.

**Pre-Process and Model**
1.	Dropping the outliers
2.	Filling the missing values.
3.  Checking for correlation and multi-collinearity
4.	Adding log-predictors to raw data
5.	Adding dummy variables and concate with numeric predictors. And adding some squared predictors
6.	Model used: 30-fold cross-validation

On each run of cross-validation 5 models were fitted (l2, l1, GBR, ENet and LGB). Then we make 5 predictions using these models on left-out fold and add geometric mean of these predictions. Finally, use lasso on these six predictors to forecast values on the left-out fold.

**EDA**

The sales price of the houses depends on various independent attributes, such as location, living area, number of floors, kitchen area, pool area, their quality and condition. They are divided into categorical variables as well as numeric variables. According to the data, there are total more than 70 attributes that decides the sale price of the house. It becomes difficult to make a model to predict the price of the house using all the attributes present. Thus, Exploratory data analysis becomes very necessary. Tableau can be used to explore the data and decide which are the most significant variables. 
Initial exploration of the data suggests, which variables effect the price of the house directly. Plotting sales price against all the variables gives from the results that Gr living area, 1st SF area are the variables which are most linearly related to the sales price which is evident from the scatter plot. Other variables that effect the sales price are lot area, total bsmt sf, Fireplace, 2ndFlrSF, Garage area etc. 
Comparing both the following dashboards we can see that, the sake condition of “partial’ have the greatest average sales price. And we can see that, they all have Central Air. As well as from the overall graph it can be deduded that the house that has central house have overall higher sale price. Thus while modelling, these are the variables that should be included in the models. 

<img src="https://github.com/vaibhavdiyora88/house_prices_advanced_regression_techniques/blob/master/EDA%20Images/Image1.png">

<img src="https://github.com/vaibhavdiyora88/house_prices_advanced_regression_techniques/blob/master/EDA%20Images/Image2.png">
<img src="https://github.com/vaibhavdiyora88/house_prices_advanced_regression_techniques/blob/master/EDA%20Images/Image3.png">