## Summary
Explore the following dataset using R: https://reshare.ukdataservice.ac.uk/857402/ (open dataset obtained via the UK Data Service). 

The data consists of a list of countries and variables relating to maternity leave and childcare policies. The goal of the exploration is to see how these policies and boardroom quotas impact on the percentage of women who sit on corporate boards.

### Steps taken 

- Step 1: Load data from csv and change datatypes as required
- Step 2: Introduce functions for common visualizations
- Step 3: Plot variables individually to assess distribution shape and whether there are outliers
- Step 4: Plot dependent and independent variables to assess strength and linearity of relationships
- Step 5: Use correlation, ANOVA and Kruskal Wallis tests to confirm relationships statistically
- Step 6: Use tidymodels with k-fold cross validation to fit a linear regression model
- Step 7: See if a lasso regression model is an improvement on the linear regression model
- Step 8: Try k-means and PAM clustering to group the countries

## Findings
### [1] The percentage of women on boards is approximately normal

![image](https://github.com/user-attachments/assets/eead9fc3-9236-49cb-a4ce-824c116e8bdb)

### [2] There's a strong relationship between boardroom quotas and the percentage of women on company boards

![image](https://github.com/user-attachments/assets/7fdfa4ab-35d5-4cda-b162-50853f6ebf89)

This relationship was confirmed using a Kruskal-Wallis test:

![image](https://github.com/user-attachments/assets/0bceb1fa-56af-4b67-976d-0da39984cc69)

The effect size was large:

![image](https://github.com/user-attachments/assets/8aed9786-92c9-4e72-a333-37aadf1cfebb)

### [3] There are three moderate, positive linear relationships between dependent and indepdendent variables
Childcare spending, childcare enrolment rate and maternity leave weeks all have a moderate positive relationship with the percentage of women on boards (see graphs below). Note: the maternity leave graph shows the relationship with four extreme outliers removed (more than 3 IQR above 3rd quartile).

![image](https://github.com/user-attachments/assets/e5b3cf5e-3605-455a-a4fe-3163d4b1b332)

![image](https://github.com/user-attachments/assets/6fb2b68b-378e-4ab0-81d7-31d63adee8f3)

![image](https://github.com/user-attachments/assets/012f23ed-b8c7-4a57-8620-b3a2fb2a2c85)

These relationships were confirmed using Pearson or Kendall correlation (whichever was appropriate considering the assumptions). 

Childcare spending and childcare enrolment rate are also correlated with each other (Kendall correlation returned a value of 0.47).

### [4] Removing maternity leave outliers affected relationships between independent variables
ANOVA or Kruskal Wallis tests were used to assess the relationship between countries with boardroom quotas and their other family and childcare policies.

The results for childcare spending and childcare enrolment rate were significant when all data points were included but not when the maternity leave outliers were removed. The results for childcare spending are shown below including the effect size. 

![kruskal wallis any quota childcare spending with](https://github.com/user-attachments/assets/1f9d1a70-9b8f-4ae0-b2d0-12b8040beb49)

![image](https://github.com/user-attachments/assets/1028b91d-a7ce-436d-8592-95140eca59da)

![image](https://github.com/user-attachments/assets/d1280363-279e-47e8-89ad-f9a8ae6ed453)

### [5] Childcare spending and quota alone don't explain enough variation in boardroom rate
Based on the analysis above the most interesting variables for predicting boardroom rate were: any quota, childcare spending, childcare enrolment rate and maternity leave weeks. Because of the maternity outliers and correlation between childcare spending and childcare enrolment rate we started by visualizing boardroom rate, childcare spending and any quota (see graph below). 

![image](https://github.com/user-attachments/assets/bda9774f-6807-42f5-b42a-96efd736a2df)

As we'd expect boardroom rate generally increases for each level of quota. The hard and soft quotas also show a moderate, positive linear relationship between childcare spending and boardroom rate. However, the same doesn't hold for the no quota countries.

The table below shows the results of fitting a linear model on a training set where boardroom rate is the dependent variable and childcare spending and any quota are the independent variables. Childcare spending was not as significant as we'd like but not bad. The r-squared value was 65% and the mean absolute error 4.1. Again, not bad but not the best.

![image](https://github.com/user-attachments/assets/87497819-ebf1-4f07-99df-3ad0090db04b)

Plotting the residuals showed no significant violation of linear regression assumptions (homoscedasticity and normal errors). 

![image](https://github.com/user-attachments/assets/74905375-0818-4eef-b596-a200b8d2d960)

![image](https://github.com/user-attachments/assets/6f3f685e-9f56-4ed4-ad0f-749cfedd5644)

5-fold cross validation was performed to give a more accurate idea of how the model was likely to perform on an unseen test dataset. The average metrics can be seen in the table below. On average only 13% of the variation in boardroom rate was explained by the model and the average mean absolute error was 6.83 (higher than we saw for the full training set). When we looked at the metrics across the different folds there was a lot of variation. For example, the r-squared value varied from 0.4 to 1 and mean absolute error varied from 4.68 to 13.5. The same was observed for the statistical significance of childcare spending, in some of the folds it wasn't significant at all. 

![image](https://github.com/user-attachments/assets/29a15172-40b4-46c4-966c-99b4310b8ce1)

Finally, the linear model was evaluated on the test dataset (see metrics below). 66% of the variation in boardroom rate was explained and the average absolute error was 6.88. Not too bad but not ideal. Looking at the predictions showed some large residuals (see below). Almost 10% in this case is significant.

![image](https://github.com/user-attachments/assets/316dcc9d-aa80-4964-adfb-011be1e85cb0)

![image](https://github.com/user-attachments/assets/919dd647-44a3-4b9e-bba0-29bb84e41293)

### [6] Lasso regression doesn't improve on linear regression
Because of multicollinearity, a lasso regression model was attempted next using all independent variables. 
A grid of lambdas between 0.1 and 100 were tried to minimize the mean absolute error. The graph below shows how an optimal lambda was around 0.7 to 1. It also seemed that trying smaller lambda values wouldn't improve the metrics significantly. 

![image](https://github.com/user-attachments/assets/978752c4-5588-4005-9db7-4cd66737c70e)

When the model was fitted to the training data two variables, childcare enrolment rate and maternity leave weeks, were removed from the model (see below). 86% of the variation in boardroom rate was explained by the model and the mean absolute error was 2.82. Plotting the residuals against the fitted values and a histogram of the residuals showed no signs of heteroscedasticity or non-normal errors. 

![image](https://github.com/user-attachments/assets/9b8c3784-c433-4217-bbad-9dc8da18dea0)

The results for the test dataset weren't as good. Only 61% of the variation in boardroom rate was explained and the mean absolute error was 6.09 (this compared to 66% and 6.88 for linear regression). The table of predictions below shows some large discrepancies.

![image](https://github.com/user-attachments/assets/b868aab1-dcb6-4d47-a6a0-aedfa645320b)

Interestingly, the same test data points (row 5 and 15) were heavily overestimated by linear and lasso regression and the same data point (row 18) was heavily underestimated. 

### [7] Two distinct clusters exist in the data but the remaining points are more varied
Next we tried k-means clustering to identify groupings within the data.

Plotting an elbow graph suggested four was a good candidate for k. By an alternative metric, average silhouette score, three was the optimal k. Therefore, we ran k-means clustering with k = 3 and k = 4 to compare the results.

The screenshots below show the within cluster sum of squares for three and four clusters. Increasing the clusters to four saw a 12% increase in the between sum / total sum ratio. This is a significant improvement.

![image](https://github.com/user-attachments/assets/5a4f4f27-d261-4528-b303-15605d7974d6)

![image](https://github.com/user-attachments/assets/0a84ad37-eb37-452b-9f14-96fa0dafa9d3)

The following image plots the four clusters to show how distinct they are. Clusters 1 and 2 are more distinct and relatively compact. Clusters 3 and 4 are closer together (and merged in the 3 cluster solution). Furthermore, cluster 3 has a larger within cluster variance (this is the largest WCSS, 22.9, in the image above). 

![image](https://github.com/user-attachments/assets/c0cceedb-abd3-4ee9-b982-ccac363d4092)

So how can we describe the differences between these clusters?
- Cluster 1: Have the highest percentage of women on boards, childcare spending and childcare enrolment and at least a soft boardroom quota. This could suggest that childcare policies are more helpful than maternity leave policies in helping women progress in their careers. 
- Cluster 2: Have the lowest percentage of women on boards, lowest childcare enrolment rate and highest maternity leave weeks. This could suggest a more traditional, at home approach to childcare. 3 of the 4 countries don't have a boardroom quota. 
- Cluster 3: Nothing significant stands out. This is the most varied cluster. 
- Cluster 4: The only significant measure is that maternity payment rate is much higher. 

![image](https://github.com/user-attachments/assets/27afbef2-a77d-4d44-928f-8bbf82d18dfa)

Overall, k-means clustering identified two distinct clusters with commonalities.

Note: PAM clustering was also attempted as a robust alternative to k-means clustering. The results were the same except:
- The optimal k was 3 instead of 4 
- PAM clustering returns medoids which can be considered representative points of their respective clusters. In the 3 cluster case they were Portugal, Denmark and Hungary.
- Only one of the clusters was considered an isolated cluster (the one containing Denmark). From k-means it seemed both the Denmark and Hungary clusters were isolated. 
