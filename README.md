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

## Findings
### [1] The percentage of women on boards is approximately normal

![image](https://github.com/user-attachments/assets/8e33ec88-2cd4-4264-8c3b-8e35afc9948a)

### [2] Four countries have extreme values for maternity leave weeks
These were more than 3 IQR above the 3rd quartile.

![image](https://github.com/user-attachments/assets/aac373cf-a59e-4e00-8d44-8d47c1d7cbb1)

### [3] There's a strong relationship between whether countries have boardroom quotas and the percentage of women on company boards

![image](https://github.com/user-attachments/assets/7fdfa4ab-35d5-4cda-b162-50853f6ebf89)

This relationship was confirmed using a Kruskal-Wallis test:

![image](https://github.com/user-attachments/assets/0bceb1fa-56af-4b67-976d-0da39984cc69)

### [4] There are three moderate, positive linear relationships between dependent and indepdendent variables
Childcare spending, childcare enrolment rate and maternity leave weeks all have a moderate positive relationship with the percentage of women on boards (see graphs below). Note: the maternity leave graph shows the relationship with the four extreme outliers removed.

![image](https://github.com/user-attachments/assets/e5b3cf5e-3605-455a-a4fe-3163d4b1b332)

![image](https://github.com/user-attachments/assets/6fb2b68b-378e-4ab0-81d7-31d63adee8f3)

![image](https://github.com/user-attachments/assets/012f23ed-b8c7-4a57-8620-b3a2fb2a2c85)

These relationships were confirmed using Pearson or Kendall correlation (whichever was appropriate considering the assumptions). 

Childcare spending and childcare enrolment rate are also correlated with each other (Kendall correlation returned a value of 0.47).

### [5] Removing maternity leave outliers affected relationships between independent variables
ANOVA or Kruskal Wallis tests were used to assess the relationship between countries with boardroom quotas and their other family and childcare policies.

The results for childcare spending and childcare enrolment rate were significant when all data points were included but not when the maternity leave outliers were removed. The results for childcare spending are shown below. 

![kruskal wallis any quota childcare spending with](https://github.com/user-attachments/assets/1f9d1a70-9b8f-4ae0-b2d0-12b8040beb49)

![image](https://github.com/user-attachments/assets/1028b91d-a7ce-436d-8592-95140eca59da)

### [6] Childcare spending and quota alone don't explain enough variation in boardroom rate
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

