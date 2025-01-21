#load packages
library(tidyverse)
library(tidymodels)
library(mgcv)
library(rstatix)

#data notes
#data from 2018 except 2017 public sector rates (wouldn't change much in a year)
#boardroom rates are from Deloitte (plus some quota information). Rest from OECD
#boardroom rates are for largest companies
#childcare enrolment rate is an average of two figures for 0-2 and 3-5 yr olds

#load data
women_on_boards <- read.csv("~/WomenOnBoards/Data/WomenOnBoards.csv", 
                            header = TRUE) 

#for now remove the fuzzy values
women_on_boards_raw <- women_on_boards %>% select(!contains("Fuzzy"))

#check datatypes are what we expect
glimpse(women_on_boards_raw)

#change datatypes (need to remove commas from childcare spending)
women_on_boards_raw$ChildcareSpending.<- 
  as.integer(str_replace_all(women_on_boards_raw$ChildcareSpending, ",", ""))

women_on_boards_raw$HardBoardroomQuota <- 
  as.factor(women_on_boards_raw$HardBoardroomQuota)

women_on_boards_raw$SoftBoardroomQuota <- 
  as.factor(women_on_boards_raw$SoftBoardroomQuota)

#is there any null data, no
women_on_boards_raw %>% 
  summarize(across(everything(), ~sum(is.na(.x))))

#functions for repeated visualizations
#plot histogram for individual variables
plot_histogram <- function(col, binwidth){
  plt <- ggplot(women_on_boards_raw, aes({{col}})) +
    geom_histogram(aes(y = after_stat(density)), binwidth = {{binwidth}}) +
    theme_bw()

  plt
}

#plot scatter between variable and boardroom rate
plot_continuous <- function(col){
  plt <- women_on_boards_raw %>% 
    ggplot(aes(x={{col}}, y=WomenBoardroomRate)) +
    geom_point() +
    geom_smooth(se = F, color = "red") +
    theme_bw()
  
  plt
}

#function for plotting new AnyQuota category variable - no/soft/hard quota
plot_categorical <- function(col){
  plt <- women_on_boards_raw %>% 
    ggplot(aes(x= AnyQuota, y={{col}})) +
    geom_boxplot() +
    theme_bw()
  
  plt
}


# Visualizations ----------------------

#plot the distribution of the boardroom rate
#12.5-17.5% and 22.5-27.5% most common
#looks approx normal
plot_histogram(WomenBoardroomRate, 5)

median(women_on_boards_raw$WomenBoardroomRate)
mean(women_on_boards_raw$WomenBoardroomRate)

#plot distributions of other variables
#plot maternity leave box plot, 4 outliers
ggplot(women_on_boards_raw, aes(MaternityLeaveWeeks)) +
  geom_boxplot()

#are the outliers more than 3 IQR away? Yes
outlier_min <- quantile(women_on_boards_raw$MaternityLeaveWeeks, probs = 0.75) + 
  3 * (IQR(women_on_boards_raw$MaternityLeaveWeeks))

#without outliers looks approx normal
ggplot(women_on_boards_raw %>% filter(MaternityLeaveWeeks < outlier_min), 
       aes(MaternityLeaveWeeks)) +
  geom_histogram(aes(y = after_stat(density)), binwidth = 10)  +
  labs(x = "Maternity leave (weeks)",
       y = "Density")

#plot Maternity Payment, no outliers
ggplot(women_on_boards_raw, aes(MaternityPaymentRate)) +
  geom_boxplot()

#plot maternity payment distribution, looks slightly skewed
plot_histogram(MaternityPaymentRate, 10)

median(women_on_boards_raw$MaternityPaymentRate)
mean(women_on_boards_raw$MaternityPaymentRate)

#childcare spending, no outliers
ggplot(women_on_boards_raw, aes(ChildcareSpending.)) +
  geom_boxplot()

#plot childcare spending distribution, skewed
plot_histogram(ChildcareSpending., 1000)

#childcare rate, no outliers
ggplot(women_on_boards_raw, aes(ChildcareEnrolmentRate)) +
  geom_boxplot()

#plot childcare rate distribution, approx normal
plot_histogram(ChildcareEnrolmentRate, 10)

median(women_on_boards_raw$ChildcareEnrolmentRate)
mean(women_on_boards_raw$ChildcareEnrolmentRate)

#public sector rate, no outliers
ggplot(women_on_boards_raw, aes(PublicSectorRate)) +
  geom_boxplot()

#public sector rate distribution, lots of countries between 12.5 and 17.5%
plot_histogram(PublicSectorRate, 5)

#what is the number of hard/soft/no quotas
#soft no can mean hard yes...
#4 countries hard quota, 11 soft quota, 7 neither
ggplot(women_on_boards_raw, aes(x = HardBoardroomQuota, fill = SoftBoardroomQuota)) +
  geom_bar()

#add a new variable to say whether there's any quota, change to factor
women_on_boards_raw <- women_on_boards_raw %>%
  mutate(AnyQuota = case_when(HardBoardroomQuota == "Yes" ~ "Hard",
                              SoftBoardroomQuota == "Yes" ~ "Soft",
                              .default = "No quota"
  ))
women_on_boards_raw$AnyQuota <- as.factor(women_on_boards_raw$AnyQuota)


#quick plot of all variables (except country)
#boardroom rate is what we're predicting, first impressions
#clear relationship:
  #any quota - obvious
#some relationship: 
  #maternity leave weeks (w/o outliers) - positive linear
  #childcare spending - positive non-linear?
  #childcare enrolment - positive non-linear?
#no relationship:
  #maternity payment rate
  #public sector rate
plot(women_on_boards_raw %>% select(MaternityLeaveWeeks:PublicSectorRate, 
                                    WomenBoardroomRate:AnyQuota))

#plot quota variable 
#clear relationship
ggplot(women_on_boards_raw, aes(x= reorder(AnyQuota, WomenBoardroomRate, 
                                           FUN =median),y =WomenBoardroomRate)) +
  geom_boxplot() +
  theme_bw() +
  labs(x = "Boardroom quota",
       y = "Percentage of women on company boards")

#try density plot on quota
#soft looks normal, no - positive skew, hard - negative skew
ggplot(women_on_boards_raw, 
       aes(WomenBoardroomRate, after_stat(density), colour = AnyQuota)) +
  geom_density(linewidth = 1) +
  theme_bw() +
  scale_x_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60)) +
  labs(x = "Percentage of women on company boards",
       y = "Density") 

#plot individual pairings using function
#maternity leave weeks vs boardroom rate
#a few outliers are skewing the data, without them positive relationship
plot_continuous(MaternityLeaveWeeks)

#maternity rate without outliers, positive
ggplot(women_on_boards_raw_no_outliers, 
       aes(x = MaternityLeaveWeeks, y = WomenBoardroomRate)) +
  geom_point() + 
  geom_smooth(method = "lm", se = F, col = "red") +
  theme_bw() +
  labs(x = "Maternity leave (weeks)",
       y = "Percentage of women on company boards")

#maternity payment rate vs boardroom rate
#no clear relationship
plot_continuous(MaternityPaymentRate)

#childcare spending vs boardroom rate
#positive relationship, non-linear?
plot_continuous(ChildcareSpending.)

#check for linearity
#almost quadratic edf but confidence intervals are wide, not clearly quadratic
gam_model <- gam(WomenBoardroomRate ~ s(ChildcareSpending.), 
                   data = women_on_boards_raw)
summary(gam_model)
plot(gam_model)
par(mfrow = c(2,2))
gam.check(gam_model)
par(mfrow = c(1,1))

#childcare enrolment rate vs boardroom rate
#positive relationship
#could argue it's non linear
plot_continuous(ChildcareEnrolmentRate)

#childcare enrolment rate is significant but linear
par(mfrow = c(2,2))
gam_model_2 <- gam(WomenBoardroomRate ~ s(ChildcareEnrolmentRate), 
                 data = women_on_boards_raw)
summary(gam_model_2)
plot(gam_model_2)
gam.check(gam_model_2)
par(mfrow = c(1,1))

#public sector rate vs boardroom rate
#no relationship
plot_continuous(PublicSectorRate)

#are childcare spending and enrolment rate correlated?
#visualize with a log transform, looks non-linear
plot(log(women_on_boards_raw$ChildcareSpending.)
     , women_on_boards_raw$ChildcareEnrolmentRate)

#kendall correlation assumptions:
# variables at least ordinal scale - yes
# monotonic relationship - yes
# moderate correlation
cor(women_on_boards_raw$ChildcareSpending., 
    women_on_boards_raw$ChildcareEnrolmentRate, 
    method = "kendall")

# Statistics ----------------
#alternative dataset which removes 4 maternity leave outliers
women_on_boards_raw_no_outliers <- women_on_boards_raw %>% 
  filter(MaternityLeaveWeeks < outlier_min)

#calculate correlation between dependent and indepdendent variables
#type of correlation will depend on normality/linearity

#maternity leave weeks (no outliers) and boardroom rate, 0.42, moderate +ve
cor(women_on_boards_raw_no_outliers$MaternityLeaveWeeks, 
women_on_boards_raw_no_outliers$WomenBoardroomRate, 
method = "pearson")

#maternity payment rate and boardroom rate, relationship doesn't look monotonic
#no relationship
cor(women_on_boards_raw$MaternityPaymentRate, 
    women_on_boards_raw$WomenBoardroomRate, 
    method = "kendall")

#childcare spending and boardroom rate, moderate positive
cor(women_on_boards_raw$ChildcareSpending., 
    women_on_boards_raw$WomenBoardroomRate, 
    method = "kendall")

#childcare enrolment rate and boardroom rate, moderate positive
cor(women_on_boards_raw$ChildcareEnrolmentRate, 
    women_on_boards_raw$WomenBoardroomRate, 
    method = "kendall")

#public sector rate, weak positive
cor(women_on_boards_raw$PublicSectorRate, 
    women_on_boards_raw$WomenBoardroomRate, 
    method = "kendall")

#correlation with outcome (boardroom rate)
#none: maternity payment rate
#weak: public sector rate
#moderate: childcare enrolment, childcare spending, maternity leave weeks

#correlation between variables (only selected ones that may be correlated)
#already established moderate correlation between childcare spending/enrolment
#weak
cor(women_on_boards_raw_no_outliers$MaternityLeaveWeeks, 
    women_on_boards_raw_no_outliers$ChildcareEnrolmentRate, 
    method = "pearson")

#weak
cor(women_on_boards_raw$MaternityPaymentRate, 
    women_on_boards_raw$ChildcareEnrolmentRate, 
    method = "kendall")

#weak
cor(women_on_boards_raw$ChildcareSpending., 
    women_on_boards_raw$PublicSectorRate, 
    method = "kendall")

#weak
cor(women_on_boards_raw$ChildcareEnrolmentRate, 
    women_on_boards_raw$PublicSectorRate, 
    method = "kendall")

#can we conclude statistically that boardroom rate differs among quota?
#assumptions for ANOVA:
#independent populations - yes, different quotas
#populations have equal standard deviation - yes, see below
#populations follow normal - no, from earlier plot
no_quota <- women_on_boards_raw %>% filter(AnyQuota == "No quota")
soft_quota <- women_on_boards_raw %>% filter(AnyQuota == "Soft")
hard_quota <- women_on_boards_raw %>% filter(AnyQuota == "Hard")

#another way to assess normality
qqnorm(hard_quota$WomenBoardroomRate, pch = 1, frame = FALSE)
qqline(hard_quota$WomenBoardroomRate, col = "blue", lwd = 2)

#Kruskal Wallis instead of ANOVA. Assumptions:
#Dependent variable is ordinal or continuous - yes
#Independent variable has more than two independent groups - yes
#Observations within and between groups are independent - yes
#shapes of distributions aren't the same so compare mean ranks

#there's a significant difference in values among groups
kruskal.test(WomenBoardroomRate ~ AnyQuota, data = women_on_boards_raw)
kruskal.test(WomenBoardroomRate ~ AnyQuota, 
             data = women_on_boards_raw_no_outliers)

#which groups? use Games-Howell post hoc test
#hard/no quota significant
#other two are borderline
games_howell_test(WomenBoardroomRate ~ AnyQuota, 
                  data = women_on_boards_raw)

#is there a relationship between AnyQuota and other numeric variables
#visually yes for childcare/maternity leave
plot_categorical(ChildcareSpending.)
plot_categorical(ChildcareEnrolmentRate)
women_on_boards_raw_no_outliers %>% 
  ggplot(aes(x= AnyQuota, y=MaternityLeaveWeeks)) +
  geom_boxplot() +
  theme_bw()

#assess statistically, either ANOVA or Kruskal Wallis depending on normality

#does childcare spending differ by AnyQuota?
#connected for full dataset, not connected for no outlier dataset
kruskal.test(ChildcareSpending. ~ AnyQuota, data = women_on_boards_raw)
kruskal.test(ChildcareSpending. ~ AnyQuota, 
             data = women_on_boards_raw_no_outliers)

#does childcare enrolment rate differ by AnyQuota?
#connected for full dataset, not connected for no outlier dataset
enrolment_anova <- aov(ChildcareEnrolmentRate ~ AnyQuota, 
                       data = women_on_boards_raw)
enrolment_anova_2 <- aov(ChildcareEnrolmentRate ~ AnyQuota, 
                       data = women_on_boards_raw_no_outliers)

summary(enrolment_anova)
summary(enrolment_anova_2)

#does maternity leave weeks differ by AnyQuota?
#no, from graph no quota has higher maternity leave than soft quota
maternity_anova <- aov(MaternityLeaveWeeks ~ AnyQuota, 
                       data = women_on_boards_raw_no_outliers)

summary(maternity_anova)


# Model ----------------
# Most interesting variables: AnyQuota, ChildcareSpending, ChildcareEnrolment
#  and MaternityLeaveWeeks

# Because of maternity leave outliers and correlation between childcare
#   spending and enrolment look at AnyQuota and ChildcareSpending

#First visualize to see what the patterns are
#lots of uncertainty, wide confidence intervals
#hard/soft quota have same slope, no quota doesn't
#  but not much data and one no quota is an outlier
#try ChildcareSpending and AnyQuota without interaction term
ggplot(data = women_on_boards_raw, aes(x = ChildcareSpending., 
                                       y = WomenBoardroomRate, 
                                       colour = AnyQuota)) +
  geom_point() + 
  geom_smooth(method = "lm") +
  theme_bw()

#Boardroom Rate looks approximately normal
#No outliers for chosen variables
#Only one numerical independent variable so leave scaling for now

#split into test train
set.seed(1353)
split <- initial_split(women_on_boards_raw, strata = AnyQuota)
train <- training(split)
test <- testing(split)

#firstly fit model on whole training set to assess lm assumptions

#create recipe, no preprocessing for now but get used to structure
recipe <- recipe(
  WomenBoardroomRate ~ ChildcareSpending. + AnyQuota,
  data = train
) 

#set up linear regression model
linear_model <- linear_reg() 

#set up workflow
workflow <- workflow() %>% 
  add_model(linear_model) %>%
  add_recipe(recipe)

#fit model on whole train set
model_fit <- workflow %>% fit(data = train)

#assess fit of model, childcare spending only marginally significant
model_fit %>% extract_fit_parsnip() %>% tidy() 

#add predictions and residuals to train dataset 
train_augment <-augment(model_fit, train) 

#look at rsq, 65% of variation in boardroom rate explained by model
rsq(train_augment, truth = WomenBoardroomRate, estimate = .pred) 

#mean absolute error, average prediction is wrong by 4.1, not too bad
mae(train_augment, truth = WomenBoardroomRate, estimate = .pred)

#plot residuals against fitted values, looks ok
plot(train_augment$.pred, 
     train_augment$.resid,
     xlab = "Fitted Value",
     ylab = "Residual",
     main = "Fitted vs residuals")
abline(a=0, b=0, col = "red")

#plot distribution of residuals, looks slightly negatively skewed
hist(train_augment$.resid, breaks = 10,
     xlab = "Residual", 
     main = "Histogram of residuals")

#plot qq-plot, not too bad
qqnorm(train_augment$.resid, pch = 1, frame = FALSE) 
qqline(train_augment$.resid, col = "blue", lwd = 2) 

#overall thoughts: model assumptions are roughly fulfilled,
  # metrics are OK but not the best

#try cross validation to get a more accurate idea of how the 
#  model will perform on unseen data

#use 5-fold cross validation
set.seed(1001)
train_folds <- vfold_cv(train, v = 5, strata = AnyQuota)

#function to get model results
get_model <- function(x) {
  extract_fit_parsnip(x) %>% tidy()
}

#fit model on 5 folds
cv_model_fit <- workflow %>% fit_resamples(resamples = train_folds,
                                           control = control_resamples(
                                             extract = get_model,
                                             save_pred = TRUE),
                                           metrics = metric_set(
                                             rsq, mae
                                           ))

#this calculates the average metrics over all folds
#on average only 13.4% of variance in boardroom rate explained by model
#on average 6.83 error (measured in %, seems high)
cv_model_fit %>% collect_metrics()

#this lists the rsq and mae for each fold
cv_model_fit$.metrics[1:5]
#Same breakdown but easier to compare
#rsq varies from 0.40 to 1
#mae varies from 4.68 to 13.5 (reasonable to not good)
cv_model_fit %>% collect_metrics(summarize = FALSE)

#Look at all predictions across folds
cv_model_predictions <- cv_model_fit %>% collect_predictions() %>%
  mutate(resid = WomenBoardroomRate - .pred)
#some large residuals (both +ve and -ve)
cv_model_predictions %>% arrange(resid)

#this lists the linear regression results for one fold (first number kth fold)
cv_model_fit$.extracts[[5]][[1]]

#this lists the fit for each fold (including p values)
#childcare spending is sometimes not significant
#any quota is more consistently significant (one exception)
cv_model_extracts <- cv_model_fit %>% collect_extracts()
cv_model_extracts$.extracts

#overall r2 and mae vary considerably between the different folds
#sometimes childcare spending isn't statistically significant

#fit the model to the test dataset
test_fit <- last_fit(
  workflow,
  split = split, 
  metrics = metric_set(mae, rsq)
)


#on the test dataset the average error is 6.88 (measured in %)
#the model explains 66% of the variation in boardroom rate
test_fit$.metrics

#this lists the actual and predicted values
(test_predictions <- test_fit$.predictions %>% as.data.frame() %>% 
  mutate(.resid = WomenBoardroomRate - .pred))

#this lists the coefficients of the equation
test_fit$.workflow

#Ideas:

#Try ridge or lasso regression because ChildcareSpending/AnyQuota are correlated
# note: without no quota countries, quota/childcare spend looks like a good model,
# which variables (if any) are correlated to boardroom rate for no quota countries
#What other methods could be used?
#Later: Are there clusters within data for similar countries?

# Second model ---------------------------
#Plot any quota, childcare enrolment rate and boardroom rate
#lots of uncertainty
#strangely for no quota countries the slope is negative
ggplot(data = women_on_boards_raw, aes(x = ChildcareEnrolmentRate, 
                                       y = WomenBoardroomRate, 
                                       colour = AnyQuota)) +
  geom_point() + 
  geom_smooth(method = "lm") +
  theme_bw()

#try lasso regression 
#create recipe, add dummy encoding for any quota
#now we have multiple numerical variables normalize them
lasso_recipe <- recipe(
  WomenBoardroomRate ~ ChildcareSpending. + ChildcareEnrolmentRate + AnyQuota,
  data = train
) %>%
  step_dummy(AnyQuota) %>%
  step_normalize(ChildcareSpending., ChildcareEnrolmentRate)

#first find the optimal lamdba using 5-fold cross validation
#model
tune_lambda <- linear_reg(penalty = tune(), mixture = 1) %>% 
  set_engine("glmnet")

#set up workflow
lasso_workflow <- workflow() %>% 
  add_model(tune_lambda) %>%
  add_recipe(lasso_recipe)

#try different values of lambda
test_grid <- 10^seq(2, -2, length = 100) %>% as.data.frame() %>%
  rename("penalty" = ".")

lambda_grid <- tune_grid(
  lasso_workflow,
  resamples = train_folds,
  grid = test_grid,
  metrics = metric_set(
    rsq, mae
  )
  
)
#look at the metrics for the different penalty levels
lambda_grid %>% collect_metrics()

#visualize
#at high lambdas no variation explained
#the smaller the lambda the better the metrics
autoplot(lambda_grid)

#define the best penalty level
#chooses the lowest lambda, is this because we only have 3 variables?
lowest_mae <- lambda_grid %>%
  select_best(metric = "mae")

#alternative way of choosing best parameter
#in this case doubt will make much difference
lambda_grid %>% 
  select_by_one_std_err(metric = "mae", desc(penalty))

#set up lasso regression model using best lambda
lasso_model_fit <- finalize_workflow(lasso_workflow, lowest_mae) %>% 
  fit(data = train)

#assess fit of model, no variables dropped 
lasso_model_fit %>% extract_fit_parsnip() %>% tidy() 

#add predictions and residuals to train dataset 
lasso_train_augment <-augment(lasso_model_fit, train) 

#look at rsq, 76% of variation in boardroom rate explained by model
rsq(lasso_train_augment, truth = WomenBoardroomRate, estimate = .pred) 

#mean absolute error, average prediction is wrong by 3.03, not too bad
mae(lasso_train_augment, truth = WomenBoardroomRate, estimate = .pred)

#add residuals to augmented dataset
lasso_train_augment$.resid <- lasso_train_augment$WomenBoardroomRate - 
  lasso_train_augment$.pred

#plot residuals against fitted values, looks ok
plot(lasso_train_augment$.pred, 
     lasso_train_augment$.resid,
     xlab = "Fitted Value",
     ylab = "Residual",
     main = "Fitted vs residuals")
abline(a=0, b=0, col = "red")

#plot distribution of residuals
hist(lasso_train_augment$.resid, breaks = 10,
     xlab = "Residual", 
     main = "Histogram of residuals")

#plot qq-plot, not great
qqnorm(lasso_train_augment$.resid, pch = 1, frame = FALSE) 
qqline(lasso_train_augment$.resid, col = "blue", lwd = 2) 

#test for normality, ok
shapiro.test(lasso_train_augment$.resid)

#fit the model to the test dataset
lasso_test_fit <- last_fit(
  finalize_workflow(lasso_workflow, lowest_mae),
  split = split,
  metrics = metric_set(
    rsq, mae
  )
)

#this lists the metrics, rsq 38%, mae 9.13
lasso_test_fit$.metrics

#this lists the actual and predicted values, some big residuals
(lasso_test_predictions <- lasso_test_fit$.predictions %>% 
    as.data.frame() %>% 
    mutate(.resid = WomenBoardroomRate - .pred))

#overall: the model assumptions hold but the metrics are worse than lm
#suspect lowest lambda was chosen because we only have three variables

#curious to see what would happen if all variables were included
all_lasso_recipe <- recipe(
  WomenBoardroomRate ~ ChildcareSpending. + ChildcareEnrolmentRate + AnyQuota +
    MaternityLeaveWeeks + MaternityPaymentRate + PublicSectorRate,
  data = train
) %>%
  step_dummy(AnyQuota) %>%
  step_normalize(ChildcareSpending., ChildcareEnrolmentRate, 
                 MaternityLeaveWeeks, MaternityPaymentRate,
                 PublicSectorRate)

#new workflow
all_lasso_workflow <- workflow() %>% 
  add_model(tune_lambda) %>%
  add_recipe(all_lasso_recipe)

#tune lambda
all_lambda_grid <- tune_grid(
  all_lasso_workflow,
  resamples = train_folds,
  grid = test_grid,
  metrics = metric_set(
    rsq, mae
  )
)

#look at the metrics for the different penalty levels
all_lambda_grid %>% collect_metrics()

#visualize 
#mae much better from 5 to 0.9, then stable
#rsq much better from 75 to 1, then stable
#whether we choose to make mae/rsq better wouldn't matter
#seems lower lambda values wouldn't improve the model
autoplot(all_lambda_grid) + theme_bw()

#define the best penalty level
#this is no longer the smallest possible lambda (as defined by grid)
all_lowest_mae <- all_lambda_grid %>%
  select_best(metric = "mae")

#tried this as an alternative way of choosing lambda, worse results
# and same variables were removed from model
all_lambda_grid %>% 
  select_by_one_std_err(metric = "mae", desc(penalty))

#fit model on whole train set
all_lasso_model_fit <- finalize_workflow(all_lasso_workflow, 
                                      all_lowest_mae) %>% fit(data = train)

#assess fit of model
#childcare enrolment rate and maternity leave weeks are removed
all_lasso_model_fit %>% extract_fit_parsnip() %>% tidy() 

#add predictions and residuals to train dataset 
all_lasso_train_augment <-augment(all_lasso_model_fit, train) 

#look at rsq, 86% of variation in boardroom rate explained by model
rsq(all_lasso_train_augment, truth = WomenBoardroomRate, estimate = .pred) 

#mean absolute error, average prediction is wrong by 2.82, good
mae(all_lasso_train_augment, truth = WomenBoardroomRate, estimate = .pred)

#add residuals to augmented dataset
all_lasso_train_augment$.resid <- all_lasso_train_augment$WomenBoardroomRate - 
  all_lasso_train_augment$.pred

#plot residuals against fitted values, looks ok
plot(all_lasso_train_augment$.pred, 
     all_lasso_train_augment$.resid,
     xlab = "Fitted Value",
     ylab = "Residual",
     main = "Fitted vs residuals")
abline(a=0, b=0, col = "red")

#plot distribution of residuals
hist(all_lasso_train_augment$.resid, breaks = 10,
     xlab = "Residual", 
     main = "Histogram of residuals")

#plot qq-plot, ok
qqnorm(all_lasso_train_augment$.resid, pch = 1, frame = FALSE) 
qqline(all_lasso_train_augment$.resid, col = "blue", lwd = 2) 

#test for normality, ok
shapiro.test(all_lasso_train_augment$.resid)

#fit the model to the test dataset
all_lasso_test_fit <- last_fit(
  finalize_workflow(all_lasso_workflow, all_lowest_mae),
  split = split,
  metrics = metric_set(
    rsq, mae
  )
)

#this lists the metrics, 61% r-squared, 6.09 mae
all_lasso_test_fit$.metrics

#this lists the actual and predicted values, still some large values
(all_lasso_test_predictions <- all_lasso_test_fit$.predictions %>% 
    as.data.frame() %>% 
    mutate(.resid = WomenBoardroomRate - .pred))

# Ideas:
# The same data points were over/underestimated using linear/lasso regression
# Do these data points have anything in common?
# Could we try clustering the data to identify groupings?