library(ggplot2)
library(dplyr)

setwd("D:\\GitHub\\MelbourneHousing")
housing_data = read.csv("Melbourne_housing_FULL.csv")

############################### Question 1 ###############################

str(housing_data)
# Removing examples where Price is missing
housing_data = housing_data[!is.na(housing_data$Price),]
# Replacing "#N/A" by NA in categorical vars
housing_data = data.frame(lapply(housing_data, function(x) { gsub("#N/A", NA, x) } ) )
# Looking at the number of missing values in each column
sapply(housing_data, function(x) sum(is.na(x)) )

# Plot latitude and longitude
housing_data$Longtitude = as.numeric( as.character(housing_data$Longtitude) )
housing_data$Lattitude = as.numeric( as.character(housing_data$Lattitude) )
housing_data$Price = as.numeric( as.character(housing_data$Price) )
long_df = subset(housing_data, !is.na(Lattitude))
# Plot lat and long with price
ggplot(data = long_df, aes(y=Lattitude, x=Longtitude)) + 
  geom_point(aes(colour=Price)) + 
  scale_colour_gradient(low = "blue", high = "white") +
  theme_bw()
long_df = NULL
# Removing Address
housing_data$Address = NULL
# Removing Postcode
housing_data$Postcode = NULL
# Check missing values in Regionname
housing_data[is.na(housing_data["Regionname"]),]
housing_data = housing_data[!is.na(housing_data$Regionname),]
# plot Mean Price vs Region Name
ggplot(housing_data, aes(x=Regionname, y=Price)) + #geom_dotplot(stat="identity", fill="steelblue") + 
  geom_violin(fill="steelblue", draw_quantiles = TRUE, scale = "width") +
  theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + xlab("Region Name") + 
  ylab("Mean Price")
# Summary Suburb
str(housing_data$Suburb)
table(housing_data$Suburb)[order(table(housing_data$Suburb))]
summary(housing_data$Propertycount)
typeof(housing_data$Propertycount)
# Property Count with Type and Price
ggplot(housing_data, aes(x=as.numeric(as.character(Propertycount)), y=Price)) + 
  geom_point(aes(colour=Type)) +
  theme_bw() + xlab("Property count") + 
  ylab("Price")
# Correlation between Rooms and Bedroom2
cor(as.numeric(as.character(housing_data$Rooms)), as.numeric(as.character(housing_data$Bedroom2)), use="pairwise.complete.obs")
# Drop Bedroom2
housing_data$Bedroom2 = NULL
# Distance
housing_data$Distance = as.numeric(as.character(housing_data$Distance))
# Plot of distance with Landsize
ggplot(housing_data, aes(x=Distance, y=log(as.numeric(as.character(Landsize))))) + 
  geom_point(aes(colour=as.numeric(as.character(Price))), alpha=0.8) +
  scale_colour_gradient(low = "blue", high = "green") +
  theme_bw() + xlab("Distance from CBD") + 
  ylab("log(Landsize)") + labs(color = "Price")
median_landsize = median(as.numeric(as.character(housing_data$Landsize)), na.rm = TRUE)
housing_data$Landsize[is.na(housing_data$Landsize)] = median_landsize
# Plot of car and bathroom
ggplot(housing_data, aes(x=Car, y=Bathroom)) + 
  geom_count(aes(colour=as.numeric(as.character(Price)))) +
  scale_colour_gradient(low = "blue", high = "green") +
  theme_bw() + xlab("Car") + 
  ylab("Bathroom") + labs(color = "Price")
# Plot of bathroom and rooms
ggplot(subset(housing_data, !is.na(Bathroom)), aes(x=as.numeric(as.character(Rooms)), y=as.numeric(as.character(Bathroom)) )) + 
  geom_count(colour = "steelblue") + 
  stat_smooth(method = "lm", col = "red") +
  theme_bw() + xlab("Rooms") + 
  ylab("Bathroom")


# Fit linear model Bathroom ~ Rooms
bathroom_lm = lm(as.numeric(as.character(Bathroom)) ~ as.numeric(as.character(Rooms)), 
                 data = subset(housing_data, !is.na(Bathroom)))
# Fill missing value of Bathroom by predicting from above model
housing_data$Bathroom[is.na(housing_data$Bathroom)] = ceiling(predict(object = bathroom_lm, newdata = data.frame(Rooms = housing_data$Rooms[is.na(housing_data$Bathroom)])))


ggplot(subset(housing_data, !is.na(Car)), aes(x=as.numeric(as.character(Rooms)), y=as.numeric(as.character(Car)) )) + 
  geom_count(colour = "steelblue") + 
  stat_smooth(method = "lm", col = "red") +
  theme_bw() + xlab("Rooms") + 
  ylab("Car")

# Fit linear model Car ~ Rooms
car_lm = lm(as.numeric(as.character(Car)) ~ as.numeric(as.character(Rooms)), 
                 data = subset(housing_data, !is.na(Car)))
# Fill missing value of Car by predicting from above model
housing_data$Car[is.na(housing_data$Car)] = ceiling(predict(object = car_lm, newdata = data.frame(Rooms = housing_data$Rooms[is.na(housing_data$Car)])))

# Remove Bulding area and year built
housing_data$BuildingArea = NULL
housing_data$YearBuilt = NULL
# Changing type of Date to datetime 
housing_data$Datesold = as.Date(housing_data$Date, format = "%d/%m/%Y")
sum(is.na(housing_data$Datesold))
housing_data$Date = NULL
# summarising price with date sold and type
mean_date = group_by(housing_data, Datesold, Type) %>%
  summarise(Pricemean = mean(as.numeric(as.character(Price)), na.rm = TRUE))
# Plot of price vs date sold segmented by type
ggplot(mean_date, aes(x=Datesold)) + 
  geom_line(aes(y=Pricemean, col=Type), size=0.7) +
  labs(y="Mean Price", x="Date Sold") +
  theme_bw()
# Removing SellerG
housing_data$SellerG = NULL
# Plot of method vs Price
ggplot(housing_data, aes(x=Method, y=as.numeric(as.character(Price)))) +
  geom_violin(fill="steelblue", draw_quantiles = c(0.25, 0.5, 0.75)) +
  theme_bw() + theme(axis.text.x = element_text(hjust = 1)) + xlab("Method") + 
  ylab("Price")


write.csv(housing_data, "housing_q1.csv")

################## question 2 ########################
library(ggplot2)
library(dplyr)

setwd("D:\\GitHub\\MelbourneHousing")
housing_data = read.csv("housing_q1.csv")
housing_data$X = NULL
str(housing_data)
sapply(housing_data, function(x) sum(is.na(x)) )
housing_data$Landsize = as.numeric(housing_data$Landsize)

# deciding number of clusters
ggplot(housing_data, aes(Price))+
  geom_histogram(bins=7, color="darkblue", fill="lightblue", size=.1) +
  labs(title="Histogram with 7 Bins") + theme_bw()

ggplot(housing_data, aes(log(Price))) +
  geom_histogram(bins=7, color="darkblue", fill="lightblue", size=.1) +
  labs(title="Histogram with 7 Bins") + theme_bw()
# Setting priceclusters or bins that we divided the price distribution into to each example.
intr = (max(log(housing_data$Price)) - min(log(housing_data$Price)))/5
breaks = seq(from = min(log(housing_data$Price)), to = max(log(housing_data$Price)), by = intr)
housing_data$priceclusters = 0
for( i in 1:(length(breaks)-1) )
{
  housing_data$priceclusters[(log(housing_data$Price) >= breaks[i]) & (log(housing_data$Price) <= breaks[i+1])] = i
}
housing_data$priceclusters = as.factor(housing_data$priceclusters)
# KMeans with just Rooms and Bathroom
# Preprocess Rooms and Bathrooms
M = length(unique(housing_data$Rooms))
housing_data$Rooms = (housing_data$Rooms - 0.5) / M
M = length(unique(housing_data$Bathroom))
housing_data$Bathroom = (housing_data$Bathroom - 0.5) / M
# Fit K-means cluster
fit = kmeans(housing_data[, c("Rooms", "Bathroom")], 7)
results = data.frame(housing_data[, c("Rooms", "Bathroom", "priceclusters")], fit$cluster)
results$fit.cluster = as.factor(results$fit.cluster)
# plot the clusters based on fitted cluster
ggplot(data = results, aes(y=Bathroom, x=Rooms, colour=fit.cluster)) + 
  geom_count() + 
  theme_bw() + labs(color = "cluster")
# plot the clusters based on our assumption
ggplot(data = results, aes(y=Bathroom, x=Rooms, colour=priceclusters)) + 
  geom_count() + 
  theme_bw() + labs(color = "cluster")
# Within cluster SS
fit$withinss
# 0.7672562 112.1465022   0.5100000   0.2339394   0.2278854  14.2854712   4.9796185
fit$tot.withinss
# 133.1507
# Between cluster SS
fit$betweenss
# 216.4314

M = length(unique(housing_data$Car))
housing_data$Car = (housing_data$Car - 0.5) / M

fit = kmeans(housing_data[, c("Rooms", "Bathroom", "Car")], 7)
results = data.frame(housing_data[, c("Rooms", "Bathroom", "Car", "priceclusters")], fit$cluster)
results$fit.cluster = as.factor(results$fit.cluster)

ggplot(data = results, aes(y=Car, x=Bathroom, colour=fit.cluster)) + 
  geom_count() + 
  theme_bw() + labs(color = "cluster")

# Within cluster SS
fit$withinss
# 1.894401  3.418057  1.920223  4.591875 25.712113 18.096513  1.493235
fit$tot.withinss
# 57.12642
# Between cluster SS
fit$betweenss
# 323.3267

df=housing_data[, c("Rooms", "Bathroom", "Car", "Distance", "priceclusters")]
# df$Landsize = log(df$Landsize+1)
fit = kmeans(df[, c("Rooms", "Bathroom", "Car", "Distance")], 7)
results = data.frame(df, fit$cluster)
results$fit.cluster = as.factor(results$fit.cluster)

ggplot(data = results, aes(y=Distance, x=Bathroom, colour=fit.cluster)) + 
  geom_count() + 
  theme_bw() + labs(color = "cluster")

# Within cluster SS
fit$withinss
# 13279.765  2051.438 16554.619  2825.868  3611.635 13712.438  5270.140
fit$tot.withinss
# 57305.9
# Between cluster SS
fit$betweenss
# 1219330


#################### Question 3 ################
library(ggplot2)
library(caret)
library(glmnet)
library(onehot)
library(gbm)
setwd("D:\\GitHub\\MelbourneHousing")
housing_data = read.csv("housing_q1.csv")
str(housing_data)
housing_data$X = NULL

# Normalize numerical variables
norm_function = function(x){
  (x-min(x))/(max(x)-min(x))
}

housing_data$Rooms = norm_function(housing_data$Rooms)
housing_data$Distance = norm_function(housing_data$Distance)
housing_data$Bathroom = norm_function(housing_data$Bathroom)
housing_data$Car = norm_function(housing_data$Car)
housing_data$Landsize = norm_function(log(housing_data$Landsize + 1))
housing_data$Propertycount = norm_function(housing_data$Propertycount)

df = housing_data[, c("Rooms", "Type", "Method", "Distance", "Bathroom", "Car", "Landsize", "Propertycount", 
                      "CouncilArea", "Price")]
# One hot encoding
encoder = onehot(df, max_levels = 40)
df = predict(encoder, df)
df = as.data.frame(df)
str(df)
# Train Test Split
set.seed(5)
train_ind = sample(seq_len(nrow(df)), size = floor(0.7 * nrow(df)))
train_data = df[train_ind, ]
test_data = df[-train_ind, ]

head(train_data)
# Ridge
fit_ridge_cv = cv.glmnet(as.matrix( train_data[, !(colnames(train_data) == "Price")] ), 
                         log(train_data$Price), alpha = 0)
plot(fit_ridge_cv)
# best lambda
fit_ridge_cv$lambda.min
# 0.02953548
# Best Train RMSE
train_pred = exp(predict(fit_ridge_cv, as.matrix( train_data[, !(colnames(train_data) == "Price")] ), s = "lambda.min"))
sqrt(mean((train_data$Price - train_pred) ^ 2)) # 395895.2
# Test RMSE
test_pred = exp(predict(fit_ridge_cv, as.matrix( test_data[, !(colnames(test_data) == "Price")] ), s = "lambda.min"))
test_pred_ridge = exp(predict(fit_ridge_cv, as.matrix( test_data[, !(colnames(test_data) == "Price")] ), s = "lambda.min"))
sqrt(mean((test_data$Price - test_pred) ^ 2)) # 407655.3
# Coefficients
coeff_mat = as.matrix(coef(fit_ridge_cv, s = "lambda.min"))
coeff_df = as.data.frame(coeff_mat)
coeff_df$vars = rownames(coeff_df)
colnames(coeff_df) = c("coef", "vars")
coeff_df$impact = ifelse(coeff_df$coef < 0, "negative", "positive")
coeff_df = coeff_df[order(coeff_df$coef), ]
coeff_df$vars = factor(coeff_df$vars, levels = coeff_df$vars)
coeff_df = coeff_df[coeff_df$vars != "(Intercept)", ]

ggplot(coeff_df, aes(x=vars, y=coef, label=coef)) + 
  geom_bar(stat='identity', aes(fill=impact), width=.5)  +
  scale_fill_manual(name="Impact", 
                    labels = c("Negative", "Positive"), 
                    values = c("positive"="#00ba38", "negative"="#f8766d")) + 
  labs(x = "Variables", y = "Coefficients") + theme_bw() + 
  coord_flip()

# Lasso
fit_lasso_cv = cv.glmnet(as.matrix( train_data[, !(colnames(train_data) == "Price")] ), 
                         log(train_data$Price), alpha = 1)
plot(fit_lasso_cv)
# best lambda
fit_lasso_cv$lambda.min
# 0.0004385925
# Best Train RMSE
train_pred = exp(predict(fit_lasso_cv, as.matrix( train_data[, !(colnames(train_data) == "Price")] ), s = "lambda.min"))
sqrt(mean((train_data$Price - train_pred) ^ 2)) # 395302
# Test RMSE
test_pred = exp(predict(fit_lasso_cv, as.matrix( test_data[, !(colnames(test_data) == "Price")] ), s = "lambda.min"))
test_pred_lasso = exp(predict(fit_lasso_cv, as.matrix( test_data[, !(colnames(test_data) == "Price")] ), s = "lambda.min"))
sqrt(mean((test_data$Price - test_pred) ^ 2)) # 402914.1
# Coefficients
coeff_mat = as.matrix(coef(fit_lasso_cv, s = "lambda.min"))
coeff_df = as.data.frame(coeff_mat)
coeff_df$vars = rownames(coeff_df)
colnames(coeff_df) = c("coef", "vars")
coeff_df$impact = ifelse(coeff_df$coef < 0, "negative", "positive")
coeff_df = coeff_df[order(coeff_df$coef), ]
coeff_df$vars = factor(coeff_df$vars, levels = coeff_df$vars)
coeff_df = coeff_df[coeff_df$vars != "(Intercept)", ]

ggplot(coeff_df, aes(x=vars, y=coef, label=coef)) + 
  geom_bar(stat='identity', aes(fill=impact), width=.5)  +
  scale_fill_manual(name="Impact", 
                    labels = c("Negative", "Positive"), 
                    values = c("positive"="#00ba38", "negative"="#f8766d")) + 
  labs(x = "Variables", y = "Coefficients") + theme_bw() + 
  coord_flip()

#### KNN

default_knn_mod = train(
  Price ~ .,
  data = train_data,
  method = "knn",
  trControl = trainControl(method = "cv", number = 5),
  tuneGrid = expand.grid(k = seq(3, 30, by = 6))
)
plot(default_knn_mod)
# Best configuration
default_knn_mod$results[default_knn_mod$results$RMSE == min(default_knn_mod$results$RMSE), ]
# test rmse
test_pred = predict(default_knn_mod, newdata = as.data.frame( test_data[, !(colnames(test_data) == "Price")] ))
test_pred_knn = predict(default_knn_mod, newdata = as.data.frame( test_data[, !(colnames(test_data) == "Price")] ))
sqrt(mean((test_pred - test_data$Price) ^ 2)) # 372030.7
ggplot(data = test_data, aes(x=test_data$Price, y=(test_pred-test_data$Price))) +
  geom_point(color="dodgerblue") + theme_bw() + labs(x = "Actual Test Price", y = "Error (Predicted - Actual)") +
  geom_hline(yintercept = 0, color = "red")
# Best config
default_knn_mod$results[default_knn_mod$results$RMSE == min(default_knn_mod$results$RMSE), ]


# GBM
ntrees = c(1000, 2000, 2500)
interationdepth = c(1, 5, 9)
res = data.frame(n.trees = integer(), interaction.depth = integer(), trainRMSE = double(), testRMSE = double())
bestmodel = NULL
minrmse = 99999999
for (nt in ntrees)
{
  for (id in interationdepth)
  {
    gbm1 = gbm(Price ~ ., data = train_data, distribution="gaussian", n.minobsinnode = 20, n.trees=2000, 
               shrinkage=0.1, interaction.depth=1, bag.fraction=0.8)
    test_pred = predict(gbm1, newdata = as.data.frame( test_data[, !(colnames(test_data) == "Price")] ), n.trees = nt)
    testrmse = sqrt(mean((test_data$Price - test_pred) ^ 2))
    if(minrmse > testrmse)
    {
      bestmodel = gbm1
    }
    res = rbind(res, c(nt, id, sqrt(mean(gbm1$train.error)), testrmse))
    print(paste0(nt, " ", id, " done"))
  }
}

colnames(res) = c("n.trees", "interaction.depth", "trainRMSE", "testRMSE")
# Min RMSE configuration
res[res$testRMSE == min(res$testRMSE), ] # 2000                 1  386418.8 393516.6
# plot n.trees vs int depth with test RMSE
ggplot(data = res, aes(x=n.trees, y=interaction.depth)) +
  geom_point(aes(size = testRMSE), color="dodgerblue") + theme_bw()
# residue plot
test_pred = predict(bestmodel, newdata = as.data.frame( test_data[, !(colnames(test_data) == "Price")] ), n.trees = 2500)
test_pred_gbm = predict(bestmodel, newdata = as.data.frame( test_data[, !(colnames(test_data) == "Price")] ), n.trees = 2500)
ggplot(data = test_data, aes(x=test_data$Price, y=(test_pred-test_data$Price))) +
  geom_point(color="dodgerblue") + theme_bw() + labs(x = "Actual Test Price", y = "Error (Predicted - Actual)") +
  geom_hline(yintercept = 0, color = "red")
# importance plot
imp = as.data.frame(summary(bestmodel))
imp = imp[order(imp$rel.inf), ]
imp$var = factor(imp$var, levels = imp$var)
ggplot(tail(imp, 30), aes(x=var, y=rel.inf)) + 
  geom_bar(stat='identity', fill="dodgerblue", color="black", width=.5)  +
  labs(x = "Variables", y = "Relative Importance") + theme_bw() + 
  coord_flip()


############## Question 4 #####################
test_pred_stack = (0.3*test_pred_gbm + 0.4*test_pred_knn + 0.2*test_pred_lasso + 0.1*test_pred_ridge)
# Test RMSE
sqrt(mean((test_data$Price - test_pred_stack) ^ 2)) #363988.7


