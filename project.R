# 1
install.packages("readxl")
library("readxl")

df <- read_excel("/Data8.xlsx")
df <- df[,!names(df) %in% c("sp")]

# 2
df$lithotype="S5"
df$lithotype[df$vsh<0.2]="S1"
df$lithotype[df$vsh>0.2 & df$vsh<0.4]="S2"
df$lithotype[df$vsh>0.4 & df$vsh<0.6]="S3"
df$lithotype[df$vsh>0.6 & df$vsh<0.8]="S4"


# 3
Dimensions of the dataset
df_dimension=dim(df)

# Types of the attributes for each variable
for (i in df)
{
print(typeof(i))
}

Peek at the data itself (print first few rows)
print(df[1:7,])

# Levels of the class attribute.
print(attributes(df)$names)
print(attributes(df)$class)

# Breakdown of the instances in each class
for (i in df)
{
print(levels(as.factor(i)))
}

# Statistical summary of all attributes.
statistical_summary=summary(df)


# 4
# Draw boxplots of facies vs depth
options(repr.plot.width=2, repr.plot.height=2)
boxplot(df$facies,df$depth,main="facies vs depth",
xlab="Selected columns",
ylab="Value",
names = c("facies","depth"))


# #  for lithotypes vs depth
install.packages("ggplot2")
library("ggplot2")
ggplot(df, aes(x=depth, y=lithotype, color=lithotype)) + geom_point()
boxplot(as.numeric(df$lithotype),df$depth,main="lithotypes vs depth",
xlab="Selected columns",
ylab="Value",
names = c("lithotypes","depth"))

# 5

install.packages("e1071")
install.packages("caTools")
install.packages("class")
library(e1071)
library(caTools)
library(class)
split = floor(0.75 * nrow(df))
print(split)
train_idx = sample(seq_len(nrow(df)), size = split)
train = df[train_idx, ]
test = df[-train_idx, ]


# K Nearest Neighbor
# for facies
train_scale_facies = scale(train[, 1:2])
test_scale_facies = scale(test[, 1:2])
knn = knn(train = train_scale_facies,test = test_scale_facies,cl = train$facies,k = 3)
confussion_matrix_knn = table(test$facies, knn)
print(confussion_matrix_knn)
accuracy_knn = 1-mean(knn != test$facies)
print(accuracy_knn)

# # for lithotype
train_scale_lithotype = scale(train[, 1:3])
test_scale_lithotype = scale(test[, 1:3])
knn = knn(train = train_scale_lithotype,test = test_scale_lithotype,cl = train$lithotype,k = 8)
confussion_matrix_knn = table(test$lithotype, knn)
print(confussion_matrix_knn)
accuracy_knn = 1-mean(knn != test$lithotype)
print(accuracy_knn)


# # Support Vector Machine
# # for facies
svm <- svm(facies ~ ., data=train, type = 'C-classification',kernel="linear")
y_pred_svm = predict(svm, newdata = test)
confussion_matrix_svm = table(test$facies, y_pred_svm)
print(confussion_matrix_svm)
accuracy_svm = 1-mean(y_pred_svm != test$facies)
print(accuracy_svm)

# # for lithotype
svm <- svm(lithotype ~ ., data=train, type = 'C-classification',kernel="linear")
y_pred_svm = predict(svm, newdata = test)
confussion_matrix_svm = table(test$lithotype, y_pred_svm)
print(confussion_matrix_svm)
accuracy_svm = 1-mean(y_pred_svm != test$lithotype)
print(accuracy_svm)

# Random Forest 
urlPackage <- "https://cran.r-project.org/src/contrib/Archive/randomForest/randomForest_4.6-12.tar.gz"
install.packages(urlPackage, repos=NULL, type="source") 
install.packages('caret')
library(randomForest)
library(datasets)
library(caret)

# for facies
rf <- randomForest(train$facies ~ ., data = train, importance = TRUE,proximity = TRUE, sampsize=c(20, 30, 20))
y_pred_rf = predict(rf, newdata = test)
confussion_matrix_rf = table(test$facies, y_pred_rf)
print(confussion_matrix_rf)
accuracy_rf = 1-mean(y_pred_rf != test$facies)
print(accuracy_rf)


for lithotype
rf <- randomForest(as.factor(lithotype) ~ ., train, importance = TRUE,proximity = TRUE, sampsize=c(2, 3, 2))
# rf <- randomForest(train[,1:4], as.factor(train$lithotype), sampsize=c(20, 30, 20))
y_pred_rf = predict(rf, newdata = test)
confussion_matrix_rf = table(test$lithotype, y_pred_rf)
print(confussion_matrix_rf)
accuracy_rf = 1-mean(y_pred_rf != test$lithotype)
print(accuracy_rf)
# rf2 <- randomForest(iris[1:4], iris$Species, sampsize=c(20, 30, 20))