## load the library
library(rpart)
library(lattice)

titanic.total <- read.csv("dsd-workspace/HW2/titanic.train.feature.enhanced.csv")
levels(titanic.total$Title) <- append(levels(titanic.total$Title), "Dona.")
titanic.holdout <- read.csv("dsd-workspace/HW2/titanic.test.feature.enhanced.csv")

## BUILD MODEL
## randomly choose 70% of the data set as training data
set.seed(777)
train.index <- sample(1:nrow(titanic.total), 0.7*nrow(titanic.total))
titanic.train <- titanic.total[train.index,]
dim(titanic.train)

## select the 30% left as the testing data
titanic.test <- titanic.total[-train.index,]
dim(titanic.test)



# # Default decision tree model
#     # Builds a decision tree from the titanic dataset to predict
#     # Survival given all other columns as predictors
# titanic.tree <- rpart(Survived~LastNameFreq+Sex+Age+Title+Embarked,data=titanic.train)

# ## VISUALIZE THE MODEL
# ## plot the tree structure
# plot(titanic.tree, margin=c(.25))
# title(main = "Decision Tree Model of Titanic Data")
# text(titanic.tree, use.n = TRUE)
# ## print the tree structure
# summary(titanic.tree)

# ## MODEL EVALUATION
# ## make prediction using decision model
# titanic.predictions <- predict(titanic.tree, titanic.test, type="class")
# head(titanic.predictions)



# ## Extract the test data survival to build the confusion matrix
# titanic.confusion <- table(titanic.predictions, titanic.test$Survived)


#### Parameter Tuning ####

## Setting control parameters for rpart
## Check ?rpart.control for what the parameters do
tree.params <- rpart.control(minsplit=20, minbucket=7, maxdepth=5, cp=0.01)

## Fit decision model to training set
## Use parameters from above and Gini index for splitting
titanic.tree <- rpart(Survived ~LastNameFreq+Sex+Age+Title+Embarked, data = titanic.train, 
                       control=tree.params, parms=list(split="gini"))


## MODEL EVALUATION
## make prediction using decision model
titanic.test.predictions <- predict(titanic.tree, titanic.test, type="class")
titanic.total.predictions <- predict(titanic.tree, titanic.total, type="class")
titanic.holdout.predictions <- predict(titanic.tree, titanic.holdout, type="class")


head(titanic.test.predictions)
head(titanic.total.predictions)
head(titanic.holdout.predictions)


## Extract the test data survival to build the confusion matrix
titanic.test.confusion <- table(titanic.test.predictions, titanic.test$Survived)
titanic.total.confusion <- table(titanic.total.predictions, titanic.total$Survived)
# titanic.holdout.confusion <- table(titanic.holdout.predictions, titanic.holdout$Survived)

head(titanic.test.confusion)
head(titanic.total.confusion)
# head(titanic.holdout.confusion)


results <- cbind(titanic.holdout, titanic.holdout.predictions)[,c(3,17)]
names(results)[2] = "Survived"
# titanic.with.result <- cbind(titanic.total, data.frame(titanic.result))

# head(titanic.with.result[,c(2,14)])

# results <- titanic.with.result[,c(2,14)]

## Restore integers
levels(results$Survived) <- c(0,1)
write.csv(results, file="dsd-workspace/HW2/titanic.test.results.HW2.csv", row.names = F, quote = F)