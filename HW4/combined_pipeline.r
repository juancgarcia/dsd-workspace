## load the libraries
library(rpart)
library(lattice)

## load the library
library(rpart)
library(lattice)
library(plyr)

## Feature Engineering

ticket.prefix.col <- function(dataframe){
    ticket.column <- as.character(dataframe$Ticket)
    
    # Split Tickets column
    ticket.splits <- strsplit(ticket.column, " ")

    # Get prefix as first split
    ticket.prefixes <- data.frame(cbind(sapply(ticket.splits, `[`, 1)))

    # Make a new data frame of prefixes and original ticket names
    ticket.dframe <- cbind(ticket.prefixes, ticket.column)
    names(ticket.dframe) <- c("TicketPrefix", "Ticket")

    # Assign class for simple vs prefixed ticket
    ticket.plain <- data.frame(as.character(ticket.dframe$TicketPrefix) == as.character(ticket.dframe$Ticket))
    ticket.dframe <- cbind(ticket.dframe, ticket.plain)
    names(ticket.dframe)[3] <- "TicketSimple"

    # Enforce Character just in case it was cast to Factor
    ticket.dframe$TicketPrefix <- as.character(ticket.dframe$TicketPrefix)

    # Set "plain" tickets
    ticket.dframe[ticket.dframe$TicketSimple, ]$TicketPrefix <- "plain"
    ticket.dframe$TicketPrefix <- as.factor(ticket.dframe$TicketPrefix)
    ticket.dframe[,"TicketPrefix"]
}

fix.survived.col <- function (dataframe) {
    outframe <- dataframe

    if(is.null(outframe$Survived)) {
        # make a dummy column (everbody died)
        outframe$Survived <- integer(nrow(dataframe))
    }
    outframe$Survived[is.na(outframe$Survived)] <- 0
    outframe$Survived <- as.factor(outframe$Survived)
    # levels(outframe$Survived) <- c("Died", "Survived")
    levels(outframe$Survived) <- c(0, 1)
    outframe[,"Survived"]
}

lname.freq.title.cols <- function(dataframe) {
    result.dframe <- dataframe

    # Split Name by ","
    lname.remainder <- strsplit(as.character(result.dframe$Name), ",")

    # Turn into data frame
    lname.remainder <- data.frame(Reduce(rbind, lname.remainder))

    # Relabel
    names(lname.remainder) <- c("LastName", "RemainderName")

    # # Add names
    # result.dframe <- cbind(result.dframe, lname.remainder)

    # Process Last Name Frequency
    last.name.freq <- data.frame(table(lname.remainder$LastName))

    names(last.name.freq) <- c("LastName", "LastNameFreq")

    # Join last name frequency back with names
    names.dframe <- join(lname.remainder, last.name.freq, by="LastName", type="left" )

    #Plot
    # densityplot(result.dframe$Freq, groups=result.dframe$Survived)


    split.rNames <- strsplit(trimws(as.character(names.dframe$RemainderName)), " ")

    # cabin.dframe <- data.frame(Reduce(rbind, strsplit(dataframe$Cabin, " ")))

    name.title <- data.frame(cbind(sapply(split.rNames, `[`, 1)))
    names(name.title) <- c("Title")

    names.dframe <- cbind(names.dframe, name.title)

    names.dframe
}

titanic.feature.engineering <- function(dataframe) {
    result.dframe <- dataframe

    ## Add Ticket Types
    result.dframe$TicketPrefix <- ticket.prefix.col(dataframe)

    ## Fix Survival
    result.dframe$Survived <- fix.survived.col(result.dframe)

    ## Substitute NA aged with median
    result.dframe$Age[is.na(result.dframe$Age)] <- median(dataframe$Age, na.rm=TRUE)

    # Age Tended to Survive
    result.dframe$AgeBelow14 <- result.dframe$Age < 14
    # Age Tended to Die
    result.dframe$AgeBetween25To32 <- result.dframe$Age > 25 && result.dframe$Age < 32

    ## LastName LastNameFreq Title Cols
    result.dframe <- cbind(result.dframe, lname.freq.title.cols(result.dframe))

    ## Doctors
    IsDoctor <- result.dframe$Title == "Dr."
    IsDoctor <- data.frame(IsDoctor)
    result.dframe <- cbind(result.dframe, IsDoctor)

    ## Clergy
    IsClergy <- result.dframe$Title == "Rev."
    IsClergy <- data.frame(IsClergy)
    result.dframe <- cbind(result.dframe, IsClergy)
    
    # remove.columns <- c("RemainderName", "LastName", "Name", "Age", "Ticket")
    # result.dframe <- result.dframe[ , -which(names(result.dframe) %in% remove.columns)]

    result.dframe
}


titanic.hw.train <- read.csv("dsd-workspace/Datasets/Titanic/train.csv")
titanic.hw.test <- read.csv("dsd-workspace/Datasets/Titanic/test.csv")
titanic.hw.test$Survived <- fix.survived.col(titanic.hw.test)
# titanic.hw.test$Survived <- integer(nrow(titanic.hw.test))

titanic.hw.train.len <- nrow(titanic.hw.train)
titanic.hw.test.len <- nrow(titanic.hw.test)

collossus <- rbind(titanic.hw.train, titanic.hw.test)

collossus.eng <- titanic.feature.engineering(collossus)

titanic.hw.train.eng <- head(collossus.eng, n=titanic.hw.train.len)
titanic.hw.test.eng <- tail(collossus.eng, n=titanic.hw.test.len)

write.csv(titanic.hw.train.eng, "dsd-workspace/HW4/titanic.train.feature.enhanced.csv", row.names=F)
write.csv(titanic.hw.test.eng,  "dsd-workspace/HW4/titanic.test.feature.enhanced.csv", row.names=F)



# titanic.total <- read.csv("dsd-workspace/HW4/titanic.train.feature.enhanced.csv")
# titanic.total$IsClergy <- as.factor(titanic.total$IsClergy)
# titanic.total$IsDoctor <- as.factor(titanic.total$IsDoctor)

## Add missing levels
# levels(titanic.total$Title) <- append(levels(titanic.total$Title), "Dona.")
# levels(titanic.total$TicketPrefix) <- append(levels(titanic.total$TicketPrefix), c("A.", "AQ/3.", "AQ/4", "LP", "SC/A.3", "SC/A4", "STON/OQ."))

# titanic.holdout <- read.csv("dsd-workspace/HW4/titanic.test.feature.enhanced.csv")
# titanic.holdout$IsClergy <- as.factor(titanic.holdout$IsClergy)
# titanic.holdout$IsDoctor <- as.factor(titanic.holdout$IsDoctor)

# titanic.bootcamp.eng <- read.csv("dsd-workspace/HW4/titanic.bootcamp.feature.enhanced.csv")

titanic.total <- titanic.hw.train.eng
titanic.holdout <- titanic.hw.test.eng

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
titanic.tree <- rpart(Survived~TicketPrefix+AgeBelow14+IsDoctor+IsClergy+Pclass+Title+LastNameFreq+Sex+Embarked, data = titanic.train, 
                       control=tree.params, parms=list(split="gini"))
titanic.rf.model <- randomForest(Survived~TicketPrefix+AgeBelow14+IsDoctor+IsClergy+Pclass+Title+LastNameFreq+Sex+Embarked, data=titanic.train, ntree=250, maxnodes=10)
varImpPlot(titanic.rf.model)


## MODEL EVALUATION
## make prediction using decision model
titanic.test.predictions <- predict(titanic.rf.model, titanic.test, type="class")
titanic.total.predictions <- predict(titanic.rf.model, titanic.total, type="class")
titanic.holdout.predictions <- predict(titanic.rf.model, titanic.holdout, type="class")


head(titanic.test.predictions)
head(titanic.total.predictions)
head(titanic.holdout.predictions)


## Extract the test data survival to build the confusion matrix
titanic.test.confusion <- table(titanic.test.predictions, titanic.test$Survived)
titanic.total.confusion <- table(titanic.total.predictions, titanic.total$Survived)
titanic.holdout.confusion <- table(titanic.holdout.predictions, titanic.holdout$Survived)

head(titanic.test.confusion)
head(titanic.total.confusion)
head(titanic.holdout.confusion)

## prepare results
results <- data.frame(titanic.holdout.predictions)
names(results)[1] <- "Survived"

## Restore integer levels
levels(results$Survived) <- c(0,1)

## Attach PassengerId
results <- cbind(titanic.holdout["PassengerId"], results)

write.csv(results, file="dsd-workspace/HW4/titanic.test.results.rf.HW4.csv", row.names = F, quote = F)