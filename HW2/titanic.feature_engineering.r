## load the library
library(rpart)
library(lattice)


# titanic.data <- read.csv("bootcamp/Datasets/titanic.csv")
#titanic.data <- read.csv("dsd-workspace/Datasets/Titanic/train.csv")
titanic.data <- read.csv("dsd-workspace/Datasets/Titanic/test.csv")

## Comment out on test set since it's missing Survived
#titanic.data$Survived <- as.factor(titanic.data$Survived)
#levels(titanic.data$Survived) <- c("Died", "Survived")

## Feature Engineering

# Split Name by ","
lname.remainder <- strsplit(as.character(titanic.data$Name), ",")

# Turn into data frame
lname.remainder.dframe <- data.frame(Reduce(rbind, lname.remainder))

# Relabel
names(lname.remainder.dframe) <- c("LastName", "RemainderName")

# Add names
titanic.named <- cbind(titanic.data, lname.remainder.dframe)

# Process Last Name Frequency
last.name.freq <- data.frame(table(titanic.named$LastName))

names(last.name.freq) <- c("lname", "LastNameFreq")

# Merge name frequency back onto titanic
titanic.named.freq <- merge(titanic.named, last.name.freq, by.x="LastName", by.y="lname" )

#Plot
# densityplot(titanic.named.freq$Freq, groups=titanic.named.freq$Survived)


split.names <- strsplit(trimws(as.character(titanic.named.freq$RemainderName)), " ")

# cabin.dframe <- data.frame(Reduce(rbind, strsplit(titanic.data$Cabin, " ")))

name.title <- data.frame(cbind(sapply(split.names, `[`, 1)))
names(name.title) <- c("Title")

titanic.named.freq.title <- cbind(titanic.named.freq, name.title)

titanic.total <- titanic.named.freq.title

#write.csv(titanic.total, "dsd-workspace/HW2/titanic.train.feature.enhance.csv")
 write.csv(titanic.total, "dsd-workspace/HW2/titanic.test.feature.enhance.csv")