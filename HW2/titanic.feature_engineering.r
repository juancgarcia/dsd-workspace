## load the library
library(rpart)
library(lattice)


titanic.bootcamp <- read.csv("bootcamp/Datasets/titanic.csv")
titanic.hw.train <- read.csv("dsd-workspace/Datasets/Titanic/train.csv")
titanic.hw.test <- read.csv("dsd-workspace/Datasets/Titanic/test.csv")

## Feature Engineering

ticket.prefix <- function(dataframe){
    dataframe$Ticket <- as.character(dataframe$Ticket)
    
    # Split Tickets column
    ticket.splits <- strsplit(as.character(dataframe$Ticket), " ")

    # Get prefix as first split
    ticket.prefixes <- data.frame(cbind(sapply(ticket.splits, `[`, 1)))

    # Make a new data frame of prefixes and original ticket names
    ticket.dframe <- cbind(ticket.prefixes,(dataframe$Ticket))
    names(ticket.dframe) <- c("TicketPrefix", "Ticket")

    # Assign class for simple vs prefixed ticket
    ticket.plain <- data.frame(as.character(ticket.dframe$TicketPrefix) == as.character(ticket.dframe$Ticket))
    ticket.dframe <- cbind(ticket.dframe, ticket.plain)
    names(ticket.dframe)[3] <- "TicketSimple"

    # Enforce Character just in case it was cast to Factor
    ticket.dframe$TicketPrefix <- as.character(ticket.dframe$TicketPrefix)

    # Set "plain" tickets
    ticket.dframe[ticket.dframe$TicketSimple, ]$TicketPrefix <- "plain"
    ticket.dframe$Ticket <- as.factor(ticket.dframe$Ticket)
    ticket.dframe
}

add.level.survial <- function (dataframe) {
    result.dframe <- dataframe

    if(!is.null(dataframe$Survived)) {
        result.dframe$Survived <- as.factor(dataframe$Survived)
    } else {
        # make a dummy column (everbody died)
        result.dframe$Survived <- integer(nrow(dataframe))
    }
    levels(result.dframe$Survived) <- c("Died", "Survived")
    result.dframe
}

set.titles <- function(dataframe) {

}

titanic.feature.engineering <- function(dataframe) {
    result.dframe <- dataframe

    ## Alter Tickets to type
    result.dframe$TicketPrefix <- as.factor(ticket.prefix(dataframe)$TicketPrefix)

    ## Fix Survival
    result.dframe <- add.level.survial(result.dframe)

    ## Substitute NA aged with median
    result.dframe$Age[is.na(result.dframe$Age)] <- median(dataframe$Age, na.rm=TRUE)

    # Age Tended to Survive
    result.dframe$AgeBelow14 <- result.dframe$Age < 14
    # Age Tended to Die
    result.dframe$AgeBetween25To32 <- result.dframe$Age > 25 && result.dframe$Age < 32

    # Split Name by ","
    lname.remainder <- strsplit(as.character(result.dframe$Name), ",")

    # Turn into data frame
    lname.remainder.dframe <- data.frame(Reduce(rbind, lname.remainder))

    # Relabel
    names(lname.remainder.dframe) <- c("LastName", "RemainderName")

    # Add names
    dataframe.named <- cbind(result.dframe, lname.remainder.dframe)

    # Process Last Name Frequency
    last.name.freq <- data.frame(table(dataframe.named$LastName))

    names(last.name.freq) <- c("lname", "LastNameFreq")

    # Merge name frequency back onto titanic
    dataframe.named.freq <- merge(dataframe.named, last.name.freq, by.x="LastName", by.y="lname" )

    #Plot
    # densityplot(dataframe.named.freq$Freq, groups=dataframe.named.freq$Survived)


    split.names <- strsplit(trimws(as.character(dataframe.named.freq$RemainderName)), " ")

    # cabin.dframe <- data.frame(Reduce(rbind, strsplit(dataframe$Cabin, " ")))

    name.title <- data.frame(cbind(sapply(split.names, `[`, 1)))
    names(name.title) <- c("Title")

    dataframe.named.freq.title <- cbind(dataframe.named.freq, name.title)
    
    remove.columns <- c("RemainderName", "LastName", "Name", "Age")
    result.dframe <- dataframe.named.freq.title[ , -which(names(dataframe.named.freq.title) %in% remove.columns)]

    result.dframe
}

titanic.bootcamp.eng <- titanic.feature.engineering(titanic.bootcamp)
titanic.hw.train.eng <- titanic.feature.engineering(titanic.hw.train)
titanic.hw.test.eng <- titanic.feature.engineering(titanic.hw.test)

write.csv(titanic.bootcamp.eng, "dsd-workspace/HW2/titanic.bootcamp.feature.enhanced.csv", row.names=F)
write.csv(titanic.hw.train.eng, "dsd-workspace/HW2/titanic.train.feature.enhanced.csv", row.names=F)
write.csv(titanic.hw.test.eng,  "dsd-workspace/HW2/titanic.test.feature.enhanced.csv", row.names=F)