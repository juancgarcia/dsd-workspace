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
# titanic.hw.test <- fix.survived.col(titanic.hw.test)
# titanic.hw.test$Survived <- integer(nrow(titanic.hw.test))

titanic.hw.train.len <- nrow(titanic.hw.train)
titanic.hw.test.len <- nrow(titanic.hw.test)

collossus <- rbind(titanic.hw.train, titanic.hw.test)

collossus.eng <- titanic.feature.engineering(collossus)

titanic.hw.train.eng <- head(collossus.eng, n=titanic.hw.train.len)
titanic.hw.test.eng <- tail(collossus.eng, n=titanic.hw.test.len)

write.csv(titanic.hw.train.eng, "dsd-workspace/HW3/titanic.train.feature.enhanced.csv", row.names=F)
write.csv(titanic.hw.test.eng,  "dsd-workspace/HW3/titanic.test.feature.enhanced.csv", row.names=F)