## This function returns the hospital name in the state that has the
# lowest 30-day mortality rate
# It takes two arguments: state (two letter abbreviation)
# and outcome, either "heart attack", "heart failure", or "pneumonia"
# it returns the hospital in that state with the lowest 30 day mortality rate
# for that specified outcome.
# in case of ties, it returns the hospital that comes first in the alphabet

best <- function(state, outcome) {
        ##read outcome data
        #read the spreadsheet and write to data
        data <- read.csv("outcome-of-care-measures.csv", 
                         colClasses = "character")
        #pull only columns that we need
        coldata <- data[,c(2,7,11,17,23)]
        #change last 3 columns from character to integer
        #this line throws a warning about coercing NAs, so I suppressed it
        suppressWarnings(coldata[3:5] <- 
                                 lapply(coldata[3:5], FUN = as.numeric))
        #get rid of NAs
        coldata <- na.omit(coldata)
        #rename columns
        colnames(coldata) <- c("hospitalname", "state", "heartattack", 
                               "heartfailure", "pneumonia")

        ##check that state and outcome are valid
        #first, create a vector that contains all the state abbreviations
        valid_states <- coldata$state
        #then, check the state argument against the list
        if (state %in% valid_states) {
                #if it's a valid state, make a variable
                #with only the state data you need
                final_data <- coldata[coldata$state == state,]
        }
        #if it isn't a valid state, stop() the function
        else {
                stop("invalid state")
        }
        #now check the outcome, is it valid?
        #first, strip the space out of the input argument outcome
        #so I can match the outcome input with the column names
        outcome <- gsub(" ","", outcome)
        #make a vector with the column names
        valid_outcomes <- colnames(coldata)
        #does the outcome match a column name?
        if (outcome %in% valid_outcomes) {
                #if it's a valid outcome, which column is it?
                #this returns the column index to select the right column later
                correctColumn <- (which(valid_outcomes == outcome))
        }
        #if it isn't a valid outcome, stop() the function
        else {
                stop("invalid outcome")
        }
        ##return hospital name in that state with lowest 30 day death rate
        #calculate the lowest 30 day death rate of the correct column
        #and make it a new variable
        min_days <- min(final_data[,correctColumn])
        #find the rows that have the equivalent min value, make new variable
        result <- subset(final_data, final_data[,correctColumn] == min_days)
        #alphabetize by hospital name, to return the correct hospital
        result <- result[order(result$hospitalname),]
        #print just the first name of the list
        print(result[1,1])
}

##quiz##
best("SC", "heart attack")
best("NY", "pneumonia")
best("AK", "pneumonia") #I didn't get the right answer on this one?

# best("TX", "heart attack")
# best("TX", "heart failure")
# best("MD", "heart attack")
# best("MD", "pneumonia")
# best("BB", "heart attack")
# best("NY", "hert attack")
