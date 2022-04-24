## rank hospital
## This function returns the hospital with the corresponding rank in the outcome
## provided. It takes 3 arguments:
## state, the two letter abbreviation, where the hospital is located
## outcome, the death rate queried (either "heart attack", "heart failure", 
## or "pneumonia")
## num, the rank of the hospital desired. valid entries are integers, "best",
## or "worst".

rankhospital <- function(state, outcome, num = "best") {
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
        
        ##Return hospital name in that state with the given rank
        ##30-day death rate
        
        #first, pull the columns we need
        final_data <- final_data[,c(1,correctColumn)]
        #now we eliminate NAs
        final_data <- na.omit(final_data)
        #order the data, first by outcome, then by name
        final_data <- final_data[
                order(final_data[,2], final_data[,1]),]
        #create a rank vector by counting the rows
        rank <- (1:nrow(final_data))
        #create a new column called rank
        final_data <- cbind(final_data, rank)
        
        #what about num? what if someone writes "best" or "worst"?
        if (num == "best") {
                num <- 1
        } else if (num == "worst") {
                num <- nrow(final_data)
        }
        #print out the hospital name that corresponds with the
        #rank requested
        #if the rank requested is larger than the number of possible
        #hospitals, return NA
        if (num > nrow(final_data)) {
                print(NA)
        } else {
        print(final_data[final_data$rank == num,1])
        }
}

##QUIZ##
rankhospital("NC", "heart attack", "worst")
rankhospital("WA", "heart attack", 7)
rankhospital("TX", "pneumonia", 10)
rankhospital("NY", "heart attack", 7)

# rankhospital("TX", "heart failure", 4)
# rankhospital("MD", "heart attack", "worst")
# rankhospital("MN", "heart attack", 5000)
