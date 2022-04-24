## rank all
## this function returns a list of hospitals, one in each state, by their state
## ranking on a given outcome.
## it has two arguments, outcome ("heart attack", "heart failure", or 
## "pneumonia")
## and num, which is the rank of the hospital.

rankall <- function(outcome, num = "best") {
        ## Read outcome data
        
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
        colnames(coldata) <- c("hospital", "state", "heartattack", 
                               "heartfailure", "pneumonia")
        
        ## check that state and outcome are valid (why state?)
        
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
        
        ## For each state, find the hospital of the given rank
        
        ## so what I want to do is loop through the data frame, and find
        ## the hospital of a given rank in each state, and add it to a 
        ## new dataframe
        
        #first, pull the columns we need
        final_data <- coldata[,c(1,2,correctColumn)]
        #now we eliminate NAs
        final_data <- na.omit(final_data)
        #create a vector that contains all the state abbreviations
        valid_states <- unique(coldata$state)
        #create an empty dataframe to gather all the ranked hospitals
        results_data <- data.frame()
        
        #then loop through the state abbreviations, using rankhospital.R
        #as a guide:
        #but first, initialize a vector to store whether or not
        #the num input is "worst"
        reservednum <- "foo"
        for (i in valid_states) {
                #first, pull just that state data
                state_data <- final_data[final_data$state == i,]
                #order the data, first by outcome, then by name
                state_data <- state_data[
                        order(state_data[,3], state_data[,1]),]
                #create a rank vector by counting the rows
                rank <- (1:nrow(state_data))
                #create a new column in state_data called rank
                state_data <- cbind(state_data, rank)
                
                #what about num? what if someone writes "best" or "worst"?
                if (num == "best") {
                        num <- 1
                } else if (num == "worst") {
                        #have to save the initial num was "worst"
                        reservednum <- "worst"
                        #set num to the # of rows (last row)
                        num <- nrow(state_data)
                }
                #if the rank requested is larger than the number of possible
                #hospitals, return NA
                if (num > nrow(state_data)) {
                        ##it should do something different here
                        results_data <-rbind(results_data, 
                                             c(NA, i))
                        
                } else {
                results_data <- rbind(results_data, state_data
                                      [state_data$rank == num, ] )
                }
                state_data <- NULL
                #from the if/else if above, if num == "worst" the function
                #overrites num, this resets it.
                if (reservednum == "worst") {
                        num <- "worst"
                }
                
        }
        #orders the results_data df by state 
        #(it wasn't in alpha order initially)
        results_data <- results_data[
                order(results_data[,2]),]
        #return just the hospital and state
        results_data[,1:2]
}

## QUIZ ##
r <- rankall("heart attack", 4)
as.character(subset(r, state == "HI")$hospital)
r <- rankall("pneumonia", "worst")
as.character(subset(r, state == "NJ")$hospital)
r <- rankall("heart failure", 10)
as.character(subset(r, state == "NV")$hospital)

# head(rankall("heart attack", 20), 10)
# tail(rankall("pneumonia", "worst"),3)
# tail(rankall("heart failure"),10)
