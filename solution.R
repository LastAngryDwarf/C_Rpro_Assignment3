##Stuff goes here


## Part 1: Plot the 30-day mortality rates for heart attack
outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
head(outcome)
ncol(outcome)
names(outcome)
str(outcome)

outcome[,11] <- as.numeric(outcome[,11]) #switch from char to num
hist(outcome[,11]) #histogram, has to be numeric to create a histogram

#Part 2: Finding the best hospital in a state

best <- function(state, outcome) {
        ##read outcome data
        #read the spreadsheet and write to data
        data <- read.csv("outcome-of-care-measures.csv", 
                            colClasses = "character")
        #pull only columns that we need
        coldata <- data[,c(2,7,11,17,23)]
        #rename columns
        colnames(coldata) <- c("hospital_name", "state", "heart_attack", 
                               "heart_failure", "pneumonia")
        #pull out not available rows
        subset_coldata <-subset(coldata, heart_attack != "Not Available" &
                                heart_failure != "Not Available" &
                                pneumonia != "Not Available")
        
        ##check that state and outcome are valid
        valid_states <- subset_coldata$state
        if (state %in% valid_states) {
                #if it's a valid state, make a variable
                #with only the state data you need
                final_data <- subset_coldata[subset_coldata$state == state,]
                
        }
                #if it isn't a valid state, stop() the function
        else {
                stop("invalid state")
        }
        
        valid_outcome <- colnames(coldata)
        if (outcome %in% valid_outcome) {
                #if it's a valid outcome, change final_data
                #to only have the data you need
        }
        else {
                stop("invalid outcome")
        }
        ##return hospital name in that state with lowest 30 day death rate
        if (outcome == "heart_attack") {
                
                final_data[,3] <- as.numeric(final_data[,3])
                result <- subset(final_data, heart_attack == min(heart_attack))
                result[order(result$hospital_name),]
                print(result[1,1])
        } else if (outcome == "heart_failure") {
               
                final_data[,4] <- as.numeric(final_data[,4])
                result <- subset(final_data, heart_failure == 
                                         min(heart_failure))
                result[order(result$hospital_name),]
                print(result[1,1])
        } else if (outcome == "pneumonia") {
                
                final_data[,5] <- as.numeric(final_data[,5])
                
                result <- subset(final_data, pneumonia == 
                                         min(pneumonia))
                result[order(result$hospital_name),]
                print(result[1,1])
        }
        
        
}
best("TX", "heart_attack")