rankall <- function(outcome, num="best") {
        ## Read outcome data
        dat <- read.csv("rprog-data-ProgAssignment3-data/outcome-of-care-measures.csv", colClasses = "character")
        ## Check that state and outcome are valid
        
        if (!((outcome == "heart failure") | (outcome == "heart attack") | (outcome == "pneumonia"))){
                stop("Invalid outcome")
        }
        
        col <- if (outcome == "heart failure") {
                17
        }
        else if (outcome == "heart attack"){
                11
        }
        else if ( outcome == "pneumonia"){
                23
        }
        
        
        ## Return hospital name in that state with the given rank
        dat[, col] <-suppressWarnings(as.numeric(dat[, col]))
        dat[, 2] <- as.character(dat[, 2])
        dat <- dat[order(dat[,7], dat[,col], dat[,2], na.last=NA),]
        state <- as.character(dat[, 7])
        
        hospital_state <- split(dat[,2], state)
        
        rankHospitals <- function(x, num) {
                if (num=="best") {
                        head(x, 1)
                } else if (num=="worst") {
                        tail(x, 1)
                } else {
                        x[num]
                }
        }
        
        result <- lapply(hospital_state, rankHospitals, num)
        org_result <- as.data.frame(unlist(result), state= names(result), row.names(state))
        colnames(org_result) <- ("hospital")
        org_result
        
 
}
 
     