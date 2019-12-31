#########################################
############ AG Santa in R ##############
#########################################

#Load Libraries
library(dplyr)
library(data.table)

#Load Files
family_data <- data.table::fread("santa_data/family_data.csv")
submission <- data.table::fread("santa_data/sample_submission.csv")

# Function to calculate score
calculate_score <- function(data,submission){
  
  data <- as.data.table(data)
  submission <- as.data.table(submission)
  
  cols <- paste0("choice_",seq(0,9))
  fix_cost <- c(0,50,50,100,200,200,300,300,400,500,500)
  per_member_cost <- c(0,0,9,9,9,18,18,36,36,235,434)
  
  data[submission, on="family_id", assigned_day := i.assigned_day][
    , (cols) := lapply(.SD, "==", data$assigned_day), .SDcols = cols][ 
      ,choice_none := ifelse(rowSums(.SD) == 0, TRUE, FALSE), .SDcols = cols]
  
  cols <- c(cols,"choice_none")
  
  # Total Preference cost
  preference_cost <- sum(data[ ,lapply(.SD,sum), .SDcols=cols] * fix_cost) + 
    sum(data[ , lapply(.SD, "*", data$n_people), .SDcols = cols][ 
      , lapply(.SD,sum),.SDcols=cols] * per_member_cost)
  
  dt <- data[, lapply(.SD,sum), by=list(assigned_day), .SDcols = "n_people"][order(assigned_day)]
  dt <- rbind(dt, data.frame(assigned_day = 101,dt[100,2]))
  
  # Total Accounting cost
  accounting_penalty <- dt[ , calc := ((n_people - 125)/400) * (n_people ^ ((abs(Reduce('-', shift(n_people, 0:1,type = "lead"))))/50 + .5) )][ 
    1:100, lapply(.SD,sum),.SDcols="calc"]
  #accounting_penalty <- dt[ , calc := ((n_people - 125)/400) * (n_people ^ ((abs(Reduce(`-`, shift(n_people, 0:1))))/50 + .5) )][
  #                          2:101, lapply(.SD,sum),.SDcols="calc"]
  
  score = as.numeric(preference_cost + accounting_penalty)
}

print('cost calculation with data.table is............:')
cost <- calculate_score(family_data, submission)
print(cost)


#Benchmarking
library(microbenchmark)

print(microbenchmark('data.table' = {calculate_score(data, submission)))