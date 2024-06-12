##### ENGEL McADAMS
##### DATA PREPARATION
##### 240120

library(dplyr)
library(stringr)

rm(list = ls())
graphics.off()
setwd("/Users/engel/Documents/word/Manuskript/Verhalten/Engel McAdams/Data/ReplicationPackage")

###### LOGIC FOR CHOICE DATA ----
# veh1: "vehicle"
veh1 <- read.delim("responses/response_veh1.txt",
                   header = FALSE)
# workaround to directly generate dataframe
veh1$hid <- 1:nrow(veh1)

##### rename choice variable
colnames(veh1)[which(names(veh1) ==
                       "V1")] <- "rchoice"

##### skip header
# repetition of treatment manipulation
veh1[grepl("^Response", veh1[[1]]),] -> veh1

##### true Nobs
veh1$id <- 1:nrow(veh1)
veh1$hid <- NULL

##### choice
#### check for lengthy explanations
veh1[veh1$rchoice %>% nchar() > 19,]

#### define lengthy explanations as NA
veh1$rchoice[veh1$rchoice %>% nchar() > 19] <- NA

#### extract choices
veh1$choice <- ifelse(str_detect(veh1$rchoice, "No"), 0,
                      ifelse(str_detect(veh1$rchoice, "Yes"), 1, NA))

##### condition
veh1$term <- "vehicle"

##### summarize results
table(veh1$choice)


###### LOGIC FOR BELIEF DATA ----
 # veh1: "vehicle"
veh1 <- read.delim("responses/response_bel1.txt",
                  header = FALSE)
  # workaround to directly generate dataframe
veh1$hid <- 1:nrow(veh1)
  
##### rename choice variable
colnames(veh1)[which(names(veh1) ==
                     "V1")] <- "rchoice"

##### skip header
  # repetition of treatment manipulation
veh1[grepl("^Response", veh1[[1]]),] -> veh1

##### true Nobs
veh1$id <- 1:nrow(veh1)
veh1$hid <- NULL

##### choice
#### check for lengthy explanations
veh1[veh1$rchoice %>% nchar() > 19,]

#### define lengthy explanations as NA
veh1$rchoice[veh1$rchoice %>% nchar() > 19] <- NA

#### extract choices
veh1$choice <- as.numeric(sub(".*:\\s(\\d+)", "\\1", veh1$rchoice))

##### condition
veh1$term <- "belief"

##### summarize results
table(veh1$choice)



###### DIRECT ----
  ##### write function ----
response <- function(key) {
  # Generate the file path for the response file
  response_file_path <- paste0("responses/response_veh", key, ".txt")
  
  # Read the response file
  response_data <- read.delim(response_file_path, header = FALSE)
  
  # Workaround to directly generate a data frame
  response_data$hid <- 1:nrow(response_data)
  
  # Rename the choice variable
  colnames(response_data)[which(names(response_data) == "V1")] <- "rchoice"
  
  # Skip header
  response_data <- response_data[grepl("^Response", response_data[[1]]), ]
  
  # Define true Nobs
  response_data$id <- 1:nrow(response_data)
  response_data$hid <- NULL
  
  # Check for lengthy explanations
  lengthy_explanations <- response_data[response_data$rchoice %>% nchar() > 19, ]
  
  # Define lengthy explanations as NA
  response_data$rchoice[response_data$rchoice %>% nchar() > 19] <- NA
  
  # Extract choices
  response_data$choice <- ifelse(str_detect(response_data$rchoice, "No"), 0,
                                 ifelse(str_detect(response_data$rchoice, "Yes"), 1, NA))
  
  # Define the condition
  response_data$term <- paste0("veh", key)
  
  # Skip excess data
    # do to the fact that the process has stopped prematurely
  response_data <- response_data[1:100, ]
  
  # generate summary, as input for next processing step
  result_summary <- table(response_data$choice)
  
  return(result_summary)
}

  ##### call function ----


keys <- c(1:25)
result_list <- lapply(keys, response)

  ##### extract data frame ----
#### Create an empty data frame to store the results
results <- data.frame(Choice_0 = integer(0), Choice_1 = integer(0), key = integer(0))

#### Process each element in the result_list
for (i in 1:length(result_list)) {
  # Create a data frame from the element
  df <- as.data.frame(result_list[[i]])
  
  # If "0" or "1" is missing, add it with a frequency of 0
  if (!"0" %in% df$Var1) {
    df <- rbind(df, data.frame(Var1 = "0", Freq = 0))
  }
  if (!"1" %in% df$Var1) {
    df <- rbind(df, data.frame(Var1 = "1", Freq = 0))
  }
  
  #### Append the key column
  df$key <- i
  
  #### Combine results
  results <- rbind(results, df)
}

#### Reset row names and convert to data frame
rownames(results) <- NULL

#### only keep number of "yes" responses
  # as frequency of "no" responses is implied

results %>% 
  filter(Var1 == 1) %>% 
  select(key, Freq) %>% 
  rename(yes = Freq) -> results

#### add vehicle names
codebook <- read.csv("tasks/vehicle_table002.csv",
                     header = FALSE, 
                     col.names = c("key", "name"))
#### remove article (if any)
codebook$name <- gsub("^(?:a|an)\\s+", "", codebook$name)

#### add to results
results <- left_join(codebook, results)
results_conc <- results
results_conc$cond <- "concept"
results_conc %>% 
  filter(key == c(1:25)) -> results_conc
save(results_conc,
     file = "results_conc.RData")

#### generate master key 
results$key_hat <- results$key - 25
results$master <- ceiling(results$key_hat/5)
results$key_hat[results$key < 26] <- results$key[results$key < 26]
results$master[results$key < 26] <- results$key[results$key < 26]
save(results,
     file = "results_allveh.RData")

###### CHAIN OF THOUGHT ----
  ##### write function ----
response <- function(key) {
  # Generate the file path for the response file
  response_file_path <- paste0("responses/response_def", key, ".txt")
  
  # Read the response file
  response_data <- read.delim(response_file_path, header = FALSE)
  
  # id
  response_data$id <- rep(1:(nrow(response_data)/2), each = 2)
  
  # target length
  all_rows <- nrow(response_data)/2
  
  # Extract "def" and "rchoice" from the first column
  response_data <- data.frame(
    id = response_data$id,
    def = response_data$V1[c(TRUE, FALSE)],
    rchoice = response_data$V1[c(FALSE, TRUE)]
  )
  
  # remove duplicates
  response_data <- response_data[1:all_rows,]
  
  # remove superfluous identifier
  response_data$id <- NULL
  
  # Check for lengthy explanations
  lengthy_explanations <- response_data[response_data$rchoice %>% nchar() > 38, ]
  
  # Define lengthy explanations as NA
  response_data$rchoice[response_data$rchoice %>% nchar() > 38] <- NA
  
  # Extract choices
  response_data$choice <- ifelse(str_detect(response_data$rchoice,
                                            regex("no",
                                                  ignore_case = TRUE)), 0, 1)
  
  # Define the condition
  response_data$term <- paste0("definition", key)
  
  # Skip excess data
  # do to the fact that the process has stopped prematurely
  response_data %>% 
    filter(!is.na(rchoice)) -> response_data
  response_data[1:100,] -> response_data
  
  # generate summary, as input for next processing step
  result_summary <- table(response_data$choice)
}

  ##### call function ----
keys <- c(1:25)
result_list <- lapply(keys, response)

  ##### extract data frame ----
#### Create an empty data frame to store the results
results_definition <- data.frame(Choice_0 = integer(0), Choice_1 = integer(0), key = integer(0))

#### Process each element in the result_list
for (i in 1:length(result_list)) {
  # Create a data frame from the element
  df <- as.data.frame(result_list[[i]])
  
  # If "0" or "1" is missing, add it with a frequency of 0
  if (!"0" %in% df$Var1) {
    df <- rbind(df, data.frame(Var1 = "0", Freq = 0))
  }
  if (!"1" %in% df$Var1) {
    df <- rbind(df, data.frame(Var1 = "1", Freq = 0))
  }
  
  #### Append the key column
  df$key <- i
  
  #### Combine results
  results_definition <- rbind(results_definition, df)
}

#### Reset row names and convert to data frame
rownames(results_definition) <- NULL

#### only keep number of "yes" responses
# as frequency of "no" responses is implied

results_definition %>% 
  filter(Var1 == 1) %>% 
  select(key, Freq) %>% 
  rename(yes = Freq) -> results_definition

#### add vehicle names
codebook <- read.csv("tasks/vehicle_table002.csv",
                     header = FALSE, 
                     col.names = c("key", "name"))
codebook <- codebook[1:25, ]

#### remove article (if any)
codebook$name <- gsub("^(?:a|an)\\s+", "", codebook$name)

#### add to results
results_definition <- left_join(codebook, results_definition)

#### add condition
results_definition$cond <- "definition"

#### rename estimate
names(results_definition)[names(results_definition) %in% "Var1"] <- "choice"

# View(results_definition)
save(results_definition,
     file = "results_definition.RData")

###### BELIEF CONTINUOUS ----
  ##### write function ----
response <- function(key) {
  # Generate the file path for the response file
  response_file_path <- paste0("responses/response_bel", key, ".txt")
  
  # Read the response file
  response_data <- read.delim(response_file_path, header = FALSE)
  
  # Workaround to directly generate a data frame
  response_data$hid <- 1:nrow(response_data)
  
  # Rename the choice variable
  colnames(response_data)[which(names(response_data) == "V1")] <- "rchoice"
  
  # Skip header
  response_data <- response_data[grepl("^Response", response_data[[1]]), ]
  
  # Define true Nobs
  response_data$id <- 1:nrow(response_data)
  response_data$hid <- NULL
  
  # Check for lengthy explanations
  lengthy_explanations <- response_data[response_data$rchoice %>% nchar() > 19, ]
  
  # Define lengthy explanations as NA
  response_data$rchoice[response_data$rchoice %>% nchar() > 19] <- NA
  
  # Extract choices
  response_data$choice <- as.numeric(sub(".*:\\s(\\d+)", "\\1", response_data$rchoice))
  
  # Define the condition
  response_data$term <- paste0("belief", key)
  
  # Skip excess data
  # due to the fact that the process has stopped prematurely
  response_data <- response_data[1:100, ]
  
  # generate summary, as input for next processing step
  result_summary <- table(response_data$choice)
  
  return(result_summary)
}

  ##### call function ----
# # Example usage for a specific key (replace '1' with your desired key)
# key_result <- process_response_file(1)
# print(key_result)

keys <- c(1:25)
result_list <- lapply(keys, response)

  ##### preprocess result data frames ----
# Create a template data frame with all possible values
template_df <- data.frame(Var1 = 0:100)

# Function to add missing values with a frequency of 0
add_missing <- function(df) {
  merged_df <- merge(template_df, df, by = "Var1", all = TRUE)
  merged_df$Freq[is.na(merged_df$Freq)] <- 0
  return(merged_df)
}

# Apply the function to each data frame in the list
result_list <- lapply(result_list, add_missing)

  ##### extract data frame ----
#### Create an empty data frame to store the results
results_belief <- data.frame(Choice_0 = integer(0), Choice_1 = integer(0), key = integer(0))

#### Process each element in the result_list
for (i in 1:length(result_list)) {
  # Create a data frame from the element
  df <- as.data.frame(result_list[[i]])
  
  #### Append the key column
  df$key <- i
  
  #### Combine results
  results_belief <- rbind(results_belief, df)
}

#### Reset row names and convert to data frame
rownames(results_belief) <- NULL

#### rename estimate
names(results_belief)[names(results_belief) %in% "Var1"] <- "bel"

#### change class of "bel" to numeric
results_belief$bel <- as.numeric(results_belief$bel)

#### calculate weighted belief
results_belief$wbel <- results_belief$bel * results_belief$Freq

#### summarize over vehicles
results_belief %>% 
  group_by(key) %>% 
  summarize(yes = mean(wbel)) -> results_belief

#### add vehicle names
codebook <- read.csv("tasks/vehicle_table002.csv",
                     header = FALSE, 
                     col.names = c("key", "name"))
codebook <- codebook[1:25, ]

#### remove article (if any)
codebook$name <- gsub("^(?:a|an)\\s+", "", codebook$name)

#### add to results
results_belief <- left_join(codebook, results_belief)

#### add condition
results_belief$cond <- "belief"

save(results_belief,
     file = "results_belief.RData")

###### BELIEF LIKERT ----
  ##### write function ----
response <- function(key) {
  # Generate the file path for the response file
  response_file_path <- paste0("responses/response_belc", key, ".txt")
  
  # Read the response file
  response_data <- read.delim(response_file_path, header = FALSE)
  
  # Workaround to directly generate a data frame
  response_data$hid <- 1:nrow(response_data)
  
  # Rename the choice variable
  colnames(response_data)[which(names(response_data) == "V1")] <- "rchoice"
  
  # Skip header
  response_data <- response_data[grepl("^Response", response_data[[1]]), ]
  
  # Define true Nobs
  response_data$id <- 1:nrow(response_data)
  response_data$hid <- NULL
  
  # Check for lengthy explanations
  lengthy_explanations <- response_data[response_data$rchoice %>% nchar() > 38, ]
  
  # Define lengthy explanations as NA
  response_data$rchoice[response_data$rchoice %>% nchar() > 38] <- NA
  
  # Extract choices
  # response_data$choice <- as.numeric(sub(".*:\\s(\\d+)", "\\1", response_data$rchoice))
  response_data$choice <- case_when(
    str_detect(response_data$rchoice, regex("none", ignore_case = TRUE))          ~ 0,
    str_detect(response_data$rchoice, regex("very few", ignore_case = TRUE))       ~ 1,
    str_detect(response_data$rchoice, regex("few", ignore_case = TRUE))            ~ 2,
    str_detect(response_data$rchoice, regex("about half", ignore_case = TRUE))     ~ 3,
    str_detect(response_data$rchoice, regex("many", ignore_case = TRUE))           ~ 4,
    str_detect(response_data$rchoice, regex("very many", ignore_case = TRUE))      ~ 5,
    str_detect(response_data$rchoice, regex("all", ignore_case = TRUE))            ~ 6,
    TRUE                                                ~ NA_real_
  )
  
  
  # Define the condition
  response_data$term <- paste0("bellik", key)
  
  # Skip excess data
  # do to the fact that the process has stopped prematurely
  response_data <- response_data[1:100, ]
  
  # generate summary, as input for next processing step
  result_summary <- table(response_data$choice)
  
  return(result_summary)
}

  ##### call function ----
keys <- c(1:25)
result_list <- lapply(keys, response)

  ##### preprocess result data frames ----
# Create a template data frame with all possible values
template_df <- data.frame(Var1 = 0:6)

# Function to add missing values with a frequency of 0
add_missing <- function(df) {
  merged_df <- merge(template_df, df, by = "Var1", all = TRUE)
  merged_df$Freq[is.na(merged_df$Freq)] <- 0
  return(merged_df)
}

# Apply the function to each data frame in the list
result_list <- lapply(result_list, add_missing)


  ##### extract data frame ----
#### Create an empty data frame to store the results
results_bellik <- data.frame(Choice_0 = integer(0), Choice_1 = integer(0), key = integer(0))

#### Process each element in the result_list
for (i in 1:length(result_list)) {
  # Create a data frame from the element
  df <- as.data.frame(result_list[[i]])
  
  #### Append the key column
  df$key <- i
  
  #### Combine results
  results_bellik <- rbind(results_bellik, df)
}

#### Reset row names and convert to data frame
rownames(results_bellik) <- NULL

#### rename estimate
names(results_bellik)[names(results_bellik) %in% "Var1"] <- "bel"

#### change class of "bel" to numeric
results_bellik$bel <- as.numeric(results_bellik$bel)

#### calculate weighted belief
results_bellik$wbel <- results_bellik$bel * results_bellik$Freq

#### summarize over vehicles
results_bellik %>% 
  group_by(key) %>% 
  mutate(yes = sum(wbel)/6) %>% 
  filter(bel == 0) -> results_bellik

#### add vehicle names
codebook <- read.csv("tasks/vehicle_table002.csv",
                     header = FALSE, 
                     col.names = c("key", "name"))
codebook <- codebook[1:25, ]

#### remove article (if any)
codebook$name <- gsub("^(?:a|an)\\s+", "", codebook$name)

#### add to results
results_bellik <- left_join(codebook, results_bellik)

#### add condition
results_bellik$cond <- "bellik"

#### keep relevant variables
results_bellik %>% 
  select(name, yes, cond) -> results_bellik

# View(results_bellik)
save(results_bellik,
     file = "results_bellik.RData")

###### CONTEXT PARK ----
  ##### write function ----
response <- function(key) {
  
  # Generate the file path for the response file
  response_file_path <- paste0("responses/response_why001belc", key, ".txt")
  
  # Read the response file
  response_data <- read.delim(response_file_path, header = FALSE)
  
  # Workaround to directly generate a data frame
  response_data$hid <- 1:nrow(response_data)
  
  # Rename the choice variable
  colnames(response_data)[which(names(response_data) == "V1")] <- "rchoice"
  
  # Skip lines not beginning with "Response" (header/multi-line responses)
  response_data <- response_data[grepl("^Response", response_data[[1]]), ]
  
  # Check for lengthy explanations
  lengthy_explanations <- response_data[response_data$rchoice %>% nchar() > 38, ]
  
  # Define and filter out lengthy explanations as NA
  response_data$rchoice[response_data$rchoice %>% nchar() > 38] <- NA
  response_data %>% 
    filter(!is.na(rchoice)) -> response_data
  
  # Extract choices
  response_data$choice <- case_when(
    str_detect(response_data$rchoice, regex("none", ignore_case = TRUE))          ~ 0,
    str_detect(response_data$rchoice, regex("very few", ignore_case = TRUE))       ~ 1,
    str_detect(response_data$rchoice, regex("few", ignore_case = TRUE))            ~ 2,
    str_detect(response_data$rchoice, regex("about half", ignore_case = TRUE))     ~ 3,
    str_detect(response_data$rchoice, regex("many", ignore_case = TRUE))           ~ 4,
    str_detect(response_data$rchoice, regex("very many", ignore_case = TRUE))      ~ 5,
    str_detect(response_data$rchoice, regex("all", ignore_case = TRUE))            ~ 6,
    TRUE                                                ~ NA_real_
  )
  
  # remove if NA
  response_data %>% 
    filter(!is.na(choice)) -> response_data
  
  # Define condition
  response_data$term <- paste0("contpark", key)
  
  # Skip excess data
  # due to the fact that the process has stopped prematurely
  response_data <- response_data[1:100, ]
  
  # generate summary, as input for next processing step
  result_summary <- table(response_data$choice)
}

  ##### call function ----
keys <- c(1:25)
result_list <- lapply(keys, response)

  ##### preprocess result data frames ----
# Create a template data frame with all possible values
template_df <- data.frame(Var1 = 0:6)

# Function to add missing values with a frequency of 0
add_missing <- function(df) {
  merged_df <- merge(template_df, df, by = "Var1", all = TRUE)
  merged_df$Freq[is.na(merged_df$Freq)] <- 0
  return(merged_df)
}

# Apply the function to each data frame in the list
result_list <- lapply(result_list, add_missing)

  ##### extract data frame ----
#### Create an empty data frame to store the results
results_contpark <- data.frame(Choice_0 = integer(0), Choice_1 = integer(0), key = integer(0))

#### Process each element in the result_list
for (i in 1:length(result_list)) {
  # Create a data frame from the element
  df <- as.data.frame(result_list[[i]])
  
  #### Append the key column
  df$key <- i
  
  #### Combine results
  results_contpark <- rbind(results_contpark, df)
}

#### Reset row names and convert to data frame
rownames(results_contpark) <- NULL

#### rename estimate
names(results_contpark)[names(results_contpark) %in% "Var1"] <- "bel"

#### change class of "bel" to numeric
results_contpark$bel <- as.numeric(results_contpark$bel)

#### calculate weighted belief
results_contpark$wbel <- results_contpark$bel * results_contpark$Freq

#### summarize over vehicles
results_contpark %>% 
  group_by(key) %>% 
  mutate(yes = sum(wbel)/6) %>% 
  filter(bel == 0) -> results_contpark

#### add vehicle names
codebook <- read.csv("tasks/vehicle_table002.csv",
                     header = FALSE, 
                     col.names = c("key", "name"))
codebook <- codebook[1:25, ]

#### remove article (if any)
codebook$name <- gsub("^(?:a|an)\\s+", "", codebook$name)

#### add to results
results_contpark <- left_join(codebook, results_contpark)

#### add condition
results_contpark$cond <- "contpark"

#### keep relevant variables
results_contpark %>% 
  select(name, yes, cond) -> results_contpark

# View(results_belcpark)
save(results_contpark,
     file = "results_contpark.RData")

###### CONTEXT DUI ----
  ##### write function ----
response <- function(key) {
  
  # Generate the file path for the response file
  response_file_path <- paste0("responses/response_why002belc", key, ".txt")
  
  # Read the response file
  response_data <- read.delim(response_file_path, header = FALSE)
  
  # Workaround to directly generate a data frame
  response_data$hid <- 1:nrow(response_data)
  
  # Rename the choice variable
  colnames(response_data)[which(names(response_data) == "V1")] <- "rchoice"
  
  # Skip lines not beginning with "Response" (header/multi-line responses)
  response_data <- response_data[grepl("^Response", response_data[[1]]), ]
  
  # Check for lengthy explanations
  lengthy_explanations <- response_data[response_data$rchoice %>% nchar() > 38, ]
  
  # Define and filter out lengthy explanations as NA
  response_data$rchoice[response_data$rchoice %>% nchar() > 38] <- NA
  response_data %>% 
    filter(!is.na(rchoice)) -> response_data
  
  # Extract choices
  response_data$choice <- case_when(
    str_detect(response_data$rchoice, regex("none", ignore_case = TRUE))          ~ 0,
    str_detect(response_data$rchoice, regex("very few", ignore_case = TRUE))       ~ 1,
    str_detect(response_data$rchoice, regex("few", ignore_case = TRUE))            ~ 2,
    str_detect(response_data$rchoice, regex("about half", ignore_case = TRUE))     ~ 3,
    str_detect(response_data$rchoice, regex("many", ignore_case = TRUE))           ~ 4,
    str_detect(response_data$rchoice, regex("very many", ignore_case = TRUE))      ~ 5,
    str_detect(response_data$rchoice, regex("all", ignore_case = TRUE))            ~ 6,
    TRUE                                                ~ NA_real_
  )
  
  # remove if NA
  response_data %>% 
    filter(!is.na(choice)) -> response_data
  
  # Define condition
  response_data$term <- paste0("contdui", key)
  
  # Skip excess data
  # due to the fact that the process has stopped prematurely
  response_data <- response_data[1:100, ]
  
  # generate summary, as input for next processing step
  result_summary <- table(response_data$choice)
}

  ##### call function ----
keys <- c(1:25)
result_list <- lapply(keys, response)

  ##### preprocess result data frames ----
# Create a template data frame with all possible values
template_df <- data.frame(Var1 = 0:6)

# Function to add missing values with a frequency of 0
add_missing <- function(df) {
  merged_df <- merge(template_df, df, by = "Var1", all = TRUE)
  merged_df$Freq[is.na(merged_df$Freq)] <- 0
  return(merged_df)
}

# Apply the function to each data frame in the list
result_list <- lapply(result_list, add_missing)

  ##### extract data frame ----
#### Create an empty data frame to store the results
results_contdui <- data.frame(Choice_0 = integer(0), Choice_1 = integer(0), key = integer(0))

#### Process each element in the result_list
for (i in 1:length(result_list)) {
  # Create a data frame from the element
  df <- as.data.frame(result_list[[i]])
  
  #### Append the key column
  df$key <- i
  
  #### Combine results
  results_contdui <- rbind(results_contdui, df)
}

#### Reset row names and convert to data frame
rownames(results_contdui) <- NULL

#### rename estimate
names(results_contdui)[names(results_contdui) %in% "Var1"] <- "bel"

#### change class of "bel" to numeric
results_contdui$bel <- as.numeric(results_contdui$bel)

#### calculate weighted belief
results_contdui$wbel <- results_contdui$bel * results_contdui$Freq

#### summarize over vehicles
results_contdui %>% 
  group_by(key) %>% 
  mutate(yes = sum(wbel)/6) %>% 
  filter(bel == 0) -> results_contdui

#### add vehicle names
codebook <- read.csv("tasks/vehicle_table002.csv",
                     header = FALSE, 
                     col.names = c("key", "name"))
codebook <- codebook[1:25, ]

#### remove article (if any)
codebook$name <- gsub("^(?:a|an)\\s+", "", codebook$name)

#### add to results
results_contdui <- left_join(codebook, results_contdui)

#### add condition
results_contdui$cond <- "contdui"

#### keep relevant variables
results_contdui %>% 
  select(name, yes, cond) -> results_contdui

# View(results_belcpark)
save(results_contdui,
     file = "results_contdui.RData")

###### CONTEXT LIABILITY ----
  ##### write function ----
response <- function(key) {
  
  # Generate the file path for the response file
  response_file_path <- paste0("responses/response_why003belc", key, ".txt")
  
  # Read the response file
  response_data <- read.delim(response_file_path, header = FALSE)
  
  # Workaround to directly generate a data frame
  response_data$hid <- 1:nrow(response_data)
  
  # Rename the choice variable
  colnames(response_data)[which(names(response_data) == "V1")] <- "rchoice"
  
  # Skip lines not beginning with "Response" (header/multi-line responses)
  response_data <- response_data[grepl("^Response", response_data[[1]]), ]
  
  # Check for lengthy explanations
  lengthy_explanations <- response_data[response_data$rchoice %>% nchar() > 38, ]
  
  # Define and filter out lengthy explanations as NA
  response_data$rchoice[response_data$rchoice %>% nchar() > 38] <- NA
  response_data %>% 
    filter(!is.na(rchoice)) -> response_data
  
  # Extract choices
  response_data$choice <- case_when(
    str_detect(response_data$rchoice, regex("none", ignore_case = TRUE))          ~ 0,
    str_detect(response_data$rchoice, regex("very few", ignore_case = TRUE))       ~ 1,
    str_detect(response_data$rchoice, regex("few", ignore_case = TRUE))            ~ 2,
    str_detect(response_data$rchoice, regex("about half", ignore_case = TRUE))     ~ 3,
    str_detect(response_data$rchoice, regex("many", ignore_case = TRUE))           ~ 4,
    str_detect(response_data$rchoice, regex("very many", ignore_case = TRUE))      ~ 5,
    str_detect(response_data$rchoice, regex("all", ignore_case = TRUE))            ~ 6,
    TRUE                                                ~ NA_real_
  )
  
  # remove if NA
  response_data %>% 
    filter(!is.na(choice)) -> response_data
  
  # Define condition
  response_data$term <- paste0("contliab", key)
  
  # Skip excess data
  # due to the fact that the process has stopped prematurely
  response_data <- response_data[1:100, ]
  
  # generate summary, as input for next processing step
  result_summary <- table(response_data$choice)
}

  ##### call function ----
keys <- c(1:25)
result_list <- lapply(keys, response)

  ##### preprocess result data frames ----
# Create a template data frame with all possible values
template_df <- data.frame(Var1 = 0:6)

# Function to add missing values with a frequency of 0
add_missing <- function(df) {
  merged_df <- merge(template_df, df, by = "Var1", all = TRUE)
  merged_df$Freq[is.na(merged_df$Freq)] <- 0
  return(merged_df)
}

# Apply the function to each data frame in the list
result_list <- lapply(result_list, add_missing)

  ##### extract data frame ----
#### Create an empty data frame to store the results
results_contliab <- data.frame(Choice_0 = integer(0), Choice_1 = integer(0), key = integer(0))

#### Process each element in the result_list
for (i in 1:length(result_list)) {
  # Create a data frame from the element
  df <- as.data.frame(result_list[[i]])
  
  #### Append the key column
  df$key <- i
  
  #### Combine results
  results_contliab <- rbind(results_contliab, df)
}

#### Reset row names and convert to data frame
rownames(results_contliab) <- NULL

#### rename estimate
names(results_contliab)[names(results_contliab) %in% "Var1"] <- "bel"

#### change class of "bel" to numeric
results_contliab$bel <- as.numeric(results_contliab$bel)

#### calculate weighted belief
results_contliab$wbel <- results_contliab$bel * results_contliab$Freq

#### summarize over vehicles
results_contliab %>% 
  group_by(key) %>% 
  mutate(yes = sum(wbel)/6) %>% 
  filter(bel == 0) -> results_contliab

#### add vehicle names
codebook <- read.csv("tasks/vehicle_table002.csv",
                     header = FALSE, 
                     col.names = c("key", "name"))
codebook <- codebook[1:25, ]

#### remove article (if any)
codebook$name <- gsub("^(?:a|an)\\s+", "", codebook$name)

#### add to results
results_contliab <- left_join(codebook, results_contliab)

#### add condition
results_contliab$cond <- "contliab"

#### keep relevant variables
results_contliab %>% 
  select(name, yes, cond) -> results_contliab

# View(results_belcpark)
save(results_contliab,
     file = "results_contliab.RData")

###### CONTEXT ENHANCEMENT ----
  ##### write function ----
response <- function(key) {
  
  # Generate the file path for the response file
  response_file_path <- paste0("responses/response_why004belc", key, ".txt")
  
  # Read the response file
  response_data <- read.delim(response_file_path, header = FALSE)
  
  # Workaround to directly generate a data frame
  response_data$hid <- 1:nrow(response_data)
  
  # Rename the choice variable
  colnames(response_data)[which(names(response_data) == "V1")] <- "rchoice"
  
  # Skip lines not beginning with "Response" (header/multi-line responses)
  response_data <- response_data[grepl("^Response", response_data[[1]]), ]
  
  # Check for lengthy explanations
  lengthy_explanations <- response_data[response_data$rchoice %>% nchar() > 38, ]
  
  # Define and filter out lengthy explanations as NA
  response_data$rchoice[response_data$rchoice %>% nchar() > 38] <- NA
  response_data %>% 
    filter(!is.na(rchoice)) -> response_data
  
  # Extract choices
  response_data$choice <- case_when(
    str_detect(response_data$rchoice, regex("none", ignore_case = TRUE))          ~ 0,
    str_detect(response_data$rchoice, regex("very few", ignore_case = TRUE))       ~ 1,
    str_detect(response_data$rchoice, regex("few", ignore_case = TRUE))            ~ 2,
    str_detect(response_data$rchoice, regex("about half", ignore_case = TRUE))     ~ 3,
    str_detect(response_data$rchoice, regex("many", ignore_case = TRUE))           ~ 4,
    str_detect(response_data$rchoice, regex("very many", ignore_case = TRUE))      ~ 5,
    str_detect(response_data$rchoice, regex("all", ignore_case = TRUE))            ~ 6,
    TRUE                                                ~ NA_real_
  )
  
  # remove if NA
  response_data %>% 
    filter(!is.na(choice)) -> response_data
  
  # Define condition
  response_data$term <- paste0("contenha", key)
  
  # Skip excess data
  # due to the fact that the process has stopped prematurely
  response_data <- response_data[1:100, ]
  
  # generate summary, as input for next processing step
  result_summary <- table(response_data$choice)
}

  ##### call function ----
keys <- c(1:25)
result_list <- lapply(keys, response)

  ##### preprocess result data frames ----
# Create a template data frame with all possible values
template_df <- data.frame(Var1 = 0:6)

# Function to add missing values with a frequency of 0
add_missing <- function(df) {
  merged_df <- merge(template_df, df, by = "Var1", all = TRUE)
  merged_df$Freq[is.na(merged_df$Freq)] <- 0
  return(merged_df)
}

# Apply the function to each data frame in the list
result_list <- lapply(result_list, add_missing)

  ##### extract data frame ----
#### Create an empty data frame to store the results
results_contenha <- data.frame(Choice_0 = integer(0), Choice_1 = integer(0), key = integer(0))

#### Process each element in the result_list
for (i in 1:length(result_list)) {
  # Create a data frame from the element
  df <- as.data.frame(result_list[[i]])
  
  #### Append the key column
  df$key <- i
  
  #### Combine results
  results_contenha <- rbind(results_contenha, df)
}

#### Reset row names and convert to data frame
rownames(results_contenha) <- NULL

#### rename estimate
names(results_contenha)[names(results_contenha) %in% "Var1"] <- "bel"

#### change class of "bel" to numeric
results_contenha$bel <- as.numeric(results_contenha$bel)

#### calculate weighted belief
results_contenha$wbel <- results_contenha$bel * results_contenha$Freq

#### summarize over vehicles
results_contenha %>% 
  group_by(key) %>% 
  mutate(yes = sum(wbel)/6) %>% 
  filter(bel == 0) -> results_contenha

#### add vehicle names
codebook <- read.csv("tasks/vehicle_table002.csv",
                     header = FALSE, 
                     col.names = c("key", "name"))
codebook <- codebook[1:25, ]

#### remove article (if any)
codebook$name <- gsub("^(?:a|an)\\s+", "", codebook$name)

#### add to results
results_contenha <- left_join(codebook, results_contenha)

#### add condition
results_contenha$cond <- "contenha"

#### keep relevant variables
results_contenha %>% 
  select(name, yes, cond) -> results_contenha

# View(results_belcpark)
save(results_contenha,
     file = "results_contenha.RData")

###### CONTEXT CENSUS ----
  ##### write function ----
response <- function(key) {
  
  # Generate the file path for the response file
  response_file_path <- paste0("responses/response_why005belc", key, ".txt")
  
  # Read the response file
  response_data <- read.delim(response_file_path, header = FALSE)
  
  # Workaround to directly generate a data frame
  response_data$hid <- 1:nrow(response_data)
  
  # Rename the choice variable
  colnames(response_data)[which(names(response_data) == "V1")] <- "rchoice"
  
  # Skip lines not beginning with "Response" (header/multi-line responses)
  response_data <- response_data[grepl("^Response", response_data[[1]]), ]
  
  # Check for lengthy explanations
  lengthy_explanations <- response_data[response_data$rchoice %>% nchar() > 38, ]
  
  # Define and filter out lengthy explanations as NA
  response_data$rchoice[response_data$rchoice %>% nchar() > 38] <- NA
  response_data %>% 
    filter(!is.na(rchoice)) -> response_data
  
  # Extract choices
  response_data$choice <- case_when(
    str_detect(response_data$rchoice, regex("none", ignore_case = TRUE))          ~ 0,
    str_detect(response_data$rchoice, regex("very few", ignore_case = TRUE))       ~ 1,
    str_detect(response_data$rchoice, regex("few", ignore_case = TRUE))            ~ 2,
    str_detect(response_data$rchoice, regex("about half", ignore_case = TRUE))     ~ 3,
    str_detect(response_data$rchoice, regex("many", ignore_case = TRUE))           ~ 4,
    str_detect(response_data$rchoice, regex("very many", ignore_case = TRUE))      ~ 5,
    str_detect(response_data$rchoice, regex("all", ignore_case = TRUE))            ~ 6,
    TRUE                                                ~ NA_real_
  )
  
  # remove if NA
  response_data %>% 
    filter(!is.na(choice)) -> response_data
  
  # Define condition
  response_data$term <- paste0("contcens", key)
  
  # Skip excess data
  # due to the fact that the process has stopped prematurely
  response_data <- response_data[1:100, ]
  
  # generate summary, as input for next processing step
  result_summary <- table(response_data$choice)
}

  ##### call function ----
keys <- c(1:25)
result_list <- lapply(keys, response)

  ##### preprocess result data frames ----
# Create a template data frame with all possible values
template_df <- data.frame(Var1 = 0:6)

# Function to add missing values with a frequency of 0
add_missing <- function(df) {
  merged_df <- merge(template_df, df, by = "Var1", all = TRUE)
  merged_df$Freq[is.na(merged_df$Freq)] <- 0
  return(merged_df)
}

# Apply the function to each data frame in the list
result_list <- lapply(result_list, add_missing)

  ##### extract data frame ----
#### Create an empty data frame to store the results
results_contcens <- data.frame(Choice_0 = integer(0), Choice_1 = integer(0), key = integer(0))

#### Process each element in the result_list
for (i in 1:length(result_list)) {
  # Create a data frame from the element
  df <- as.data.frame(result_list[[i]])
  
  #### Append the key column
  df$key <- i
  
  #### Combine results
  results_contcens <- rbind(results_contcens, df)
}

#### Reset row names and convert to data frame
rownames(results_contcens) <- NULL

#### rename estimate
names(results_contcens)[names(results_contcens) %in% "Var1"] <- "bel"

#### change class of "bel" to numeric
results_contcens$bel <- as.numeric(results_contcens$bel)

#### calculate weighted belief
results_contcens$wbel <- results_contcens$bel * results_contcens$Freq

#### summarize over vehicles
results_contcens %>% 
  group_by(key) %>% 
  mutate(yes = sum(wbel)/6) %>% 
  filter(bel == 0) -> results_contcens

#### add vehicle names
codebook <- read.csv("tasks/vehicle_table002.csv",
                     header = FALSE, 
                     col.names = c("key", "name"))
codebook <- codebook[1:25, ]

#### remove article (if any)
codebook$name <- gsub("^(?:a|an)\\s+", "", codebook$name)

#### add to results
results_contcens <- left_join(codebook, results_contcens)

#### add condition
results_contcens$cond <- "contcens"

#### keep relevant variables
results_contcens %>% 
  select(name, yes, cond) -> results_contcens

#### save
save(results_contcens,
     file = "results_contcens.RData")

###### ORIGINALISM EXTENSIONAL ----
  ##### write function ----
response <- function(key) {
  
  # Generate the file path for the response file
  response_file_path <- paste0("responses/response_why001bela", key, ".txt")
  
  # Read the response file
  response_data <- read.delim(response_file_path, header = FALSE)
  
  # Workaround to directly generate a data frame
  response_data$hid <- 1:nrow(response_data)
  
  # Rename the choice variable
  colnames(response_data)[which(names(response_data) == "V1")] <- "rchoice"
  
  # Skip lines not beginning with "Response" (header/multi-line responses)
  response_data <- response_data[grepl("^Response", response_data[[1]]), ]
  
  # Check for lengthy explanations
  lengthy_explanations <- response_data[response_data$rchoice %>% nchar() > 38, ]
  
  # Define and filter out lengthy explanations as NA
  response_data$rchoice[response_data$rchoice %>% nchar() > 38] <- NA
  response_data %>% 
    filter(!is.na(rchoice)) -> response_data
  
  # Extract choices
  response_data$choice <- case_when(
    str_detect(response_data$rchoice, regex("none", ignore_case = TRUE))          ~ 0,
    str_detect(response_data$rchoice, regex("very few", ignore_case = TRUE))       ~ 1,
    str_detect(response_data$rchoice, regex("few", ignore_case = TRUE))            ~ 2,
    str_detect(response_data$rchoice, regex("about half", ignore_case = TRUE))     ~ 3,
    str_detect(response_data$rchoice, regex("many", ignore_case = TRUE))           ~ 4,
    str_detect(response_data$rchoice, regex("very many", ignore_case = TRUE))      ~ 5,
    str_detect(response_data$rchoice, regex("all", ignore_case = TRUE))            ~ 6,
    TRUE                                                ~ NA_real_
  )
  
  # remove if NA
  response_data %>% 
    filter(!is.na(choice)) -> response_data
  
  # Define condition
  response_data$term <- paste0("orext", key)
  
  # Skip excess data
  # due to the fact that the process has stopped prematurely
  response_data <- response_data[1:100, ]
  
  # generate summary, as input for next processing step
  result_summary <- table(response_data$choice)
}

  ##### call function ----
keys <- c(1:25)
result_list <- lapply(keys, response)

  ##### preprocess result data frames ----
# Create a template data frame with all possible values
template_df <- data.frame(Var1 = 0:6)

# Function to add missing values with a frequency of 0
add_missing <- function(df) {
  merged_df <- merge(template_df, df, by = "Var1", all = TRUE)
  merged_df$Freq[is.na(merged_df$Freq)] <- 0
  return(merged_df)
}

# Apply the function to each data frame in the list
result_list <- lapply(result_list, add_missing)

  ##### extract data frame ----
#### Create an empty data frame to store the results
results_orext <- data.frame(Choice_0 = integer(0), Choice_1 = integer(0), key = integer(0))

#### Process each element in the result_list
for (i in 1:length(result_list)) {
  # Create a data frame from the element
  df <- as.data.frame(result_list[[i]])
  
  #### Append the key column
  df$key <- i
  
  #### Combine results
  results_orext <- rbind(results_orext, df)
}

#### Reset row names and convert to data frame
rownames(results_orext) <- NULL

#### rename estimate
names(results_orext)[names(results_orext) %in% "Var1"] <- "bel"

#### change class of "bel" to numeric
results_orext$bel <- as.numeric(results_orext$bel)

#### calculate weighted belief
results_orext$wbel <- results_orext$bel * results_orext$Freq

#### summarize over vehicles
results_orext %>% 
  group_by(key) %>% 
  mutate(yes = sum(wbel)/6) %>% 
  filter(bel == 0) -> results_orext

#### add vehicle names
codebook <- read.csv("tasks/vehicle_table002.csv",
                     header = FALSE, 
                     col.names = c("key", "name"))
codebook <- codebook[1:25, ]

#### remove article (if any)
codebook$name <- gsub("^(?:a|an)\\s+", "", codebook$name)

#### add to results
results_orext <- left_join(codebook, results_orext)

#### add condition
results_orext$cond <- "orext"

#### keep relevant variables
results_orext %>% 
  select(name, yes, cond) -> results_orext

#### save
save(results_orext,
     file = "results_orext.RData")

###### ORIGINALISM INTENSIONAL ----
  ##### write function ----
response <- function(key) {
  
  # Generate the file path for the response file
  response_file_path <- paste0("responses/response_why001beli", key, ".txt")
  
  # Read the response file
  response_data <- read.delim(response_file_path, header = FALSE)
  
  # Workaround to directly generate a data frame
  response_data$hid <- 1:nrow(response_data)
  
  # Rename the choice variable
  colnames(response_data)[which(names(response_data) == "V1")] <- "rchoice"
  
  # Skip lines not beginning with "Response" (header/multi-line responses)
  response_data <- response_data[grepl("^Response", response_data[[1]]), ]
  
  # Check for lengthy explanations
  lengthy_explanations <- response_data[response_data$rchoice %>% nchar() > 38, ]
  
  # Define and filter out lengthy explanations as NA
  response_data$rchoice[response_data$rchoice %>% nchar() > 38] <- NA
  response_data %>% 
    filter(!is.na(rchoice)) -> response_data
  
  # Extract choices
  response_data$choice <- case_when(
    str_detect(response_data$rchoice, regex("none", ignore_case = TRUE))          ~ 0,
    str_detect(response_data$rchoice, regex("very few", ignore_case = TRUE))       ~ 1,
    str_detect(response_data$rchoice, regex("few", ignore_case = TRUE))            ~ 2,
    str_detect(response_data$rchoice, regex("about half", ignore_case = TRUE))     ~ 3,
    str_detect(response_data$rchoice, regex("many", ignore_case = TRUE))           ~ 4,
    str_detect(response_data$rchoice, regex("very many", ignore_case = TRUE))      ~ 5,
    str_detect(response_data$rchoice, regex("all", ignore_case = TRUE))            ~ 6,
    TRUE                                                ~ NA_real_
  )
  
  # remove if NA
  response_data %>% 
    filter(!is.na(choice)) -> response_data
  
  # Define condition
  response_data$term <- paste0("orint", key)
  
  # Skip excess data
  # due to the fact that the process has stopped prematurely
  response_data <- response_data[1:100, ]
  
  # generate summary, as input for next processing step
  result_summary <- table(response_data$choice)
}

  ##### call function ----
keys <- c(1:25)
result_list <- lapply(keys, response)

  ##### preprocess result data frames ----
# Create a template data frame with all possible values
template_df <- data.frame(Var1 = 0:6)

# Function to add missing values with a frequency of 0
add_missing <- function(df) {
  merged_df <- merge(template_df, df, by = "Var1", all = TRUE)
  merged_df$Freq[is.na(merged_df$Freq)] <- 0
  return(merged_df)
}

# Apply the function to each data frame in the list
result_list <- lapply(result_list, add_missing)

  ##### extract data frame ----
#### Create an empty data frame to store the results
results_orint <- data.frame(Choice_0 = integer(0), Choice_1 = integer(0), key = integer(0))

#### Process each element in the result_list
for (i in 1:length(result_list)) {
  # Create a data frame from the element
  df <- as.data.frame(result_list[[i]])
  
  #### Append the key column
  df$key <- i
  
  #### Combine results
  results_orint <- rbind(results_orint, df)
}

#### Reset row names and convert to data frame
rownames(results_orint) <- NULL

#### rename estimate
names(results_orint)[names(results_orint) %in% "Var1"] <- "bel"

#### change class of "bel" to numeric
results_orint$bel <- as.numeric(results_orint$bel)

#### calculate weighted belief
results_orint$wbel <- results_orint$bel * results_orint$Freq

#### summarize over vehicles
results_orint %>% 
  group_by(key) %>% 
  mutate(yes = sum(wbel)/6) %>% 
  filter(bel == 0) -> results_orint

#### add vehicle names
codebook <- read.csv("tasks/vehicle_table002.csv",
                     header = FALSE, 
                     col.names = c("key", "name"))
codebook <- codebook[1:25, ]

#### remove article (if any)
codebook$name <- gsub("^(?:a|an)\\s+", "", codebook$name)

#### add to results
results_orint <- left_join(codebook, results_orint)

#### add condition
results_orint$cond <- "orint"

#### keep relevant variables
results_orint %>% 
  select(name, yes, cond) -> results_orint

#### save
save(results_orint,
     file = "results_orint.RData")

###### PURPOSIVISM ANNOYANCE ----
  ##### write function ----
response <- function(key) {
  # Generate the file path for the response file
  response_file_path <- paste0("responses/response_fpurp001_", key, ".txt")
  
  # Read the response file
  raw_data <- read.delim(response_file_path, header = FALSE)
  
  # Replace all line breaks with empty strings
  no_linebreaks <- gsub(pattern = "\n", replacement = "", x = raw_data, perl = TRUE)
  
  # Split the text into individual lines
  split_lines <- strsplit(no_linebreaks, "Response")
  
  # Create an empty data frame to store the extracted data
  response_data <- data.frame()
  
  # Set initial response number to 1
  current_response_number <- 1
  
  # Iterate through each response chunk
  for (i in 1:length(split_lines)) {
    # Skip the first element, which is the response number
    for (j in 2:length(split_lines[[i]])) {
      # Extract the response level
      response_level <- str_extract(split_lines[[i]][j], pattern = "(about half of)|many|very many|((almost)\\s+)?all|very few|few|((almost)\\s+)?none")
      
      # Create a data frame row with extracted data
      extracted_row <- data.frame(response_number = current_response_number, response_level = response_level)
      
      # Update the response number for the next iteration
      current_response_number <- current_response_number + 1
      
      # Add the row to the extracted data data frame
      response_data <- rbind(response_data, extracted_row)
    }
  }
  # Extract choices
  response_data$choice <- case_when(
    str_detect(response_data$response_level, regex("none", ignore_case = TRUE))          ~ 0,
    str_detect(response_data$response_level, regex("very few", ignore_case = TRUE))       ~ 1,
    str_detect(response_data$response_level, regex("few", ignore_case = TRUE))            ~ 2,
    str_detect(response_data$response_level, regex("about half", ignore_case = TRUE))     ~ 3,
    str_detect(response_data$response_level, regex("many", ignore_case = TRUE))           ~ 4,
    str_detect(response_data$response_level, regex("very many", ignore_case = TRUE))      ~ 5,
    str_detect(response_data$response_level, regex("all", ignore_case = TRUE))            ~ 6,
    TRUE                                                ~ NA_real_
  )
  
  # remove if NA
  response_data %>% 
    filter(!is.na(choice)) -> response_data
  
  # Define the condition
  response_data$term <- paste0("purpannoy", key)
  
  # Skip excess data
  # due to the fact that the process has stopped prematurely
  response_data <- response_data[1:100, ]
  
  # generate summary, as input for next processing step
  result_summary <- table(response_data$choice)
}

  ##### call function ----
keys <- c(1:25)
result_list <- lapply(keys, response)

  ##### preprocess result data frames ----
# Create a template data frame with all possible values
template_df <- data.frame(Var1 = 0:6)

# Function to add missing values with a frequency of 0
add_missing <- function(df) {
  merged_df <- merge(template_df, df, by = "Var1", all = TRUE)
  merged_df$Freq[is.na(merged_df$Freq)] <- 0
  return(merged_df)
}

# Apply the function to each data frame in the list
result_list <- lapply(result_list, add_missing)

  ##### extract data frame ----
#### Create an empty data frame to store the results
results_purpannoy <- data.frame(Choice_0 = integer(0), Choice_1 = integer(0), key = integer(0))

#### Process each element in the result_list
for (i in 1:length(result_list)) {
  # Create a data frame from the element
  df <- as.data.frame(result_list[[i]])
  
  #### Append the key column
  df$key <- i
  
  #### Combine results
  results_purpannoy <- rbind(results_purpannoy, df)
}

#### Reset row names and convert to data frame
rownames(results_purpannoy) <- NULL

#### rename estimate
names(results_purpannoy)[names(results_purpannoy) %in% "Var1"] <- "bel"

#### change class of "bel" to numeric
results_purpannoy$bel <- as.numeric(results_purpannoy$bel)

#### calculate weighted belief
results_purpannoy$wbel <- results_purpannoy$bel * results_purpannoy$Freq

#### summarize over vehicles
results_purpannoy %>% 
  group_by(key) %>% 
  mutate(yes = sum(wbel)/6) %>% 
  filter(bel == 0) -> results_purpannoy

#### add vehicle names
codebook <- read.csv("tasks/vehicle_table002.csv",
                     header = FALSE, 
                     col.names = c("key", "name"))
codebook <- codebook[1:25, ]

#### remove article (if any)
codebook$name <- gsub("^(?:a|an)\\s+", "", codebook$name)

#### add to results
results_purpannoy <- left_join(codebook, results_purpannoy)

#### add condition
results_purpannoy$cond <- "purpannoy"

#### keep relevant variables
results_purpannoy %>% 
  select(name, yes, cond) -> results_purpannoy

#### save
save(results_purpannoy,
     file = "results_purpannoy.RData")

###### PURPOSIVISM ACCIDENT ----
  ##### write function ----
response <- function(key) {
  # Generate the file path for the response file
  response_file_path <- paste0("responses/response_fpurp002_", key, ".txt")
  
  # Read the response file
  raw_data <- read.delim(response_file_path, header = FALSE)
  
  # Replace all line breaks with empty strings
  no_linebreaks <- gsub(pattern = "\n", replacement = "", x = raw_data, perl = TRUE)
  
  # Split the text into individual lines
  split_lines <- strsplit(no_linebreaks, "Response")
  
  # Create an empty data frame to store the extracted data
  response_data <- data.frame()
  
  # Set initial response number to 1
  current_response_number <- 1
  
  # Iterate through each response chunk
  for (i in 1:length(split_lines)) {
    # Skip the first element, which is the response number
    for (j in 2:length(split_lines[[i]])) {
      # Extract the response level
      response_level <- str_extract(split_lines[[i]][j], pattern = "(about half of)|many|very many|((almost)\\s+)?all|very few|few|((almost)\\s+)?none")
      
      # Create a data frame row with extracted data
      extracted_row <- data.frame(response_number = current_response_number, response_level = response_level)
      
      # Update the response number for the next iteration
      current_response_number <- current_response_number + 1
      
      # Add the row to the extracted data data frame
      response_data <- rbind(response_data, extracted_row)
    }
  }
  # Extract choices
  response_data$choice <- case_when(
    str_detect(response_data$response_level, regex("none", ignore_case = TRUE))          ~ 0,
    str_detect(response_data$response_level, regex("very few", ignore_case = TRUE))       ~ 1,
    str_detect(response_data$response_level, regex("few", ignore_case = TRUE))            ~ 2,
    str_detect(response_data$response_level, regex("about half", ignore_case = TRUE))     ~ 3,
    str_detect(response_data$response_level, regex("many", ignore_case = TRUE))           ~ 4,
    str_detect(response_data$response_level, regex("very many", ignore_case = TRUE))      ~ 5,
    str_detect(response_data$response_level, regex("all", ignore_case = TRUE))            ~ 6,
    TRUE                                                ~ NA_real_
  )
  
  # remove if NA
  response_data %>% 
    filter(!is.na(choice)) -> response_data
  
  # Define the condition
  response_data$term <- paste0("purpaccident", key)
  
  # Skip excess data
  # due to the fact that the process has stopped prematurely
  response_data <- response_data[1:100, ]
  
  # generate summary, as input for next processing step
  result_summary <- table(response_data$choice)
}

  ##### call function ----
keys <- c(1:25)
result_list <- lapply(keys, response)

  ##### preprocess result data frames ----
# Create a template data frame with all possible values
template_df <- data.frame(Var1 = 0:6)

# Function to add missing values with a frequency of 0
add_missing <- function(df) {
  merged_df <- merge(template_df, df, by = "Var1", all = TRUE)
  merged_df$Freq[is.na(merged_df$Freq)] <- 0
  return(merged_df)
}

# Apply the function to each data frame in the list
result_list <- lapply(result_list, add_missing)

  ##### extract data frame ----
#### Create an empty data frame to store the results
results_purpaccident <- data.frame(Choice_0 = integer(0), Choice_1 = integer(0), key = integer(0))

#### Process each element in the result_list
for (i in 1:length(result_list)) {
  # Create a data frame from the element
  df <- as.data.frame(result_list[[i]])
  
  #### Append the key column
  df$key <- i
  
  #### Combine results
  results_purpaccident <- rbind(results_purpaccident, df)
}

#### Reset row names and convert to data frame
rownames(results_purpaccident) <- NULL

#### rename estimate
names(results_purpaccident)[names(results_purpaccident) %in% "Var1"] <- "bel"

#### change class of "bel" to numeric
results_purpaccident$bel <- as.numeric(results_purpaccident$bel)

#### calculate weighted belief
results_purpaccident$wbel <- results_purpaccident$bel * results_purpaccident$Freq

#### summarize over vehicles
results_purpaccident %>% 
  group_by(key) %>% 
  mutate(yes = sum(wbel)/6) %>% 
  filter(bel == 0) -> results_purpaccident

#### add vehicle names
codebook <- read.csv("tasks/vehicle_table002.csv",
                     header = FALSE, 
                     col.names = c("key", "name"))
codebook <- codebook[1:25, ]

#### remove article (if any)
codebook$name <- gsub("^(?:a|an)\\s+", "", codebook$name)

#### add to results
results_purpaccident <- left_join(codebook, results_purpaccident)

#### add condition
results_purpaccident$cond <- "purpaccident"

#### keep relevant variables
results_purpaccident %>% 
  select(name, yes, cond) -> results_purpaccident

#### save
save(results_purpaccident,
     file = "results_purpaccident.RData")

###### PURPOSIVISM SPACE ----
  ##### write function ----
response <- function(key) {
  # Generate the file path for the response file
  response_file_path <- paste0("responses/response_fpurp003_", key, ".txt")
  
  # Read the response file
  raw_data <- read.delim(response_file_path, header = FALSE)
  
  # Replace all line breaks with empty strings
  no_linebreaks <- gsub(pattern = "\n", replacement = "", x = raw_data, perl = TRUE)
  
  # Split the text into individual lines
  split_lines <- strsplit(no_linebreaks, "Response")
  
  # Create an empty data frame to store the extracted data
  response_data <- data.frame()
  
  # Set initial response number to 1
  current_response_number <- 1
  
  # Iterate through each response chunk
  for (i in 1:length(split_lines)) {
    # Skip the first element, which is the response number
    for (j in 2:length(split_lines[[i]])) {
      # Extract the response level
      response_level <- str_extract(split_lines[[i]][j], pattern = "(about half of)|many|very many|((almost)\\s+)?all|very few|few|((almost)\\s+)?none")
      
      # Create a data frame row with extracted data
      extracted_row <- data.frame(response_number = current_response_number, response_level = response_level)
      
      # Update the response number for the next iteration
      current_response_number <- current_response_number + 1
      
      # Add the row to the extracted data data frame
      response_data <- rbind(response_data, extracted_row)
    }
  }
  # Extract choices
  response_data$choice <- case_when(
    str_detect(response_data$response_level, regex("none", ignore_case = TRUE))          ~ 0,
    str_detect(response_data$response_level, regex("very few", ignore_case = TRUE))       ~ 1,
    str_detect(response_data$response_level, regex("few", ignore_case = TRUE))            ~ 2,
    str_detect(response_data$response_level, regex("about half", ignore_case = TRUE))     ~ 3,
    str_detect(response_data$response_level, regex("many", ignore_case = TRUE))           ~ 4,
    str_detect(response_data$response_level, regex("very many", ignore_case = TRUE))      ~ 5,
    str_detect(response_data$response_level, regex("all", ignore_case = TRUE))            ~ 6,
    TRUE                                                ~ NA_real_
  )
  
  # remove if NA
  response_data %>% 
    filter(!is.na(choice)) -> response_data
  
  # Define the condition
  response_data$term <- paste0("purpspace", key)
  
  # Skip excess data
  # due to the fact that the process has stopped prematurely
  response_data <- response_data[1:100, ]
  
  # generate summary, as input for next processing step
  result_summary <- table(response_data$choice)
}

  ##### call function ----
keys <- c(1:25)
result_list <- lapply(keys, response)

  ##### preprocess result data frames ----
# Create a template data frame with all possible values
template_df <- data.frame(Var1 = 0:6)

# Function to add missing values with a frequency of 0
add_missing <- function(df) {
  merged_df <- merge(template_df, df, by = "Var1", all = TRUE)
  merged_df$Freq[is.na(merged_df$Freq)] <- 0
  return(merged_df)
}

# Apply the function to each data frame in the list
result_list <- lapply(result_list, add_missing)

  ##### extract data frame ----
#### Create an empty data frame to store the results
results_purpspace <- data.frame(Choice_0 = integer(0), Choice_1 = integer(0), key = integer(0))

#### Process each element in the result_list
for (i in 1:length(result_list)) {
  # Create a data frame from the element
  df <- as.data.frame(result_list[[i]])
  
  #### Append the key column
  df$key <- i
  
  #### Combine results
  results_purpspace <- rbind(results_purpspace, df)
}

#### Reset row names and convert to data frame
rownames(results_purpspace) <- NULL

#### rename estimate
names(results_purpspace)[names(results_purpspace) %in% "Var1"] <- "bel"

#### change class of "bel" to numeric
results_purpspace$bel <- as.numeric(results_purpspace$bel)

#### calculate weighted belief
results_purpspace$wbel <- results_purpspace$bel * results_purpspace$Freq

#### summarize over vehicles
results_purpspace %>% 
  group_by(key) %>% 
  mutate(yes = sum(wbel)/6) %>% 
  filter(bel == 0) -> results_purpspace

#### add vehicle names
codebook <- read.csv("tasks/vehicle_table002.csv",
                     header = FALSE, 
                     col.names = c("key", "name"))
codebook <- codebook[1:25, ]

#### remove article (if any)
codebook$name <- gsub("^(?:a|an)\\s+", "", codebook$name)

#### add to results
results_purpspace <- left_join(codebook, results_purpspace)

#### add condition
results_purpspace$cond <- "purpspace"

#### keep relevant variables
results_purpspace %>% 
  select(name, yes, cond) -> results_purpspace

#### save
save(results_purpspace,
     file = "results_purpspace.RData")
###### PURPOSIVISM DAMAGE ----
  ##### write function ----
response <- function(key) {
  # Generate the file path for the response file
  response_file_path <- paste0("responses/response_fpurp004_", key, ".txt")
  
  # Read the response file
  raw_data <- read.delim(response_file_path, header = FALSE)
  
  # Replace all line breaks with empty strings
  no_linebreaks <- gsub(pattern = "\n", replacement = "", x = raw_data, perl = TRUE)
  
  # Split the text into individual lines
  split_lines <- strsplit(no_linebreaks, "Response")
  
  # Create an empty data frame to store the extracted data
  response_data <- data.frame()
  
  # Set initial response number to 1
  current_response_number <- 1
  
  # Iterate through each response chunk
  for (i in 1:length(split_lines)) {
    # Skip the first element, which is the response number
    for (j in 2:length(split_lines[[i]])) {
      # Extract the response level
      response_level <- str_extract(split_lines[[i]][j], pattern = "(about half of)|many|very many|((almost)\\s+)?all|very few|few|((almost)\\s+)?none")
      
      # Create a data frame row with extracted data
      extracted_row <- data.frame(response_number = current_response_number, response_level = response_level)
      
      # Update the response number for the next iteration
      current_response_number <- current_response_number + 1
      
      # Add the row to the extracted data data frame
      response_data <- rbind(response_data, extracted_row)
    }
  }
  # Extract choices
  response_data$choice <- case_when(
    str_detect(response_data$response_level, regex("none", ignore_case = TRUE))          ~ 0,
    str_detect(response_data$response_level, regex("very few", ignore_case = TRUE))       ~ 1,
    str_detect(response_data$response_level, regex("few", ignore_case = TRUE))            ~ 2,
    str_detect(response_data$response_level, regex("about half", ignore_case = TRUE))     ~ 3,
    str_detect(response_data$response_level, regex("many", ignore_case = TRUE))           ~ 4,
    str_detect(response_data$response_level, regex("very many", ignore_case = TRUE))      ~ 5,
    str_detect(response_data$response_level, regex("all", ignore_case = TRUE))            ~ 6,
    TRUE                                                ~ NA_real_
  )
  
  # remove if NA
  response_data %>% 
    filter(!is.na(choice)) -> response_data
  
  # Define the condition
  response_data$term <- paste0("purpdamage", key)
  
  # Skip excess data
  # due to the fact that the process has stopped prematurely
  response_data <- response_data[1:100, ]
  
  # generate summary, as input for next processing step
  result_summary <- table(response_data$choice)
}

  ##### call function ----
keys <- c(1:25)
result_list <- lapply(keys, response)

  ##### preprocess result data frames ----
# Create a template data frame with all possible values
template_df <- data.frame(Var1 = 0:6)

# Function to add missing values with a frequency of 0
add_missing <- function(df) {
  merged_df <- merge(template_df, df, by = "Var1", all = TRUE)
  merged_df$Freq[is.na(merged_df$Freq)] <- 0
  return(merged_df)
}

# Apply the function to each data frame in the list
result_list <- lapply(result_list, add_missing)

  ##### extract data frame ----
#### Create an empty data frame to store the results
results_purpdamage <- data.frame(Choice_0 = integer(0), Choice_1 = integer(0), key = integer(0))

#### Process each element in the result_list
for (i in 1:length(result_list)) {
  # Create a data frame from the element
  df <- as.data.frame(result_list[[i]])
  
  #### Append the key column
  df$key <- i
  
  #### Combine results
  results_purpdamage <- rbind(results_purpdamage, df)
}

#### Reset row names and convert to data frame
rownames(results_purpdamage) <- NULL

#### rename estimate
names(results_purpdamage)[names(results_purpdamage) %in% "Var1"] <- "bel"

#### change class of "bel" to numeric
results_purpdamage$bel <- as.numeric(results_purpdamage$bel)

#### calculate weighted belief
results_purpdamage$wbel <- results_purpdamage$bel * results_purpdamage$Freq

#### summarize over vehicles
results_purpdamage %>% 
  group_by(key) %>% 
  mutate(yes = sum(wbel)/6) %>% 
  filter(bel == 0) -> results_purpdamage

#### add vehicle names
codebook <- read.csv("tasks/vehicle_table002.csv",
                     header = FALSE, 
                     col.names = c("key", "name"))
codebook <- codebook[1:25, ]

#### remove article (if any)
codebook$name <- gsub("^(?:a|an)\\s+", "", codebook$name)

#### add to results
results_purpdamage <- left_join(codebook, results_purpdamage)

#### add condition
results_purpdamage$cond <- "purpdamage"

#### keep relevant variables
results_purpdamage %>% 
  select(name, yes, cond) -> results_purpdamage

#### save
save(results_purpdamage,
     file = "results_purpdamage.RData")

###### PURPOSIVISM LOCAL ----
  ##### write function ----
response <- function(key) {
  # Generate the file path for the response file
  response_file_path <- paste0("responses/response_fpurp005_", key, ".txt")
  
  # Read the response file
  raw_data <- read.delim(response_file_path, header = FALSE)
  
  # Replace all line breaks with empty strings
  no_linebreaks <- gsub(pattern = "\n", replacement = "", x = raw_data, perl = TRUE)
  
  # Split the text into individual lines
  split_lines <- strsplit(no_linebreaks, "Response")
  
  # Create an empty data frame to store the extracted data
  response_data <- data.frame()
  
  # Set initial response number to 1
  current_response_number <- 1
  
  # Iterate through each response chunk
  for (i in 1:length(split_lines)) {
    # Skip the first element, which is the response number
    for (j in 2:length(split_lines[[i]])) {
      # Extract the response level
      response_level <- str_extract(split_lines[[i]][j], pattern = "(about half of)|many|very many|((almost)\\s+)?all|very few|few|((almost)\\s+)?none")
      
      # Create a data frame row with extracted data
      extracted_row <- data.frame(response_number = current_response_number, response_level = response_level)
      
      # Update the response number for the next iteration
      current_response_number <- current_response_number + 1
      
      # Add the row to the extracted data data frame
      response_data <- rbind(response_data, extracted_row)
    }
  }
  # Extract choices
  response_data$choice <- case_when(
    str_detect(response_data$response_level, regex("none", ignore_case = TRUE))          ~ 0,
    str_detect(response_data$response_level, regex("very few", ignore_case = TRUE))       ~ 1,
    str_detect(response_data$response_level, regex("few", ignore_case = TRUE))            ~ 2,
    str_detect(response_data$response_level, regex("about half", ignore_case = TRUE))     ~ 3,
    str_detect(response_data$response_level, regex("many", ignore_case = TRUE))           ~ 4,
    str_detect(response_data$response_level, regex("very many", ignore_case = TRUE))      ~ 5,
    str_detect(response_data$response_level, regex("all", ignore_case = TRUE))            ~ 6,
    TRUE                                                ~ NA_real_
  )
  
  # remove if NA
  response_data %>% 
    filter(!is.na(choice)) -> response_data
  
  # Define the condition
  response_data$term <- paste0("purplocal", key)
  
  # Skip excess data
  # due to the fact that the process has stopped prematurely
  response_data <- response_data[1:100, ]
  
  # generate summary, as input for next processing step
  result_summary <- table(response_data$choice)
}

  ##### call function ----
keys <- c(1:25)
result_list <- lapply(keys, response)

  ##### preprocess result data frames ----
# Create a template data frame with all possible values
template_df <- data.frame(Var1 = 0:6)

# Function to add missing values with a frequency of 0
add_missing <- function(df) {
  merged_df <- merge(template_df, df, by = "Var1", all = TRUE)
  merged_df$Freq[is.na(merged_df$Freq)] <- 0
  return(merged_df)
}

# Apply the function to each data frame in the list
result_list <- lapply(result_list, add_missing)

  ##### extract data frame ----
#### Create an empty data frame to store the results
results_purplocal <- data.frame(Choice_0 = integer(0), Choice_1 = integer(0), key = integer(0))

#### Process each element in the result_list
for (i in 1:length(result_list)) {
  # Create a data frame from the element
  df <- as.data.frame(result_list[[i]])
  
  #### Append the key column
  df$key <- i
  
  #### Combine results
  results_purplocal <- rbind(results_purplocal, df)
}

#### Reset row names and convert to data frame
rownames(results_purplocal) <- NULL

#### rename estimate
names(results_purplocal)[names(results_purplocal) %in% "Var1"] <- "bel"

#### change class of "bel" to numeric
results_purplocal$bel <- as.numeric(results_purplocal$bel)

#### calculate weighted belief
results_purplocal$wbel <- results_purplocal$bel * results_purplocal$Freq

#### summarize over vehicles
results_purplocal %>% 
  group_by(key) %>% 
  mutate(yes = sum(wbel)/6) %>% 
  filter(bel == 0) -> results_purplocal

#### add vehicle names
codebook <- read.csv("tasks/vehicle_table002.csv",
                     header = FALSE, 
                     col.names = c("key", "name"))
codebook <- codebook[1:25, ]

#### remove article (if any)
codebook$name <- gsub("^(?:a|an)\\s+", "", codebook$name)

#### add to results
results_purplocal <- left_join(codebook, results_purplocal)

#### add condition
results_purplocal$cond <- "purplocal"

#### keep relevant variables
results_purplocal %>% 
  select(name, yes, cond) -> results_purplocal

#### save
save(results_purplocal,
     file = "results_purplocal.RData")

###### PURPOSIVISM BEAUTY ----
  ##### write function ----
response <- function(key) {
  # Generate the file path for the response file
  response_file_path <- paste0("responses/response_fpurp006_", key, ".txt")
  
  # Read the response file
  raw_data <- read.delim(response_file_path, header = FALSE)
  
  # Replace all line breaks with empty strings
  no_linebreaks <- gsub(pattern = "\n", replacement = "", x = raw_data, perl = TRUE)
  
  # Split the text into individual lines
  split_lines <- strsplit(no_linebreaks, "Response")
  
  # Create an empty data frame to store the extracted data
  response_data <- data.frame()
  
  # Set initial response number to 1
  current_response_number <- 1
  
  # Iterate through each response chunk
  for (i in 1:length(split_lines)) {
    # Skip the first element, which is the response number
    for (j in 2:length(split_lines[[i]])) {
      # Extract the response level
      response_level <- str_extract(split_lines[[i]][j], pattern = "(about half of)|many|very many|((almost)\\s+)?all|very few|few|((almost)\\s+)?none")
      
      # Create a data frame row with extracted data
      extracted_row <- data.frame(response_number = current_response_number, response_level = response_level)
      
      # Update the response number for the next iteration
      current_response_number <- current_response_number + 1
      
      # Add the row to the extracted data data frame
      response_data <- rbind(response_data, extracted_row)
    }
  }
  # Extract choices
  response_data$choice <- case_when(
    str_detect(response_data$response_level, regex("none", ignore_case = TRUE))          ~ 0,
    str_detect(response_data$response_level, regex("very few", ignore_case = TRUE))       ~ 1,
    str_detect(response_data$response_level, regex("few", ignore_case = TRUE))            ~ 2,
    str_detect(response_data$response_level, regex("about half", ignore_case = TRUE))     ~ 3,
    str_detect(response_data$response_level, regex("many", ignore_case = TRUE))           ~ 4,
    str_detect(response_data$response_level, regex("very many", ignore_case = TRUE))      ~ 5,
    str_detect(response_data$response_level, regex("all", ignore_case = TRUE))            ~ 6,
    TRUE                                                ~ NA_real_
  )
  
  # remove if NA
  response_data %>% 
    filter(!is.na(choice)) -> response_data
  
  # Define the condition
  response_data$term <- paste0("purpbeauty", key)
  
  # Skip excess data
  # due to the fact that the process has stopped prematurely
  response_data <- response_data[1:100, ]
  
  # generate summary, as input for next processing step
  result_summary <- table(response_data$choice)
}

  ##### call function ----
keys <- c(1:25)
result_list <- lapply(keys, response)

  ##### preprocess result data frames ----
# Create a template data frame with all possible values
template_df <- data.frame(Var1 = 0:6)

# Function to add missing values with a frequency of 0
add_missing <- function(df) {
  merged_df <- merge(template_df, df, by = "Var1", all = TRUE)
  merged_df$Freq[is.na(merged_df$Freq)] <- 0
  return(merged_df)
}

# Apply the function to each data frame in the list
result_list <- lapply(result_list, add_missing)

  ##### extract data frame ----
#### Create an empty data frame to store the results
results_purpbeauty <- data.frame(Choice_0 = integer(0), Choice_1 = integer(0), key = integer(0))

#### Process each element in the result_list
for (i in 1:length(result_list)) {
  # Create a data frame from the element
  df <- as.data.frame(result_list[[i]])
  
  #### Append the key column
  df$key <- i
  
  #### Combine results
  results_purpbeauty <- rbind(results_purpbeauty, df)
}

#### Reset row names and convert to data frame
rownames(results_purpbeauty) <- NULL

#### rename estimate
names(results_purpbeauty)[names(results_purpbeauty) %in% "Var1"] <- "bel"

#### change class of "bel" to numeric
results_purpbeauty$bel <- as.numeric(results_purpbeauty$bel)

#### calculate weighted belief
results_purpbeauty$wbel <- results_purpbeauty$bel * results_purpbeauty$Freq

#### summarize over vehicles
results_purpbeauty %>% 
  group_by(key) %>% 
  mutate(yes = sum(wbel)/6) %>% 
  filter(bel == 0) -> results_purpbeauty

#### add vehicle names
codebook <- read.csv("tasks/vehicle_table002.csv",
                     header = FALSE, 
                     col.names = c("key", "name"))
codebook <- codebook[1:25, ]

#### remove article (if any)
codebook$name <- gsub("^(?:a|an)\\s+", "", codebook$name)

#### add to results
results_purpbeauty <- left_join(codebook, results_purpbeauty)

#### add condition
results_purpbeauty$cond <- "purpbeauty"

#### keep relevant variables
results_purpbeauty %>% 
  select(name, yes, cond) -> results_purpbeauty

#### save
save(results_purpbeauty,
     file = "results_purpbeauty.RData")

