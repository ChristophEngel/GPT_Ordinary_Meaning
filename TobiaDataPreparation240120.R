##### ENGEL MCADAMS
##### DATA PREPARATION TOBIA
##### 240120

library(readxl)

tobia <- read_xlsx("/Users/engel/Documents/word/Manuskript/Verhalten/Engel McAdams/Data/ReplicationPackage/TobiaFig5Comp.xlsx",
                   col_names = TRUE)
tobia <- data.frame(tobia)
tobia$cond <- "Tobia" 

save(tobia,
     file = "/Users/engel/Documents/word/Manuskript/Verhalten/Engel McAdams/Data/ReplicationPackage/Tobia.RData")