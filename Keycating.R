# Keycating task

rm(list=ls())
setwd("/home/sergiy/Documents/Work/Keycating")
classes = c("integer", "character", "integer", "character", "character", "numeric")
columnNames = c("del", "BC", "Cat", "Retailer", "Description", "Price")
descriptions = read.table("descr.txt", 
                          header = FALSE, 
                          sep =';', 
                          nrows = 1581828,
                          #nrows = 100,
                          comment.char = "",
                          colClasses = classes,
                          col.names = columnNames,
                          skipNul = TRUE)

descriptions = descriptions[,-1]

# Data set examination

## Show all lines for which Descriptions are less than 4
#descriptions[nchar(descriptions[,4])<4,]

## Show all lines which have missing price
#descriptions[is.na(descriptions[,5]),]


# Cleaning descriptions data set

descriptions = na.omit(descriptions)                    # delete empty lines
descriptions = descriptions[,-5]
descriptions = descriptions[descriptions[,4]!="*_*",]   # delete strange symbols
descriptions = descriptions[nchar(descriptions[,4])>=3,] # includes descriptions length of which is >=3
descriptions[,4] = gsub("[[:digit:] [:punct:] [:space:]]", "", descriptions[,4]) # delete figures, symbols & spaces
descriptions[,4] = tolower(descriptions[,4])

# Cycle which identifies number of possible letter combinations

n = sum(sapply(descriptions[,4], nchar))-2*nrow(descriptions) # for 3 letter combinations only
# n = 3*sum(sapply(descriptions[,4], nchar)) - 9*nrow(descriptions) # for 3, 4 & 5 letter combinations

# Load necessary libraries

library(dplyr)
library(data.table)
library(reshape2)

# Cycle for several lines table

## comment for me: nchar requires text code. Transform factor into char

y=cbind(numeric(n), character(n))
k = 0
for (j in 1:nrow(descriptions)){
    for (i in 1:(nchar(descriptions$Description[j])-2)) {
        #y = rbind(y, cbind(descriptions$Cat[j],substring(descriptions$Description[j], i, i+2)))
        k = k + 1
        y[k, 1] = descriptions$Cat[j]
        y[k, 2] = substring(descriptions$Description[j], i, i+2)
        
#        if (i <= (nchar(descriptions$Description[j])-3)) {
#            k = k + 1
#            y[k, 1] = descriptions$Cat[j]
#            y[k, 2] = substring(descriptions$Description[j], i, i+3)
#        }
#        
#        if (i <= (nchar(descriptions$Description[j])-4)) {
#            k = k + 1
#            y[k, 1] = descriptions$Cat[j]
#            y[k, 2] = substring(descriptions$Description[j], i, i+4)
#        }
    }
}
k

# Transform data into table and add column for calculating frequencies
y = as.data.table(y)
setnames(y, "V1", "cat")
setnames(y, "V2", "letters")

y2 = count(y, cat, letters)
rm(y)


# Claculation of frequencies by categories and letters

y2 = y2[y2$n > 2]    # delete all frequncies which are less than 2 as non-important


# Building table which reflects frequencies by categories

y3 = dcast(y2, letters ~ cat, sum, value.var = "n")

write.csv(y3, "matrix2.csv")

# Loop for building a table that show which letter combination appears in one category only
## Comment: probably doesn't make much sense

a=cbind(character(nrow(y3)), integer(nrow(y3)), integer(nrow(y3)))
k = 0

for (i in 1:nrow(y3)) {
    if (rowSums(y3[i,]== 0) == (ncol(y3)-2)) {
        for (j in 2:ncol(y3)){
            #if (y3[i, j] !=0) {a = rbind(a, cbind(y3[i,1], y3[i, j], colnames(y3[j])))}
            if (y3[i, j] !=0) {
                k = k + 1
                a[k, 1] = y3[i, 1]
                a[k, 2] = colnames(y3[j])
                a[k, 3] = y3[i, j]
            }
            
        }
    }
}
k
a = a[-1,]
a = as.data.table(head(a, k-1))
a = a[a$V3 > 9]

write.csv(a, "a2.csv")

write.csv(summary(y),"y.csv")

