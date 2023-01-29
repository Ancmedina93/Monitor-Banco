library(data.table)
library(dplyr)
library(foreach)
library(Hmisc)
library(tibble)

##############
# GET DATA
##############

setwd("C:/Users/ancm9/Desktop/Genebank Monitor/monitor/")

data.types <- fread("Expandedcollectiondata.csv", nrows = 2, header =TRUE) %>% colnames()
num.vars <- which(data.types == "Numerical")

data <- fread("Expandedcollectiondata.csv", sep =";", sep2 = ",", colClasses = list("character" = 2), skip = 2, data.table = F)
cnr <- fread("Paths.csv", sep = ";", colClasses = list("character" = 1))

cnr$Group <- capitalize(cnr$Group)
groups <- fread("Crop groups.csv", sep = ";", sep2 = ".", header = T)

##############
# SETTINGS
##############

#Currently, there is still germination assessment data rated 0, on a scale of 1 to 4. 
#These are extrapolated values which is assumed to be 1, but were not actually measured.
#Keeping the value as 0 leads to highly misleading averages. 
#Keep in mind: these values are only replaced to determine the averages of the sunburst diagram.
#With which value do you want to replace the zeros? Options: 1, NA.

germination.extrapolation.val <- 1

#What is the column name of the germination variable?

germination.colname <- "Germination class"

#This code replaces all zeros as specified above
germination.col <- grep(germination.colname, colnames(data), ignore.case = TRUE)
data[, germination.col] <- replace(data[, germination.col]==0, values = germination.extrapolation.val)


##############
# FORMAT DATA
##############
  
count <- foreach(i=1:nrow(cnr), .combine = "rbind") %do% {
  data[startsWith(data$PATH, cnr$PATH[i]),] %>% nrow()
}

# determine the means of every NUMERICAL variable for each path
means <- foreach(i=1:nrow(cnr), .combine = "rbind") %do% {
  data[startsWith(data$PATH, cnr$PATH[i]),] %>% 
    summarize_at(.vars = num.vars, .funs = mean, na.rm = TRUE)
}

# Count NAs of every NUMERICAL variable for each path
NA_count <- foreach(i=1:nrow(cnr), .combine = "rbind") %do% {
  data[startsWith(data$PATH, cnr$PATH[i]), colnames(data)[num.vars]] %>% is.na() %>% colSums()
}
colnames(NA_count) <- colnames(NA_count) %>% paste("NA", sep=".")

#Format collection summary
summary <- cbind(cnr, count, round(means, 2), NA_count)
colnames(summary)[1:3] <- c("CNR", "Cat", "Count"); head(summary)

summary <- filter(summary, summary$Count>0)
summary <- add_column(summary, ParentCNR = substring(summary$CNR, 1, nchar(summary$CNR)-2), .after = 1)
summary[nchar(summary$CNR) ==2, 2] <- "root"


#Add CGN root row
main.groups <- summary %>% filter(ParentCNR == "root") %>% 
  mutate_at(.vars = 4:ncol(summary), .funs = as.numeric)

columnsSelect <- grep("NA", colnames(main.groups))
matrisSummary <- c("root", "", "CGN collection", sum(main.groups$Count), main.groups[,5:(4+length(num.vars))] %>% colMeans(na.rm = T) %>% round(2), main.groups[,columnsSelect[1]: columnsSelect[length(columnsSelect)]] %>% colSums(na.rm = T))
matrisSummary <- t(matrisSummary)
matrisSummary <- data.frame(matrisSummary)
colnames(matrisSummary) <- colnames(main.groups)

summary <- rbind(summary, matrisSummary)

#Add crop groups
for (i in unique(groups$ParentCat)) {
  crops <- filter(groups, ParentCat == i) %>% pull("Cat")
  
  #set root from crops in group
  summary[summary$Cat %in% crops, 2] <- i
  
  #get summary for crops in group
  crops <- filter(summary, Cat %in% crops) %>% 
    mutate_at(.vars = 4:ncol(summary), .funs = as.numeric)
  
  #write crop group summary
  
  columnsSelect <- grep("NA", colnames(main.groups))
  matrisSummarycrop <- c(i, "root", i, sum(crops$Count), crops[,5:(4+length(num.vars))] %>% colMeans(na.rm = T) %>% round(2), crops[,columnsSelect[1]: columnsSelect[length(columnsSelect)]] %>% colSums(na.rm = T))
  matrisSummarycrop <- t(matrisSummarycrop)
  matrisSummarycrop <- data.frame(matrisSummarycrop)
  colnames(matrisSummarycrop) <- colnames(main.groups)
  
  
  #add crop group summary to summary dataframe
  summary <- rbind(summary, matrisSummarycrop)
}
summary <- filter(summary, summary$Count>0)

##############
# SAVE DATA
##############

fwrite(summary, "Sunburst_data.csv", sep = ";", row.names = FALSE, col.names = TRUE)

