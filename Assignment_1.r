install.packages("dplyr")    # alternative installation of the %>%
library(dplyr)
directory <- 'specdata'
n <- 'nitrate'
s <- 'sulfate'
get_table_of_files <- function(directory, id=1:332)
{
        numbers <- ''
    isFirst <- TRUE
    for (i in (id))
    {
        string_i <- toString(i)
        while(nchar(string_i) < 3)
        {
            string_i <- paste('0', string_i, sep='')
        }
        if (isFirst)
        {
            numbers <- paste(string_i, '.csv', sep='')
            isFirst <- FALSE
        }
        else
        {
            numbers <- paste(numbers, '|', string_i, '.csv', sep='')
        }
    }
    files <- list.files(path = directory, pattern = numbers, full.names=T)
    tbl <- sapply(files, read.csv, simplify=FALSE)
    rev(tbl)
    tbl
}
pollutantmean <- function(directory, pollutant, id=1:332){
    tbl <- get_table_of_files(directory, id)
    list_of_values = c()
    list_of_means = c()
    for (dt in tbl)
    {
        list_of_values <- c(list_of_values, (dt[pollutant][!is.na(dt[pollutant])]))        
    }
    # list_of_values
    mean <- mean(list_of_values)
    mean
}

pollutantmean(directory, s)
pollutantmean(directory, n)
pollutantmean(directory, s, 1:10)
pollutantmean(directory, n, 70:72)
pollutantmean(directory, n, 23)


complete <- function(directory, id = 1:332)
{
    tbl <- get_table_of_files(directory, id)
    complete_case_count <- ''
    for (dt in tbl)
    {
        this_clean_data_frame <- data.frame(dt[1, 'ID'], nrow(dt[complete.cases(dt), ]))
        names(this_clean_data_frame) <- c('id', 'nobs')
        if (length(complete_case_count) < 1)
        {
            complete_case_count <- this_clean_data_frame
            names(complete_case_count)<-c('id', 'nobs')
        }
        else
        {
            complete_case_count <- rbind(this_clean_data_frame, complete_case_count)
        }
    }
    complete_case_count
}

complete(directory)
complete(directory, 1)
complete(directory, c(2, 4, 8, 10, 12))
complete(directory, 30:25)
complete(directory, 3)


corr <- function(directory, threshold = 0)
{
    complete_cases <- complete(directory)
    complete_cases_above_threshold <- complete_cases[as.integer(complete_cases$nobs) > threshold, ]
    print(nrow(complete_cases_above_threshold))
    tbl <- get_table_of_files(directory, complete_cases_above_threshold$id)
    correlations <- c()
    for (dt in tbl)
    {
        dt <- dt[complete.cases(dt), ]
        # print(dt[1,'ID'])
        correlations <- c(correlations, cor(dt$nitrate, dt$sulfate))
    }
    correlations
}

cr <- corr(directory, 150)
head(cr)

summary(cr)

cr <- corr(directory, 400)
head(cr)

summary(cr)

cr <- corr("specdata", 5000)
summary(cr)

length(cr)

cr <- corr("specdata")
summary(cr)

length(cr)

pmatch(0.7266, cr)

