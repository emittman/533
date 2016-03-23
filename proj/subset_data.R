library(RSQLite)
name <- "C:/Users/emittman/GitHub/533/docs_2015/docs_2015/drive_stats.db"

#?dbConnect
conn <- dbConnect(drv = SQLite(),
          dbname = name)

#q1 <- query: get distinct serial numbers on units that began testing during 2015
q1 <- 'SELECT DISTINCT serial_number, smart_9_raw FROM drive_stats WHERE smart_9_raw < 25'

x <- dbGetQuery(conn, q1)


#q2 <- query: return data on those units which began testing in 2015
q2 <- paste(c('SELECT *
              FROM drive_stats
              WHERE serial_number IN ("',
              paste(x[,1],collapse='","'),
              '")'), collapse="")
subset <- dbGetQuery(conn, q2)
subset$date <- as.Date(subset$date)

#disconnect from database
dbDisconnect(conn)


library(plyr)

#summarize data by unit: giving model, start date and end date
subset2 <- ddply(subset[c(1:5,21)], .(serial_number), summarize,
                 start = min(date),
                 end = max(date),
                 model = model[1],
                 failed = sum(failure))
sum(subset2$failed)

saveRDS(subset2, file="unit_summaries.rds")
