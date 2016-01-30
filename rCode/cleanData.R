library(openxlsx)
library(scales)
library(DBI)
library(RPostgreSQL)
setwd("/Users/Qianqian/Downloads")

####read game sales data
dat <- read.xlsx('Applebees Game Sales By Rest - Last six weeks.xlsx',sheet=1,startRow = 5,colNames = FALSE,detectDates = TRUE)
#str(dat)


dat <- dat[,c(1:7,18:27)]
dat$X19 <- dat$X19 + dat$X20
dat$X24 <- dat$X24 + dat$X25
#str(dat)

dat <- dat[,c(1:7,8,9,11:14,16,17)]


colnames(dat) <- c('Business Day','Owner','Franchise','Restaurant Number','Restaurant Name','City',
                   'State','199 Ct_Gross','199 Ct Void','199 Ct Net','199 Net Sales',
                   '99 Ct_Gross','99 Ct Void','99 Ct Net','99 Net Sales')
dat$`Restaurant Number` <- as.numeric(dat$`Restaurant Number`)
#write.xlsx(dat,'Cleaned Game Sales.xlsx')
min(dat$`Business Day`)
max(dat$`Business Day`)


###read ticket data
dat.t <- read.xlsx('Applebees TicketCnts By Rest - Last Six weeks.xlsx',sheet=1,startRow = 4,colNames = FALSE,detectDates = TRUE)

#str(dat.t)
colnames(dat.t) <- c('Restaurant Number','Business Day','Total Tickets',
                     'TotalTTDPayment','DineInNoBarStoolTickest','DineInNoBarStoolsCredit',
                     'DineInNoBarStoolsCash','DineInNoBarStoolsGiftCard','DineInNoBarStoolsTTDPayment')

#write.xlsx(dat.t,'Cleaned Tickets.xlsx')
max(dat.t$`Business Day`)
min(dat.t$`Business Day`)
###read restaurant info

res.info <- read.xlsx("Applebee Data/Applebee's Restaurant Data Q1 2016.xlsx",sheet=3,startRow = 1,colNames = TRUE,detectDates = TRUE)
res.info$Corporate.Store.ID <- as.numeric(res.info$Corporate.Store.ID)

####merge data

merged.dat <- merge(dat,dat.t,by = c('Restaurant Number','Business Day'))

merged <- merge(merged.dat,res.info, by.x = 'Restaurant Number', by.y = 'Corporate.Store.ID')

merged$Owner <- NULL
merged$Franchise <- NULL
merged$ACCOUNTID <- NULL
merged$Account.Name <- NULL
#merged_sub <- subset(merged,merged$`199 Ct_Gross`!=0 & merged$`99 Ct_Gross`==0)
min(merged$`Business Day`)
max(merged$`Business Day`)
#length(unique(dat$`Restaurant Number`))
#length(unique(res.info$Corporate.Store.ID))


#fileName <- paste('Result/Current_Cleaned Merged Data(',min(merged$`Business Day`),'-',
 #                 max(merged$`Business Day`),').xlsx')
#write.xlsx(merged,fileName)

######merge with past data set
past.dat <- read.xlsx('Applebee Data/Result/Total Data.xlsx',detectDates = TRUE)
#past.dat <- new.total

min(past.dat$Business.Day)
max(past.dat$Business.Day)


new.week <- subset(merged,merged$`Business Day`>max(past.dat$Business.Day))

min(new.week$`Business Day`)
max(new.week$`Business Day`)
#new.week <- subset(new.week,new.week$`199 Ct_Gross`!=0 & new.week$`99 Ct_Gross`==0)


colnames(new.week) <- colnames(past.dat)
new.total <- rbind(past.dat,new.week)

#merged_remove <- subset(new.total,new.total$`199 Ct_Gross`!=0 & new.total$`99 Ct_Gross`==0)

fileName1 <- paste('Applebee Data/Result/Historical Data(',min(new.total$`Business.Day`),'-',
                  max(new.total$`Business.Day`),').xlsx')
write.xlsx(new.total,fileName1)
write.xlsx(new.total,'Applebee Data/Result/Total Data.xlsx')






############################add payment and orders#####################
payment <- read.xlsx('Order and pay data.xlsx',colNames = TRUE,detectDates = TRUE)
payment$Day.of.Day <- convertToDate(payment$Day.of.Day)
payment <- payment[is.na(payment$Day.of.Day)==FALSE,]

#####
# create a connection
# save the password that we can "hide" it as best as we can by collapsing it
pw <- {
  "SJux7eBpuZRN"
}

# loads the PostgreSQL driver
drv <- dbDriver("PostgreSQL")
# creates a connection to the postgres database
# note that "con" will be used later in each connection to the database
con <- dbConnect(drv, dbname = "elc",
                 host = "pgdatabase-1.cjkahljjqai2.us-west-2.rds.amazonaws.com", port = 5432,
                 user = "elc_logger", password = pw)
rm(pw) # removes the password

# check for the cartable
dbExistsTable(con, "daily_restaurant_stats")

# query the data from postgreSQL
query <- paste( "select restaurant_code, 
        Day, online_prestos_count, 
               pos_check_total ,
               presto_orders_count , presto_orders_total ,
               presto_payments_count,
               presto_payments_total 
               
               from daily_restaurant_stats
               where day between '",min(dat$`Business Day`),"' and '",max(dat$`Business Day`),"' 
               and restaurant_code like 'applebees%' ")
df_postgres <- dbGetQuery(con, query)
payment <- df_postgres
####


colnames(payment) <- c('Restaurant.Code','Business Day','online_prestos_count',
                       'Restaurant.Revenue','Items.Ordered','Order.Total',
                       'Presto.Payments','Presto.Payments.Total')

res.pay <- merge(merged,payment,by = c('Restaurant.Code','Business Day'))

res.pay$LiveDays <- as.numeric(difftime(res.pay$`Business Day`,res.pay$`Go-Live.Date`,units = 'days'))


#sub.dat <- res.pay[,c(2,3,6,7,8,9:12,17:23,26:37)]

res.pay$Total.Revenue.after.Void <- res.pay$`199 Net Sales`+res.pay$`99 Net Sales`

res.pay$Price.Point <- ifelse(res.pay$`199 Ct_Gross`!=0 & res.pay$`99 Ct_Gross`==0,'$1.99','$.99')

#res.pay_sub <- subset(res.pay,res.pay$`199 Ct_Gross`!=0 & res.pay$`99 Ct_Gross`==0)

#final.data <- res.pay[,c(1,29,2,3,7,8,40,39,9:23,26:28,31,33:38)]

final.data <- res.pay[,c(1,25,2,3,5,6,34,33,7:24,26:32)]

load("~/Downloads/name.Rda")
colnames(final.data) <- name

fileName2 <- paste('Applebee Data/Result/Data with Payment(',min(final.data$`Business.Day`),'-',
                   max(final.data$`Business.Day`),').xlsx')
write.xlsx(final.data,fileName2)


