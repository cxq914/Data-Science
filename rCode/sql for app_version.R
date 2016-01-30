library(openxlsx)
library(scales)
library(DBI)
library(RPostgreSQL)

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
query <- paste( "select count(main_app_version), main_app_version, desired_restaurant_code from devices d join restaurants r on d.desired_restaurant_code = r.code where status = 'Customer - Live' and r.code like 'applebees_%' group by main_app_version, desired_restaurant_code order by desired_restaurant_code; ")

query <- paste("select desired_restaurant_code,g ->>'app_name' as app_name, count(distinct mac) as Freq
from devices cross join json_array_elements(devices.other_app_versions) g
               where (g ->> 'app_name' = '7D Mine Train' or g ->> 'app_name' = 'Olaf''s Adventures') and last_heartbeat > now() - interval '2 day'
               group by desired_restaurant_code,g ->>'app_name'
               order by  count(distinct mac),g ->>'app_name' desc;")
df_postgres <- dbGetQuery(con, query)


query <- paste("select desired_restaurant_code, count(distinct mac) as HeartBeatFreq
from devices
               where last_heartbeat > now() - interval '2 day'
               group by desired_restaurant_code
               order by  count(distinct mac) desc;")
df_postgres_heartbeat <- dbGetQuery(con, query)

train <- subset(df_postgres,df_postgres$app_name=='7D Mine Train')
olaf <- subset(df_postgres,df_postgres$app_name=="Olaf's Adventures")

dat <- merge(train, olaf, by='desired_restaurant_code',all = TRUE)
dat1 <- dat[,c(1,3,5)]
colnames(dat1) <- c('Restaurant.Code','7D Mine Train',"Olaf's Adventures")


withHeart <- merge(dat1,df_postgres_heartbeat,by.x='Restaurant.Code', by.y = 'desired_restaurant_code',all=TRUE)

res <- unique(res.info[,c(3,7,8)])

res <- subset(res,res$`Number.of.Presto's.Installed`>0)



dat.new <- merge(withHeart,res, by='Restaurant.Code')

dat.new[is.na(dat.new)] <- 0
write.xlsx(dat.new,file = 'Disney Game with Heartbeat count.xlsx')


dat.final <- merge(dat1,res,by='Restaurant.Code',all=TRUE)

dat.final[is.na(dat.final)] <- 0

weired <- subset(dat.final,dat.final$`Number.of.Presto's.Installed`==0)

normal <- subset(dat.new,dat.new$`Number.of.Presto's.Installed`!=0)

withHeart[is.na(withHeart)] <- 0
normal <- withHeart
normal$`7D Mine Train Per` <- round(normal$`7D Mine Train`/normal$heartbeatfreq,4)
normal$`Olaf's Adventures Per` <- round(normal$`Olaf's Adventures`/normal$heartbeatfreq,4)

normal[normal$`7D Mine Train Per`>1,]$`7D Mine Train Per` <- 1

normal[normal$`Olaf's Adventures Per`>1,]$`Olaf's Adventures Per` <- 1

Train_level <- cut(normal$`7D Mine Train Per`,breaks=c(0,0.000001,0.5,0.9,0.99999,1),include.lowest = TRUE,right = TRUE)
levels(Train_level) <- c('0','<50%','50-90%','>90%','100%')
normal <- cbind(normal,Train_level)

Olaf_level <- cut(normal$`Olaf's Adventures Per`,breaks=c(0,0.000001,0.5,0.9,0.999999,1),include.lowest = TRUE,right = TRUE)
levels(Olaf_level) <- c('0','<50%','50-90%','>90%','100%')
normal <- cbind(normal,Olaf_level)

write.xlsx(normal,'Disney Game Downloads_with Heartbeat_new.xlsx')





query <- paste("select count(main_app_version), main_app_version, desired_restaurant_code 
from devices d join restaurants r on d.desired_restaurant_code = r.code 
               where status = 'Customer - Live' and r.code like 'applebees_%' and last_heartbeat > now() - interval '2 days'
               group by main_app_version, desired_restaurant_code 
               order by desired_restaurant_code;")

df_postgres <- dbGetQuery(con, query)


query1 <- paste("select count(distinct mac),  desired_restaurant_code 
from devices d join restaurants r on d.desired_restaurant_code = r.code 
               where status = 'Customer - Live' and r.code like 'applebees_%' and last_heartbeat > now() - interval '2 days'
               group by desired_restaurant_code 
               order by desired_restaurant_code;")

df_postgres1 <- dbGetQuery(con, query1)


version.count <- merge(df_postgres,df_postgres1, by = 'desired_restaurant_code')
version.count$diff <- version.count$count.x-version.count$count.y



query1 <- paste("select chain_name,restaurant_code, day, presto_games_total, round(avg(presto_games_total/pos_check_count),3) as AverageRevenuePerPOSCheck,pos_check_count,pos_check_total,presto_payments_count
from daily_restaurant_stats
                where restaurant_status = 'Customer - Live' and day = '2016-01-16' and restaurant_name like 'Applebee%' and pos_check_count >0
                group by chain_name,restaurant_code, day, presto_games_total,pos_check_count,pos_check_total,presto_payments_count
                order by day;
                ")

df_postgres1 <- dbGetQuery(con, query1)

query2 <- paste("select restaurant_code, presto_payments_count,pos_check_count from daily_restaurant_stats
where pos_check_count>0 and day > '2016-01-15'
                ")

df_postgres2 <- dbGetQuery(con, query2)


query3 <- paste("select chain_name,restaurant_code, day, presto_games_total, round(avg(presto_games_total/pos_check_count),3) as AverageRevenuePerPOSCheck,pos_check_count,pos_check_total,presto_payments_count
                from daily_restaurant_stats
                where restaurant_status = 'Customer - Live' and day = '2016-01-17' and restaurant_name like 'Applebee%' and pos_check_count >0
                group by chain_name,restaurant_code, day, presto_games_total,pos_check_count,pos_check_total,presto_payments_count
                order by day;
                ")

df_postgres3 <- dbGetQuery(con, query3)

query4 <- paste("select chain_name,restaurant_code, day, presto_games_total, round(avg(presto_games_total/pos_check_count),3) as AverageRevenuePerPOSCheck,pos_check_count,pos_check_total,presto_payments_count
                from daily_restaurant_stats
                where restaurant_status = 'Customer - Live' and day = '2016-01-24' and restaurant_name like 'Applebee%' and pos_check_count >0
                group by chain_name,restaurant_code, day, presto_games_total,pos_check_count,pos_check_total,presto_payments_count
                order by day;
                ")

df_postgres4 <- dbGetQuery(con, query4)


total <- rbind(df_postgres1,df_postgres2,df_postgres3,df_postgres4)

 dat <- read.xlsx('Applebee Data/Result/Data with Payment( 2015-12-07 - 2016-01-17 ).xlsx', detectDates = TRUE)
 dat <- unique(dat[,c(1,5,6)])
 
 dat.total <- merge(total,dat, by.x = 'restaurant_code', by.y = 'Restaurant.Code')

write.xlsx(dat.total,'TotalData.xlsx')


res1 <- unique(df_postgres1$restaurant_code)
res2 <- unique(df_postgres2$restaurant_code)

sunday<- merge(df_postgres4,df_postgres3,by=c('chain_name','restaurant_code'))
sunday$diff <- sunday$presto_games_total.x-sunday$presto_games_total.y

total <- merge(sunday,dat, by.x = 'restaurant_code',by.y = 'Restaurant.Code')
write.xlsx(total,'totalData.xlsx')
