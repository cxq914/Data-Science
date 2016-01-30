library(data.table)

#########identify the best and worst restaurant###########
merged_remove <- subset(merged,merged$`199 Ct_Gross`!=0 & merged$`99 Ct_Gross`==0)
fileName_remove <- paste('Cleaned Merged Data(',min(merged_remove$`Business Day`),'-',
                  max(merged_remove$`Business Day`),').xlsx')
write.xlsx(merged_remove,fileName_remove)
#write.xlsx(merged_remove,'Cleaned Merged Data.xlsx')
res.dat <- merged[,c(1,2,5,11,18)]
res.dat$average.NetRevenue.perNBNTG <- round(res.dat$`199 Net Sales`/res.dat$DineInNoBarStoolTickest,2)
res.dat$weekly <- as.Date(cut(res.dat$`Business Day`,breaks = "week",start.on.monday = TRUE))
res.avg <- aggregate(res.dat$average.NetRevenue.perNBNTG,by=list(res.dat$`Restaurant Number`,res.dat$weekly),data=res.dat,mean)
colnames(res.avg) <- c('Restaurant Number','Week','Average NetRevenue per NBNT')
res.avg$`Average NetRevenue per NBNT` <- round(res.avg$`Average NetRevenue per NBNT`,3)


period <- unique(res.avg$Week)
n <- length(period)

rank.dat <- unique(res.dat[,c(1,3)])
j=2
for (i in 1:n)
{
  sub <- subset(res.avg,res.avg$Week==period[i])
  sub$rank <- rank(-sub$`Average NetRevenue per NBNT`,ties.method= "first")
  sub<-sub[,c(1,3,4)]
  rank.dat <- merge(rank.dat,sub,by="Restaurant Number",all.x = TRUE)
  colnames(rank.dat) <- c(colnames(rank.dat)[1:j],paste('NetRevenue',period[i]),paste('Rank',period[i]))
  j = j+2
}

rank.only <- rank.dat[,c(1,2,4,6,8,10,12,14)]
rank.only$sumRank <- rank.only[,3]+rank.only[,4]+rank.only[,5]+rank.only[,6]+rank.only[,7]+rank.only[,8]
rank.only <- rank.only[order(rank.only$sumRank),]
write.xlsx(rank.only,'Restaurant_Rank.xlsx')


res.geo <- unique(merged[,c(1,6,7.28,29)])

rank.geo <- merge(rank.only,res.geo,by='Restaurant Number',all.x = TRUE)
rank.geo <- rank.geo[order(rank.geo$sumRank),]
rank.geo$sumRank <- round(rank.geo$sumRank/6,0)



write.xlsx(rank.geo,'Restaurant Rank.xlsx')

########################analysis##################
rank.dat <- rank.dat[order(rank.dat[,14],rank.dat[,12],rank.dat[,10],rank.dat[,8],
                          rank.dat[,6],rank.dat[,4]),]

rank.dat.na.rm <- na.omit(rank.dat)

a <- data.frame()

for (i in 1:6)
{
  cut1 <- quantile(rank.dat.na.rm[,(i*2+1)],probs=c(0.9,0.8,0.7,0.6,0.5,0.4,0.3,0.2,0.1,0),na.rm = TRUE)
  #a <- rbind(a,cut1)
  level <- cut(rank.dat.na.rm[,(i*2+1)],breaks=c(0.5,cut1),include.lowest = TRUE,right = TRUE)
  levels(level) <- c(10,9,8,7,6,5,4,3,2,1)
  rank.dat.na.rm <- cbind(rank.dat.na.rm,level)
}

colnames(rank.dat.na.rm) <- c(colnames(rank.dat.na.rm)[1:14],
                              'week1','week2','week3','week4','week5','week6')


################Transition Matrix#######
ranking.info <- createWorkbook()
week12.dat <- addWorksheet(wb=ranking.info,sheetName = 'Week1-Week2')
week13.dat <- addWorksheet(wb=ranking.info,sheetName = 'Week1-Week3')
week14.dat <- addWorksheet(wb=ranking.info,sheetName = 'Week1-Week4')
week15.dat <- addWorksheet(wb=ranking.info,sheetName = 'Week1-Week5')
week16.dat <- addWorksheet(wb=ranking.info,sheetName = 'Week1-Week6')


for (i in 16:20)
{
  week <- as.matrix(table(rank.dat.na.rm[,15],rank.dat.na.rm[,i]))
  week <- week[10:1,10:1]
  week.per <- matrix(data = 0,nrow = 10,ncol = 10)
  for (j in 1:10)
  {
    week.per[j,] <- percent(round(week[j,]/sum(week[j,]),2))
  }
  
  colnames(week.per) <- c('Level1','Level2','Level3','Level4','Level5','Level6','Level7',
                             'Level8','Level9','Level10')
  
  writeData(ranking.info,i-15,week,rowNames = TRUE,colNames = TRUE)
  writeData(ranking.info,i-15,week.per,startRow = 14,rowNames = TRUE,colNames = TRUE)
  
}

saveWorkbook(ranking.info,'RestaurantRank_Analysis_final.xlsx')

####################second method########################
ranking.info1 <- createWorkbook()
week12.dat <- addWorksheet(wb=ranking.info1,sheetName = 'Week1-Week2')
week23.dat <- addWorksheet(wb=ranking.info1,sheetName = 'Week2-Week3')
week34.dat <- addWorksheet(wb=ranking.info1,sheetName = 'Week3-Week4')
week45.dat <- addWorksheet(wb=ranking.info1,sheetName = 'Week4-Week5')
week56.dat <- addWorksheet(wb=ranking.info1,sheetName = 'Week5-Week6')


for (i in 15:19)
{
  week <- as.matrix(table(rank.dat.na.rm[,i],rank.dat.na.rm[,i+1]))
  week <- week[10:1,10:1]
  week.per <- matrix(data = 0,nrow = 10,ncol = 10)
  for (j in 1:10)
  {
    week.per[j,] <- percent(round(week[j,]/sum(week[j,]),2))
  }
  
  colnames(week.per) <- c('Level1','Level2','Level3','Level4','Level5','Level6','Level7',
                          'Level8','Level9','Level10')
  
  writeData(ranking.info1,i-14,week,rowNames = TRUE,colNames = TRUE)
  writeData(ranking.info1,i-14,week.per,startRow = 14,rowNames = TRUE,colNames = TRUE)
  
}

saveWorkbook(ranking.info1,'RestaurantRank_Analysis_method2.xlsx')


rank.dat.na.rm$general_level <- NULL


write.xlsx(rank.dat.na.rm,'Restaurant_Rank_Level.xlsx')


#########
res.rank <- read.xlsx('Restaurant_Rank_Level.xlsx')
res.rank[,15] <- as.numeric(res.rank[,15])
res.rank[,16] <- as.numeric(res.rank[,16])
res.rank[,17] <- as.numeric(res.rank[,17])
res.rank[,18] <- as.numeric(res.rank[,18])
res.rank[,19] <- as.numeric(res.rank[,19])
res.rank[,20] <- as.numeric(res.rank[,20])

res.rank$rank_avg <- round((res.rank$week1+res.rank$week2+res.rank$week3
                    +res.rank$week4+res.rank$week5+res.rank$week6)/6,2)
res.rank$avg_netRevenue <- round((res.rank[,3]+res.rank[,5]+res.rank[,7]
                                  +res.rank[,9]+res.rank[,11]+res.rank[,13])/6,2)

res.rank$type <- ifelse(res.rank$rank_avg<2,'Top10%',ifelse(res.rank$rank_avg>=9,'Bottom10%','others'))
#res.rank <- res.rank[order(res.rank$rank_avg),]
#top <- res.rank[1:120,]
#bottom <- res.rank[1051:1170,]

#top$type <- 'Top10%'
#bottom$type <- 'Bottom10%'

#rest.list <- rbind(top,bottom)
#write.xlsx(rest.list,'TopBottomRestaurants.xlsx')


total.dat <- read.xlsx('Applebee Data/Result/totalData_Payment.xlsx',detectDates = TRUE)

test <- subset(total.dat,total.dat$`199.Net.Sales`!=0)


sub.dat <- total.dat[,c(1,2,3,6,9:12,17:23,26:37)]

#aggregated.dat <- aggregate(.~Restaurant.Number,data = rest.merged,mean)

rest.merged <- merge(sub.dat,res.add,by.x = 'Restaurant.Code', by.y = 'Code') 

res.rank.sub <- res.rank[,c(1,21,22,23)]

rest.rank.merged <- merge(rest.merged,res.rank.sub,by='Restaurant.Number')
                     
write.xlsx(rest.rank.merged,'merged_restaurant_rank.xlsx')



