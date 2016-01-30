library(data.table)

#########identify the best and worst restaurant###########
fileName2 <- paste('Applebee Data/Result/Data with Payment( 2015-11-23 - 2016-01-03 ).xlsx')
merged <- read.xlsx(fileName2,detectDates = TRUE)

aag.sub <- subset(merged,merged$Parent.Account=='Apple American Group, LLC (AAG)')

res.dat <- aag.sub[,c(3,4,12,19)]
res.dat$average.NetRevenue.perNBNTG <- round(res.dat$`199.Net.Sales`/res.dat$DineInNoBarStoolTickest,2)
res.dat$weekly <- as.Date(cut(res.dat$`Business.Day`,breaks = "week",start.on.monday = TRUE))
res.avg <- aggregate(res.dat$average.NetRevenue.perNBNTG,by=list(res.dat$`Restaurant.Number`,res.dat$weekly),data=res.dat,mean)
colnames(res.avg) <- c('Restaurant.Number','Week','Average.NetRevenue.per.NBNT')
res.avg$`Average.NetRevenue.per.NBNT` <- round(res.avg$`Average.NetRevenue.per.NBNT`,3)


period <- unique(res.avg$Week)
n <- length(period)

rank.dat <- as.data.frame(unique(res.dat$Restaurant.Number))
colnames(rank.dat) <- 'Restaurant.Number'
j=1
for (i in 1:n)
{
  sub <- subset(res.avg,res.avg$Week==period[i])
  sub$rank <- rank(-sub$`Average.NetRevenue.per.NBNT`,ties.method= "first")
  sub<-sub[,c(1,3,4)]
  rank.dat <- merge(rank.dat,sub,by="Restaurant.Number",all.x = TRUE)
  colnames(rank.dat) <- c(colnames(rank.dat)[1:j],paste('NetRevenue',period[i]),paste('Rank',period[i]))
  j = j+2
}

########################analysis##################
rank.dat[is.na(rank.dat)] <- 0

for (i in 1:6)
{
  cut1 <- quantile(rank.dat[,(i*2)],probs=c(0.9,0.8,0.7,0.6,0.5,0.4,0.3,0.2,0.1,0))
  #a <- rbind(a,cut1)
  level <- cut(rank.dat[,(i*2)],breaks=c(0.5,cut1),include.lowest = TRUE,right = TRUE)
  levels(level) <- c(10,9,8,7,6,5,4,3,2,1)
  rank.dat <- cbind(rank.dat,level)
}

colnames(rank.dat) <- c(colnames(rank.dat)[1:13],
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


write.xlsx(rank.dat,'Applebee Data/Restaurant_Rank_Level_aag.xlsx')


#########
res.rank <- read.xlsx('Applebee Data/Restaurant_Rank_Level_aag.xlsx')
res.rank[,14] <- as.numeric(res.rank[,14])
res.rank[,15] <- as.numeric(res.rank[,15])
res.rank[,16] <- as.numeric(res.rank[,16])
res.rank[,17] <- as.numeric(res.rank[,17])
res.rank[,18] <- as.numeric(res.rank[,18])
res.rank[,19] <- as.numeric(res.rank[,19])


res.rank$rank_avg <- round((res.rank$week1+res.rank$week2+res.rank$week3
                            +res.rank$week4+res.rank$week5+res.rank$week6)/6,2)
res.rank$avg_netRevenue <- round((res.rank[,2]+res.rank[,4]+res.rank[,6]
                                  +res.rank[,8]+res.rank[,10]+res.rank[,12])/6,2)

#res.rank$type <- ifelse(res.rank$rank_avg<2,'Top10%',ifelse(res.rank$rank_avg>=9,'Bottom10%','others'))

res.rank <- res.rank[order(res.rank$rank_avg),]

res.rank$type <- c(rep('Top10%',49),rep('10%-20%',48),rep('20%-30%',48),rep('30%-40%',48),
                   rep('40%-50%',48),rep('50%-60%',48),rep('60%-70%',48),rep('70%-80%',48),
                   rep('80%-90%',48),rep('Bottom10%',49))
#top <- res.rank[1:120,]
#bottom <- res.rank[1051:1170,]

#top$type <- 'Top10%'
#bottom$type <- 'Bottom10%'

#rest.list <- rbind(top,bottom)
#write.xlsx(rest.list,'TopBottomRestaurants.xlsx')


total.dat <- read.xlsx('Applebee Data/Result/Data with Payment( 2015-11-23 - 2016-01-03 ).xlsx',detectDates = TRUE)


#aggregated.dat <- aggregate(.~Restaurant.Number,data = rest.merged,mean)

res.rank.sub <- res.rank[,c(1,20,21,22)]

rest.rank.merged <- merge(total.dat,res.rank.sub,by='Restaurant.Number')

rest.rank.merged <- rest.rank.merged[,c(1,2,4,9:12,17:26,3,27,28:33,5,6,34:36)]

write.xlsx(rest.rank.merged,'Applebee Data/merged_restaurant_rank_aag.xlsx')


