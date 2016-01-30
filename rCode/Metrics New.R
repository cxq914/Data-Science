##read files from total_payment
###data set name as: merged
###create the matrix to store the metrics

fileName2 <- paste('Applebee Data/Result/Data with Payment( 2015-11-23 - 2016-01-03 ).xlsx')
merged <- final.data
#merged <- read.xlsx(fileName2,detectDates = TRUE)

num.parent <- length(unique(merged$Parent.Account))+1

merged$weekly <- as.Date(cut(merged$`Business.Day`,breaks = "week",start.on.monday = TRUE))
merged$VoidPer199 <- round(merged$`199.Ct.Void`/merged$`199.Ct_Gross`,4)*-1
merged$VoidPer99 <- round(merged$`99.Ct.Void`/merged$`99.Ct_Gross`,4)*-1

week <- unique(merged$weekly)

#sub.dat <- merged[,c(39,12,40,32,19,33,20,36,37,34,35,38)]
sub.dat <- merged[,c(8,12,16,28,17,19,20,31,32,29,30,33,34)]
avg.metric <-aggregate(.~weekly,data=sub.dat,mean)
Num.Location <- vector()
Location.199 <- vector()
Location.99 <- vector()
Void.199 <- vector()
Void.99 <- vector()
for (i in 1:6)
{
  week.sub <- subset(merged,merged$weekly==week[i])
  Num <- length(unique(week.sub$`Restaurant.Number`))
  Num.Location <- c(Num.Location,Num)
  
  sub.199 <- subset(week.sub,week.sub$Price.Point=='$1.99')
  Num199 <- length(unique(sub.199$Restaurant.Number))
  Location.199 <- c(Location.199,Num199)
  VoidPer199 <- percent(round(mean(sub.199$VoidPer199),4))
  Void.199 <- c(Void.199,VoidPer199)
  
  sub.99 <- subset(week.sub,week.sub$Price.Point=='$.99')
  Num99 <- length(unique(sub.99$Restaurant.Number))
  Location.99 <- c(Location.99,Num99)
  VoidPer99 <- percent(round(mean(sub.99$VoidPer99),4))
  Void.99 <- c(Void.99,VoidPer99)
}

avg.metric <- t(avg.metric[,2:13])

avg.metric <- round(avg.metric,2)
avg.metric <- rbind(Num.Location,Location.199,Location.99,avg.metric)
avg.metric <- rbind(Void.199,Void.99,avg.metric)
colnames(avg.metric) <- c(as.character(sort(as.Date(unique(merged$weekly)))))
avg.metric <- avg.metric[c(3,6,4,7,1,5,8,2,9:17),]


game.metrics <- createWorkbook()
overview <- addWorksheet(wb=game.metrics,sheetName = 'Overview')
franchiseReport <- addWorksheet(wb=game.metrics,sheetName = 'Franchise Report')

writeData(game.metrics,overview,avg.metric,rowNames = TRUE,colNames = TRUE)

##################franchise calculation##############
writeData(game.metrics,franchiseReport,t(c('Franchise Name',colnames(avg.metric))),startCol = 2,colNames = FALSE)
writeData(game.metrics,franchiseReport,rownames(avg.metric),startRow = 2,rowNames = FALSE)


franchise1 <- unique(merged$Parent.Account)

franchise <- load("~/Downloads/franchise.Rda")
franchise <- c(franchise,setdiff(franchise1,franchise))

for (i in 1:length(franchise))
{
  f.sub <- subset(merged,merged$Parent.Account==franchise[i])
  Num.Location <- franchise[i]
  Location.199 <- franchise[i]
  Location.99 <- franchise[i]
  Void.199 <- franchise[i]
  Void.99 <- franchise[i]
  for (j in 1:6)
  {
    w.sub <- subset(f.sub,f.sub$weekly==week[j])
    Num <- length(unique(w.sub$`Restaurant.Number`))
    Num.Location <- c(Num.Location,Num)
    
    sub.199 <- subset(w.sub,w.sub$Price.Point=='$1.99')
    Num199 <- length(unique(sub.199$Restaurant.Number))
    Location.199 <- c(Location.199,Num199)
    if (Num199!=0) {
      VoidPer199 <- percent(round(mean(sub.199$VoidPer199),4))
    }else{
      VoidPer199 = 0
    }
    #VoidPer199 <- percent(round(mean(sub.199$VoidPer199),4))
    Void.199 <- c(Void.199,VoidPer199)
    
    sub.99 <- subset(w.sub,w.sub$Price.Point=='$.99')
    Num99 <- length(unique(sub.99$Restaurant.Number))
    Location.99 <- c(Location.99,Num99)
    if (Num99!=0) {
      VoidPer99 <- percent(round(mean(sub.99$VoidPer99),4))
    }else{
      VoidPer99 = 0
    }
    Void.99 <- c(Void.99,VoidPer99)
    
  }
    #f.sub.dat <- f.sub[,c(39,12,40,32,19,33,20,36,37,34,35,38)]
    f.sub.dat <- f.sub[,c(8,12,16,28,17,19,20,31,32,29,30,33,34)]
    avg.parent <-aggregate(.~weekly,data=f.sub.dat,mean)
    
    #avg.parent <- t(avg.parent[,2:12])
    #avg.parent[c(1,3:11),] <- round(avg.parent[c(1,3:11),],2)
    #avg.parent[2,] <- percent(round(avg.parent[2,],4))
    #Franchise.Name <- rep(franchise[i],11)
    #avg.parent <- cbind(Franchise.Name,avg.parent)
    #avg.parent <- rbind(Num.Location,avg.parent)
    
    avg.parent <- t(avg.parent[,2:13])
    
    avg.parent <- round(avg.parent,2)
    Franchise.Name <- rep(franchise[i],12)
    avg.parent <- cbind(Franchise.Name,avg.parent)
    avg.parent <- rbind(Num.Location,Location.199,Location.99,avg.parent)
    avg.parent <- rbind(Void.199,Void.99,avg.parent)
    colnames(avg.parent) <- c('Franchise.Name',as.character(sort(as.Date(unique(merged$weekly)))))
    avg.parent <- avg.parent[c(3,6,4,7,1,5,8,2,9:17),]
    #avg.parent$`Total.Revenue.after.Void` <- as.character(paste('$',avg.parent$`Total.Revenue.after.Void`))
    avg.parent[2,] <- paste('$',avg.parent[2,])
    avg.parent[4,] <- paste('$',avg.parent[4,])
    avg.parent[7,] <- paste('$',avg.parent[7,])
    avg.parent[9,] <- paste('$',avg.parent[9,])
    avg.parent[14,] <- paste('$',avg.parent[14,])
    avg.parent[16,] <- paste('$',avg.parent[16,])
    
    
  writeData(game.metrics,franchiseReport,rownames(avg.metric),startRow = 2+(i-1)*17,rowNames = FALSE)
    
  writeData(game.metrics,franchiseReport,avg.parent,startCol = 2,startRow = 2+(i-1)*17,rowNames = FALSE,colNames = FALSE)
  
}  

fileName.metric <- paste('Applebee Data/Result/Weekly Metrics(',min(merged$`Business.Day`),'-',
                          max(merged$`Business.Day`),')_revised.xlsx')
saveWorkbook(game.metrics,fileName.metric)
