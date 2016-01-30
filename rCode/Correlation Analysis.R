rank.dat <- read.xlsx('Applebee Data/merged_restaurant_rank.xlsx',detectDates = TRUE)


barplot(table(rank.dat$`199.Net.Sales`))

bottom <- subset(rank.dat,rank.dat$type=='Bottom10%')

middle <- subset(rank.dat,rank.dat$type=='others')

top <- subset(rank.dat,rank.dat$type=='Top10%')

franchise <- unique(rank.dat$Parent.Account)

n <- length(franchise)

cor.value <- vector()

for (i in 1:n)
{
  sub <- subset(rank.dat,rank.dat$Parent.Account==franchise[i])
  cor.value <- c(cor.value,round(cor(sub$`199.Net.Sales`,sub$DineInNoBarStoolTickest),2))
  
}

cor.franchise <- cbind(franchise,cor.value)

library(MASS)
library(ppcor)

rank.dat$voidPer <- round(rank.dat$`199.Ct.Void`/rank.dat$`199.Ct_Gross`,2)
rank.dat$orderPer <- round(rank.dat$Order.Total/rank.dat$Restaurant.Revenue,2)

sub.rank <- rank.dat[,c(35,8,11,19,27,22,30,34)]

sub.rank$Parent.Account <- as.factor(sub.rank$Parent.Account)
levels(sub.rank$Parent.Account) <- c(1:18)
sub.rank$Parent.Account <- as.numeric(as.character(sub.rank$Parent.Account))

sub.rank$State[sub.rank$State=='California'] <- 'CA'
sub.rank$State[sub.rank$State=='Colorado'] <- 'CO'
sub.rank$State[sub.rank$State=='Florida'] <- 'FL'
sub.rank$State[sub.rank$State=='Maryland'] <- 'MD'
sub.rank$State[sub.rank$State=='Maine'] <- 'ME'
sub.rank$State[sub.rank$State=='Minnesota'] <- 'MN'
sub.rank$State[sub.rank$State=='New Jersey'] <- 'NJ'
sub.rank$State[sub.rank$State=='New Mexico'] <- 'NM'
sub.rank$State[sub.rank$State=='Nevada'] <- 'NV'
sub.rank$State[sub.rank$State=='Ohio'] <- 'OH'
sub.rank$State[sub.rank$State=='Pennsylvania'] <- 'PA'
sub.rank$State[sub.rank$State=='Texas'] <- 'TX'

#sub.rank$State <- as.character(sub.rank$State)

sub.rank$State <- as.factor(sub.rank$State)
levels(sub.rank$State) <- c(1:47)
sub.rank$State <- as.numeric(as.character(sub.rank$State))

pcor(sub.rank[,c(2,4,7)])
sub.rank[is.na==TRUE,]
sum(is.na(sub.rank))
#new_DF <- sub.rank[rowSums(is.na(sub.rank)) > 0,]
new.df <- sub.rank[is.na(sub.rank$orderPer)==FALSE,]

pcor(new.df[,c(1,3:9)])



top.sub <- subset(sub.rank,sub.rank$type=='Top10%')
pcor(top.sub[,c(1,3:7,9)])



write.xlsx(dat.zip,'explore_data.xlsx')

dat.zip$averageRevenue<- dat.zip$`199.Net.Sales`/dat.zip$DineInNoBarStoolTickest

dat.zip1 <- dat.zip[,c(59,36,37,38,40,42:58)]
dat.zip1$Percent.Unemployed <- as.numeric(dat.zip1$Percent.Unemployed)
dat.zip1$`Median.age.(years)` <- as.numeric(dat.zip1$`Median.age.(years)`)
dat.zip1$`INCOME.AND.BENEFITS.(IN.2012.INFLATION-ADJUSTED.DOLLARS).-.Mean.household.income.(dollars)`<- as.numeric(dat.zip1$`INCOME.AND.BENEFITS.(IN.2012.INFLATION-ADJUSTED.DOLLARS).-.Mean.household.income.(dollars)`)

library(MASS)
library(ppcor)
library(ggplot2)
library(ggmap)

x <- pcor(dat.zip2)

y <- pcor(dat.zip1)

esti <-x$estimate
colnames(esti) <-colnames(dat.zip)
rownames(esti) <- colnames(dat.zip)


esti1 <- y$estimate
colnames(esti1) <-colnames(dat.zip1)
rownames(esti1) <- colnames(dat.zip1)


fremont <- subset(dat.zip,dat.zip$Restaurant.Code=='applebees_fremont')
curtner <- subset(dat.zip,dat.zip$Restaurant.Code=='applebees_curtner_7630')


zip.d <- unique(dat.zip[,c(1,2)])

