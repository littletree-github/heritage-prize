# load data
test_n = 10000 # set to -1 for all data
claims.y1 <- read.csv('../data/Claims_Y1.csv', nrows=test_n, string=F)
member.y1 <- read.csv('../data/Members_Y1.csv', string=F)
dih.y1 <- read.csv('../data/DayInHospital_Y2.csv', string=F); #dih.y1[,2] <- log(dih.y1[,2]+1)

# compare the once claim and multiple claim
claims.y1.count <- aggregate(ProviderID ~ MemberID, data=claims.y1, length); colnames(claims.y1.count)[2] <- 'Count'; claims.y1.count <- merge(claims.y1.count, dih.y1, by=1);

layout(matrix(1:2,2,1))
barplot(table(claims.y1.count$Count))
boxplot(with(claims.y1.count, split(DaysInHospital_Y2, Count)))

# focus on people who only claim once
claims.y1.once <- subset(claims.y1, MemberID %in% with(claims.y1.count, MemberID[Count==1]))
once.df <- merge(merge(claims.y1.once, member.y1, by=1), dih.y1, by=1)

# keep the useful features
useful.feat <- c('MemberID','specialty','placesvc','paydelay','PrimaryConditionGroup', 'CharlsonIndex', 'sex', 'AgeAtFirstClaim', 'DaysInHospital_Y2')
once.df.2 <- once.df[, useful.feat]

# generate the diag plot
barchart.pct <- function(dta=once.df.2, lab='PrimaryConditionGroup') {
  dta.sub <- dta[,c('MemberID', lab, 'DaysInHospital_Y2')]; colnames(dta.sub)[2] <- 'Condition'
  dta.agg <- within(merge(aggregate(MemberID ~ Condition + DaysInHospital_Y2, data=dta.sub, length), aggregate(MemberID ~ Condition, data=dta.sub, length), by=1), {pct = MemberID.x / MemberID.y})
  print(dta.agg)
  print(barchart(Condition ~ pct, group = DaysInHospital_Y2, stack=T, data=dta.agg))
}

library(lattice)

barchart.pct(once.df.2, 'specialty')
barchart.pct(once.df.2, 'PrimaryConditionGroup')
barchart.pct(once.df.2, 'CharlsonIndex')
barchart.pct(once.df.2, 'placesvc')
barchart.pct(once.df.2, 'sex')
barchart.pct(once.df.2, 'AgeAtFirstClaim')
