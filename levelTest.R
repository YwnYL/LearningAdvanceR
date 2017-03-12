#levelTest
#1.
findNum <- function(y,x,l){
  B <- rep(x,l)
  num <- numeric(0)
  if(l <= length(y)){
    for(i in 1:(length(y)-l+1)){
      A <- y[i:(i+l-1)]
      if (isTRUE(all.equal(A,B))){
        num <- cbind(num,i)
      }
    }
  }
  return(num)
}

# findNum2 <- function(y,x,l){
#   B <- rep(x,l)
#   num <- numeric(0)
#   while
# }

y <-  c(1,0,0,1,1,1,0,1,1)
x <- findNum(y,1,2);x

#2.
raw <- read.delim("data/weather.txt",check.names = F, na.strings = ".")
library(reshape2)
data2.1 <- melt(raw,c("year","month","element"),variable.name = "date",na.rm = T)

tmax <- data2.1[which(data2.1$element=="tmax"),c("year","month","date","value")]
tmin <- data2.1[which(data2.1$element=="tmin"),c("year","month","date","value")]
total <- merge(tmax,tmin,by=c("year","month","date"))
names(total)[names(total)=="value.x"] <- "tmax"   #强迫症>_<
names(total)[names(total)=="value.y"] <- "tmin"
total$tdiff <- total$tmax-total$tmin
total <- total[order(total[,1],total[,2],total[,3]),]


#3.
#计算hfflights
library(hflights)
library(reshape2)
str(hflights)
raw1 <- hflights
raw1$UniqueCarrier <- as.factor(raw1$UniqueCarrier)

data3.1 <- raw1[c("Month","UniqueCarrier","ArrDelay")]
data3.1 <- data3.1[order(data3.1[,1],data3.1[,2]),]
a<- acast(data3.1,Month~UniqueCarrier,quantile,probs=2/10,na.rm=T)
dim1 <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
dim2 <- t(unique(data3.1["UniqueCarrier"]))
dim3 <- c("1/10 quant","2/10 quant","3/10 quant","4/10 quant","5/10 quant","6/10 quant","7/10 quant","8/10 quant","9/10 quant")
quantiles <- array(,c(dim(a),9),list(dim1,dim2,dim3))
for (i in 1:9){
  quantiles[,,i] <- acast(data3.1,Month~UniqueCarrier,quantile,probs=i/10,na.rm=T)
}
quantiles <- aperm(quantiles,c(3,1,2))
