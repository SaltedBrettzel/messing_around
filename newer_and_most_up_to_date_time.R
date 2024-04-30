library(lubridate)
library(forecast)

USD <- read.csv("~/Downloads/EURUSD.csv")

WSJ <- read.csv("~/Downloads/EURUSD_3.csv")

USD <- USD[,-c(6:7)]

WSJ <- WSJ[order(WSJ$Date, decreasing = FALSE),]

WSJ$Date <- gsub("/","-",WSJ$Date)




WSJ$Date <- gsub("(\\d{2}-\\d{2}-)20","\\12020",WSJ$Date)
WSJ$Date <- gsub("(\\d{2}-\\d{2}-)08","\\12008",WSJ$Date)
WSJ$Date <- gsub("(\\d{2}-\\d{2}-)09","\\12009",WSJ$Date)
WSJ$Date <- gsub("(\\d{2}-\\d{2}-)10","\\12010",WSJ$Date)
WSJ$Date <- gsub("(\\d{2}-\\d{2}-)11","\\12011",WSJ$Date)
WSJ$Date <- gsub("(\\d{2}-\\d{2}-)12","\\12012",WSJ$Date)
WSJ$Date <- gsub("(\\d{2}-\\d{2}-)13","\\12013",WSJ$Date)
WSJ$Date <- gsub("(\\d{2}-\\d{2}-)14","\\12014",WSJ$Date)
WSJ$Date <- gsub("(\\d{2}-\\d{2}-)15","\\12015",WSJ$Date)
WSJ$Date <- gsub("(\\d{2}-\\d{2}-)16","\\12016",WSJ$Date)
WSJ$Date <- gsub("(\\d{2}-\\d{2}-)17","\\12017",WSJ$Date)
WSJ$Date <- gsub("(\\d{2}-\\d{2}-)18","\\12018",WSJ$Date)
WSJ$Date <- gsub("(\\d{2}-\\d{2}-)19","\\12019",WSJ$Date)
WSJ$Date <- gsub("(\\d{2}-\\d{2}-)21","\\12021",WSJ$Date)
WSJ$Date <- gsub("(\\d{2}-\\d{2}-)22","\\12022",WSJ$Date)
WSJ$Date <- gsub("(\\d{2}-\\d{2}-)23","\\12023",WSJ$Date)
WSJ$Date <- gsub("(\\d{2}-\\d{2}-)24","\\12024",WSJ$Date)


WSJ$Date <- as.Date(WSJ$Date, format="%m-%d-%Y")


WSJ <- WSJ[order(WSJ$Date, decreasing =FALSE),]

take_one <- WSJ[c(1:571),]

take_two <- WSJ[c(1232,1626,1718,2698,2918,4034:4044,4046:4050),]

USD <- USD[-c(239,473,986,1041,1057,1132,1154,1197,1220:1224,1226:1236,2352,2572,3552,3644,4038),]

combo <- rbind.data.frame(take_one,take_two)

row.names(combo) <- 1:nrow(combo)


mega_meld <- rbind.data.frame(combo,USD)

mega_meld$Date <- as.Date(mega_meld$Date,format="%Y-%m-%d")

mega_meld <- mega_meld[order(mega_meld$Date, decreasing = FALSE),]

row.names(mega_meld) <- 1:nrow(mega_meld)


mega_meld$t <- seq(1:5261)
t <- ts(mega_meld$t[1:5261])

Bicycle <- sin(2*pi*t/5261)

Tricycle <- cos(2*pi*t/5261)


break_point <- mega_meld$t[3500]

mega_meld$SBLaw <- ifelse(mega_meld$t>=break_point,0,1)
plot(mega_meld$Close, type = 'l')

Close <- ts(mega_meld$Close, frequency = 5)

DTCMoney <- tslm(Close~Bicycle+Tricycle)
summary(DTCMoney)
plot(DTCMoney$residuals)

plot(DTSB$residuals)

DTSBClose <- tslm(Close~Bicycle+Tricycle+mega_meld$SBLaw)
summary(DTSBClose)

par(mfrow=c(1,1))
plot(DTSBClose$residuals)

par(mfrow=c(1,2))
Acf(DTSBClose$residuals)
Pacf(DTSBClose$residuals)

ArimaClose <- Arima(DTSBClose$residuals, order=c(1,0,0))
summary(ArimaClose)

par(mfrow=c(1,2))
Acf(ArimaClose$residuals)
Pacf(ArimaClose$residuals)
