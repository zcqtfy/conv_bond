library(xml2)
library(rvest)
library(RSelenium) 
library(RJSONIO)
library(jsonlite)
library(mongolite)
library(VIM) 
library(plyr)
library(gridExtra)
library(ggplot2)
library(graphics)
library(cluster)
library(corrplot)



#数据获取(东方财富)
url0 <- 'http://quote.eastmoney.com/center/fullscreenlist.html#bp_8'
number=code=name=close=changepct=stockcode=stockname=stockprice=stockcp=conprice=convalue=conprerate=bondprerate=trigput=trigred=redprice=strbondvaule=condate=blistdate=move=stchpct=sd=NULL

remDr = remoteDriver('localhost',4444L,browserName='firefox')
remDr$open()
remDr$navigate(url0)

for(i in 1:2)
{
web  <- remDr$getPageSource()[[1]] %>% read_html()
number <- c(number,web %>% html_nodes('tbody') %>% html_nodes('.listview-col-number') %>% html_text())
code <- c(code,web %>% html_nodes('tbody') %>% html_nodes('.listview-col-Code') %>% html_text())
name <- c(name,web %>% html_nodes('tbody') %>% html_nodes('.listview-col-Name') %>% html_text())
close <- c(close,web %>% html_nodes('tbody') %>% html_nodes('.listview-col-Close') %>% html_text())
changepct <- c(changepct,web %>% html_nodes('tbody') %>% html_nodes('.listview-col-ChangePercent') %>% html_text())
stockcode <- c(stockcode,web %>% html_nodes('tbody') %>% html_nodes('.listview-col-UnderlyingStockCode') %>% html_text())
stockname <- c(stockname,web %>% html_nodes('tbody') %>% html_nodes('.listview-col-UnderlyingStockName') %>% html_text())
stockprice <- c(stockprice,web %>% html_nodes('tbody') %>% html_nodes('.listview-col-UnderlyingStockPrice') %>% html_text())
stockcp <- c(stockcp,web %>% html_nodes('tbody') %>% html_nodes('.listview-col-UnderlyingStockCP') %>% html_text())
conprice <- c(conprice,web %>% html_nodes('tbody') %>% html_nodes('.listview-col-ConversionPrice') %>% html_text())
convalue <- c(convalue,web %>% html_nodes('tbody') %>% html_nodes('.listview-col-ConversionValue') %>% html_text())
conprerate <- c(conprerate,web %>% html_nodes('tbody') %>% html_nodes('.listview-col-ConvertiblePremiumRate') %>% html_text())
bondprerate <- c(bondprerate,web %>% html_nodes('tbody') %>% html_nodes('.listview-col-BondPremiumRate') %>% html_text())
trigput <- c(trigput,web %>% html_nodes('tbody') %>% html_nodes('.listview-col-TriggerPriceOfSpecialPut') %>% html_text())
trigred <- c(trigred,web %>% html_nodes('tbody') %>% html_nodes('.listview-col-TriggerPriceOfSpecialRedemption') %>% html_text())
redprice <- c(redprice,web %>% html_nodes('tbody') %>% html_nodes('.listview-col-Redemptionprice') %>% html_text())
strbondvaule <- c(strbondvaule,web %>% html_nodes('tbody') %>% html_nodes('.listview-col-StraightBondValue') %>% html_text())
condate <- c(condate,web %>% html_nodes('tbody') %>% html_nodes('.listview-col-ConversionDate') %>% html_text())
blistdate <- c(blistdate,web %>% html_nodes('tbody') %>% html_nodes('.listview-col-BAdinLISTDATE') %>% html_text())

pg <- remDr$findElement(using = "css selector", 'span#listview_next.next.paginate_button')
remDr$executeScript("arguments[0].click();", list(pg))
Sys.sleep(1)
}

#数据获取(网易财经)
urlst <- 'http://quotes.money.163.com/trade/lsjysj_'

for(i in stockcode)
{
url  <- paste(urlst,i,".html#01b07",sep = '')
webst  <- read_html(url)

stchpct <- c(stchpct,webst %>% html_nodes('table') %>% html_nodes('tr') %>% html_nodes('td:nth-child(7)') %>% html_text())
stchpct=as.numeric(stchpct)
stchpct <- na.omit(stchpct)
sd <- c(sd(stchpct))
move <- c(move,sd)
stchpct = sd = NULL
}

#构造数据框，并储存于各种媒介
bond=data.frame(number,code,name,close,changepct,stockcode,stockname,stockprice,stockcp,conprice,convalue,conprerate,bondprerate,trigput,trigred,redprice,strbondvaule,condate,blistdate,move)
print(bond)
write.table(bond, file = "c:/bond.txt", row.names = F, quote = F)
write.csv(bond,"c:/bond2.csv")
jsdata <- toJSON(bond)
write_json(jsdata,"c:/bond3.json")
con <- mongo(collection="bond",db="test",url="mongodb://localhost")
con$insert(bond)

#数据预处理
bond <- read.table("C:/Users/14113/Desktop/learn/data science/bond/bond.txt", head=TRUE)
bond$changepct <- as.numeric(sub("%", "", bond$changepct))/100
bond$stockcp <- as.numeric(sub("%", "", bond$stockcp))/100
bond$conprerate <- as.numeric(sub("%", "", bond$conprerate))/100
bond$bondprerate  <- as.numeric(sub("%", "", bond$bondprerate ))/100
bond$close=as.numeric(as.character(bond$close))
bond$stockprice=as.numeric(as.character(bond$stockprice))
bond$conprice=as.numeric(as.character(bond$conprice))
bond$convalue=as.numeric(as.character(bond$convalue))
bond$trigput=as.numeric(as.character(bond$trigput))
bond$trigred=as.numeric(as.character(bond$trigred))
bond$redprice=as.numeric(as.character(bond$redprice))
bond$strbondvaule=as.numeric(as.character(bond$strbondvaule))
bond$condate=as.Date(bond$condate)
bond$blistdate=as.Date(bond$blistdate)
bond$move=bond$move/100
is.na(bond)
bond <- na.omit(bond)

#去除转债字样与添加到期时间
bond[,3] <- gsub("转债","",bond[,3])
t=expiredate=NULL
for(i in 1:length(bond$number))
{
years=6+as.numeric(difftime(bond[i,19], "2018-05-27", units="days"))/365
expiredate <- c(years)
t <- c(t,expiredate)
}
bond <- cbind(bond,t)

#图1
h1<-ggplot(bond,aes(x=conprerate)) + geom_histogram(binwidth=0.1, fill="white", colour="black")
h2<-ggplot(bond,aes(x=bondprerate)) + geom_histogram(binwidth=0.1, fill="white", colour="black")
h3<-ggplot(bond,aes(x=move)) + geom_histogram(binwidth=0.005, fill="white", colour="black")
h4<-ggplot(bond,aes(x=t)) + geom_histogram(binwidth=0.1, fill="white", colour="black")
grid.arrange(h1,h2,h3,h4)

#聚类分析
prerate = as.data.frame(bond[,12:13])
km = kmeans(prerate,2, nstart=25)
prerate$cluster = factor(km$cluster)
centers=as.data.frame(km$centers)
prerate$name = bond$name

#图2
g1 <- ggplot(data=prerate, aes(x=conprerate, y=bondprerate, color=cluster )) + 
  geom_point() + theme(legend.position="right") +
  geom_point(data=centers, aes(x=conprerate,y=bondprerate, color=as.factor(c(1,2))), 
  size=10, alpha=.3, show.legend=FALSE) +xlab("转股溢价率")+ylab("纯债溢价率")

#图3
g2 <- g1 + geom_text(aes(label=name), size=3) + scale_color_discrete(name="分簇",breaks=c("1", "2"),labels=c("债性为主", "股性为主"))

#图4
bond$cluster = factor(km$cluster)
g3 <- ggplot(bond,aes(x=stockcp, y=changepct, color=cluster)) +geom_point() +stat_smooth(method=lm) +xlab("对应股票涨跌幅")+ylab("可转债涨跌幅")+ scale_color_discrete(name="分簇",breaks=c("1", "2"),labels=c("债性为主", "股性为主"))

#分类及分析统计性质
bondlike <- bond[bond$cluster==1, ]
stocklike <- bond[bond$cluster==2, ]
class <- c("bondlike","stocklike")
bond_mean <- c(mean(bondlike$changepct),mean(stocklike$changepct))
stock_mean <- c(mean(bondlike$stockcp),mean(stocklike$stockcp))
mean_div <- c(mean(bondlike$stockcp)/mean(bondlike$changepct),mean(stocklike$stockcp)/mean(stocklike$changepct))
bond_sd <- c(sd(bondlike$changepct),sd(stocklike$changepct))
stock_sd <- c(sd(bondlike$stockcp),sd(stocklike$stockcp))
sd_div <- c(sd(bondlike$stockcp)/sd(bondlike$changepct),sd(stocklike$stockcp)/sd(stocklike$changepct))
stat1 <- data.frame(class,bond_mean,stock_mean,mean_div,bond_sd,stock_sd,sd_div)
write.csv(stat1,"c:/change_stat.csv")

#图5
para <- c("平均数(债性为主)","平均数(债性为主)","标准差(债性为主)","标准差(债性为主)","平均数(股性为主)","平均数(股性为主)","标准差(股性为主)","标准差(股性为主)")
value <- c(mean(bondlike$changepct),mean(bondlike$stockcp),sd(bondlike$changepct),sd(bondlike$stockcp),mean(stocklike$changepct),mean(stocklike$stockcp),sd(stocklike$changepct),sd(stocklike$stockcp))
class <- c("可转债涨跌幅","对应股票涨跌幅","可转债涨跌幅","对应股票涨跌幅","可转债涨跌幅","对应股票涨跌幅","可转债涨跌幅","对应股票涨跌幅")
stat2 <- data.frame(para,value,class)
g4 <- ggplot(stat2, aes(x=para,y=value,fill=class)) +   geom_bar(stat="identity", width=0.5, position=position_dodge(0.5))+xlab("变量")+ylab("值")+ scale_fill_discrete(name="分类")

#相关分析
cor.test(stocklike$changepct,stocklike$stockcp,method="pearson")
cor.test(bondlike$changepct,bondlike$stockcp,method="pearson")

#股性可转债线性回归分析及残差分析及图6
lm.sol<-lm(stocklike$changepct ~ 1+stocklike$stockcp)
summary(lm.sol)
op<-par(mfrow=c(2, 2))
plot(lm.sol)
par(op)
#去除异常值后重新分析及图7
del<-stocklike[-13,]
lm.resol<-lm(del$changepct ~ 1+del$stockcp)
summary(lm.resol)
op2<-par(mfrow=c(2, 2))
plot(lm.resol)
par(op2)
#债性可转债线性回归分析
lm.sol2<-lm(bondlike$changepct ~ 1+bondlike$stockcp)
summary(lm.sol2)

#图8
other <- bond[c(12,13,20,21)]
corbond <- cor(other)
corrplot(corbond,method="shade",shade.col=NA),addCoef.col="black")

#计算内在价值
opvalue=div=v=d=NULL
for(i in 1:length(bond$number))
{
d1 <- log(bond[i,8]/bond[i,10])/bond[i,20]/sqrt(bond[i,21]*245/30)+(0.03462+bond[i,20]*bond[i,20]/30*245/2)*sqrt(bond[i,21]/245*30)/bond[i,20]
d2 <- d1-bond[i,20]*sqrt(bond[i,21]*245/30)
p1 <- pnorm(d1, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)
p2 <- pnorm(d2, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)
op <-  bond[i,8]*p1-bond[i,10]*p2*exp(-0.03462*bond[i,21])
v <-  c(op)
d <-  c((bond[i,4]/(bond[i,17]+op)-1))
opvalue <- c(opvalue,v)
div <-  c(div,d)
}
bond <- cbind(bond,opvalue,div)

#图9
g5<-ggplot(bond,aes(x=opvalue,y=..density..)) + geom_histogram(binwidth=1, fill="cornsilk", colour="grey")+geom_density()

#图10及图11
bondlike <- bond[bond$cluster==1, ]
stocklike <- bond[bond$cluster==2, ]
g6<-ggplot(bondlike,aes(x=opvalue,y=..density..)) + geom_histogram(binwidth=0.01, fill="cornsilk", colour="grey")+geom_density()
g7<-ggplot(stocklike,aes(x=opvalue,y=..density..)) + geom_histogram(binwidth=1, fill="cornsilk", colour="grey")+geom_density()

#图12
g8<-ggplot(bond,aes(x=div,y=..density..)) + geom_histogram(binwidth=0.05, fill="cornsilk", colour="grey")+geom_density()
g9<-ggplot(bondlike,aes(x=div,y=..density..)) + geom_histogram(binwidth=0.02, fill="cornsilk", colour="grey")+geom_density()
g10<-ggplot(stocklike,aes(x=div,y=..density..)) + geom_histogram(binwidth=0.05, fill="cornsilk", colour="grey")+geom_density()
grid.arrange(g8,g9,g10)

#投资策略建议
group1 <- bond[bond$bondprerate<0, ]
group1[c(2:9,13,24)]
group2 <- bond[bond$div<0, ]
arrstocklike <- arrange(stocklike,stocklike[,24])
group3 <- stocklike[stocklike$div<0.1, ]
group3[c(2:9,12,24)]
group4 <- bond[bond$conprerate<0, ]
group4[c(2:9,12,18,24)]
