setwd('C:/Users/sgvin/Desktop/개인2/대학/2022/22-2/비즈니스정보시각화프로그래밍/팀플/data/')

# 버스 데이터 불러오기
data <- read.csv("버스/버스데이터_최종.csv")
head(data)
str(data)

# 총 승차인원 생성 
data$total <- data$X00시승차총승객수+data$X1시승차총승객수+data$X2시승차총승객수+
  data$X3시승차총승객수+data$X4시승차총승객수+data$X5시승차총승객수+data$X6시승차총승객수+
  data$X7시승차총승객수+data$X8시승차총승객수+data$X9시승차총승객수+data$X10시승차총승객수+
  data$X11시승차총승객수+data$X12시승차총승객수+data$X13시승차총승객수+data$X14시승차총승객수+
  data$X15시승차총승객수+data$X16시승차총승객수+data$X17시승차총승객수+data$X18시승차총승객수+
  data$X19시승차총승객수+data$X20시승차총승객수+data$X21시승차총승객수+data$X22시승차총승객수+
  data$X23시승차총승객수

library(dplyr)
month_data = data %>% group_by(사용년월) %>% summarise(month_total = sum(total),
                                                   month_am7 = sum(X7시승차총승객수),
                                                   month_am8 = sum(X8시승차총승객수),
                                                   month_am9 = sum(X9시승차총승객수),
                                                   month_pm2 = sum(X14시승차총승객수),
                                                   month_pm3 = sum(X15시승차총승객수),
                                                   month_pm4 = sum(X16시승차총승객수))
month_data$사용년월 <- paste(substr(month_data$사용년월,1,4),substr(month_data$사용년월,5,6),sep='-')


# 코로나 데이터 불러오기
covid <- read.csv('수정_서울시 코로나19 확진자 발생동향.csv', fileEncoding = "euc-kr")
head(covid)
str(covid)
# 코로나 데이터 기준일 숫자형으로 변경
covid$서울시.기준일 <- gsub('\\D','', covid$서울시.기준일)
covid$서울시.기준일 <- as.numeric(substr(covid$서울시.기준일,1,8))
covid[994:1016,]$서울시.기준일 = covid[994:1016,]$서울시.기준일+2000000000
covid$기준월  <- substr(covid$서울시.기준일,1,6)


month_covid = covid %>% group_by(기준월) %>% summarise(month_p = sum(서울시.추가.확진),
                                                    month_stack = sum(서울시.확진자))
month_covid$기준월 <- paste(substr(month_covid$기준월,1,4),substr(month_covid$기준월,5,6),sep='-')

## 그래프 --------------------
# 월별 코로나 확진자 라인그래프 ----------
library(ggplot2)
p <- ggplot(data=month_covid, aes(x= factor(기준월) ,y = month_p, group=1))
p + geom_area(alpha=0.1, fill = 'purple 3',colour='purple 3',size=0.7)  + 
  geom_text(aes(x='2022-01',y=2000000,label = "위드코로나 시행"), color="black",nudge_x=-2, size=4)+
  theme_classic()+
  theme(axis.text.x = element_text(size =10, angle = 45, vjust  = 1, hjust=1)
        ,legend.position = "none")+
  geom_vline(xintercept = '2022-01',linetype = 2 ,alpha=0.5,size=0.7)+
  labs(x='월',y='확진자수')+ 
  ggtitle("서울시 코로나 확진자 수") + theme(plot.title=element_text(face="bold", size=15, hjust=0.5))


# 2020-2021
p <- ggplot(data=month_covid[1:23,], aes(x= factor(기준월) ,y = month_p, group=1))
p + geom_area(alpha=0.1,size=0.7,fill = 'midnight blue' ,colour='midnight blue') + 
  theme_classic()+
  theme(axis.text.x = element_text(size =10, angle = 45, vjust  = 1, hjust=1)
        ,legend.position = "none")+
  labs(x='월',y='확진자수')+ 
  ggtitle("2020-2021 서울시 확진자 수") + theme(plot.title=element_text(face="bold", size=15, hjust=0.5))


# 2022
p <- ggplot(data=month_covid[24:29,], aes(x= factor(기준월) ,y = month_p, group=1))
p + geom_area(alpha=0.1,size=0.7,fill = 'midnight blue' ,colour='midnight blue') + 
  theme_classic()+
  theme(axis.text.x = element_text(size =10, angle = 45, vjust  = 1, hjust=1)
        ,legend.position = "none")+
  labs(x='월',y='확진자수')+ 
  ggtitle("2022 서울시 확진자 수") + theme(plot.title=element_text(face="bold", size=15, hjust=0.5))


# 누적확진자그래프 ----------
head(month_covid)
month_covid[1:23,]
p <- ggplot(data=month_covid[1:29,], aes(x= factor(기준월) ,y = month_stack, group=1))
p + geom_area(colour='red', alpha=0.3, fill='pink') + geom_point(size=1, shape=22, fill="pink", color="red")+
  geom_text(aes(x='2022-01',y=100000000,label = "위드코로나 시행"), color="black",nudge_x=-2, size=4)+
  theme_classic()+
  theme(axis.text.x = element_text(size =10, angle = 45, vjust  = 1, hjust=1))+ 
  geom_vline(xintercept = '2022-01',linetype = 2 ,alpha=0.5,size=0.7)+
  labs(x='월',y='누적 확진자 수')+ 
  ggtitle("서울시 누적 확진자 그래프") + theme(plot.title=element_text(face="bold", size=15, hjust=0.5))


## 구별 확진자 수 ----------------------
library(sf)
map <- st_read("sig.shp")
map$SIG_KOR_NM <- iconv(map$SIG_KOR_NM, localeToCharset(), from = "CP949", to = "UTF-8")
seoul_map <- map[map$SIG_CD <= 11740,]

seoul_covid <- read.csv('C:/Users/sgvin/Desktop/개인2/대학/2022/22-2/비즈니스정보시각화프로그래밍/팀플/data/자치구별확진자.csv')
seoul_covid$year  <- substr(seoul_covid$date,1,4)
library(dplyr)
library(tidyverse)
library(viridis)
library(RColorBrewer)
total_covid = seoul_covid %>% group_by(year,variable) %>% summarise(month_p = sum(value),.groups='keep')
M <- merge(seoul_map, total_covid, by.x = "SIG_KOR_NM", by.y='variable')

## 로그화
p <- ggplot() + 
  geom_sf(data = M, aes(fill = log(month_p))) +
  scale_fill_gradient(limits= c(min(log(M$month_p)),max(log(M$month_p))), low="light steel blue", high="blue 4")+
  theme(legend.text.align = 1,
        legend.title.align = 0.5,
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())+
  geom_sf_text(data = M, aes(label = SIG_KOR_NM),size=3,family="sans",color='white')
p+facet_wrap(~year)+ 
  ggtitle("서울시 자치구별 확진자 그래프") + theme(plot.title=element_text(face="bold", size=15, hjust=0.5))


## 철도 ------------
seoul_board <- read.csv('train/서울역승차.csv', fileEncoding = 'euc-kr')
seoul_alight <- read.csv('train/서울역하차.csv', fileEncoding = 'euc-kr')
yong_board <- read.csv('train/용산역승차.csv', fileEncoding = 'euc-kr')
yong_alight <- read.csv('train/용산역하차.csv', fileEncoding = 'euc-kr')
srt_board <- read.csv('train/수서 승차.csv', fileEncoding = 'euc-kr')
srt_alight <- read.csv('train/수서 하차.csv', fileEncoding = 'euc-kr')
ktx <- read.csv('train/ktx.csv', fileEncoding = 'euc-kr')
srt <- read.csv('train/srt.csv', fileEncoding = 'euc-kr')

#srt, ktx 이용률 비교
library(scales)
library(ggplot2)

srt$날짜 <- as.Date(srt$날짜)

datebreaks <- seq(as.Date("2018-01-01"), as.Date("2022-02-01"), by="1 month")

ggplot(data = srt[18:50,], aes(x=날짜))+
  geom_line(aes(y=rate), group=1, col='red',size=0.7)+
  geom_point(aes(y=rate), col='red', size=1)+
  geom_line(aes(y=ktx[18:50,]$rate), group=1, col='blue',size=0.7)+
  geom_point(aes(y=ktx[18:50,]$rate), col='blue', size=1)+
  geom_text(aes(x=as.Date('2019-06-01'),y=120000,label = "SRT"), color="black", size=5)+
  geom_text(aes(x=as.Date('2019-06-01'),y=90000,label = "KTX"), color="black", size=5)+
  scale_x_date(breaks=datebreaks, labels=date_format("%Y-%m"))+
  labs(x='날짜', y='이용고객' )+
  theme_classic()+
  theme(axis.text.x = element_text(size =10, angle = 45, vjust  = 1, hjust=1))+
  scale_y_continuous(labels = scales::comma)+ 
  ggtitle("SRT와 KTX 이용량 비교") + theme(plot.title=element_text(face="bold", size=15, hjust=0.5))


#서울역, 용산역 기차 승차인원 비교(ktx, 무궁화 포함)
ggplot(data=seoul_board, aes(x=날짜))+
  geom_line(aes(y=승차인원), col='green', size=1)+
  geom_point(aes(y=승차인원), col='green', size=2)+
  geom_text(aes(y=승차인원, label=승차인원), vjust=-1, size=3)+
  geom_line(aes(y=yong_board$승차인원), col='purple', size=1)+
  geom_point(aes(y=yong_board$승차인원), col='purple', size=2)+
  geom_text(aes(x=2017,y=16500000,label = "서울역"), color="black", size=5)+
  geom_text(aes(x=2017,y=8000000,label = "용산역"), color="black", size=5)+
  theme_classic()+
  geom_text(aes(y=yong_board$승차인원, label=yong_board$승차인원), vjust=-1, size=3)+
  theme(axis.text.x = element_text(size =10, angle = 45, vjust  = 0.6))+
  labs(x='날짜', y='승차인원' )+
  scale_y_continuous(labels = scales::comma)+ 
  ggtitle("서울역과 용산역 승차인원 비교") + theme(plot.title=element_text(face="bold", size=15, hjust=0.5))


#서울역 승, 하차 인원 비교
ggplot(data = seoul_board, aes(x=날짜))+
  geom_bar(aes(y=seoul_alight$하차인원), stat = 'identity', width = 0.2, fill='yellow')+
  geom_line(aes(y=승차인원), col='orange',size=0.7)+
  geom_point(aes(y=승차인원), col='orange', size=1)+
  geom_text(aes(y=승차인원, label=승차인원), vjust=+1)+
  geom_text(aes(y=seoul_alight$하차인원, label=seoul_alight$하차인원), vjust=-1)+
  theme_classic()+
  theme(axis.text.x = element_text(size =10, angle = 45, vjust  = 1, hjust=1))+
  labs(x='날짜', y='인원' )+
  scale_y_continuous(labels = scales::comma)+ 
  ggtitle("서울역 승하차인원 비교") + theme(plot.title=element_text(face="bold", size=15, hjust=0.5))



#용산역 승, 하차 인원 비교
ggplot(data = yong_board, aes(x=연도))+
  geom_bar(aes(y=yong_alight$하차인원), stat = 'identity', width=0.2, fill='yellow')+
  geom_line(aes(y=승차인원), col='orange',size=0.7)+
  geom_point(aes(y=승차인원), col='orange', size=1)+
  geom_text(aes(y=승차인원, label=승차인원), vjust=+1)+
  theme_classic()+
  geom_text(aes(y=yong_alight$하차인원, label=yong_alight$하차인원), vjust=-1)+
  theme(axis.text.x = element_text(size =10, angle = 45, vjust  = 1, hjust=1))+
  labs(x='날짜', y='인원' )+
  scale_y_continuous(labels = scales::comma)+ 
  ggtitle("용산역 승하차인원 비교") + theme(plot.title=element_text(face="bold", size=15, hjust=0.5))


#수서역 승, 하차 인원 비교
srt_board$날짜 <- as.numeric(srt_board$날짜)
srt_alight$날짜 <- as.numeric(srt_alight$날짜)

ggplot(data = srt_board, aes(x=날짜))+
  geom_bar(aes(y=srt_alight$하차인원), stat='identity', fill='yellow', width = 0.2)+
  geom_line(aes(y=승차인원), col='orange',size=0.7)+
  geom_point(aes(y=승차인원), col='orange', size=1)+
  geom_text(aes(y=승차인원, label=승차인원), vjust=+1)+
  geom_text(aes(y=srt_alight$하차인원, label=srt_alight$하차인원), vjust=-1)+
  labs(x='날짜', y='인원' )+
  scale_x_continuous(breaks = seq(2019, 2021, 1))+
  theme_classic()+
  theme(axis.text.x = element_text(size =10, angle = 45, vjust  = 1, hjust=1))+
  scale_y_continuous(labels = scales::comma)+ 
  ggtitle("수서역 승하차인원 비교") + theme(plot.title=element_text(face="bold", size=15, hjust=0.5))



## 버스 -----------------
# 월별 승차인원 라인그래프
p <- ggplot(data=month_data[6:42,], aes(x= factor(사용년월) ,y = month_total, group=1))
p + geom_line(colour='indian red 1', size=0.7) + 
  theme_classic()+
  theme(axis.text.x = element_text(size =10, angle = 45, vjust  = 1, hjust=1))+
  geom_vline(xintercept = c('2020-02', '2020-08','2020-11','2021-06','2022-01'),linetype = 2 ,alpha=0.5,size=0.7)+
  labs(x='월',y='승차인원')+ 
  ggtitle("서울시 버스 승차인원") + theme(plot.title=element_text(face="bold", size=15, hjust=0.5))

# 전월 대비 증감 그래프 -------
library(reshape)
(month_data$month_total[2]-month_data$month_total[1])/month_data$month_total[1]*100
m8 <- c(0)
m3 <- c(0)
for (i in 1:nrow(month_data)){
  m8 <- c(m8,(month_data$month_am8[i+1]-month_data$month_am8[i])/month_data$month_am8[i]*100)
  m3 <- c(m3,(month_data$month_pm3[i+1]-month_data$month_pm3[i])/month_data$month_pm3[i]*100)
}

month_data$am8 <- m8[1:46]
month_data$pm3 <- m3[1:46]

si <- data.frame(month_data[8:43,])
new.2 <- melt(data=si, id.vars=c('사용년월'),
            measure.vars=c('am8', 'pm3'))

p <- ggplot(data=new.2, aes(x= factor(사용년월) ,y = value, group=variable,fill=variable))
p + geom_bar(stat = 'identity', position = "dodge") + 
  theme_classic()+
  theme(axis.text.x = element_text(size =10, angle = 45, vjust  = 1, hjust=1))+
  labs(x='월',y='증감')+ 
  ggtitle("시간대별 전월 대비 승차인원 증감") + theme(plot.title=element_text(face="bold", size=15, hjust=0.5))


##  따릉이 --------------
bicyle <- read.csv('따릉이.csv')
bicyle$대여일시 <- substr(bicyle$대여일시, 1,7)
bicyle$대여건수 <- as.numeric(gsub('\\D','', bicyle$대여건수))
bicyle
ggplot(bicyle, aes(x = 대여일시, y = 대여건수, fill = 대여일시),element_text(size=7,angle=45,vjist=0.7))+ 
  geom_boxplot() + 
  stat_summary(fun = "mean", geom = "point", shape = 8,
               size = 2, color = "white")+
  theme_classic()+
  theme(axis.text.x = element_text(size =10, angle = 45, vjust  = 1, hjust=1),
        legend.position = "none")+
  labs(x='월',y='이용자수')+ 
  ggtitle("서울시 따릉이 대여횟수") + theme(plot.title=element_text(face="bold", size=15, hjust=0.5))




##지하철 --------------
subway <- read.csv('지하철/서울시 지하철 호선별 역별 시간대별 승하차 인원 정보.csv', fileEncoding='euc-kr')
head(subway)

subway$날짜 = as.factor(subway$날짜)

par(mfrow=c(1,2))
plot(x=subway$'날짜', y=subway$'승차인원', ann=FALSE, col='red', lwd=1)
title("월별 지하철 승차인원")
plot(x=subway$'날짜', y=subway$'하차인원', ann=FALSE, col='blue', lwd=1)
title("월별 지하철 하차인원")
plot(mfrow=c(1,1))


subway$날짜 <- paste(substr(subway$날짜,1,4),substr(subway$날짜,5,6),sep='-')

month_sub = subway %>% group_by(날짜) %>% summarise(month_on = sum(승차인원),
                                                  month_off = sum(하차인원))

p <- ggplot(data=month_sub, aes(x= 날짜 ,y = month_on, group=1))
p + geom_line(colour='blue 3', size=0.7) + 
  theme_classic()+
  theme(axis.text.x = element_text(size =10, angle = 45, vjust  = 1, hjust=1))+
  geom_vline(xintercept = c('2020-02', '2020-08','2020-11','2021-06'),linetype = 2 ,alpha=0.5,size=0.7)+
  geom_vline(xintercept = c('2022-01'), colour = 'dark green',linetype = 1 ,alpha=0.5,size=0.7)+
  labs(x='월',y='승차인원')+ 
  ggtitle("서울시 지하철 승차인원") + theme(plot.title=element_text(face="bold", size=15, hjust=0.5))


# 월별 평균 승차인원이 2백만명 이상인 역: 강남역
month_subway = subway %>% group_by(지하철역) %>% summarise(month_average = mean(승차인원))
subway_a = month_subway[month_subway$month_average >=2000000,] # 강남뿐

# 승차인원이 가장 많은 강남역
subway_g = subway[subway$지하철역 =='강남',]
subway_g

# 월별 강남역 승차인원
ggplot(subway_g, aes(x=날짜, y=승차인원, group=1)) +
  geom_bar(fill="dark sea green", stat="identity", width=0.8) + 
  ggtitle("월별 강남역 승차인원") + 
  theme_classic()+
  theme(axis.text.x = element_text(size =10, angle = 45, vjust  = 1, hjust=1))+
  theme(plot.title=element_text(face="bold", size=20, hjust=0.5))


