
library(lubridate)
library(dplyr)
library(dplyr)
library(reshape2)

call <- read.csv("D:/hr04/callcenter.csv")


summary(call)

# 통화상태가 불분명한 오류를 제거하고(서비스와 끊어짐 현상만 잔류)
call1 <- subset(call, subset = (outcome != "PHANTOM"))

# 호출시간 없이 즉각 수신한 데이터만 잔류
call1 <- subset(call1, subset = (vru_time >= 0))

# call1 <- filter(call,outcome !="PHANTOM",vru_time>=0)


# 통화서비스가 이루어지기전까지의 시간을 산출하여 wait_time변수 설ㅈ
call1$wait_time <- call1$vru_time + call1$q_time

# 날짜 함수 조정하여 콜센터 호출 월을 설정
call1$date <- ymd(call1$date)
call1$month <- substr(call1$date,6,7)
call1$month <- as.factor(call1$month)

# 호출시간대별 확인하기 위해 문자열로 바꾸고 추출
call_time = strsplit(as.character(call1$vru_entry), split=":")

call_hour <- numeric(nrow(call1))
for (q in 1:nrow(call1)) 
  call_hour[q] <- as.numeric(call_time[[q]][1])
call1$call_hour <- call_hour

# 날짜를 요일로 변경하여 새로운 변수 할당
call1$week <- wday(call1$date)
call1$week <- factor(call1$week,levels = c(1:7), labels = c("sun","mon","tue","wed","thu","fri","sat"))



# 콜 호출 시간대별/요일별 / 월별 통화 호출건수


outline1 <- call1 %>%
  group_by(month,week,call_hour) %>%
  summarize(call_case = sum(call_hour, na.rm = TRUE))

library(gridExtra)

g1<-ggplot(outline1,aes(month,call_case/1000,fill=week))+geom_bar(stat="identity")+theme_light()
g2<-ggplot(outline1,aes(month,call_case/1000,fill=week))+geom_bar(stat="identity",position="fill")+theme_light()
g3<-ggplot(outline1,aes(week,call_case/1000,fill=week))+geom_bar(stat="identity",position="dodge")+theme_light()
g4<-ggplot(outline1,aes(call_hour,call_case/1000))+geom_bar(stat="identity")+theme_light()

grid.arrange(g1,g2,g4, ncol=3)


#===================================================================

library(reshape2)

a <- aggregate(wait_time~month,call1,mean)
b <- aggregate(vru_entry~month,call1,length)

c <- aggregate(vru_entry~call_hour+outcome,call1,length)
d <- dcast(c,call_hour~outcome)

tab1<-aggregate(vru_time~call_hour,call1,length)

tab1$wait_mean <- aggregate(wait_time~call_hour,call1,mean)[,2]
tab1$ser_mean <- aggregate(ser_time~call_hour,call1,mean)[,2]

tab2 <- merge(d,tab1,by="call_hour")


library(dplyr)
mutate(tab2,response_rate=AGENT/(AGENT+HANG))



#===========================================

shift <- read.csv("D:/hr04/call_shift.csv")


filter(call,outcome=="AGENT")

call1 <- filter(call,outcome !="PHANTOM",vru_time>=0)
call2 <- filter(call1,outcome=="AGENT")

# 콜 시간대별 근무인원
tab21$person <- rowSums(shift[3:10])

# 콜 시간대별 통화 건수(년간)
tab21<-aggregate(vru_time~call_hour,call2,length)

# 콜 시간대별 대기시간 및 통화서비스 시간 합계
tab21$wait_sum <- aggregate(wait_time~call_hour,call2,sum)[,2]
tab21$ser_sum <- aggregate(ser_time~call_hour,call2,sum)[,2]

# 일 평균 통화건수
tab21 <- mutate(tab21,vru_day_rate=vru_time/363)

# 평균 대기및 서비스 시간(분 단위)
tab21 <- mutate(tab21,wait_min_ave=(wait_sum/60)/363)
tab21 <- mutate(tab21,ser_min_ave=(ser_sum/60)/363)

# 담당자 개별 통화건수
tab21 <- mutate(tab21,call_p=vru_day_rate/person)

# 담당자 평균 통화시간
tab21 <- mutate(tab21,ser_p=ser_min_ave/person)

# 평균 대기시간
tab21 <- mutate(tab21,wait_p=wait_min_ave/person)

# 건당 서비스 통화시간
tab21 <- mutate(tab21,call_ser_min=ser_min_ave/vru_day_rate)


# 평균 서비스통화 시간
mean(tab21$call_ser_min)

# 시간당 적정 통화건수(50분 설정)
50/mean(tab21$call_ser_min)

# 따라서 시간당 적정한 통화건수를 14건으로 목표를 설정한다면
tab21 <- mutate(tab21,t_person=vru_day_rate/14)

tab21$t_person <- ceiling(tab21$t_person)


g11<-ggplot(tab21,aes(call_hour,call_p))+geom_point()+geom_line()
g12<-ggplot(tab21,aes(call_hour,ser_p))+geom_point()+geom_line()
g13<- ggplot(tab21,aes(call_hour,t_person))+geom_point()+geom_line()
g14<- ggplot(tab21,aes(call_hour,person))+geom_point()+geom_line()
grid.arrange(g11,g12,g13,g4, ncol=2)


plot(tab21$call_hour,tab21$person,ylab="평균 근무인원",xlab="콜 시간대",ylim=c(0,10),pch=21,type="o",col="#228B22")
par(new=T)
plot(tab21$call_hour,tab21$t_person,ylab="평균 근무인원",xlab="콜 시간대",ylim=c(0,10),pch=20,type="o",col="#FF3030")
legend(x=10,y=13, c("현재근무인원","필요인원"), cex=0.7, pch=c(20,21),col=c("#FF3030","#228B22"))

ggplot(tab21,aes(call_hour,person))+geom_line()+
  geom_line(aes(y=t_person,col="red"))






