# 전체 기간에서 각 지역의 평균 미세먼지량(PM10) 비교
f <- read.csv('pm10_per_place.csv', encoding="UTF-8") #전체기간에 대한 지역별 평균 미세먼지량 데이터
View(f)

library(ggplot2)

ggplot(f, aes(x=지역)) + geom_bar(aes(x=지역,y=PM10,fill=지역),stat='identity') +ggtitle('시,도별 평균 미세먼지량')+theme(plot.title = element_text(hjust = 0.5))

ggplot(f, aes(x=지역)) + geom_bar(aes(x=지역,y=PM10, fill=지역),stat='identity', size=1, color='black')+coord_flip()+ggtitle('시,도별 평균 미세먼지량')+theme(plot.title = element_text(hjust = 0.5))

#이 데이터에서 미세먼지량 상위 5개 지역만을 담은 새로운 데이터프레임 구축
newf=f[f$지역=='서울'|f$지역=='경기'|f$지역=='인천'|f$지역=='대구'|f$지역=='충북',]
ggplot(newf, aes(x=지역)) + geom_bar(aes(x=지역,y=PM10, fill=지역),stat='identity')+ggtitle('상위 다섯개 지역 평균 미세먼지량')+theme(plot.title = element_text(hjust = 0.5))


#지역과 측정일시를 그룹으로 한 월평균 미세먼지량 데이터
#각 지역의 2001년부터 2021년까지의 월별 미세먼지량
f2 <- read.csv('pm10perplacedate.csv', header=T, encoding='UTF-8') #지역과 측정일시를 그룹으로 한 평균 미세먼지량 데이터
View(f2)

#미세먼지량 상위 5개 지역에 대한 새로운 데이터프레임 구축
newf2=f2[f2$지역=='서울'|f2$지역=='경기'|f2$지역=='인천'|f2$지역=='대구'|f2$지역=='충북',]

#월평균에서 월만 지우고 연도만 남김 - 각 지역의 연도별 평균 미세먼지량을 알아내기 위해.
newf2 <- transform(newf2, 측정년도=substr(측정일시, 1, 4)) #측정년도라는 새로운 변수를 만듦.

# 지역과 연도를 그룹으로 하여 평균 미세먼지량을 나타내는 데이터프레임 생성
year_5places <- newf2 %>%
     group_by(지역, 측정년도) %>%
     summarise(PM10 = mean(PM10))
year_5places$측정년도<- as.numeric(year_5places$측정년도) #측정년도가 수치형으로 인식되도록 형변환
ggplot(year_5places, aes(x=측정년도, y=PM10)) + geom_point(aes(color=지역))+ggtitle('시,도별 연간 평균 미세먼지량')+theme(plot.title = element_text(hjust = 0.5))

ggplot(year_5places, aes(x=측정년도, y=PM10)) + geom_point(aes(x=측정년도,y=PM10,color=지역))+geom_smooth(method='lm')+ggtitle('시,도별 연간 평균 미세먼지량')+theme(plot.title = element_text(hjust = 0.5))

#충북의 연간 평균 미세먼지량 데이터
year_chungbuk = read.csv('year_chungbuk.csv', header=T, encoding='UTF-8', ignore_index=T)

ggplot(year_chungbuk, aes(x=측정일시, y=PM10)) + geom_bar(aes(x=측정일시, y=PM10, fill=측정일시), stat='identity')+geom_line(size=3, color='red')+ggtitle('충청북도 연간 평균 미세먼지량')+theme(plot.title = element_text(hjust = 0.5))
