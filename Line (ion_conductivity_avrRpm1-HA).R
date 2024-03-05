#설치해야 할 기본 package들. Install all 버튼 누르고 한 줄마다 Ctrl+Enter 누르기.
library(tidyverse)
library(dplyr)
library(ggplot2) 
library(dslabs)
library(rstatix)
library(ggpubr)

#엑셀에서 raw data 긁어오기 위한 패키지: readxl
library(readxl)
#ic라는 변수에 긁어올 raw data 저장. " "안에 파일 저장한 경로 복붙
ic <- read_excel("E:/KU/2023-2/PIPL files/ion conductivity/ion_conductivity.xlsx")

#Hour, Group, Mean, Sd 4개의 column으로 이미 엑셀상에서 한번 정리한 raw data를 긁어왔으므로
#별다른 전처리 없이 바로 plot 만드는 코드를 실행한다. 

#lineplot(24h 포함)
#ic로 그래프를 그릴 것이다. 색은 Group별로 나눠서. 
ggplot(ic, aes(x=Hour, y=Mean, group=Group, color=Group)) + 
  #그래프 종류는 line
  geom_line(size=1) + 
  #각 시간마다 점을 찍어라. 
  geom_point(aes(shape=Group), size=2) + 
  #errorbar 범위는 mean과 Sd값을 고려해서 설정. 
  geom_errorbar(aes(ymin=Mean-Sd, ymax=Mean+Sd), size=.7, width=.7) + 
  #그래프상에 나타날 x값 범위
  scale_x_continuous(breaks=c(0, 2, 4, 6, 8, 10, 12, 24)) + 
  #그래프상에 나타날 y값 범위
  scale_y_continuous(breaks=c(0, 20, 40, 60, 80)) + 
  #제목과 x, y축 이름
  labs(title="avrRpm1-HA",x="Time post onset of mesurement (hour)", y = "Ion conductivity (µS/cm)") +
  theme_classic() + 
  #추가적인 디자인 조정- 범례 추가, 사이즈 조절 등등
  theme(legend.title = element_blank(), legend.key.width= unit(5,'mm'), legend.key.height= unit(2,'mm'), 
        legend.position = c(.15, .9), legend.text=element_text(size=10), 
        axis.text.x = element_text(size=10), axis.text.y = element_text(size =10), 
        plot.title = element_text(hjust = 0.5, size=13))
#세부적인 그래프 디자인을 하고 싶다면 https://boring9.tistory.com/17 참고!
#각 줄을 추가할 때 ~~ "+" 꼭 붙이고 엔터 쳐서 다음 줄로 넘어가기!


#lineplot(24h 미포함)
#기본 ic 에서 Hour가 24가 아닌 것만 filter해서 ic_12가는 새로운 변수에 저장해 시각화
ic_12 <- ic %>% filter(Hour != 24)
#ic 대신 ic_12 사용
ggplot(ic_12, aes(x=Hour, y=Mean, group=Group, color=Group)) + 
  geom_line(size=1) + 
  geom_point(aes(shape=Group), size=2) + 
  geom_errorbar(aes(ymin=Mean-Sd, ymax=Mean+Sd), size=.7, width=.3) + 
  scale_x_continuous(breaks=c(0, 2, 4, 6, 8, 10, 12)) + 
  scale_y_continuous(breaks=c(0, 20, 40, 60, 80)) + 
  labs(title="avrRpm1-HA",x="Time post onset of mesurement (hour)", y = "Ion conductivity (µS/cm)") +
  theme_classic() + 
  theme(legend.title = element_blank(), legend.key.width= unit(5,'mm'), legend.key.height= unit(2,'mm'), 
        legend.position = c(.15, .9), legend.text=element_text(size=10), 
        axis.text.x = element_text(size=10), axis.text.y = element_text(size =10), 
        plot.title = element_text(hjust = 0.5, size=13))
