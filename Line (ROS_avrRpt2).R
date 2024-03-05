#설치해야 할 기본 package들. Install all 버튼 누르고 한 줄마다 Ctrl+Enter 누르기. 
library(tidyverse)
library(dplyr)
library(ggplot2) 
library(dslabs)
library(rstatix)
library(ggpubr)

#엑셀에서 raw data 긁어오기 위한 패키지: readxl
library(readxl)
#raw_RA라는 변수에 긁어올 raw data 저장. " "안에 파일 저장한 경로 복붙
raw_RA <- read_excel("E:/KU/PIPL(2023-2024)/아카이브용/raw_ROS_AvrRpt2.xlsx")
#raw_RA 상위 여섯섯 줄만 확인하고
head(raw_RA)
#Time이라고 이름붙인 column에 시간(raw_RA의 1행) 넣고 mod_RA라는 변수에 저장
names(raw_RA)[1] <- c("Time")
mod_RA <- raw_RA[ ,1]

# 8가지 sample에 대해 시간별로 mean과 sd 구해서 mod_RA에 vfd_mean, vfd_sd라는 새로운 행을 mod_RA에 넣기
# vfd
vfd_mean <- apply(raw_RA[ ,2:13], 1, mean)
vfd_sd <- apply(raw_RA[ ,2:13], 1, sd)
head(vfd_mean)
head(vfd_sd)
mod_RA <- mod_RA %>% cbind(vfd_mean, vfd_sd)
head(mod_RA)
#시간만 있던 mod_RA 뒤에 vfd_mean, vfd_sd 붙은 것 확인 가능

# vd
vd_mean <- apply(raw_RA[ ,14:25], 1, mean)
vd_sd <- apply(raw_RA[ ,14:25], 1, sd)
mod_RA <- mod_RA %>% cbind(vd_mean, vd_sd)

#vf
vf_mean <- apply(raw_RA[ ,26:37], 1, mean)
vf_sd <- apply(raw_RA[ ,26:37], 1, sd)
mod_RA <- mod_RA %>% cbind(vf_mean, vf_sd)

#v
v_mean <- apply(raw_RA[ ,38:49], 1, mean)
v_sd <- apply(raw_RA[ ,38:49], 1, sd)
mod_RA <- mod_RA %>% cbind(v_mean, v_sd)

#fd
fd_mean <- apply(raw_RA[ ,50:61], 1, mean)
fd_sd <- apply(raw_RA[ ,50:61], 1, sd)
mod_RA <- mod_RA %>% cbind(fd_mean, fd_sd)

#d
d_mean <- apply(raw_RA[ ,62:73], 1, mean)
d_sd <- apply(raw_RA[ ,62:73], 1, sd)
mod_RA <- mod_RA %>% cbind(d_mean, d_sd)

#f
f_mean <- apply(raw_RA[ ,74:85], 1, mean)
f_sd <- apply(raw_RA[ ,74:85], 1, sd)
mod_RA <- mod_RA %>% cbind(f_mean, f_sd)

#m
m_mean <- apply(raw_RA[ ,86:97], 1, mean)
m_sd <- apply(raw_RA[ ,86:97], 1, sd)
mod_RA <- mod_RA %>% cbind(m_mean, m_sd)

#평균과 분산 포함 결과 확인
head(mod_RA)

#모든 mean을 하나의 column으로, 모든 sd를 하나의 column으로 모아준다. 
mod2_RA <- mod_RA %>% pivot_longer(c('vfd_mean','vd_mean','vf_mean','v_mean','fd_mean','d_mean','f_mean','m_mean'), 
                                   names_to = 'Type', values_to = 'Mean')
mod2_RA <- mod2_RA %>% pivot_longer(c('vfd_sd','vd_sd','vf_sd','v_sd','fd_sd','d_sd','f_sd','m_sd'), 
                                    names_to = 'Type2', values_to = 'Sd')
mod2_RA <- mod2_RA[mod2_RA$Type == 'vfd_mean' & mod2_RA$Type2 == 'vfd_sd'|mod2_RA$Type == 'vd_mean' & mod2_RA$Type2 == 'vd_sd'
                   |mod2_RA$Type == 'vf_mean' & mod2_RA$Type2 == 'vf_sd'|mod2_RA$Type == 'v_mean' & mod2_RA$Type2 == 'v_sd'
                   |mod2_RA$Type == 'fd_mean' & mod2_RA$Type2 == 'fd_sd'|mod2_RA$Type == 'd_mean' & mod2_RA$Type2 == 'd_sd'
                   |mod2_RA$Type == 'f_mean' & mod2_RA$Type2 == 'f_sd'|mod2_RA$Type == 'm_mean' & mod2_RA$Type2 == 'm_sd', ]
mod2_RA <- subset(mod2_RA, select=-c(Type2))
head(mod2_RA)

#위 mod2_RA의 Type에 8개 sample들의 이름교정
mod2_RA$Type[mod2_RA$Type == "vfd_mean"] <- "Valinomycin + flg22 + Dex"
mod2_RA$Type[mod2_RA$Type == "vd_mean"] <- "Valinomycin + Dex"
mod2_RA$Type[mod2_RA$Type == "vf_mean"] <- "Valinomycin + flg22"
mod2_RA$Type[mod2_RA$Type == "v_mean"] <- "Valinomycin"
mod2_RA$Type[mod2_RA$Type == "fd_mean"] <- "flg22 + Dex"
mod2_RA$Type[mod2_RA$Type == "d_mean"] <- "Dex"
mod2_RA$Type[mod2_RA$Type == "f_mean"] <- "flg22"
mod2_RA$Type[mod2_RA$Type == "m_mean"] <- "Mock"
#SE값 추가: Sd / sqrt(12번 시행)
mod2_RA <- mod2_RA %>% mutate(Sd = Sd/sqrt(12))
colnames(mod2_RA)[4] <- "SE"
#시각화할 자료 최종 확인
mod2_RA

#그래프 그리기
#ggplot이라는 함수 안에 시각화할 mod_RA를 담고, +로 세부적인 조정 시행한다. 
#1: line plot
ggplot(mod2_RA) + 
  #그래프 형식을 line으로. mod2_RA 중 x축은 Mean이라는 column, y축은 Type라는 column으로. 선 종류는 1(실선), 크기는 1. 
  geom_line(aes(x = Time, y = Mean, group = Type, color = Type), linetype = 1, size = 1) + 
  #x축에 Time, y축에 ROS burst(RLU)라고 라벨링
  labs(x='Time',y='ROS burst(RLU)') + 
  scale_y_continuous(expand = c(0, 0)) + 
  #범례(legend) 추가
  theme(legend.title=element_blank(), legend.key.size = unit(3, "mm"), legend.position="bottom") 
#2: line+errorbar
ggplot(mod2_RA) + 
  geom_line(aes(x = Time, y = Mean, group = Type, color = Type), linetype = 1, size = 1) + 
  geom_errorbar(aes(x = Time, y = Mean, ymin = Mean - SE, ymax = Mean + SE, color = Type), alpha = 0.3, width=0.3) + 
  #scale_color_grey(start = 0.2, end = 0.8) +
  #scale_color_manual(values = c('A'='red', 'B'='blue', 'C'='green')) + 
  #scale_color_brewer(palette = 3) +
  labs(x='Time',y='ROS burst(RLU)') + 
  scale_y_continuous(expand = c(0, 0)) + 
  theme_classic() + 
  theme(legend.title=element_blank(), legend.key.size = unit(3, "mm"), legend.position="bottom") 
#3: 8분할 line, PTI/ETI 구분
ggplot(mod2_RA) + 
  geom_line(aes(x = Time, y = Mean, group = Type, color = Type), linetype = 1, size = 0.7) + 
  labs(x='Time',y='ROS burst(RLU)') + 
  scale_y_continuous(expand = c(0, 0)) + 
  theme(legend.title=element_blank(), legend.key.size = unit(3, "mm"), legend.position="bottom") + 
  geom_vline(xintercept=50, linetype = 'dashed', color='black', size = 0.25) + 
  facet_wrap( ~ Type, ncol=2) 
#4: 8분할 line, errorbar, PTI/ETI 구분
ggplot(mod2_RA) + 
  geom_line(aes(x = Time, y = Mean, group = Type, color = Type), linetype = 1, size = 0.7) + 
  geom_errorbar(aes(x = Time, y = Mean, ymin = Mean - SE, ymax = Mean + SE, color = Type), alpha = 0.3) + 
  labs(x='Time',y='ROS burst(RLU)') + 
  scale_y_continuous(expand = c(0, 0)) + 
  theme(legend.title=element_blank(), legend.key.size = unit(3, "mm"), legend.position="bottom") + 
  geom_vline(xintercept=50, linetype = 'dashed', color='black', size = 0.25) + 
  facet_wrap( ~ Type, ncol=2) 
