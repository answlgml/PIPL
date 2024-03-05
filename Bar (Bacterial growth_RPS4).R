#설치해야 할 기본 package들. Install all 버튼 누르고 한 줄마다 Ctrl+Enter 누르기.
library(tidyverse)
library(dplyr)
library(ggplot2) 
library(dslabs)
library(rstatix)
library(ggpubr)

#엑셀에서 raw data 긁어오기 위한 패키지: readxl
library(readxl)
#bg라는 변수에 긁어올 raw data 저장. " "안에 파일 저장한 경로 복붙
bg <- read_excel("E:/KU/PIPL(2023-2024)/아카이브용/raw_Bacterial growth_RPS4.xlsx")

#bg into dataframe
bg <- as.data.frame(bg)

#gather the columns t1, t2 and t3 into long format
bg <- bg %>% gather(key = "time", value = "BGLog", t1, t2, t3)
head(bg)

#평균과 분산 확인
bg %>%
  group_by(Day, Group) %>%
  get_summary_stats(BGLog, type = "mean_sd")

#group 내부 순서 고정
bg$Group <- factor(bg$Group, levels = c("WT", "E88V", "Filler", "N"))

#column의 anova
bg_aov <- aov(BGLog ~ Day*Group, bg)
summary(bg_aov)
#결과: 별 표시로 day와 group 간간 상호작용 고려해 통계적 유의성 있음 확인

#tukey test
library(multcompView)
bg_tukey <- TukeyHSD(bg_aov, which = "Group")

#HSD test
library(agricolae)
bg_HSD <- HSD.test(bg_aov, trt = c("Day", "Group"), console = TRUE)

#그래프 그리기 위해 위의 HSD 결과 변수에 저장
HSD_label <- c("a", "a", "a", "a", "b", "b", "c", "d")

#평균과 분산 담은 변수 만들기
MeanSE_bg <- bg %>% 
  group_by(Day, Group) %>%
  summarise(mean_bg = mean(BGLog), se_bg = sd(BGLog)/sqrt(length(BGLog)))

#HSD 적용 bargraph 그리기
#기본적인 bar 형식 설정하고
bg_ggbar <- ggplot(MeanSE_bg, aes(x = Day, y = mean_bg, fill = factor(Group)))
#plot1
bg_ggbar + 
  #위의 기본 그래프에 추가 설정
  #color = white로 하면 bar 테두리 없음
  geom_bar(stat = "identity", width = 0.75, color = "black", position = position_dodge(width=0.8)) +
  #grey scale 사용용
  scale_fill_manual(values = gray(1:4/4), labels = c("WT", "E88V", "Filler", "N")) + 
  #bar마다 errorbar 표시
  geom_errorbar(aes(ymax = mean_bg + se_bg, ymin = mean_bg - se_bg), position = position_dodge(width=0.8), width = 0.3) + 
  #밑에 #표시된 줄은 원래 log10으로 y값 바꿔주는데 이 경우 애초에 log값이이 raw data이므로 필요 없다. 
  #scale_y_continuous(trans = "log10", labels = scales::math_format(.x, format = "log")) +
  scale_y_continuous(breaks = c(0, 2, 4, 6, 8)) + 
  labs(title="RPS4", x="", y = expression(Log~(cfu~cm^{2})), fill = "") +
  geom_text(aes(x = Day, y = mean_bg + se_bg, 
                label = as.matrix(HSD_label)), position = position_dodge(width = 0.8), vjust = -(0.7)) + 
  theme_classic() + 
  theme(plot.title = element_text(color="black", size=16, face="bold.italic", hjust = 0.5), 
        legend.key.width= unit(5,'mm'), legend.key.height= unit(1,'mm'), legend.position = c(.1, .9), 
        axis.text.x = element_text(size=13), axis.text.y = element_text(size =13)) 
#plot2
bg_ggbar + 
  geom_bar(stat = "identity", width = 0.75, color = "black", position = position_dodge(width=0.8)) + 
  #scale_fill_manual(values = gray(1:4/4), labels = c("WT", "E88V", "Filler", "N")) + 
  geom_errorbar(aes(ymax = mean_bg + se_bg, ymin = mean_bg - se_bg), position = position_dodge(width=0.8), width = 0.3) + 
  #밑의 줄에서 expand가 y축 0 밑 부분 없애주는 거고, limits가 최대 최소 범위 설정이다. 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 8), breaks = c(0, 2, 4, 6, 8, 10)) + 
  labs(title="RPS4", x="", y = expression(Log~(cfu~cm^{2})), fill = "") +
  geom_text(aes(x = Day, y = mean_bg + se_bg, 
                label = as.matrix(HSD_label)), position = position_dodge(width = 0.8), vjust = -(0.7)) + 
  #밑에 panel. ~ 세 줄이 네모 형태로 나타내주는 code
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_rect(fill='white', color='black'),
        plot.title = element_text(color="black", size=16, face="bold.italic", hjust = 0.5), 
        legend.key.width= unit(5,'mm'), legend.key.height= unit(1,'mm'), legend.position = c(.1, .9), 
        axis.text.x = element_text(size=13), axis.text.y = element_text(size =13)) 
