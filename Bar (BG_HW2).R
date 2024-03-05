#설치해야 할 기본 package들. Install all 버튼 누르고 한 줄마다 Ctrl+Enter 누르기.
library(tidyverse)
library(dplyr)
library(ggplot2) 
library(dslabs)
library(rstatix)
library(ggpubr)

#엑셀에서 raw data 긁어오기 위한 패키지: readxl
library(readxl)
#bghw라는 변수에 긁어올 raw data 저장. " "안에 파일 저장한 경로 복붙
bghw <- read_excel("E:/KU/PIPL(2023-2024)/아카이브용/raw_BG_HW2.xlsx")

#bghw into dataframe
bghw <- as.data.frame(bghw)

#gather the columns t1, t2, t3 and t4
bghw <- bghw %>% gather(key = "time", value = "BGLog", t1, t2, t3, t4)
head(bghw)

#평균과 분산 확인
bghw %>%
  group_by(Day, Group) %>%
  get_summary_stats(BGLog, type = "mean_sd")

#group 내부 순서 고정
bghw$Group <- factor(bghw$Group, levels = c("Col-0 + DC3000(EV)", "Col-0 + DC3000(avrRps4)", "rps4 + DC3000(EV)", "rps4 + DC3000(avrRps4)", "rps4rrs1b + DC3000(EV)", "rps4rrs1b + DC3000(avrRps4)"))

#bar그래프
bghw_bar <- ggbarplot(bghw, x = "Group", y = "BGLog", 
                    add="mean_sd", add.param = list(color = "black", width=0.3), 
                    color = "Day", fill = "Day", position=position_dodge(0.8))

#anova
bghw_aov <- aov(BGLog ~ Day*Group, bghw)
summary(bghw_aov)
#결과: 별 표시로 day와 group 간 상호작용 고려해 통계적 유의성 있음 확인

#HSD test
library(agricolae)
bghw_HSD <- HSD.test(bghw_aov, trt = c("Day", "Group"), console = TRUE)

#먼저 HSD label 설정. group을 day0 6개, day3 6개 순으로 고정했으니까 그에 맞춰서. 
HSD_label <- c("a", "a", "a", "a", "a", "a", "b", "c", "b", "c", "b", "b")

#bac, RPS4 구분
bghw_f <- bghw %>% mutate(Bac = case_when(Group %in% c('RPS4 WT + ΔHopQ1(EV)', 'RPS4 E88V + ΔHopQ1(EV)') 
                                      ~ 'ΔHopQ1(EV)', TRUE ~ 'ΔHopQ1(a4)'), 
                      RPS4 = case_when(Group %in% c('RPS4 WT + ΔHopQ1(EV)', 'RPS4 WT + ΔHopQ1(a4)') 
                                       ~ 'RPS4 WT', TRUE ~ 'RPS4 E88V'))

#시각화할 최종 data
MeanSE_bghw <- bghw_f %>% 
  group_by(Day, Group) %>%
  summarise(mean_bghw = mean(BGLog), se_bghw = sd(BGLog)/sqrt(length(BGLog)))

#bargraph
bghw_ggbar <- ggplot(MeanSE_bghw, aes(x = Group, y = mean_bghw, fill = factor(Day)))

bghw_ggbar + 
  geom_bar(stat = "identity", width = 0.75, color = "white", position = position_dodge(width=0.8)) + 
  scale_fill_grey(start=0, end=.8) + 
  geom_errorbar(aes(ymax = mean_bghw + se_bghw, ymin = mean_bghw - se_bghw), 
                position = position_dodge(width=0.8), width = 0.3) + 
  #scale_y_continuous(trans = "log10", labels = scales::math_format(.x, format = "log")) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 8), breaks = c(0, 2, 4, 6, 8)) + 
  labs(title="", x="", y = expression(Log~(cfu/cm^{2})), fill = "") +
  geom_text(aes(x = Group, y = mean_bghw + se_bghw, 
                label = as.matrix(HSD_label)), position = position_dodge(width = 0.8), vjust = -(0.7)) + 
  theme_classic() + 
  theme(legend.key.width= unit(5,'mm'), legend.key.height= unit(1,'mm'), 
        legend.position = c(.05, .98), legend.text=element_text(size=10), 
        axis.text.x = element_text(size=9, angle=45, hjust=1), axis.text.y = element_text(size =13)) 
