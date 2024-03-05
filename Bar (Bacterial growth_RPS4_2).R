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
bg <- read_excel("E:/KU/PIPL(2023-2024)/아카이브용/raw_Bacterial growth_RPS4_2.xlsx")

#bg into dataframe
bg <- as.data.frame(bg)

#gather the columns t1, t2, t3 and t4
bg <- bg %>% gather(key = "time", value = "BGLog", t1, t2, t3, t4)
head(bg)

#평균과 분산 확인
bg %>%
  group_by(Day, Group) %>%
  get_summary_stats(BGLog, type = "mean_sd")

#group 내부 순서 고정
bg$Group <- factor(bg$Group, levels = c("RPS4 WT + ΔHopQ1(EV)", "RPS4 WT + ΔHopQ1(a4)", "RPS4 E88V + ΔHopQ1(EV)", "RPS4 E88V + ΔHopQ1(a4)"))

#raw로 그린 기본 bar그래프
bg_bar <- ggbarplot(bg, x = "Day", y = "BGLog", 
                    add="mean_sd", add.param = list(color = "black", width=0.3), 
                    color = "Group", fill = "Group", palette = "jco", position=position_dodge(0.8))

#anova
bg_aov <- aov(BGLog ~ Day*Group, bg)
summary(bg_aov)
#결과: 별 표시로 day와 group 간 상호작용 고려해 통계적 유의성 있음 확인

#HSD test
library(agricolae)
bg_HSD <- HSD.test(bg_aov, trt = c("Day", "Group"), console = TRUE)

#추가~

#anova
bg_aov_0 <- aov(BGLog ~ Group, bg[bg$"Day" == "day0", ])
summary(bg_aov_0)
#결과: 별 표시로 day0에서만 group별별 상호작용 고려해 통계적 유의성 있음 확인

#HSD test
library(agricolae)
bg_HSD <- HSD.test(bg_aov_0, trt = c("Group"), console = TRUE)

#추가끝~

#GRAPH 1: HSD label 올라간 그래프
#먼저 HSD label 설정
HSD_label <- c("a", "b", "a", "b", "c", "d", "c", "c")

#raw data에 있었던 NA 값 삭제
bg <- bg[-c(21, 22, 23, 24, 29, 30, 31, 32), ]
rownames(bg) <- NULL

#bac, RPS4 구분
bg_f <- bg %>% mutate(Bac = case_when(Group %in% c('RPS4 WT + ΔHopQ1(EV)', 'RPS4 E88V + ΔHopQ1(EV)') 
                                      ~ 'ΔHopQ1(EV)', TRUE ~ 'ΔHopQ1(a4)'), 
                      RPS4 = case_when(Group %in% c('RPS4 WT + ΔHopQ1(EV)', 'RPS4 WT + ΔHopQ1(a4)') 
                                       ~ 'RPS4 WT', TRUE ~ 'RPS4 E88V'))

#HSD 적용 bargraph 그리기
MeanSE_bg <- bg_f %>% 
  group_by(Day, Group) %>%
  summarise(mean_bg = mean(BGLog), se_bg = sd(BGLog)/sqrt(length(BGLog)))

bg_ggbar <- ggplot(MeanSE_bg, aes(x = Day, y = mean_bg, fill = factor(Group)))
bg_ggbar + 
  geom_bar(stat = "identity", width = 0.75, color = "white", position = position_dodge(width=0.8)) + 
  scale_fill_grey(start=0, end=.8) + 
  geom_errorbar(aes(ymax = mean_bg + se_bg, ymin = mean_bg - se_bg), position = position_dodge(width=0.8), width = 0.3) + 
  #밑의 줄은 raw data의 값들이 log값이 아닐 경우 실행
  #scale_y_continuous(trans = "log10", labels = scales::math_format(.x, format = "log")) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 8), breaks = c(0, 2, 4, 6, 8)) + 
  labs(title="", x="RPS4(Agrobacterium infiltrated)", y = expression(Log~(cfu/cm^{2})), fill = "") +
  geom_text(aes(x = Day, y = mean_bg + se_bg, 
                label = as.matrix(HSD_label)), position = position_dodge(width = 0.8), vjust = -(0.7)) + 
  theme_classic() + 
  theme(legend.key.width= unit(5,'mm'), legend.key.height= unit(1,'mm'), legend.position = c(.2, .95), legend.text=element_text(size=7.5), 
        axis.text.x = element_text(size=13), axis.text.y = element_text(size =13)) 


#GRAPH 2: bac 별로 구분(facet)한 그래프
#이 그래프는 문자로 유의성을 표시하지 않고 별로 나타낼 것이기 때문에 emmeans_test 시행
#pairwise comparison: emmeans_test()
install.packages("emmeans")
library(emmeans)
bg_f_em <- bg_f %>% 
  group_by(Day, Bac) %>%
  emmeans_test(BGLog ~ RPS4, p.adjust.method = "bonferroni") 

#xy position 지정!!!!!!!
bg_f_em <- bg_f_em %>% add_xy_position(x = "Day")
bg_f_em['y.position'] = c(3.15, 3.71, 5.54, 6.03)
#y position에 넣은 소수점들은 bar 별 위치 맞춰서 수동으로 조정하기

#RPS4 순서 고정
bg_f$RPS4 <- factor(bg_f$RPS4, levels = c("RPS4 WT", "RPS4 E88V"))
#facet 적용, t-test한 그래프
ggbar <- ggbarplot(bg_f, x = "Day", y = "BGLog", facet.by = "Bac",
                    add="mean_sd", add.param = list(color = "black", width=0.3), 
                    color = "white", fill = "RPS4", palette = "jco", position=position_dodge(0.8))
                    #테두리 검은색으로 하려면 color black. 

ggbar + 
  stat_pvalue_manual(bg_f_em, label = "p.adj.signif", tip.length = 0.02) +
  #scale_fill_manual(values = gray(1:2/2)) + 
  scale_fill_grey(start=.3, end=.8) + 
  #위에 줄은 grey scale 2가지 방법. 앞에#표시 지우면 적용 가능하다. 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 8), breaks = c(0, 2, 4, 6, 8)) + 
  labs(title="", x="", y = expression(Log~(cfu/cm^{2})), fill = "") + 
  #theme_bw() + 
  theme(legend.title = element_blank(), legend.key.width= unit(5,'mm'), legend.key.height= unit(1,'mm'), 
        legend.text=element_text(size=9))
