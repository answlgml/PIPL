#설치해야 할 기본 package들. Install all 버튼 누르고 한 줄마다 Ctrl+Enter 누르기.
library(tidyverse)
library(dplyr)
library(ggplot2) 
library(dslabs)
library(rstatix)
library(ggpubr)

#엑셀에서 raw data 긁어오기 위한 패키지: readxl
library(readxl)
#raw_cc라는 변수에 긁어올 raw data 저장. " "안에 파일 저장한 경로 복붙
raw_cc <- read_excel("E:/KU/PIPL(2023-2024)/아카이브용/raw_Callose count.xlsx")
#dataframe으로 바꿔주고
raw_cc <- as.data.frame(raw_cc)
#상위 줄만 확인차 출력
head(raw_cc)

#시각화 위한 data 형식 만든다. 
# 4 sample들을 count 수에 맞게 총 8번씩 반복해 쓰도록 한다. 
treatment <- rep(c("Mock", "Valinomycin", "flg22", "Valinomycin + flg22"), 8)
#raw_cc의 count1 ~ count8까지를 deposits 하에 하나의 column으로 만든다. 
deposits <- c(raw_cc[,2], raw_cc[,3], raw_cc[,4], raw_cc[,5], raw_cc[,6], raw_cc[,7], raw_cc[,8], raw_cc[,9])
#mod_cc에 위 treatment, deposits 저장하고
mod_cc <- data.frame(treatment, deposits)
#상위 6개만 출력해서 잘 되었는지 확인
head(mod_cc)

#각 treatment 별 deposits의 평균 구해보기(그냥 확인용!) 
aggregate(deposits ~ treatment, mean, data = mod_cc)

#boxplot1
mod_cc$treatment <- factor(mod_cc$treatment, levels=c("Mock", "Valinomycin", "flg22", "Valinomycin + flg22"))
boxplot(deposits~treatment, mod_cc, xlab = "", ylab = "Callose deposits", col=rgb(0.3,0.5,0.4,0.6), cex.axis=0.7)

#검정법
#1. 통계적 유의성 확인(Statistical Analysis)
#두 집단을 비교하는 경우, t-test를 사용.
#세 개 이상의 그룹을 비교하는 경우, anova를 사용. 
#p-value < 0.05이면 그룹 간 유의한 차이가 있다. 
#2. 사후검정(Post-Hoc test) 
#집단 간 통계적 유의미한 차이가 존재하면 사후 테스트(TUKEY HSD, DMRT 등등) 진행해 어떤 그룹이 다른지 문자로 식별. 

#1-1: 통계적 유의성 확인 by t-test
#mod_cc는 treatment에서 집단이 4개이므로 2개 비교하는 t-test 수행해야 해서 비효율적이다. 그래도 해보자면
#Mock 그룹과 flg22 그룹 비교
t.test(mod_cc$deposits[mod_cc$treatment=="Mock"], mod_cc$deposits[mod_cc$treatment=="flg22"])  
#결과: 분산 차이 있다고 가정했을 때 Mock과 flg22는 p-value = 0.0001308 < 0.05 이므로 통계으로 유의미한 차이가 있다. 
#위와 같은 과정을 Mock&Val, Mock&vf...6번 반복해야 한다. 

#1-2: 통계적 유의성 확인 by ANOVA(등분산검정)
#집단의 개수가 4개이므로 사용한다.
#1. aov 사용
aov_cc=aov(mod_cc$deposits~mod_cc$treatment)
aov_cc
#결과는 summary 함수로 확인한다.
summary(aov_cc) 
#결과: mod_cc$treatment 맨 끝에 별 세개이므로 유의확률 즉 p값이 0과 0.001 사이이다. 
#0.05보다 작으므로 집단들이 통계적으로 유의성 있고 유의미하다. 
#분산분석에서 p<0.05이면 사후분석(post hoc) 시행. 어떤 집단간 평균 차이 있는지 알려고 진행. 

#2. Anova 결과 사용한 tukey test
install.packages("multcompView")
library(multcompView)
TUKEY_cc <- TukeyHSD(x=aov_cc, 'mod_cc$treatment', conf.level=0.95)
#두 가지씩 유의성 정도 나타낸 table
plot(TUKEY_cc , las=1 , col="brown")

#tukey test 결과대로 group을 labeling
#group the treatments that are simmilar.
generate_label_df <- function(TUKEY_cc, variable){
  # Extract labels and factor levels from Tukey post-hoc 
  Tukey.levels <- TUKEY_cc[[variable]][,4]
  Tukey.labels <- data.frame(multcompLetters(Tukey.levels)['Letters'])
  #I need to put the labels in the same order as in the boxplot :
  Tukey.labels$treatment=rownames(Tukey.labels)
  Tukey.labels=Tukey.labels[order(Tukey.labels$treatment), ]
  return(Tukey.labels)
}
# Apply the function on my dataset
LABELS <- generate_label_df(TUKEY_cc, "mod_cc$treatment")
LABELS <- LABELS[c(2,3,1,4), ]

# Draw the basic boxplot
cc_box <- boxplot(mod_cc$deposits~mod_cc$treatment, ylim=c(min(mod_cc$deposits), 1.1*max(mod_cc$deposits)), 
                  col=c(rep('steelblue3', 2), 'orchid3', 'goldenrod3') , 
                  xlab=" ", ylab="Callose deposits" , main=" ", cex.axis=0.7)
#write the letter over each box
group_sim <- 0.1*max(cc_box$stats[nrow(cc_box$stats), ])
#Add the labels
text( c(1:nlevels(mod_cc$treatment)), cc_box$stats[nrow(cc_box$stats),]+group_sim, LABELS[,1], 
      col=c(rep('steelblue3', 2), 'orchid3', 'goldenrod3'))
text(1, 26.625, 'M=26.625', cex=0.5, col='black', pos=3)
text(2, 79.375, 'M=79.375', cex=0.5, col='black', pos=3)
text(3, 685.125, 'M=685.125', cex=0.5, col='black', pos=3)
text(4, 940.500, 'M=940.500', cex=0.5, col='black', pos=1)

#최종 boxplot
cc_box_req <- boxplot(mod_cc$deposits~mod_cc$treatment, ylim=c(min(mod_cc$deposits), 1.1*max(mod_cc$deposits)), 
                      col=c(rep('white', 2), 'gray', 'dimgray') , xlab=" ", ylab="Callose deposits / 2.0mm²" , 
                      main=" ", cex.axis=0.85)
group_sim_req <- 0.1*max(cc_box_req$stats[nrow(cc_box_req$stats), ])
text( c(1:nlevels(mod_cc$treatment)), cc_box_req$stats[nrow(cc_box_req$stats),]+group_sim_req, LABELS[,1], 
      col=c(rep('black', 4)))
# Add data points
mylevels <- levels(mod_cc$treatment)
levelProportions <- summary(mod_cc$treatment)/nrow(mod_cc)
for(i in 1:length(mylevels)){
  thislevel <- mylevels[i]
  thisvalues <- mod_cc[mod_cc$treatment==thislevel, "deposits"]
  # take the x-axis indices and add a jitter, proportional to the N in each level
  #amount=levelProportions[i]/1000 으로 해서 jitter 거의 일렬로 배열
  myjitter <- jitter(rep(i, length(thisvalues)), amount=levelProportions[i]/1000)
  points(myjitter, thisvalues, pch=20, cex=0.7, col=rgb(0,0,0,.9)) 
}
