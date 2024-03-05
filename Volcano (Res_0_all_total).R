#설치해야 할 기본 package들. Install all 버튼 누르고 한 줄마다 Ctrl+Enter 누르기.
library(DESeq2)
library(ggplot2)
library(pheatmap)
library(readxl)
Res0alltotal <- read_excel("E:/KU/2023-2/PIPL files/6_DEGs/Res_0_all_total.xlsx")
View(Res0alltotal)
#DEG를 사용해서 분석 완료된 raw data
#log2FoldChange, pvalue, padj라고 쓰여진 column이 필요

#Volcano
#엑셀에서 옮긴 자료 중 사용할 일부를 numeric으로 바꾼다. 
Res0alltotal$log2FoldChange <- as.numeric(Res0alltotal$log2FoldChange)
Res0alltotal$pvalue <- as.numeric(Res0alltotal$pvalue)
Res0alltotal$padj <- as.numeric(Res0alltotal$padj)
#한계점 설정. 
cut_lfc <- 1
cut_pvalue <- 0.01
#with 안에 plot 같이 사용해서 columns 쉽게 가져올 수 있다. 
with(Res0alltotal, plot(log2FoldChange, -log10(padj), pch=20, #pch는 점의 모양. 20은 채워진 원.
                        main="Res_0_all_total", col='grey', cex=1.0, #cex는 점의 크기. 기본이 1. 
                        xlab=bquote(~Log[2]~fold~change), ylab=bquote(~-log[10]~adjusted~P)))
with(subset(Res0alltotal, padj<cut_pvalue & log2FoldChange>cut_lfc), 
     points(log2FoldChange, -log10(padj), pch=20, col='red', cex=1.0))
with(subset(Res0alltotal, padj<cut_pvalue & log2FoldChange<(-cut_lfc)), 
     points(log2FoldChange, -log10(padj), pch=20, col='blue', cex=1.0))
# FC&P-value cut-off 경계 지정
abline(v=0, col='black', lty=3, lwd=1.0) #v는 수직선, 검은색 기본, lty는 선종류, lwd는 굵기 1이 기본. 
abline(v=-cut_lfc, col='black', lty=2, lwd=1.0)
abline(v=cut_lfc, col='black', lty=2, lwd=1.0)
abline(h=-log10(max(Res0alltotal$padj[Res0alltotal$padj<cut_pvalue], na.rm=TRUE)), 
       col='black', lty=2, lwd=1.0)
# plot에 주석 달기. 
legend("topright", c("up-regulated","no-changed","down-regulated"), 
       pch=20, cex=0.7, col=c("red","grey","blue"), box.lwd = 0, bg = 'transparent') 
#cex는 주석 사이즈, box.lwd는 테두리 제거, bg는 주위 투명하게. 
