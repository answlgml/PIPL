# TCGAbiolinks로 GDC data protal에서 TCGA data 다운
setwd("E:/KU/2023-2/PIPL/TCGAbiolinks")
if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install(version = "3.18")
BiocManager::install("TCGAbiolinks")
library(TCGAbiolinks)
library(tidyverse)
BiocManager::install("maftools")
library(maftools)
library(pheatmap)
library(SummarizedExperiment)
BiocManager::install("sesame")
BiocManager::install("sesameData")
library(sesameData)

# GDC에서 기존 모든 projects 받고 그중 GBM 자료만 요약
gdc_all <- getGDCprojects()
getProjectSummary('TCGA-GBM')
# 원하는 몇몇 GBM의 data만 선별해 query 만들기
q_TG <- GDCquery(project = "TCGA-GBM", 
                 data.category = "Transcriptome Profiling", 
                 data.type = "Gene Expression Quantification",
                 workflow.type = "STAR - Counts", 
                 barcode =  c("TCGA-14-0736-02A-01R-2005-01", "TCGA-06-0211-02A-02R-2005-01", "TCGA-19-1389-02A-21R-2005-01")
                 )
#결과확인
res_q_TG <- getResults(q_TG)
# q_TG 다운로드
GDCdownload(q_TG)
# count만 matrix로 만들기
q_TG_forcount <- GDCprepare(q_TG, summarizedExperiment = TRUE)
count_q_TG <- assay(q_TG_forcount, 'fpkm_unstrand')
# 위의 count_q_TG가 pheatmap을 만들 수 있는 gene x sample 형식의 dataset!
# plot에 나타날 row의 gene들은 상위 10개로 지정
top10_TG <- count_q_TG %>% 
  rowVars() %>% 
  order(decreasing = TRUE) %>% 
  head(10)
# plot: showing differences in beta values between samples
# 세 sample 간 가장 유의미한 Gene Expression 차이를 보이는 상위 10개 gene
pheatmap(count_q_TG[top10_TG,])
