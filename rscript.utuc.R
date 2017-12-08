setwd("~/Documents/MSKCC/utuc_genomics/data")
library(readxl)

nirmals <- as.data.frame(read_excel("UTUC IMPACT.xlsx"))

nirmals[(which(nirmals$`DMP ID` == "No IMPACT")),"DMP ID"] <- NA


ids <- c(nirmals$`DMP ID`, nirmals$`DMP ID__1`, nirmals$`DMP ID__2`)
ids <- ids[which(!is.na(ids))]

maf <- read.csv("data_mutations_extended.txt", header = TRUE, sep="\t", comment.char = "#", stringsAsFactors = FALSE)


utuc <- maf[(which(maf$Tumor_Sample_Barcode %in% ids)),]

rrr <- read.csv("RRR.csv", header=T, stringsAsFactors = F)

for(r in rownames(rrr)){
  stage <- rrr[r, "Path.stage"]
  if(stage == "pT1" | stage== "pT0" | stage =="T1" | stage =="pTa" | stage == "pTis"){rrr[r, "stage.simp"] <- "<=pT1"}else{rrr[r, "stage.simp"] <- ">=pT2"}
}
rm(r, stage)

library(ggplot2)

ttest <- t.test(rrr$RRR ~ rrr$stage.simp)


plot <- ggplot(data = rrr, aes(rrr$stage, rrr$RRR, colour = rrr$stage, show.legend = F)) +
  geom_boxplot() +
  geom_point(position = "jitter") +
  theme_classic() +
  ggtitle("Radiologic response rate by stage") +
  xlab("Stage") +
  ylab("Radiologic response rate") +
  geom_hline(yintercept=0, linetype="dashed") +
  annotate("text", x=1.5, y = -0.25, label="p<0.0001") + 
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        legend.title=element_blank())
ggpar(plot, font.title = c(15, "bold", "black"))


