#' ---
#' title: "sciATAC "
#' author: "Caleb Lareau"
#' date: "July 12th, 2017"
#' ---

#+ echo=FALSE, message=FALSE, warning=FALSE
library(plotly)
library(BuenColors)
library(reshape2)
library(data.table)

"%ni%" <- Negate("%in%")

if(FALSE){
  gzin <- list.files("../idxstatsOut", full.names = TRUE)
  x <- sapply(gzin, function(g) as.numeric(data.frame(fread(paste0("zcat < ",g)))[,3]))
  rownames(x) <- data.frame(fread(paste0("zcat < ",gzin[1])))[,1]
  
  d <- data.frame(
    mouseReads = colSums(x[grepl("mouse", rownames(x)),]),
    humanReads = colSums(x[grepl("human", rownames(x)),]),
    mouseMT = x["mouseMT",],
    humanMT = x["humanMT",]
  )
  rownames(d) <- NULL
  d$mouseNuclearReads <- d$mouseReads - d$mouseMT; d$humanNuclearReads <- d$humanReads - d$humanMT
  d$nuclearMouseHumanRatio <- d$mouseNuclearReads / d$humanNuclearReads
  d$mtMouseHumanRatio <- d$mouseMT / d$humanMT
  d$fracMito <- (d$mouseMT + d$humanMT)/(d$mouseReads + d$humanReads)
  d$totalAlignedReads <- d$mouseReads + d$humanReads
  
  
  write.table(d, file = "../july12.sumstats.txt", sep = "\t", row.names = FALSE, col.names = TRUE, quote = FALSE)
}


#' # Species mixing plots
#+ echo=FALSE, message=FALSE, warning=FALSE, fig.height = 10, fig.width = 10, fig.align='center'
d <- read.table( "../july12.sumstats.txt", header = TRUE, stringsAsFactors = FALSE)
plot_ly(
  d, x = ~log2(nuclearMouseHumanRatio), y = ~log2(mtMouseHumanRatio), color = ~log2(totalAlignedReads),
  # Hover text:
  text = ~paste("TF: ", fracMito)
) %>% toWebGL()
