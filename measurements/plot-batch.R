library(readr)
library(ggplot2)
library(reshape2)

dataAsm <- read.csv("~/Code/ixy/measurements/asm/raw-batch.csv", header=FALSE)
dataLlvm <- read.csv("~/Code/ixy/measurements/llvm/raw-batch.csv", header=FALSE)
data <- merge(dataAsm, dataLlvm, by="V1")
data$V2.x <- data$V2.x * 2
data$V2.y <- data$V2.y * 2

b <- c(1,2,4,8,16,32,64,128,256)

d <- melt(data, id.vars=c("V1"))

png("hs-batch-33.png")
p <- ggplot(d) + geom_line(aes(x=V1, y=value, color=variable), size=2) + scale_x_continuous(trans="log2", breaks = b, labels = b) +
    xlab("Batch size") + ylab("Packet rate [Mpps]") + ggtitle("Packet rate for different batch sizes (@ 3.3GHz)") +
    scale_y_continuous(limits=c(0, 12)) + scale_colour_manual(name="Backend", labels=c("GHC Native", "LLVM"), values=c("#396ab1", "#922428")) + 
    geom_point(aes(x=V1, y=value, color=variable), size=3)
print(p)