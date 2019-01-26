library(readr)
library(ggplot2)
library(reshape2)

dataAsm <- read.csv("~/Code/ixy/measurements/asm/raw-freq.csv", header=FALSE)
dataLlvm <- read.csv("~/Code/ixy/measurements/llvm/raw-freq.csv", header=FALSE)
data <- merge(dataAsm, dataLlvm, by="V1")
data$V2.x <- data$V2.x * 2
data$V2.y <- data$V2.y * 2

b <- c(49, 55, 64, 71, 79, 82, 91, 100, 110)
l <- c(1.6, 1.8, 2.1, 2.3, 2.6, 2.7, 3.0, 3.3, 3.6)

d <- melt(data, id.vars=c("V1"))

png("hs-freq.png")
p <- ggplot(d) + geom_line(aes(x=V1, y=value, color=variable), size=2) + scale_x_continuous(breaks = b, labels = l) +
    xlab("CPU Frequency [GHz]") + ylab("Packet rate [Mpps]") + ggtitle("Packet rate at different CPU frequencies (batch size = 128)") +
    scale_y_continuous(limits=c(0, 12)) + scale_colour_manual(name="Backend", labels=c("GHC Native", "LLVM"), values=c("#396ab1", "#922428")) + 
    geom_point(aes(x=V1, y=value, color=variable), size=3)
print(p)