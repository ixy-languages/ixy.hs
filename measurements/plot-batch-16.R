library(readr)
library(ggplot2)
library(reshape2)

dataAsm <- read.csv("./asm/raw-batch-16.csv", header=FALSE)
dataLlvm <- read.csv("./llvm/raw-batch-16.csv", header=FALSE)
dataAsmLess <- read.csv("./asm-less-opt/raw-batch-16.csv", header=FALSE)
data <- merge(dataAsm, dataLlvm, by="V1")
data <- merge(data, dataAsmLess, by="V1")
data$V2.x <- data$V2.x * 2
data$V2.y <- data$V2.y * 2
data$V2 <- data$V2 * 2

b <- c(1,2,4,8,16,32,64,128,256)

d <- melt(data, id.vars=c("V1"))

png("hs-batch-16.png")
p <- ggplot(d) + geom_line(aes(x=V1, y=value, color=variable), size=2) + scale_x_continuous(trans="log2", breaks = b, labels = b) +
    xlab("Batch size") + ylab("Packet rate [Mpps]") + ggtitle("Packet rate for different batch sizes (@ 1.6GHz)") +
    scale_y_continuous(limits=c(0, 12)) + scale_colour_manual(name="Backend", labels=c("GHC Native (-O2)", "LLVM", "GHC Native (-O1)"), values=c("#396ab1", "#3e9651", "#922428")) + 
    geom_point(aes(x=V1, y=value, color=variable), size=3)
print(p)
