library(readr)
library(ggplot2)
library(reshape2)

dataAsm <- read.csv("./asm/raw-gc.csv", header=FALSE)
dataLlvm <- read.csv("./llvm/raw-gc.csv", header=FALSE)
dataAsmLess <- read.csv("./asm-less-opt/raw-gc.csv", header=FALSE)
data <- merge(dataAsm, dataLlvm, by="V1")
data <- merge(data, dataAsmLess, by="V1")

b <- c(1,2,4,8,16,32,64,128,256)

d <- melt(data, id.vars=c("V1"))

png("hs-gc.png")
p <- ggplot(d) + geom_line(aes(x=V1, y=value, color=variable), size=2) + scale_x_continuous(trans="log2", breaks = b, labels = b) +
    xlab("Batch size") + ylab("Time spent in GC [%]") + ggtitle("Time spent in garbage collection for differing batch sizes (@ 3.3GHz)") +
    scale_y_continuous(limits=c(0, 2)) + scale_colour_manual(name="Backend", labels=c("GHC Native (-O2)", "LLVM", "GHC Native (-O1)"), values=c("#396ab1","#3e9651", "#922428")) + 
    geom_point(aes(x=V1, y=value, color=variable), size=3)
print(p)
