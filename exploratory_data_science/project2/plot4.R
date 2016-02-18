library(ggplot2)

nei <- readRDS("./data/summarySCC_PM25.rds")
scc <- readRDS("./data/Source_Classification_Code.rds")

all <- merge(nei, scc, by="SCC")

coal  <- grepl("coal", all$Short.Name, ignore.case = TRUE)
plotData <- all[coal, ]

yearly_total <- aggregate(Emissions ~ year, plotData, sum)

png("plot4.png", width = 480, height = 480)
g1 <- ggplot(yearly_total, aes(factor(year), Emissions))
g2 <- g1 + 
  geom_bar(stat = "identity", width = .25, fill = "darkblue", color = "darkblue") +
  xlab("Year") +
  ylab("Total PM2.5 Emissions") +
  ggtitle("Total coal-based emissions between 1999 and 2008")
print(g2)
dev.off()
