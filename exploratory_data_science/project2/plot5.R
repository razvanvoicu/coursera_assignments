library(ggplot2)

nei <- readRDS("./data/summarySCC_PM25.rds")

on_road <- nei[nei$fips == "24510" & nei$type == "ON-ROAD", ]

yearly_total <- aggregate(Emissions ~ year, on_road, sum)

png("plot5.png", width=480, height=480)
g1 <- ggplot(yearly_total, aes(factor(year), Emissions))
g2 <- g1 + 
  geom_bar(stat = "identity", width = .25, fill = "darkblue", color = "darkblue") +
  xlab("Year") +
  ylab("Total PM2.5 emissions") +
  ggtitle("Total ON-ROAD motor vehicle emissions in Baltimore between 1999 and 2008")
print(g2)
dev.off()
