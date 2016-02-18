library(ggplot2)

nei <- readRDS("./data/summarySCC_PM25.rds")

baltimore  <- nei[nei$fips == "24510", ]

yearly_by_type <- aggregate(Emissions ~ year + type, baltimore, sum)

png("plot3.png", width=480, height=480)

g1 <- ggplot(yearly_by_type, aes(year, Emissions, color = type))
g2 <- g1 + 
  geom_line() +
  xlab("Year") +
  ylab("Total PM2.5 Emissions") +
  ggtitle("Total emissions in Baltimore between 1999 and 2008")
print(g2)
dev.off()
