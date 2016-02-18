library(ggplot2)

nei <- readRDS("./data/summarySCC_PM25.rds")

on_road <- nei[(nei$fips=="24510"|nei$fips=="06037") & nei$type=="ON-ROAD",  ]

yearly_by_fips <- aggregate(Emissions ~ year + fips, on_road, sum)
yearly_by_fips$fips[yearly_by_fips$fips=="24510"] <- "Baltimore"
yearly_by_fips$fips[yearly_by_fips$fips=="06037"] <- "Los Angeles"

png("plot6.png", width=480, height=480)
g <- ggplot(yearly_by_fips, aes(factor(year), Emissions)) +
  facet_grid(. ~ fips) +
  geom_bar(stat = "identity", width = .25, fill = "darkblue", color = "darkblue") +
  xlab("Year") +
  ylab("Total PM2.5 Emissions") +
  ggtitle("Total ON-ROAD motor vehicle emisions in\nBaltimore vs Los Angeles between 1999 and 2008")
print(g)
dev.off()
