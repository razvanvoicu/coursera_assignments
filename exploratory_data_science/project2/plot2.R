nei <- readRDS("./data/summarySCC_PM25.rds")

baltimore <- nei[nei$fips == "24510", ]

yearly_total <- aggregate(Emissions ~ year, baltimore, sum)

png('plot2.png')
barplot(
  height = yearly_total$Emissions,
  names.arg = yearly_total$year,
  xlab = "years",
  ylab = "Yearly PM2.5 emissions",
  main = "Yearly PM2.5 emissions in Baltimore",
  col = "darkblue",
  space = 2
)
dev.off()
