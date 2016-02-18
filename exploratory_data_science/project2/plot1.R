nei <- readRDS("./data/summarySCC_PM25.rds")

yearly_total <- aggregate(Emissions ~ year, nei, sum)

png('plot1.png')
barplot(
  height = yearly_total$Emissions, 
  names.arg = yearly_total$year, 
  xlab = "Years", 
  ylab = "PM2.5 level",
  main = "Yearly PM2.5 emissions",
  col = "darkblue",
  space = 2
)
dev.off()
