qf.charts <- read.csv("data/qf_charts.csv") 
qf.dots   <- read.csv("data/qf_dots.csv") 
qf.series <- read.csv("data/qf_series.csv") 

# vq1 ease of use    # vq2 understand   # vq3 vis. appeal    # vq4 engaging
# vq5 like           # vq6 trustworthy  # vq7 credible       # vq8 confidence
# vq9 match          # vq10 suitable    # vq11 easy to see   # vq12 clear
# vq13 ease of tasks # vq14 unc. factor # v15 expect

par(bty='n')
par(las=1)

# Charts
#boxplot(qf.charts$vq8, qf.charts$vq1, qf.charts$vq2, qf.charts$vq3, qf.charts$vq6, qf.charts$vq14, qf.charts$vq15, ylim = c(0, max(qf.charts$vq8)))
#points(1:7, c(mean(qf.charts$vq8), mean(qf.charts$vq1), mean(qf.charts$vq2), mean(qf.charts$vq3), mean(qf.charts$vq6), mean(qf.charts$vq14), mean(qf.charts$vq15)), pch="+", cex = 2)

# Dots
#boxplot(qf.dots$vq8, qf.dots$vq1, qf.dots$vq2, qf.dots$vq3, qf.dots$vq6, qf.dots$vq14, qf.dots$vq15, ylim = c(0, max(qf.dots$vq8)))
#points(1:7, c(mean(qf.dots$vq8), mean(qf.dots$vq1), mean(qf.dots$vq2), mean(qf.dots$vq3), mean(qf.dots$vq6), mean(qf.dots$vq14), mean(qf.dots$vq15)), pch="+", cex = 2)

# Series
boxplot(qf.series$vq8, qf.series$vq1, qf.series$vq2, qf.series$vq3, qf.series$vq6, qf.series$vq14, qf.series$vq15, ylim = c(0, max(qf.series$vq8)))
points(1:7, c(mean(qf.series$vq8), mean(qf.series$vq1), mean(qf.series$vq2), mean(qf.series$vq3), mean(qf.series$vq6), mean(qf.series$vq14), mean(qf.series$vq15)), pch="+", cex = 2)