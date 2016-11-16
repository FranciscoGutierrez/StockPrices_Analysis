require(ggplot2)
library(reshape2)
library(car)

conf.interval <- function(data) {
    data <- t.test(data)
    mean   <- data$estimate
    min    <- data$conf.int[1]
    max    <- data$conf.int[2]
    output <- data.frame(mean,min,max)
    return(output)
}


#### First Study
s2.series.boxplot.easy   <- read.csv("data/blastprocessed/s2.series.boxplot.easy.csv"  )
s2.charts.boxplot.easy   <- read.csv("data/blastprocessed/s2.charts.boxplot.easy.csv"  )
s2.dots.boxplot.easy     <- read.csv("data/blastprocessed/s2.dots.boxplot.easy.csv"    )
s2.series.boxplot.medium <- read.csv("data/blastprocessed/s2.series.boxplot.medium.csv")
s2.charts.boxplot.medium <- read.csv("data/blastprocessed/s2.charts.boxplot.medium.csv")
s2.dots.boxplot.medium   <- read.csv("data/blastprocessed/s2.dots.boxplot.medium.csv"  )
s2.series.boxplot.hard   <- read.csv("data/blastprocessed/s2.series.boxplot.hard.csv"  )
s2.charts.boxplot.hard   <- read.csv("data/blastprocessed/s2.charts.boxplot.hard.csv"  )
s2.dots.boxplot.hard     <- read.csv("data/blastprocessed/s2.dots.boxplot.hard.csv"    )

#### Second Study
s1.series.boxplot.easy   <- read.csv("data/blastprocessed/s1.series.boxplot.easy.csv"  )
s1.charts.boxplot.easy   <- read.csv("data/blastprocessed/s1.charts.boxplot.easy.csv"  )
s1.dots.boxplot.easy     <- read.csv("data/blastprocessed/s1.dots.boxplot.easy.csv"    )
s1.series.boxplot.medium <- read.csv("data/blastprocessed/s1.series.boxplot.medium.csv")
s1.charts.boxplot.medium <- read.csv("data/blastprocessed/s1.charts.boxplot.medium.csv")
s1.dots.boxplot.medium   <- read.csv("data/blastprocessed/s1.dots.boxplot.medium.csv"  )
s1.series.boxplot.hard   <- read.csv("data/blastprocessed/s1.series.boxplot.hard.csv"  )
s1.charts.boxplot.hard   <- read.csv("data/blastprocessed/s1.charts.boxplot.hard.csv"  )
s1.dots.boxplot.hard     <- read.csv("data/blastprocessed/s1.dots.boxplot.hard.csv"    )


s2.e.acc.int <- conf.interval(s2.series.boxplot.easy$ls.accuracy)
s2.e.acc.det <- conf.interval(s2.charts.boxplot.easy$ls.accuracy)
s2.e.acc.com <- conf.interval(s2.dots.boxplot.easy$ls.accuracy)
s2.m.acc.int <- conf.interval(s2.series.boxplot.medium$ls.accuracy)
s2.m.acc.det <- conf.interval(s2.charts.boxplot.medium$ls.accuracy)
s2.m.acc.com <- conf.interval(s2.dots.boxplot.medium$ls.accuracy)
s2.h.acc.int <- conf.interval(s2.series.boxplot.hard$ls.accuracy)
s2.h.acc.det <- conf.interval(s2.charts.boxplot.hard$ls.accuracy)
s2.h.acc.com <- conf.interval(s2.dots.boxplot.hard$ls.accuracy)

s2.e.act.int <- conf.interval(s2.series.boxplot.easy$ls.actions)
s2.e.act.det <- conf.interval(s2.charts.boxplot.easy$ls.actions)
s2.e.act.com <- conf.interval(s2.dots.boxplot.easy$ls.actions)
s2.m.act.int <- conf.interval(s2.series.boxplot.medium$ls.actions)
s2.m.act.det <- conf.interval(s2.charts.boxplot.medium$ls.actions)
s2.m.act.com <- conf.interval(s2.dots.boxplot.medium$ls.actions)   
s2.h.act.int <- conf.interval(s2.series.boxplot.hard$ls.actions)
s2.h.act.det <- conf.interval(s2.charts.boxplot.hard$ls.actions)
s2.h.act.com <- conf.interval(s2.dots.boxplot.hard$ls.actions)

s2.e.speed.int <- conf.interval(s2.series.boxplot.easy$ls.speed)
s2.e.speed.det <- conf.interval(s2.charts.boxplot.easy$ls.speed)
s2.e.speed.com <- conf.interval(s2.dots.boxplot.easy$ls.speed)
s2.m.speed.int <- conf.interval(s2.series.boxplot.medium$ls.speed)
s2.m.speed.det <- conf.interval(s2.charts.boxplot.medium$ls.speed)
s2.m.speed.com <- conf.interval(s2.dots.boxplot.medium$ls.speed) 
s2.h.speed.int <- conf.interval(s2.series.boxplot.hard$ls.speed)
s2.h.speed.det <- conf.interval(s2.charts.boxplot.hard$ls.speed)
s2.h.speed.com <- conf.interval(s2.dots.boxplot.hard$ls.speed)   

#####

s1.e.acc.int <- conf.interval(s1.series.boxplot.easy$ls.accuracy)
s1.e.acc.det <- conf.interval(s1.charts.boxplot.easy$ls.accuracy)
s1.e.acc.com <- conf.interval(s1.dots.boxplot.easy$ls.accuracy)
s1.m.acc.int <- conf.interval(s1.series.boxplot.medium$ls.accuracy)
s1.m.acc.det <- conf.interval(s1.charts.boxplot.medium$ls.accuracy)
s1.m.acc.com <- conf.interval(s1.dots.boxplot.medium$ls.accuracy)
s1.h.acc.int <- conf.interval(s1.series.boxplot.hard$ls.accuracy)
s1.h.acc.det <- conf.interval(s1.charts.boxplot.hard$ls.accuracy)
s1.h.acc.com <- conf.interval(s1.dots.boxplot.hard$ls.accuracy)

s1.e.act.int <- conf.interval(s1.series.boxplot.easy$ls.actions)
s1.e.act.det <- conf.interval(s1.charts.boxplot.easy$ls.actions)
s1.e.act.com <- conf.interval(s1.dots.boxplot.easy$ls.actions)
s1.m.act.int <- conf.interval(s1.series.boxplot.medium$ls.actions)
s1.m.act.det <- conf.interval(s1.charts.boxplot.medium$ls.actions)
s1.m.act.com <- conf.interval(s1.dots.boxplot.medium$ls.actions)   
s1.h.act.int <- conf.interval(s1.series.boxplot.hard$ls.actions)
s1.h.act.det <- conf.interval(s1.charts.boxplot.hard$ls.actions)
s1.h.act.com <- conf.interval(s1.dots.boxplot.hard$ls.actions)

s1.e.speed.int <- conf.interval(s1.series.boxplot.easy$ls.speed)
s1.e.speed.det <- conf.interval(s1.charts.boxplot.easy$ls.speed)
s1.e.speed.com <- conf.interval(s1.dots.boxplot.easy$ls.speed)
s1.m.speed.int <- conf.interval(s1.series.boxplot.medium$ls.speed)
s1.m.speed.det <- conf.interval(s1.charts.boxplot.medium$ls.speed)
s1.m.speed.com <- conf.interval(s1.dots.boxplot.medium$ls.speed) 
s1.h.speed.int <- conf.interval(s1.series.boxplot.hard$ls.speed)
s1.h.speed.det <- conf.interval(s1.charts.boxplot.hard$ls.speed)
s1.h.speed.com <- conf.interval(s1.dots.boxplot.hard$ls.speed)   


ggplot(s1.e.acc.int, aes(x = c("intuitive","detailed", "compact"), y = mean)) + 
    geom_bar(  data =    s1.e.acc.int, aes(x="Intuitive.1", y = 1.0), fill = "#ebebeb", stat="identity", position = position_dodge(width = 0.50)) +
    geom_bar(  data =    s2.e.acc.int, aes(x="Intuitive.2", y = 1.0), fill = "#ebebeb", stat="identity", position = position_dodge(width = 0.50)) +
    geom_bar(  data =    s1.e.acc.det, aes(x="Detailed.1" , y = 1.0), fill = "#ebebeb", stat="identity", position = position_dodge(width = 0.50)) +
    geom_bar(  data =    s2.e.acc.det, aes(x="Detailed.2" , y = 1.0), fill = "#ebebeb", stat="identity", position = position_dodge(width = 0.50)) +
    geom_bar(  data =    s1.e.acc.com, aes(x="Compact.1"  , y = 1.0), fill = "#ebebeb", stat="identity", position = position_dodge(width = 0.50)) +
    geom_bar(  data =    s2.e.acc.com, aes(x="Compact.2"  , y = 1.0), fill = "#ebebeb", stat="identity", position = position_dodge(width = 0.50)) +
    geom_point(data =    s1.e.acc.int, aes(x="Intuitive.1", y = mean), color = "#08519c") + 
    geom_point(data =    s2.e.acc.int, aes(x="Intuitive.2", y = mean), color = "#de2d26") + 
    geom_point(data =    s1.e.acc.det, aes(x="Detailed.1" , y = mean), color = "#08519c") + 
    geom_point(data =    s2.e.acc.det, aes(x="Detailed.2" , y = mean), color = "#de2d26") + 
    geom_point(data =    s1.e.acc.com, aes(x="Compact.1"  , y = mean), color = "#08519c") + 
    geom_point(data =    s2.e.acc.com, aes(x="Compact.2"  , y = mean), color = "#de2d26") + 
    geom_errorbar(data = s1.e.acc.int, aes(x="Intuitive.1", ymax = max, ymin = min), width=0.2, color = "#08519c") + 
    geom_errorbar(data = s2.e.acc.int, aes(x="Intuitive.2", ymax = max, ymin = min), width=0.2, color = "#de2d26") + 
    geom_errorbar(data = s1.e.acc.det, aes(x="Detailed.1" , ymax = max, ymin = min), width=0.2, color = "#08519c") + 
    geom_errorbar(data = s2.e.acc.det, aes(x="Detailed.2" , ymax = max, ymin = min), width=0.2, color = "#de2d26") + 
    geom_errorbar(data = s1.e.acc.com, aes(x="Compact.1"  , ymax = max, ymin = min), width=0.2, color = "#08519c") + 
    geom_errorbar(data = s2.e.acc.com, aes(x="Compact.2"  , ymax = max, ymin = min), width=0.2, color = "#de2d26") + 
    scale_y_continuous(expand = c(0, 0), limits = c(0,1)) +
    coord_flip() + 
    labs(title ="", x = "Easy", y = "Accuracy")

ggplot(s1.m.acc.int, aes(x = "Intuitive", y = mean)) + 
    geom_point(data =    s1.m.acc.int, aes(x="Intuitive.1", y = mean), color = "#08519c") + 
    geom_point(data =    s2.m.acc.int, aes(x="Intuitive.2", y = mean), color = "#de2d26") + 
    geom_point(data =    s1.m.acc.det, aes(x="Detailed.1" , y = mean), color = "#08519c") + 
    geom_point(data =    s2.m.acc.det, aes(x="Detailed.2" , y = mean), color = "#de2d26") + 
    geom_point(data =    s1.m.acc.com, aes(x="Compact.1"  , y = mean), color = "#08519c") + 
    geom_point(data =    s2.m.acc.com, aes(x="Compact.2"  , y = mean), color = "#de2d26") + 
    geom_errorbar(data = s1.m.acc.int, aes(x="Intuitive.1", ymax = max, ymin = min), width=0.2, color = "#08519c") + 
    geom_errorbar(data = s2.m.acc.int, aes(x="Intuitive.2", ymax = max, ymin = min), width=0.2, color = "#de2d26") + 
    geom_errorbar(data = s1.m.acc.det, aes(x="Detailed.1" , ymax = max, ymin = min), width=0.2, color = "#08519c") + 
    geom_errorbar(data = s2.m.acc.det, aes(x="Detailed.2" , ymax = max, ymin = min), width=0.2, color = "#de2d26") + 
    geom_errorbar(data = s1.m.acc.com, aes(x="Compact.1"  , ymax = max, ymin = min), width=0.2, color = "#08519c") + 
    geom_errorbar(data = s2.m.acc.com, aes(x="Compact.2"  , ymax = max, ymin = min), width=0.2, color = "#de2d26") + 
    scale_y_continuous(expand = c(0, 0), limits = c(0,1)) +
    coord_flip() +
    labs(title ="", x = "Medium", y = "Accuracy")


ggplot(s1.h.acc.int, aes(x = "Intuitive", y = mean)) + 
    geom_point(data =    s1.h.acc.int, aes(x="Intuitive.1", y = mean), color = "#08519c") + 
    geom_point(data =    s2.h.acc.int, aes(x="Intuitive.2", y = mean), color = "#de2d26") + 
    geom_point(data =    s1.h.acc.det, aes(x="Detailed.1" , y = mean), color = "#08519c") + 
    geom_point(data =    s2.h.acc.det, aes(x="Detailed.2" , y = mean), color = "#de2d26") + 
    geom_point(data =    s1.h.acc.com, aes(x="Compact.1"  , y = mean), color = "#08519c") + 
    geom_point(data =    s2.h.acc.com, aes(x="Compact.2"  , y = mean), color = "#de2d26") + 
    geom_errorbar(data = s1.h.acc.int, aes(x="Intuitive.1", ymax = max, ymin = min), width=0.2, color = "#08519c") + 
    geom_errorbar(data = s2.h.acc.int, aes(x="Intuitive.2", ymax = max, ymin = min), width=0.2, color = "#de2d26") + 
    geom_errorbar(data = s1.h.acc.det, aes(x="Detailed.1" , ymax = max, ymin = min), width=0.2, color = "#08519c") + 
    geom_errorbar(data = s2.h.acc.det, aes(x="Detailed.2" , ymax = max, ymin = min), width=0.2, color = "#de2d26") + 
    geom_errorbar(data = s1.h.acc.com, aes(x="Compact.1"  , ymax = max, ymin = min), width=0.2, color = "#08519c") + 
    geom_errorbar(data = s2.h.acc.com, aes(x="Compact.2"  , ymax = max, ymin = min), width=0.2, color = "#de2d26") + 
    scale_y_continuous(expand = c(0, 0), limits = c(0,1)) +
    coord_flip() +
    labs(title ="", x = "Hard", y = "Accuracy")

ggplot(s1.e.act.int, aes(x = "Intuitive", y = mean)) + 
    geom_point(data =    s1.e.act.int, aes(x="Intuitive.1", y = mean), color = "#08519c") + 
    geom_point(data =    s2.e.act.int, aes(x="Intuitive.2", y = mean), color = "#de2d26") + 
    geom_point(data =    s1.e.act.det, aes(x="Detailed.1" , y = mean), color = "#08519c") + 
    geom_point(data =    s2.e.act.det, aes(x="Detailed.2" , y = mean), color = "#de2d26") + 
    geom_point(data =    s1.e.act.com, aes(x="Compact.1"  , y = mean), color = "#08519c") + 
    geom_point(data =    s2.e.act.com, aes(x="Compact.2"  , y = mean), color = "#de2d26") + 
    geom_errorbar(data = s1.e.act.int, aes(x="Intuitive.1", ymax = max, ymin = min), width=0.2, color = "#08519c") + 
    geom_errorbar(data = s2.e.act.int, aes(x="Intuitive.2", ymax = max, ymin = min), width=0.2, color = "#de2d26") + 
    geom_errorbar(data = s1.e.act.det, aes(x="Detailed.1" , ymax = max, ymin = min), width=0.2, color = "#08519c") + 
    geom_errorbar(data = s2.e.act.det, aes(x="Detailed.2" , ymax = max, ymin = min), width=0.2, color = "#de2d26") + 
    geom_errorbar(data = s1.e.act.com, aes(x="Compact.1"  , ymax = max, ymin = min), width=0.2, color = "#08519c") + 
    geom_errorbar(data = s2.e.act.com, aes(x="Compact.2"  , ymax = max, ymin = min), width=0.2, color = "#de2d26") + 
    scale_y_continuous(expand = c(0, 0), limits = c(0,15)) +
    coord_flip() +
    labs(title ="", x = "Easy", y = "Actions")

ggplot(s1.m.act.int, aes(x = "Intuitive", y = mean)) + 
    geom_point(data =    s1.m.act.int, aes(x="Intuitive.1", y = mean), color = "#08519c") + 
    geom_point(data =    s2.m.act.int, aes(x="Intuitive.2", y = mean), color = "#de2d26") + 
    geom_point(data =    s1.m.act.det, aes(x="Detailed.1" , y = mean), color = "#08519c") + 
    geom_point(data =    s2.m.act.det, aes(x="Detailed.2" , y = mean), color = "#de2d26") + 
    geom_point(data =    s1.m.act.com, aes(x="Compact.1"  , y = mean), color = "#08519c") + 
    geom_point(data =    s2.m.act.com, aes(x="Compact.2"  , y = mean), color = "#de2d26") + 
    geom_errorbar(data = s1.m.act.int, aes(x="Intuitive.1", ymax = max, ymin = min), width=0.2, color = "#08519c") + 
    geom_errorbar(data = s2.m.act.int, aes(x="Intuitive.2", ymax = max, ymin = min), width=0.2, color = "#de2d26") + 
    geom_errorbar(data = s1.m.act.det, aes(x="Detailed.1" , ymax = max, ymin = min), width=0.2, color = "#08519c") + 
    geom_errorbar(data = s2.m.act.det, aes(x="Detailed.2" , ymax = max, ymin = min), width=0.2, color = "#de2d26") + 
    geom_errorbar(data = s1.m.act.com, aes(x="Compact.1"  , ymax = max, ymin = min), width=0.2, color = "#08519c") + 
    geom_errorbar(data = s2.m.act.com, aes(x="Compact.2"  , ymax = max, ymin = min), width=0.2, color = "#de2d26") + 
    scale_y_continuous(expand = c(0, 0), limits = c(0,15)) +
    coord_flip() +
    labs(title ="", x = "Medium", y = "Actions")


ggplot(s1.h.act.int, aes(x = "Intuitive", y = mean)) + 
    geom_point(data =    s1.h.act.int, aes(x="Intuitive.1", y = mean), color = "#08519c") + 
    geom_point(data =    s2.h.act.int, aes(x="Intuitive.2", y = mean), color = "#de2d26") + 
    geom_point(data =    s1.h.act.det, aes(x="Detailed.1" , y = mean), color = "#08519c") + 
    geom_point(data =    s2.h.act.det, aes(x="Detailed.2" , y = mean), color = "#de2d26") + 
    geom_point(data =    s1.h.act.com, aes(x="Compact.1"  , y = mean), color = "#08519c") + 
    geom_point(data =    s2.h.act.com, aes(x="Compact.2"  , y = mean), color = "#de2d26") + 
    geom_errorbar(data = s1.h.act.int, aes(x="Intuitive.1", ymax = max, ymin = min), width=0.2, color = "#08519c") + 
    geom_errorbar(data = s2.h.act.int, aes(x="Intuitive.2", ymax = max, ymin = min), width=0.2, color = "#de2d26") + 
    geom_errorbar(data = s1.h.act.det, aes(x="Detailed.1" , ymax = max, ymin = min), width=0.2, color = "#08519c") + 
    geom_errorbar(data = s2.h.act.det, aes(x="Detailed.2" , ymax = max, ymin = min), width=0.2, color = "#de2d26") + 
    geom_errorbar(data = s1.h.act.com, aes(x="Compact.1"  , ymax = max, ymin = min), width=0.2, color = "#08519c") + 
    geom_errorbar(data = s2.h.act.com, aes(x="Compact.2"  , ymax = max, ymin = min), width=0.2, color = "#de2d26") + 
    scale_y_continuous(expand = c(0, 0), limits = c(0,15)) +
    coord_flip() +
    labs(title ="", x = "Hard", y = "Actions")

# ggsave("plot.png", width=3.5, height=1.5, dpi=300)

ggplot(s1.e.speed.int, aes(x = "Intuitive", y = mean)) + 
    geom_point(data =    s1.e.speed.int, aes(x="Intuitive.1", y = mean), color = "#08519c") + 
    geom_point(data =    s2.e.speed.int, aes(x="Intuitive.2", y = mean), color = "#de2d26") + 
    geom_point(data =    s1.e.speed.det, aes(x="Detailed.1" , y = mean), color = "#08519c") + 
    geom_point(data =    s2.e.speed.det, aes(x="Detailed.2" , y = mean), color = "#de2d26") + 
    geom_point(data =    s1.e.speed.com, aes(x="Compact.1"  , y = mean), color = "#08519c") + 
    geom_point(data =    s2.e.speed.com, aes(x="Compact.2"  , y = mean), color = "#de2d26") + 
    geom_errorbar(data = s1.e.speed.int, aes(x="Intuitive.1", ymax = max, ymin = min), width=0.2, color = "#08519c") + 
    geom_errorbar(data = s2.e.speed.int, aes(x="Intuitive.2", ymax = max, ymin = min), width=0.2, color = "#de2d26") + 
    geom_errorbar(data = s1.e.speed.det, aes(x="Detailed.1" , ymax = max, ymin = min), width=0.2, color = "#08519c") + 
    geom_errorbar(data = s2.e.speed.det, aes(x="Detailed.2" , ymax = max, ymin = min), width=0.2, color = "#de2d26") + 
    geom_errorbar(data = s1.e.speed.com, aes(x="Compact.1"  , ymax = max, ymin = min), width=0.2, color = "#08519c") + 
    geom_errorbar(data = s2.e.speed.com, aes(x="Compact.2"  , ymax = max, ymin = min), width=0.2, color = "#de2d26") + 
    scale_y_continuous(expand = c(0, 0), limits = c(0,80)) +
    coord_flip() +
    labs(title ="", x = "Easy", y = "Speed")

ggplot(s1.m.speed.int, aes(x = "Intuitive", y = mean)) + 
    geom_point(data =    s1.m.speed.int, aes(x="Intuitive.1", y = mean), color = "#08519c") + 
    geom_point(data =    s2.m.speed.int, aes(x="Intuitive.2", y = mean), color = "#de2d26") + 
    geom_point(data =    s1.m.speed.det, aes(x="Detailed.1" , y = mean), color = "#08519c") + 
    geom_point(data =    s2.m.speed.det, aes(x="Detailed.2" , y = mean), color = "#de2d26") + 
    geom_point(data =    s1.m.speed.com, aes(x="Compact.1"  , y = mean), color = "#08519c") + 
    geom_point(data =    s2.m.speed.com, aes(x="Compact.2"  , y = mean), color = "#de2d26") + 
    geom_errorbar(data = s1.m.speed.int, aes(x="Intuitive.1", ymax = max, ymin = min), width=0.2, color = "#08519c") + 
    geom_errorbar(data = s2.m.speed.int, aes(x="Intuitive.2", ymax = max, ymin = min), width=0.2, color = "#de2d26") + 
    geom_errorbar(data = s1.m.speed.det, aes(x="Detailed.1" , ymax = max, ymin = min), width=0.2, color = "#08519c") + 
    geom_errorbar(data = s2.m.speed.det, aes(x="Detailed.2" , ymax = max, ymin = min), width=0.2, color = "#de2d26") + 
    geom_errorbar(data = s1.m.speed.com, aes(x="Compact.1"  , ymax = max, ymin = min), width=0.2, color = "#08519c") + 
    geom_errorbar(data = s2.m.speed.com, aes(x="Compact.2"  , ymax = max, ymin = min), width=0.2, color = "#de2d26") + 
    scale_y_continuous(expand = c(0, 0), limits = c(0,80)) +
    coord_flip() +
    labs(title ="", x = "Medium", y = "Speed")


ggplot(s1.h.speed.int, aes(x = "Intuitive", y = mean)) + 
    geom_point(data =    s1.h.speed.int, aes(x="Intuitive.1", y = mean), color = "#08519c") + 
    geom_point(data =    s2.h.speed.int, aes(x="Intuitive.2", y = mean), color = "#de2d26") + 
    geom_point(data =    s1.h.speed.det, aes(x="Detailed.1" , y = mean), color = "#08519c") + 
    geom_point(data =    s2.h.speed.det, aes(x="Detailed.2" , y = mean), color = "#de2d26") + 
    geom_point(data =    s1.h.speed.com, aes(x="Compact.1"  , y = mean), color = "#08519c") + 
    geom_point(data =    s2.h.speed.com, aes(x="Compact.2"  , y = mean), color = "#de2d26") + 
    geom_errorbar(data = s1.h.speed.int, aes(x="Intuitive.1", ymax = max, ymin = min), width=0.2, color = "#08519c") + 
    geom_errorbar(data = s2.h.speed.int, aes(x="Intuitive.2", ymax = max, ymin = min), width=0.2, color = "#de2d26") + 
    geom_errorbar(data = s1.h.speed.det, aes(x="Detailed.1" , ymax = max, ymin = min), width=0.2, color = "#08519c") + 
    geom_errorbar(data = s2.h.speed.det, aes(x="Detailed.2" , ymax = max, ymin = min), width=0.2, color = "#de2d26") + 
    geom_errorbar(data = s1.h.speed.com, aes(x="Compact.1"  , ymax = max, ymin = min), width=0.2, color = "#08519c") + 
    geom_errorbar(data = s2.h.speed.com, aes(x="Compact.2"  , ymax = max, ymin = min), width=0.2, color = "#de2d26") + 
    scale_y_continuous(expand = c(0, 0), limits = c(0,80)) +
    coord_flip() +
    labs(title ="", x = "Hard", y = "Speed")


#  ggsave("plot.png", width=3.5, height=1.5, dpi=300)