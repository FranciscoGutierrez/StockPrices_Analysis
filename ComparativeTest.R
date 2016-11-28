library(brms)
require(ggplot2)
require(vioplot)

#### Second Study
s2.series.boxplot.easy   <- read.csv("data/blastprocessed/s2.series.boxplot.easy.csv"  )
s2.charts.boxplot.easy   <- read.csv("data/blastprocessed/s2.charts.boxplot.easy.csv"  )
s2.dots.boxplot.easy     <- read.csv("data/blastprocessed/s2.dots.boxplot.easy.csv"    )
s2.series.boxplot.medium <- read.csv("data/blastprocessed/s2.series.boxplot.medium.csv")
s2.charts.boxplot.medium <- read.csv("data/blastprocessed/s2.charts.boxplot.medium.csv")
s2.dots.boxplot.medium   <- read.csv("data/blastprocessed/s2.dots.boxplot.medium.csv"  )
s2.series.boxplot.hard   <- read.csv("data/blastprocessed/s2.series.boxplot.hard.csv"  )
s2.charts.boxplot.hard   <- read.csv("data/blastprocessed/s2.charts.boxplot.hard.csv"  )
s2.dots.boxplot.hard     <- read.csv("data/blastprocessed/s2.dots.boxplot.hard.csv"    )

#### First Study
s1.series.boxplot.easy   <- read.csv("data/blastprocessed/s1.series.boxplot.easy.csv"  )
s1.charts.boxplot.easy   <- read.csv("data/blastprocessed/s1.charts.boxplot.easy.csv"  )
s1.dots.boxplot.easy     <- read.csv("data/blastprocessed/s1.dots.boxplot.easy.csv"    )
s1.series.boxplot.medium <- read.csv("data/blastprocessed/s1.series.boxplot.medium.csv")
s1.charts.boxplot.medium <- read.csv("data/blastprocessed/s1.charts.boxplot.medium.csv")
s1.dots.boxplot.medium   <- read.csv("data/blastprocessed/s1.dots.boxplot.medium.csv"  )
s1.series.boxplot.hard   <- read.csv("data/blastprocessed/s1.series.boxplot.hard.csv"  )
s1.charts.boxplot.hard   <- read.csv("data/blastprocessed/s1.charts.boxplot.hard.csv"  )
s1.dots.boxplot.hard     <- read.csv("data/blastprocessed/s1.dots.boxplot.hard.csv"    )

conf.interval <- function(data) {
    data <- t.test(data)
    mean   <- data$estimate
    min    <- data$conf.int[1]
    max    <- data$conf.int[2]
    output <- data.frame(mean,min,max)
    return(output)
}


normalize      <- function(x) {(x - min(x, na.rm=TRUE))/(max(x,na.rm=TRUE) - min(x, na.rm=TRUE))}
normalize.with <- function(x,y) {(x - min(y, na.rm=TRUE))/(max(y,na.rm=TRUE) - min(y, na.rm=TRUE))}


ls.s1.easy.accuracy <- c(s1.series.boxplot.easy$ls.accuracy,   s1.charts.boxplot.easy$ls.accuracy,   s1.dots.boxplot.easy$ls.accuracy)
ls.s1.medi.accuracy <- c(s1.series.boxplot.medium$ls.accuracy, s1.charts.boxplot.medium$ls.accuracy, s1.dots.boxplot.medium$ls.accuracy)
ls.s1.hard.accuracy <- c(s1.series.boxplot.hard$ls.accuracy,   s1.charts.boxplot.hard$ls.accuracy,   s1.dots.boxplot.hard$ls.accuracy)
ls.s1.easy.speed    <- c(s1.series.boxplot.easy$ls.speed,      s1.charts.boxplot.easy$ls.speed,      s1.dots.boxplot.easy$ls.speed)
ls.s1.medi.speed    <- c(s1.series.boxplot.medium$ls.speed,    s1.charts.boxplot.medium$ls.speed,    s1.dots.boxplot.medium$ls.speed)
ls.s1.hard.speed    <- c(s1.series.boxplot.hard$ls.speed,      s1.charts.boxplot.hard$ls.speed,      s1.dots.boxplot.hard$ls.speed)
ls.s1.easy.actions  <- c(s1.series.boxplot.easy$ls.actions,    s1.charts.boxplot.easy$ls.actions,    s1.dots.boxplot.easy$ls.actions)
ls.s1.medi.actions  <- c(s1.series.boxplot.medium$ls.actions,  s1.charts.boxplot.medium$ls.actions,  s1.dots.boxplot.medium$ls.actions)
ls.s1.hard.actions  <- c(s1.series.boxplot.hard$ls.actions,    s1.charts.boxplot.hard$ls.actions,    s1.dots.boxplot.hard$ls.actions)
ls.s2.easy.accuracy <- c(s2.series.boxplot.easy$ls.accuracy,   s2.charts.boxplot.easy$ls.accuracy,   s2.dots.boxplot.easy$ls.accuracy)
ls.s2.medi.accuracy <- c(s2.series.boxplot.medium$ls.accuracy, s2.charts.boxplot.medium$ls.accuracy, s2.dots.boxplot.medium$ls.accuracy)
ls.s2.hard.accuracy <- c(s2.series.boxplot.hard$ls.accuracy,   s2.charts.boxplot.hard$ls.accuracy,   s2.dots.boxplot.hard$ls.accuracy)
ls.s2.easy.speed    <- c(s2.series.boxplot.easy$ls.speed,      s2.charts.boxplot.easy$ls.speed,      s2.dots.boxplot.easy$ls.speed)
ls.s2.medi.speed    <- c(s2.series.boxplot.medium$ls.speed,    s2.charts.boxplot.medium$ls.speed,    s2.dots.boxplot.medium$ls.speed)
ls.s2.hard.speed    <- c(s2.series.boxplot.hard$ls.speed,      s2.charts.boxplot.hard$ls.speed,      s2.dots.boxplot.hard$ls.speed)
ls.s2.easy.actions  <- c(s2.series.boxplot.easy$ls.actions,    s2.charts.boxplot.easy$ls.actions,    s2.dots.boxplot.easy$ls.actions)
ls.s2.medi.actions  <- c(s2.series.boxplot.medium$ls.actions,  s2.charts.boxplot.medium$ls.actions,  s2.dots.boxplot.medium$ls.actions)
ls.s2.hard.actions  <- c(s2.series.boxplot.hard$ls.actions,    s2.charts.boxplot.hard$ls.actions,    s2.dots.boxplot.hard$ls.actions)

# mean(normalize(s2.dots.boxplot.hard$ls.actions    )))
# df.s2.easy.actions <- data.frame(s2.series.boxplot.easy$ls.actions,    s2.charts.boxplot.easy$ls.actions,    s2.dots.boxplot.easy$ls.actions)

s1.mean.acc.easy       <- mean(ls.s1.easy.accuracy)
s1.mean.acc.medium     <- mean(ls.s1.medi.accuracy)
s1.mean.acc.hard       <- mean(ls.s1.hard.accuracy)
s1.mean.speed.easy     <- mean(normalize(ls.s1.easy.speed   ))
s1.mean.speed.medium   <- mean(normalize(ls.s1.medi.speed   ))
s1.mean.speed.hard     <- mean(normalize(ls.s1.hard.speed   ))
s1.mean.actions.easy   <- mean(normalize(ls.s1.easy.actions ))
s1.mean.actions.medium <- mean(normalize(ls.s1.medi.actions ))
s1.mean.actions.hard   <- mean(normalize(ls.s1.hard.actions ))
s2.mean.acc.easy       <- mean(ls.s2.easy.accuracy)
s2.mean.acc.medium     <- mean(ls.s2.medi.accuracy)
s2.mean.acc.hard       <- mean(ls.s2.hard.accuracy)
s2.mean.speed.easy     <- mean(normalize(ls.s2.easy.speed   ))
s2.mean.speed.medium   <- mean(normalize(ls.s2.medi.speed   ))
s2.mean.speed.hard     <- mean(normalize(ls.s2.hard.speed   ))
s2.mean.actions.easy   <- mean(normalize(ls.s2.easy.actions ))
s2.mean.actions.medium <- mean(normalize(ls.s2.medi.actions ))
s2.mean.actions.hard   <- mean(normalize(ls.s2.hard.actions ))
    
s2.e.acc.int   <- conf.interval(s2.series.boxplot.easy$ls.accuracy   - s2.mean.acc.easy)
s2.e.acc.det   <- conf.interval(s2.charts.boxplot.easy$ls.accuracy   - s2.mean.acc.easy)
s2.e.acc.com   <- conf.interval(s2.dots.boxplot.easy$ls.accuracy     - s2.mean.acc.easy)
s2.m.acc.int   <- conf.interval(s2.series.boxplot.medium$ls.accuracy - s2.mean.acc.medium)
s2.m.acc.det   <- conf.interval(s2.charts.boxplot.medium$ls.accuracy - s2.mean.acc.medium)
s2.m.acc.com   <- conf.interval(s2.dots.boxplot.medium$ls.accuracy   - s2.mean.acc.medium)
s2.h.acc.int   <- conf.interval(s2.series.boxplot.hard$ls.accuracy   - s2.mean.acc.hard)
s2.h.acc.det   <- conf.interval(s2.charts.boxplot.hard$ls.accuracy   - s2.mean.acc.hard)
s2.h.acc.com   <- conf.interval(s2.dots.boxplot.hard$ls.accuracy     - s2.mean.acc.hard)
s2.e.act.int   <- conf.interval(normalize.with(s2.series.boxplot.easy$ls.actions    ,ls.s2.easy.actions )) - s2.mean.actions.easy
s2.e.act.det   <- conf.interval(normalize.with(s2.charts.boxplot.easy$ls.actions    ,ls.s2.easy.actions )) - s2.mean.actions.easy
s2.e.act.com   <- conf.interval(normalize.with(s2.dots.boxplot.easy$ls.actions      ,ls.s2.easy.actions )) - s2.mean.actions.easy
s2.m.act.int   <- conf.interval(normalize.with(s2.series.boxplot.medium$ls.actions  ,ls.s2.medi.actions )) - s2.mean.actions.medium
s2.m.act.det   <- conf.interval(normalize.with(s2.charts.boxplot.medium$ls.actions  ,ls.s2.medi.actions )) - s2.mean.actions.medium
s2.m.act.com   <- conf.interval(normalize.with(s2.dots.boxplot.medium$ls.actions    ,ls.s2.medi.actions )) - s2.mean.actions.medium
s2.h.act.int   <- conf.interval(normalize.with(s2.series.boxplot.hard$ls.actions    ,ls.s2.hard.actions )) - s2.mean.actions.hard
s2.h.act.det   <- conf.interval(normalize.with(s2.charts.boxplot.hard$ls.actions    ,ls.s2.hard.actions )) - s2.mean.actions.hard
s2.h.act.com   <- conf.interval(normalize.with(s2.dots.boxplot.hard$ls.actions      ,ls.s2.hard.actions )) - s2.mean.actions.hard
s2.e.speed.int <- conf.interval(normalize.with(s2.series.boxplot.easy$ls.speed      ,ls.s2.easy.speed   )) - s2.mean.speed.easy
s2.e.speed.det <- conf.interval(normalize.with(s2.charts.boxplot.easy$ls.speed      ,ls.s2.easy.speed   )) - s2.mean.speed.easy
s2.e.speed.com <- conf.interval(normalize.with(s2.dots.boxplot.easy$ls.speed        ,ls.s2.easy.speed   )) - s2.mean.speed.easy
s2.m.speed.int <- conf.interval(normalize.with(s2.series.boxplot.medium$ls.speed    ,ls.s2.medi.speed   )) - s2.mean.speed.medium
s2.m.speed.det <- conf.interval(normalize.with(s2.charts.boxplot.medium$ls.speed    ,ls.s2.medi.speed   )) - s2.mean.speed.medium
s2.m.speed.com <- conf.interval(normalize.with(s2.dots.boxplot.medium$ls.speed      ,ls.s2.medi.speed   )) - s2.mean.speed.medium
s2.h.speed.int <- conf.interval(normalize.with(s2.series.boxplot.hard$ls.speed      ,ls.s2.hard.speed   )) - s2.mean.speed.hard
s2.h.speed.det <- conf.interval(normalize.with(s2.charts.boxplot.hard$ls.speed      ,ls.s2.hard.speed   )) - s2.mean.speed.hard
s2.h.speed.com <- conf.interval(normalize.with(s2.dots.boxplot.hard$ls.speed        ,ls.s2.hard.speed   )) - s2.mean.speed.hard

#####

s1.e.acc.int   <- conf.interval(s1.series.boxplot.easy$ls.accuracy   - s1.mean.acc.easy)
s1.e.acc.det   <- conf.interval(s1.charts.boxplot.easy$ls.accuracy   - s1.mean.acc.easy)
s1.e.acc.com   <- conf.interval(s1.dots.boxplot.easy$ls.accuracy     - s1.mean.acc.easy)
s1.m.acc.int   <- conf.interval(s1.series.boxplot.medium$ls.accuracy - s1.mean.acc.medium)
s1.m.acc.det   <- conf.interval(s1.charts.boxplot.medium$ls.accuracy - s1.mean.acc.medium)
s1.m.acc.com   <- conf.interval(s1.dots.boxplot.medium$ls.accuracy   - s1.mean.acc.medium)
s1.h.acc.int   <- conf.interval(s1.series.boxplot.hard$ls.accuracy   - s1.mean.acc.hard)
s1.h.acc.det   <- conf.interval(s1.charts.boxplot.hard$ls.accuracy   - s1.mean.acc.hard)
s1.h.acc.com   <- conf.interval(s1.dots.boxplot.hard$ls.accuracy     - s1.mean.acc.hard)
s1.e.act.int   <- conf.interval(normalize.with(s1.series.boxplot.easy$ls.actions    ,ls.s1.easy.actions )) - s1.mean.actions.easy
s1.e.act.det   <- conf.interval(normalize.with(s1.charts.boxplot.easy$ls.actions    ,ls.s1.easy.actions )) - s1.mean.actions.easy
s1.e.act.com   <- conf.interval(normalize.with(s1.dots.boxplot.easy$ls.actions      ,ls.s1.easy.actions )) - s1.mean.actions.easy
s1.m.act.int   <- conf.interval(normalize.with(s1.series.boxplot.medium$ls.actions  ,ls.s1.medi.actions )) - s1.mean.actions.medium
s1.m.act.det   <- conf.interval(normalize.with(s1.charts.boxplot.medium$ls.actions  ,ls.s1.medi.actions )) - s1.mean.actions.medium
s1.m.act.com   <- conf.interval(normalize.with(s1.dots.boxplot.medium$ls.actions    ,ls.s1.medi.actions )) - s1.mean.actions.medium
s1.h.act.int   <- conf.interval(normalize.with(s1.series.boxplot.hard$ls.actions    ,ls.s1.hard.actions )) - s1.mean.actions.hard
s1.h.act.det   <- conf.interval(normalize.with(s1.charts.boxplot.hard$ls.actions    ,ls.s1.hard.actions )) - s1.mean.actions.hard
s1.h.act.com   <- conf.interval(normalize.with(s1.dots.boxplot.hard$ls.actions      ,ls.s1.hard.actions )) - s1.mean.actions.hard
s1.e.speed.int <- conf.interval(normalize.with(s1.series.boxplot.easy$ls.speed      ,ls.s1.easy.speed   )) - s1.mean.speed.easy
s1.e.speed.det <- conf.interval(normalize.with(s1.charts.boxplot.easy$ls.speed      ,ls.s1.easy.speed   )) - s1.mean.speed.easy
s1.e.speed.com <- conf.interval(normalize.with(s1.dots.boxplot.easy$ls.speed        ,ls.s1.easy.speed   )) - s1.mean.speed.easy
s1.m.speed.int <- conf.interval(normalize.with(s1.series.boxplot.medium$ls.speed    ,ls.s1.medi.speed   )) - s1.mean.speed.medium
s1.m.speed.det <- conf.interval(normalize.with(s1.charts.boxplot.medium$ls.speed    ,ls.s1.medi.speed   )) - s1.mean.speed.medium
s1.m.speed.com <- conf.interval(normalize.with(s1.dots.boxplot.medium$ls.speed      ,ls.s1.medi.speed   )) - s1.mean.speed.medium
s1.h.speed.int <- conf.interval(normalize.with(s1.series.boxplot.hard$ls.speed      ,ls.s1.hard.speed   )) - s1.mean.speed.hard
s1.h.speed.det <- conf.interval(normalize.with(s1.charts.boxplot.hard$ls.speed      ,ls.s1.hard.speed   )) - s1.mean.speed.hard
s1.h.speed.com <- conf.interval(normalize.with(s1.dots.boxplot.hard$ls.speed        ,ls.s1.hard.speed   )) - s1.mean.speed.hard

########## ACCURACY

ggplot(s2.e.acc.int, aes(x = "Intuitive", y = mean)) + 
    geom_pointrange(data = s2.e.acc.int, aes(x="Intuitive",y = mean, ymax = max, ymin = min), size=1, color = "#08519c") + 
    geom_pointrange(data = s2.e.acc.det, aes(x="Detailed" ,y = mean, ymax = max, ymin = min), size=1, color = "#08519c") + 
    geom_pointrange(data = s2.e.acc.com, aes(x="Compact"  ,y = mean, ymax = max, ymin = min), size=1, color = "#08519c") + 
    scale_y_continuous(expand = c(0, 0), limits = c(-0.2,0.2)) +
    coord_flip() +
    theme_void() +
    labs(title ="", x = "Easy", y = "Accuracy")

last_plot()
ggsave("s2.accuracy.easy.png", width=6, height=3, dpi=300, bg = "transparent")

ggplot(s2.m.acc.int, aes(x = "Intuitive", y = mean)) + 
    geom_pointrange(data = s2.m.acc.int, aes(x="Intuitive",y = mean, ymax = max, ymin = min), size=1, color = "#08519c") + 
    geom_pointrange(data = s2.m.acc.det, aes(x="Detailed" ,y = mean, ymax = max, ymin = min), size=1, color = "#08519c") + 
    geom_pointrange(data = s2.m.acc.com, aes(x="Compact"  ,y = mean, ymax = max, ymin = min), size=1, color = "#08519c") + 
    scale_y_continuous(expand = c(0, 0), limits = c(-0.2,0.2)) +
    coord_flip() +
    theme_void() +
    labs(title ="", x = "Medium", y = "Accuracy")

last_plot()
ggsave("s2.accuracy.medium.png", width=6, height=3, dpi=300, bg = "transparent")


ggplot(s2.h.acc.int, aes(x = "Intuitive", y = mean)) + 
    geom_pointrange(data = s2.h.acc.int, aes(x="Intuitive",y = mean, ymax = max, ymin = min), size=1, color = "#08519c") + 
    geom_pointrange(data = s2.h.acc.det, aes(x="Detailed" ,y = mean, ymax = max, ymin = min), size=1, color = "#08519c") + 
    geom_pointrange(data = s2.h.acc.com, aes(x="Compact"  ,y = mean, ymax = max, ymin = min), size=1, color = "#08519c") + 
    scale_y_continuous(expand = c(0, 0), limits = c(-0.2,0.2)) +
    coord_flip() +
    theme_void() +
    labs(title ="", x = "Hard", y = "Accuracy")

last_plot()
ggsave("s2.accuracy.hard.png", width=6, height=3, dpi=300, bg = "transparent")


########### ACTIONS

ggplot(s2.e.act.int, aes(x = "Intuitive", y = mean)) + 
    geom_pointrange(data = s2.e.act.int, aes(x="Intuitive",y = mean, ymax = max, ymin = min), size=1, color = "#08519c") + 
    geom_pointrange(data = s2.e.act.det, aes(x="Detailed" ,y = mean, ymax = max, ymin = min), size=1, color = "#08519c") + 
    geom_pointrange(data = s2.e.act.com, aes(x="Compact"  ,y = mean, ymax = max, ymin = min), size=1, color = "#08519c") + 
    scale_y_continuous(expand = c(0, 0), limits = c(-0.2,0.2)) +
    coord_flip() +
    theme_void() +
    labs(title ="", x = "Easy", y = "Actions")

last_plot()
ggsave("s2.actions.easy.png", width=6, height=3, dpi=300, bg = "transparent")


ggplot(s2.m.act.int, aes(x = "Intuitive", y = mean)) + 
    geom_pointrange(data = s2.m.act.int, aes(x="Intuitive",y = mean, ymax = max, ymin = min), size=1, color = "#08519c") + 
    geom_pointrange(data = s2.m.act.det, aes(x="Detailed" ,y = mean, ymax = max, ymin = min), size=1, color = "#08519c") + 
    geom_pointrange(data = s2.m.act.com, aes(x="Compact"  ,y = mean, ymax = max, ymin = min), size=1, color = "#08519c") + 
    scale_y_continuous(expand = c(0, 0), limits = c(-0.2,0.2)) +
    coord_flip() +
    theme_void() +
    labs(title ="", x = "Medium", y = "Actions")

last_plot()
ggsave("s2.actions.medium.png", width=6, height=3, dpi=300, bg = "transparent")

ggplot(s2.h.act.int, aes(x = "Intuitive", y = mean)) + 
    geom_pointrange(data = s2.h.act.int, aes(x="Intuitive",y = mean, ymax = max, ymin = min), size=1, color = "#08519c") + 
    geom_pointrange(data = s2.h.act.det, aes(x="Detailed" ,y = mean, ymax = max, ymin = min), size=1, color = "#08519c") + 
    geom_pointrange(data = s2.h.act.com, aes(x="Compact"  ,y = mean, ymax = max, ymin = min), size=1, color = "#08519c") + 
    scale_y_continuous(expand = c(0, 0), limits = c(-0.2,0.2)) +
    coord_flip() +
    theme_void() +
    labs(title ="", x = "Hard", y = "Actions")

last_plot()
ggsave("s2.actions.hard.png", width=6, height=3, dpi=300, bg = "transparent")


############### SPEEED

ggplot(s2.e.speed.int, aes(x = "Intuitive", y = mean)) + 
    geom_pointrange(data = s2.e.speed.int, aes(x="Intuitive",y = mean, ymax = max, ymin = min), size=1, color = "#08519c") + 
    geom_pointrange(data = s2.e.speed.det, aes(x="Detailed" ,y = mean, ymax = max, ymin = min), size=1, color = "#08519c") + 
    geom_pointrange(data = s2.e.speed.com, aes(x="Compact"  ,y = mean, ymax = max, ymin = min), size=1, color = "#08519c") + 
    scale_y_continuous(expand = c(0, 0), limits = c(-0.2,0.2)) +
    coord_flip() +
    theme_void() +
    labs(title ="", x = "Easy", y = "Speed")

last_plot()
ggsave("s2.speed.easy.png", width=6, height=3, dpi=300, bg = "transparent")

ggplot(s2.m.speed.int, aes(x = "Intuitive", y = mean)) + 
    geom_pointrange(data = s2.m.speed.int, aes(x="Intuitive",y = mean, ymax = max, ymin = min), size=1, color = "#08519c") + 
    geom_pointrange(data = s2.m.speed.det, aes(x="Detailed" ,y = mean, ymax = max, ymin = min), size=1, color = "#08519c") + 
    geom_pointrange(data = s2.m.speed.com, aes(x="Compact"  ,y = mean, ymax = max, ymin = min), size=1, color = "#08519c") + 
    scale_y_continuous(expand = c(0, 0), limits = c(-0.2,0.2)) +
    coord_flip() +
    theme_void() +
    labs(title ="", x = "Medium", y = "Speed")

last_plot()
ggsave("s2.speed.medium.png", width=6, height=3, dpi=300, bg = "transparent")

ggplot(s2.h.speed.int, aes(x = "Intuitive", y = mean)) + 
    geom_pointrange(data = s2.h.speed.int, aes(x="Intuitive",y = mean, ymax = max, ymin = min), size=1, color = "#08519c") + 
    geom_pointrange(data = s2.h.speed.det, aes(x="Detailed" ,y = mean, ymax = max, ymin = min), size=1, color = "#08519c") + 
    geom_pointrange(data = s2.h.speed.com, aes(x="Compact"  ,y = mean, ymax = max, ymin = min), size=1, color = "#08519c") + 
    scale_y_continuous(expand = c(0, 0), limits = c(-0.2,0.2)) +
    coord_flip() +
    theme_void() +
    labs(title ="", x = "Hard", y = "Speed")

last_plot()
ggsave("s2.speed.hard.png", width=6, height=3, dpi=300, bg = "transparent")




############# FIRST STUDY 

ggplot(s1.e.acc.int, aes(x = "Intuitive", y = mean)) + 
    geom_pointrange(data = s1.e.acc.int, aes(x="Intuitive",y = mean, ymax = max, ymin = min), size=1, color = "#DB2F2E") + 
    geom_pointrange(data = s1.e.acc.det, aes(x="Detailed" ,y = mean, ymax = max, ymin = min), size=1, color = "#DB2F2E") + 
    geom_pointrange(data = s1.e.acc.com, aes(x="Compact"  ,y = mean, ymax = max, ymin = min), size=1, color = "#DB2F2E") + 
    scale_y_continuous(expand = c(0, 0), limits = c(-0.2,0.2)) +
    coord_flip() +
    theme_void() +
    labs(title ="", x = "Easy", y = "Accuracy")

last_plot()
ggsave("s1.accuracy.easy.png", width=6, height=3, dpi=300, bg = "transparent")

ggplot(s1.m.acc.int, aes(x = "Intuitive", y = mean)) + 
    geom_pointrange(data = s1.m.acc.int, aes(x="Intuitive",y = mean, ymax = max, ymin = min), size=1, color = "#DB2F2E") + 
    geom_pointrange(data = s1.m.acc.det, aes(x="Detailed" ,y = mean, ymax = max, ymin = min), size=1, color = "#DB2F2E") + 
    geom_pointrange(data = s1.m.acc.com, aes(x="Compact"  ,y = mean, ymax = max, ymin = min), size=1, color = "#DB2F2E") + 
    scale_y_continuous(expand = c(0, 0), limits = c(-0.2,0.2)) +
    coord_flip() +
    theme_void() +
    labs(title ="", x = "Medium", y = "Accuracy")

last_plot()
ggsave("s1.accuracy.medium.png", width=6, height=3, dpi=300, bg = "transparent")


ggplot(s1.h.acc.int, aes(x = "Intuitive", y = mean)) + 
    geom_pointrange(data = s1.h.acc.int, aes(x="Intuitive",y = mean, ymax = max, ymin = min), size=1, color = "#DB2F2E") + 
    geom_pointrange(data = s1.h.acc.det, aes(x="Detailed" ,y = mean, ymax = max, ymin = min), size=1, color = "#DB2F2E") + 
    geom_pointrange(data = s1.h.acc.com, aes(x="Compact"  ,y = mean, ymax = max, ymin = min), size=1, color = "#DB2F2E") + 
    scale_y_continuous(expand = c(0, 0), limits = c(-0.2,0.2)) +
    coord_flip() +
    theme_void() +
    labs(title ="", x = "Hard", y = "Accuracy")

last_plot()
ggsave("s1.accuracy.hard.png", width=6, height=3, dpi=300, bg = "transparent")

###############

########### ACTIONS

ggplot(s1.e.act.int, aes(x = "Intuitive", y = mean)) + 
    geom_pointrange(data = s1.e.act.int, aes(x="Intuitive",y = mean, ymax = max, ymin = min), size=1, color = "#DB2F2E") + 
    geom_pointrange(data = s1.e.act.det, aes(x="Detailed" ,y = mean, ymax = max, ymin = min), size=1, color = "#DB2F2E") + 
    geom_pointrange(data = s1.e.act.com, aes(x="Compact"  ,y = mean, ymax = max, ymin = min), size=1, color = "#DB2F2E") + 
    scale_y_continuous(expand = c(0, 0), limits = c(-0.2,0.2)) +
    coord_flip() +
    theme_void() +
    labs(title ="", x = "Easy", y = "Actions")

last_plot()
ggsave("s1.actions.easy.png", width=6, height=3, dpi=300, bg = "transparent")


ggplot(s1.m.act.int, aes(x = "Intuitive", y = mean)) + 
    geom_pointrange(data = s1.m.act.int, aes(x="Intuitive",y = mean, ymax = max, ymin = min), size=1, color = "#DB2F2E") + 
    geom_pointrange(data = s1.m.act.det, aes(x="Detailed" ,y = mean, ymax = max, ymin = min), size=1, color = "#DB2F2E") + 
    geom_pointrange(data = s1.m.act.com, aes(x="Compact"  ,y = mean, ymax = max, ymin = min), size=1, color = "#DB2F2E") + 
    scale_y_continuous(expand = c(0, 0), limits = c(-0.2,0.2)) +
    coord_flip() +
    theme_void() +
    labs(title ="", x = "Medium", y = "Actions")

last_plot()
ggsave("s1.actions.medium.png", width=6, height=3, dpi=300, bg = "transparent")

ggplot(s1.h.act.int, aes(x = "Intuitive", y = mean)) + 
    geom_pointrange(data = s1.h.act.int, aes(x="Intuitive",y = mean, ymax = max, ymin = min), size=1, color = "#DB2F2E") + 
    geom_pointrange(data = s1.h.act.det, aes(x="Detailed" ,y = mean, ymax = max, ymin = min), size=1, color = "#DB2F2E") + 
    geom_pointrange(data = s1.h.act.com, aes(x="Compact"  ,y = mean, ymax = max, ymin = min), size=1, color = "#DB2F2E") + 
    scale_y_continuous(expand = c(0, 0), limits = c(-0.2,0.2)) +
    coord_flip() +
    theme_void() +
    labs(title ="", x = "Hard", y = "Actions")

last_plot()
ggsave("s1.actions.hard.png", width=6, height=3, dpi=300, bg = "transparent")


############### SPEEED

ggplot(s1.e.speed.int, aes(x = "Intuitive", y = mean)) + 
    geom_pointrange(data = s1.e.speed.int, aes(x="Intuitive",y = mean, ymax = max, ymin = min), size=1, color = "#DB2F2E") + 
    geom_pointrange(data = s1.e.speed.det, aes(x="Detailed" ,y = mean, ymax = max, ymin = min), size=1, color = "#DB2F2E") + 
    geom_pointrange(data = s1.e.speed.com, aes(x="Compact"  ,y = mean, ymax = max, ymin = min), size=1, color = "#DB2F2E") + 
    scale_y_continuous(expand = c(0, 0), limits = c(-0.2,0.2)) +
    coord_flip() +
    theme_void() +
    labs(title ="", x = "Easy", y = "Speed")

last_plot()
ggsave("s1.speed.easy.png", width=6, height=3, dpi=300, bg = "transparent")

ggplot(s1.m.speed.int, aes(x = "Intuitive", y = mean)) + 
    geom_pointrange(data = s1.m.speed.int, aes(x="Intuitive",y = mean, ymax = max, ymin = min), size=1, color = "#DB2F2E") + 
    geom_pointrange(data = s1.m.speed.det, aes(x="Detailed" ,y = mean, ymax = max, ymin = min), size=1, color = "#DB2F2E") + 
    geom_pointrange(data = s1.m.speed.com, aes(x="Compact"  ,y = mean, ymax = max, ymin = min), size=1, color = "#DB2F2E") + 
    scale_y_continuous(expand = c(0, 0), limits = c(-0.2,0.2)) +
    coord_flip() +
    theme_void() +
    labs(title ="", x = "Medium", y = "Speed")

last_plot()
ggsave("s1.speed.medium.png", width=6, height=3, dpi=300, bg = "transparent")

ggplot(s1.h.speed.int, aes(x = "Intuitive", y = mean)) + 
    geom_pointrange(data = s1.h.speed.int, aes(x="Intuitive",y = mean, ymax = max, ymin = min), size=1, color = "#DB2F2E") + 
    geom_pointrange(data = s1.h.speed.det, aes(x="Detailed" ,y = mean, ymax = max, ymin = min), size=1, color = "#DB2F2E") + 
    geom_pointrange(data = s1.h.speed.com, aes(x="Compact"  ,y = mean, ymax = max, ymin = min), size=1, color = "#DB2F2E") + 
    scale_y_continuous(expand = c(0, 0), limits = c(-0.2,0.2)) +
    coord_flip() +
    theme_void() +
    labs(title ="", x = "Hard", y = "Speed")

last_plot()
ggsave("s1.speed.hard.png", width=6, height=3, dpi=300, bg = "transparent")


# 
# vioplot(normalize(s1.series.boxplot.easy$ls.speed)   - s1.mean.speed,  horizontal=TRUE, col="#DB2F2E")
# vioplot(normalize(s1.series.boxplot.medium$ls.speed) - s1.mean.speed,  horizontal=TRUE, col="#DB2F2E")
# vioplot(normalize(s1.series.boxplot.hard$ls.speed)   - s1.mean.speed,  horizontal=TRUE, col="#DB2F2E")
# vioplot(normalize(s1.charts.boxplot.easy$ls.speed)   - s1.mean.speed,  horizontal=TRUE, col="#DB2F2E")
# vioplot(normalize(s1.charts.boxplot.medium$ls.speed) - s1.mean.speed,  horizontal=TRUE, col="#DB2F2E")
# vioplot(normalize(s1.charts.boxplot.hard$ls.speed)   - s1.mean.speed,  horizontal=TRUE, col="#DB2F2E")
# vioplot(normalize(s1.dots.boxplot.easy$ls.speed)     - s1.mean.speed,  horizontal=TRUE, col="#DB2F2E")
# vioplot(normalize(s1.dots.boxplot.medium$ls.speed)   - s1.mean.speed,  horizontal=TRUE, col="#DB2F2E")
# vioplot(normalize(s1.dots.boxplot.hard$ls.speed)     - s1.mean.speed,  horizontal=TRUE, col="#DB2F2E")
# 
# 
#this.thing <- s1.series.boxplot.medium$ls.accuracy - s1.mean.acc
#accuracy.frame <- data.frame(c(this.thing))

#ggplot(accuracy.frame, aes(factor(1), accuracy.frame$c.this.thing.)) + geom_violin(trim = FALSE, fill = "#DB2F2E") + coord_flip() + theme_void()

# 
# vioplot(s1.series.boxplot.medium$ls.accuracy - s1.mean.acc,  horizontal=TRUE, col="#DB2F2E", ylim=c(-1.0, 1.0), drawRect = FALSE)
# vioplot(s1.series.boxplot.hard$ls.accuracy   - s1.mean.acc,  horizontal=TRUE, col="#DB2F2E", ylim=c(-1.0, 1.0), drawRect = FALSE)
# vioplot(s1.charts.boxplot.easy$ls.accuracy   - s1.mean.acc,  horizontal=TRUE, col="#DB2F2E", ylim=c(-1.0, 1.0), drawRect = FALSE)
# vioplot(s1.charts.boxplot.medium$ls.accuracy - s1.mean.acc,  horizontal=TRUE, col="#DB2F2E", ylim=c(-1.0, 1.0), drawRect = FALSE)
# vioplot(s1.charts.boxplot.hard$ls.accuracy   - s1.mean.acc,  horizontal=TRUE, col="#DB2F2E", ylim=c(-1.0, 1.0), drawRect = FALSE)
# vioplot(s1.dots.boxplot.easy$ls.accuracy     - s1.mean.acc,  horizontal=TRUE, col="#DB2F2E", ylim=c(-1.0, 1.0), drawRect = FALSE)
# vioplot(s1.dots.boxplot.medium$ls.accuracy   - s1.mean.acc,  horizontal=TRUE, col="#DB2F2E", ylim=c(-1.0, 1.0), drawRect = FALSE)
# vioplot(s1.dots.boxplot.hard$ls.accuracy     - s1.mean.acc,  horizontal=TRUE, col="#DB2F2E", ylim=c(-1.0, 1.0), drawRect = FALSE)
# 
# vioplot(normalize(s1.series.boxplot.easy$ls.actions)   - s1.mean.actions,  horizontal=TRUE, col="#DB2F2E")
# vioplot(normalize(s1.series.boxplot.medium$ls.actions) - s1.mean.actions,  horizontal=TRUE, col="#DB2F2E")
# vioplot(normalize(s1.series.boxplot.hard$ls.actions)   - s1.mean.actions,  horizontal=TRUE, col="#DB2F2E")
# vioplot(normalize(s1.charts.boxplot.easy$ls.actions)   - s1.mean.actions,  horizontal=TRUE, col="#DB2F2E")
# vioplot(normalize(s1.charts.boxplot.medium$ls.actions) - s1.mean.actions,  horizontal=TRUE, col="#DB2F2E")
# vioplot(normalize(s1.charts.boxplot.hard$ls.actions)   - s1.mean.actions,  horizontal=TRUE, col="#DB2F2E")
# vioplot(normalize(s1.dots.boxplot.easy$ls.actions)     - s1.mean.actions,  horizontal=TRUE, col="#DB2F2E")
# vioplot(normalize(s1.dots.boxplot.medium$ls.actions)   - s1.mean.actions,  horizontal=TRUE, col="#DB2F2E")
# vioplot(normalize(s1.dots.boxplot.hard$ls.actions)     - s1.mean.actions,  horizontal=TRUE, col="#DB2F2E")
#l1 <- normalize(s2.dots.boxplot.hard$ls.accuracy)   - s2.mean.acc
#l2 <- normalize(s2.charts.boxplot.hard$ls.accuracy) - s2.mean.acc
#l3 <- normalize(s2.series.boxplot.hard$ls.accuracy) - s2.mean.acc
#some.frame <- data.frame(l1,l2,l3)
#fit <- brm(l1 ~ ., data = some.frame)
#posterior <- as.matrix(fit)




