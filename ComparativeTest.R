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


normalize <- function(x) {(x - min(x, na.rm=TRUE))/(max(x,na.rm=TRUE) - min(x, na.rm=TRUE))}

s2.mean.acc <- mean(
               mean(s2.series.boxplot.easy$ls.accuracy  ),
               mean(s2.charts.boxplot.easy$ls.accuracy  ), 
               mean(s2.dots.boxplot.easy$ls.accuracy    ),
               mean(s2.series.boxplot.medium$ls.accuracy),
               mean(s2.charts.boxplot.medium$ls.accuracy),
               mean(s2.dots.boxplot.medium$ls.accuracy  ),
               mean(s2.series.boxplot.hard$ls.accuracy  ),
               mean(s2.charts.boxplot.hard$ls.accuracy  ),
               mean(s2.dots.boxplot.hard$ls.accuracy    ))

s2.mean.speed <- mean(
    mean(normalize(s2.series.boxplot.easy$ls.speed  )),
    mean(normalize(s2.charts.boxplot.easy$ls.speed  )), 
    mean(normalize(s2.dots.boxplot.easy$ls.speed    )),
    mean(normalize(s2.series.boxplot.medium$ls.speed)),
    mean(normalize(s2.charts.boxplot.medium$ls.speed)),
    mean(normalize(s2.dots.boxplot.medium$ls.speed  )),
    mean(normalize(s2.series.boxplot.hard$ls.speed  )),
    mean(normalize(s2.charts.boxplot.hard$ls.speed  )),
    mean(normalize(s2.dots.boxplot.hard$ls.speed    )))

s2.mean.actions <- mean(
    mean(normalize(s2.series.boxplot.easy$ls.actions  )),
    mean(normalize(s2.charts.boxplot.easy$ls.actions  )), 
    mean(normalize(s2.dots.boxplot.easy$ls.actions    )),
    mean(normalize(s2.series.boxplot.medium$ls.actions)),
    mean(normalize(s2.charts.boxplot.medium$ls.actions)),
    mean(normalize(s2.dots.boxplot.medium$ls.actions  )),
    mean(normalize(s2.series.boxplot.hard$ls.actions  )),
    mean(normalize(s2.charts.boxplot.hard$ls.actions  )),
    mean(normalize(s2.dots.boxplot.hard$ls.actions    )))


s1.mean.acc <- mean(
    mean(s1.series.boxplot.easy$ls.accuracy  ),
    mean(s1.charts.boxplot.easy$ls.accuracy  ), 
    mean(s1.dots.boxplot.easy$ls.accuracy    ),
    mean(s1.series.boxplot.medium$ls.accuracy),
    mean(s1.charts.boxplot.medium$ls.accuracy),
    mean(s1.dots.boxplot.medium$ls.accuracy  ),
    mean(s1.series.boxplot.hard$ls.accuracy  ),
    mean(s1.charts.boxplot.hard$ls.accuracy  ),
    mean(s1.dots.boxplot.hard$ls.accuracy    ))

s1.mean.speed <- mean(
    mean(normalize(s1.series.boxplot.easy$ls.speed  )),
    mean(normalize(s1.charts.boxplot.easy$ls.speed  )), 
    mean(normalize(s1.dots.boxplot.easy$ls.speed    )),
    mean(normalize(s1.series.boxplot.medium$ls.speed)),
    mean(normalize(s1.charts.boxplot.medium$ls.speed)),
    mean(normalize(s1.dots.boxplot.medium$ls.speed  )),
    mean(normalize(s1.series.boxplot.hard$ls.speed  )),
    mean(normalize(s1.charts.boxplot.hard$ls.speed  )),
    mean(normalize(s1.dots.boxplot.hard$ls.speed    )))

s1.mean.actions <- mean(
    mean(normalize(s1.series.boxplot.easy$ls.actions  )),
    mean(normalize(s1.charts.boxplot.easy$ls.actions  )), 
    mean(normalize(s1.dots.boxplot.easy$ls.actions    )),
    mean(normalize(s1.series.boxplot.medium$ls.actions)),
    mean(normalize(s1.charts.boxplot.medium$ls.actions)),
    mean(normalize(s1.dots.boxplot.medium$ls.actions  )),
    mean(normalize(s1.series.boxplot.hard$ls.actions  )),
    mean(normalize(s1.charts.boxplot.hard$ls.actions  )),
    mean(normalize(s1.dots.boxplot.hard$ls.actions    )))
    
s2.e.acc.int <- conf.interval(s2.series.boxplot.easy$ls.accuracy   - s2.mean.acc)
s2.e.acc.det <- conf.interval(s2.charts.boxplot.easy$ls.accuracy   - s2.mean.acc)
s2.e.acc.com <- conf.interval(s2.dots.boxplot.easy$ls.accuracy     - s2.mean.acc)
s2.m.acc.int <- conf.interval(s2.series.boxplot.medium$ls.accuracy - s2.mean.acc)
s2.m.acc.det <- conf.interval(s2.charts.boxplot.medium$ls.accuracy - s2.mean.acc)
s2.m.acc.com <- conf.interval(s2.dots.boxplot.medium$ls.accuracy   - s2.mean.acc)
s2.h.acc.int <- conf.interval(s2.series.boxplot.hard$ls.accuracy   - s2.mean.acc)
s2.h.acc.det <- conf.interval(s2.charts.boxplot.hard$ls.accuracy   - s2.mean.acc)
s2.h.acc.com <- conf.interval(s2.dots.boxplot.hard$ls.accuracy     - s2.mean.acc)

s2.e.act.int <- conf.interval(normalize(s2.series.boxplot.easy$ls.actions  ) - s2.mean.actions)
s2.e.act.det <- conf.interval(normalize(s2.charts.boxplot.easy$ls.actions  ) - s2.mean.actions)
s2.e.act.com <- conf.interval(normalize(s2.dots.boxplot.easy$ls.actions    ) - s2.mean.actions)
s2.m.act.int <- conf.interval(normalize(s2.series.boxplot.medium$ls.actions) - s2.mean.actions)
s2.m.act.det <- conf.interval(normalize(s2.charts.boxplot.medium$ls.actions) - s2.mean.actions)
s2.m.act.com <- conf.interval(normalize(s2.dots.boxplot.medium$ls.actions  ) - s2.mean.actions)
s2.h.act.int <- conf.interval(normalize(s2.series.boxplot.hard$ls.actions  ) - s2.mean.actions)
s2.h.act.det <- conf.interval(normalize(s2.charts.boxplot.hard$ls.actions  ) - s2.mean.actions)
s2.h.act.com <- conf.interval(normalize(s2.dots.boxplot.hard$ls.actions    ) - s2.mean.actions)

s2.e.speed.int <- conf.interval(normalize(s2.series.boxplot.easy$ls.speed  ) - s2.mean.speed)
s2.e.speed.det <- conf.interval(normalize(s2.charts.boxplot.easy$ls.speed  ) - s2.mean.speed)
s2.e.speed.com <- conf.interval(normalize(s2.dots.boxplot.easy$ls.speed    ) - s2.mean.speed)
s2.m.speed.int <- conf.interval(normalize(s2.series.boxplot.medium$ls.speed) - s2.mean.speed)
s2.m.speed.det <- conf.interval(normalize(s2.charts.boxplot.medium$ls.speed) - s2.mean.speed)
s2.m.speed.com <- conf.interval(normalize(s2.dots.boxplot.medium$ls.speed  ) - s2.mean.speed)
s2.h.speed.int <- conf.interval(normalize(s2.series.boxplot.hard$ls.speed  ) - s2.mean.speed)
s2.h.speed.det <- conf.interval(normalize(s2.charts.boxplot.hard$ls.speed  ) - s2.mean.speed)
s2.h.speed.com <- conf.interval(normalize(s2.dots.boxplot.hard$ls.speed    ) - s2.mean.speed)

#####

s1.e.acc.int <- conf.interval(s1.series.boxplot.easy$ls.accuracy   - s1.mean.acc)
s1.e.acc.det <- conf.interval(s1.charts.boxplot.easy$ls.accuracy   - s1.mean.acc)
s1.e.acc.com <- conf.interval(s1.dots.boxplot.easy$ls.accuracy     - s1.mean.acc)
s1.m.acc.int <- conf.interval(s1.series.boxplot.medium$ls.accuracy - s1.mean.acc)
s1.m.acc.det <- conf.interval(s1.charts.boxplot.medium$ls.accuracy - s1.mean.acc)
s1.m.acc.com <- conf.interval(s1.dots.boxplot.medium$ls.accuracy   - s1.mean.acc)
s1.h.acc.int <- conf.interval(s1.series.boxplot.hard$ls.accuracy   - s1.mean.acc)
s1.h.acc.det <- conf.interval(s1.charts.boxplot.hard$ls.accuracy   - s1.mean.acc)
s1.h.acc.com <- conf.interval(s1.dots.boxplot.hard$ls.accuracy     - s1.mean.acc)

s1.e.act.int <- conf.interval(normalize(s1.series.boxplot.easy$ls.actions  ) - s1.mean.actions)
s1.e.act.det <- conf.interval(normalize(s1.charts.boxplot.easy$ls.actions  ) - s1.mean.actions)
s1.e.act.com <- conf.interval(normalize(s1.dots.boxplot.easy$ls.actions    ) - s1.mean.actions)
s1.m.act.int <- conf.interval(normalize(s1.series.boxplot.medium$ls.actions) - s1.mean.actions)
s1.m.act.det <- conf.interval(normalize(s1.charts.boxplot.medium$ls.actions) - s1.mean.actions)
s1.m.act.com <- conf.interval(normalize(s1.dots.boxplot.medium$ls.actions  ) - s1.mean.actions)
s1.h.act.int <- conf.interval(normalize(s1.series.boxplot.hard$ls.actions  ) - s1.mean.actions)
s1.h.act.det <- conf.interval(normalize(s1.charts.boxplot.hard$ls.actions  ) - s1.mean.actions)
s1.h.act.com <- conf.interval(normalize(s1.dots.boxplot.hard$ls.actions    ) - s1.mean.actions)

s1.e.speed.int <- conf.interval(normalize(s1.series.boxplot.easy$ls.speed   ) - s1.mean.speed)
s1.e.speed.det <- conf.interval(normalize(s1.charts.boxplot.easy$ls.speed   ) - s1.mean.speed)
s1.e.speed.com <- conf.interval(normalize(s1.dots.boxplot.easy$ls.speed     ) - s1.mean.speed)
s1.m.speed.int <- conf.interval(normalize(s1.series.boxplot.medium$ls.speed ) - s1.mean.speed)
s1.m.speed.det <- conf.interval(normalize(s1.charts.boxplot.medium$ls.speed ) - s1.mean.speed)
s1.m.speed.com <- conf.interval(normalize(s1.dots.boxplot.medium$ls.speed   ) - s1.mean.speed)
s1.h.speed.int <- conf.interval(normalize(s1.series.boxplot.hard$ls.speed   ) - s1.mean.speed)
s1.h.speed.det <- conf.interval(normalize(s1.charts.boxplot.hard$ls.speed   ) - s1.mean.speed)
s1.h.speed.com <- conf.interval(normalize(s1.dots.boxplot.hard$ls.speed     ) - s1.mean.speed)

########## ACCURACY

ggplot(s2.e.acc.int, aes(x = "Intuitive", y = mean)) + 
    geom_pointrange(data = s2.e.acc.int, aes(x="Intuitive",y = mean, ymax = max, ymin = min), size=1, color = "#08519c") + 
    geom_pointrange(data = s2.e.acc.det, aes(x="Detailed" ,y = mean, ymax = max, ymin = min), size=1, color = "#08519c") + 
    geom_pointrange(data = s2.e.acc.com, aes(x="Compact"  ,y = mean, ymax = max, ymin = min), size=1, color = "#08519c") + 
    scale_y_continuous(expand = c(0, 0), limits = c(-0.5,0.5)) +
    coord_flip() +
    theme_void() +
    labs(title ="", x = "Easy", y = "Accuracy")

last_plot()
ggsave("s2.accuracy.easy.png", width=6, height=3, dpi=300, bg = "transparent")

ggplot(s2.m.acc.int, aes(x = "Intuitive", y = mean)) + 
    geom_pointrange(data = s2.m.acc.int, aes(x="Intuitive",y = mean, ymax = max, ymin = min), size=1, color = "#08519c") + 
    geom_pointrange(data = s2.m.acc.det, aes(x="Detailed" ,y = mean, ymax = max, ymin = min), size=1, color = "#08519c") + 
    geom_pointrange(data = s2.m.acc.com, aes(x="Compact"  ,y = mean, ymax = max, ymin = min), size=1, color = "#08519c") + 
    scale_y_continuous(expand = c(0, 0), limits = c(-0.5,0.5)) +
    coord_flip() +
    theme_void() +
    labs(title ="", x = "Medium", y = "Accuracy")

last_plot()
ggsave("s2.accuracy.medium.png", width=6, height=3, dpi=300, bg = "transparent")


ggplot(s2.h.acc.int, aes(x = "Intuitive", y = mean)) + 
    geom_pointrange(data = s2.h.acc.int, aes(x="Intuitive",y = mean, ymax = max, ymin = min), size=1, color = "#08519c") + 
    geom_pointrange(data = s2.h.acc.det, aes(x="Detailed" ,y = mean, ymax = max, ymin = min), size=1, color = "#08519c") + 
    geom_pointrange(data = s2.h.acc.com, aes(x="Compact"  ,y = mean, ymax = max, ymin = min), size=1, color = "#08519c") + 
    scale_y_continuous(expand = c(0, 0), limits = c(-0.5,0.5)) +
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
    scale_y_continuous(expand = c(0, 0), limits = c(-0.5,0.5)) +
    coord_flip() +
    theme_void() +
    labs(title ="", x = "Easy", y = "Actions")

last_plot()
ggsave("s2.actions.easy.png", width=6, height=3, dpi=300, bg = "transparent")


ggplot(s2.m.act.int, aes(x = "Intuitive", y = mean)) + 
    geom_pointrange(data = s2.m.act.int, aes(x="Intuitive",y = mean, ymax = max, ymin = min), size=1, color = "#08519c") + 
    geom_pointrange(data = s2.m.act.det, aes(x="Detailed" ,y = mean, ymax = max, ymin = min), size=1, color = "#08519c") + 
    geom_pointrange(data = s2.m.act.com, aes(x="Compact"  ,y = mean, ymax = max, ymin = min), size=1, color = "#08519c") + 
    scale_y_continuous(expand = c(0, 0), limits = c(-0.5,0.5)) +
    coord_flip() +
    theme_void() +
    labs(title ="", x = "Medium", y = "Actions")

last_plot()
ggsave("s2.actions.medium.png", width=6, height=3, dpi=300, bg = "transparent")

ggplot(s2.h.act.int, aes(x = "Intuitive", y = mean)) + 
    geom_pointrange(data = s2.h.act.int, aes(x="Intuitive",y = mean, ymax = max, ymin = min), size=1, color = "#08519c") + 
    geom_pointrange(data = s2.h.act.det, aes(x="Detailed" ,y = mean, ymax = max, ymin = min), size=1, color = "#08519c") + 
    geom_pointrange(data = s2.h.act.com, aes(x="Compact"  ,y = mean, ymax = max, ymin = min), size=1, color = "#08519c") + 
    scale_y_continuous(expand = c(0, 0), limits = c(-0.5,0.5)) +
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
    scale_y_continuous(expand = c(0, 0), limits = c(-0.5,0.5)) +
    coord_flip() +
    theme_void() +
    labs(title ="", x = "Easy", y = "Speed")

last_plot()
ggsave("s2.speed.easy.png", width=6, height=3, dpi=300, bg = "transparent")

ggplot(s2.m.speed.int, aes(x = "Intuitive", y = mean)) + 
    geom_pointrange(data = s2.m.speed.int, aes(x="Intuitive",y = mean, ymax = max, ymin = min), size=1, color = "#08519c") + 
    geom_pointrange(data = s2.m.speed.det, aes(x="Detailed" ,y = mean, ymax = max, ymin = min), size=1, color = "#08519c") + 
    geom_pointrange(data = s2.m.speed.com, aes(x="Compact"  ,y = mean, ymax = max, ymin = min), size=1, color = "#08519c") + 
    scale_y_continuous(expand = c(0, 0), limits = c(-0.5,0.5)) +
    coord_flip() +
    theme_void() +
    labs(title ="", x = "Medium", y = "Speed")

last_plot()
ggsave("s2.speed.medium.png", width=6, height=3, dpi=300, bg = "transparent")

ggplot(s2.h.speed.int, aes(x = "Intuitive", y = mean)) + 
    geom_pointrange(data = s2.h.speed.int, aes(x="Intuitive",y = mean, ymax = max, ymin = min), size=1, color = "#08519c") + 
    geom_pointrange(data = s2.h.speed.det, aes(x="Detailed" ,y = mean, ymax = max, ymin = min), size=1, color = "#08519c") + 
    geom_pointrange(data = s2.h.speed.com, aes(x="Compact"  ,y = mean, ymax = max, ymin = min), size=1, color = "#08519c") + 
    scale_y_continuous(expand = c(0, 0), limits = c(-0.5,0.5)) +
    coord_flip() +
    theme_void() +
    theme(panel.background = element_blank()) +
    labs(title ="", x = "Hard", y = "Speed")

last_plot()
ggsave("s2.speed.hard.png", width=6, height=3, dpi=300, bg = "transparent")




############# FIRST STUDY 

ggplot(s1.e.acc.int, aes(x = "Intuitive", y = mean)) + 
    geom_pointrange(data = s1.e.acc.int, aes(x="Intuitive",y = mean, ymax = max, ymin = min), size=1, color = "#DB2F2E") + 
    geom_pointrange(data = s1.e.acc.det, aes(x="Detailed" ,y = mean, ymax = max, ymin = min), size=1, color = "#DB2F2E") + 
    geom_pointrange(data = s1.e.acc.com, aes(x="Compact"  ,y = mean, ymax = max, ymin = min), size=1, color = "#DB2F2E") + 
    scale_y_continuous(expand = c(0, 0), limits = c(-0.5,0.5)) +
    coord_flip() +
    theme_void() +
    labs(title ="", x = "Easy", y = "Accuracy")

last_plot()
ggsave("s1.accuracy.easy.png", width=6, height=3, dpi=300, bg = "transparent")

ggplot(s1.m.acc.int, aes(x = "Intuitive", y = mean)) + 
    geom_pointrange(data = s1.m.acc.int, aes(x="Intuitive",y = mean, ymax = max, ymin = min), size=1, color = "#DB2F2E") + 
    geom_pointrange(data = s1.m.acc.det, aes(x="Detailed" ,y = mean, ymax = max, ymin = min), size=1, color = "#DB2F2E") + 
    geom_pointrange(data = s1.m.acc.com, aes(x="Compact"  ,y = mean, ymax = max, ymin = min), size=1, color = "#DB2F2E") + 
    scale_y_continuous(expand = c(0, 0), limits = c(-0.5,0.5)) +
    coord_flip() +
    theme_void() +
    labs(title ="", x = "Medium", y = "Accuracy")

last_plot()
ggsave("s1.accuracy.medium.png", width=6, height=3, dpi=300, bg = "transparent")


ggplot(s1.h.acc.int, aes(x = "Intuitive", y = mean)) + 
    geom_pointrange(data = s1.h.acc.int, aes(x="Intuitive",y = mean, ymax = max, ymin = min), size=1, color = "#DB2F2E") + 
    geom_pointrange(data = s1.h.acc.det, aes(x="Detailed" ,y = mean, ymax = max, ymin = min), size=1, color = "#DB2F2E") + 
    geom_pointrange(data = s1.h.acc.com, aes(x="Compact"  ,y = mean, ymax = max, ymin = min), size=1, color = "#DB2F2E") + 
    scale_y_continuous(expand = c(0, 0), limits = c(-0.5,0.5)) +
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
    scale_y_continuous(expand = c(0, 0), limits = c(-0.5,0.5)) +
    coord_flip() +
    theme_void() +
    labs(title ="", x = "Easy", y = "Actions")

last_plot()
ggsave("s1.actions.easy.png", width=6, height=3, dpi=300, bg = "transparent")


ggplot(s1.m.act.int, aes(x = "Intuitive", y = mean)) + 
    geom_pointrange(data = s1.m.act.int, aes(x="Intuitive",y = mean, ymax = max, ymin = min), size=1, color = "#DB2F2E") + 
    geom_pointrange(data = s1.m.act.det, aes(x="Detailed" ,y = mean, ymax = max, ymin = min), size=1, color = "#DB2F2E") + 
    geom_pointrange(data = s1.m.act.com, aes(x="Compact"  ,y = mean, ymax = max, ymin = min), size=1, color = "#DB2F2E") + 
    scale_y_continuous(expand = c(0, 0), limits = c(-0.5,0.5)) +
    coord_flip() +
    theme_void() +
    labs(title ="", x = "Medium", y = "Actions")

last_plot()
ggsave("s1.actions.medium.png", width=6, height=3, dpi=300, bg = "transparent")

ggplot(s1.h.act.int, aes(x = "Intuitive", y = mean)) + 
    geom_pointrange(data = s1.h.act.int, aes(x="Intuitive",y = mean, ymax = max, ymin = min), size=1, color = "#DB2F2E") + 
    geom_pointrange(data = s1.h.act.det, aes(x="Detailed" ,y = mean, ymax = max, ymin = min), size=1, color = "#DB2F2E") + 
    geom_pointrange(data = s1.h.act.com, aes(x="Compact"  ,y = mean, ymax = max, ymin = min), size=1, color = "#DB2F2E") + 
    scale_y_continuous(expand = c(0, 0), limits = c(-0.5,0.5)) +
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
    scale_y_continuous(expand = c(0, 0), limits = c(-0.5,0.5)) +
    coord_flip() +
    theme_void() +
    labs(title ="", x = "Easy", y = "Speed")

last_plot()
ggsave("s1.speed.easy.png", width=6, height=3, dpi=300, bg = "transparent")

ggplot(s1.m.speed.int, aes(x = "Intuitive", y = mean)) + 
    geom_pointrange(data = s1.m.speed.int, aes(x="Intuitive",y = mean, ymax = max, ymin = min), size=1, color = "#DB2F2E") + 
    geom_pointrange(data = s1.m.speed.det, aes(x="Detailed" ,y = mean, ymax = max, ymin = min), size=1, color = "#DB2F2E") + 
    geom_pointrange(data = s1.m.speed.com, aes(x="Compact"  ,y = mean, ymax = max, ymin = min), size=1, color = "#DB2F2E") + 
    scale_y_continuous(expand = c(0, 0), limits = c(-0.5,0.5)) +
    coord_flip() +
    theme_void() +
    labs(title ="", x = "Medium", y = "Speed")

last_plot()
ggsave("s1.speed.medium.png", width=6, height=3, dpi=300, bg = "transparent")

ggplot(s1.h.speed.int, aes(x = "Intuitive", y = mean)) + 
    geom_pointrange(data = s1.h.speed.int, aes(x="Intuitive",y = mean, ymax = max, ymin = min), size=1, color = "#DB2F2E") + 
    geom_pointrange(data = s1.h.speed.det, aes(x="Detailed" ,y = mean, ymax = max, ymin = min), size=1, color = "#DB2F2E") + 
    geom_pointrange(data = s1.h.speed.com, aes(x="Compact"  ,y = mean, ymax = max, ymin = min), size=1, color = "#DB2F2E") + 
    scale_y_continuous(expand = c(0, 0), limits = c(-0.5,0.5)) +
    coord_flip() +
    theme_void() +
    theme(panel.background = element_blank()) +
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
this.thing <- s1.series.boxplot.medium$ls.accuracy - s1.mean.acc
accuracy.frame <- data.frame(c(this.thing))

ggplot(accuracy.frame, aes(factor(1), accuracy.frame$c.this.thing.)) + geom_violin(trim = FALSE, fill = "#DB2F2E") + coord_flip() + theme_void()

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


l1 <- normalize(s2.dots.boxplot.hard$ls.accuracy)   - s2.mean.acc
l2 <- normalize(s2.charts.boxplot.hard$ls.accuracy) - s2.mean.acc
l3 <- normalize(s2.series.boxplot.hard$ls.accuracy) - s2.mean.acc


some.frame <- data.frame(l1,l2,l3)

fit <- brm(l1 ~ ., data = some.frame)
posterior <- as.matrix(fit)




