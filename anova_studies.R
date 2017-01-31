library(ggplot2)
s1.accuracy <- read.csv("data/s1.df.accuracy.all.csv")
s1.actions  <- read.csv("data/s1.df.actions.all.csv" )
s1.speed    <- read.csv("data/s1.df.speed.all.csv"   )
s2.accuracy <- read.csv("data/s2.df.accuracy.all.csv")
s2.actions  <- read.csv("data/s2.df.actions.all.csv" )
s2.speed    <- read.csv("data/s2.df.speed.all.csv"   )

# Analysis of Variance (Factorial for each data set).
s1.aov.accuracy <- aov(accuracy ~ viz * difficulty, data = s1.accuracy)
s1.aov.actions  <- aov(actions  ~ viz * difficulty, data = s1.actions )
s1.aov.speed    <- aov(speed    ~ viz * difficulty, data = s1.speed   )

s2.aov.accuracy <- aov(accuracy ~ viz * difficulty, data = s2.accuracy)
s2.aov.actions  <- aov(actions  ~ viz * difficulty, data = s2.actions )
s2.aov.speed    <- aov(speed    ~ viz * difficulty, data = s2.speed   )

# Tukey HSD posterior test to find pairwise differences.
TukeyHSD(s1.aov.accuracy)
TukeyHSD(s1.aov.actions )
TukeyHSD(s1.aov.speed   )

TukeyHSD(s2.aov.accuracy)
TukeyHSD(s2.aov.actions )
TukeyHSD(s2.aov.speed   )

# T-test samples of each set.

# Function to get CI.
conf.interval <- function(data) {
    mean   <- data$estimate
    min    <- data$conf.int[1]
    max    <- data$conf.int[2]
    output <- data.frame(mean,min,max)
    return(output)
}

t.test(s1.accuracy$accuracy)
t.test(subset(s1.accuracy, viz=="dots" , select=accuracy))
t.test(subset(s1.accuracy, viz=="chart", select=accuracy))
t.test(subset(s1.accuracy, viz=="map"  , select=accuracy))
t.test(subset(s1.accuracy, difficulty=="easy"  , select=accuracy))
t.test(subset(s1.accuracy, difficulty=="medium", select=accuracy))
t.test(subset(s1.accuracy, difficulty=="hard"  , select=accuracy))
t1.acc.e.com <- conf.interval(t.test(subset(s1.accuracy, difficulty=="easy"   & viz=="dots",  select=accuracy)))
t1.acc.e.det <- conf.interval(t.test(subset(s1.accuracy, difficulty=="easy"   & viz=="chart", select=accuracy)))
t1.acc.e.int <- conf.interval(t.test(subset(s1.accuracy, difficulty=="easy"   & viz=="map",   select=accuracy)))
t1.acc.m.com <- conf.interval(t.test(subset(s1.accuracy, difficulty=="medium" & viz=="dots",  select=accuracy)))
t1.acc.m.int <- conf.interval(t.test(subset(s1.accuracy, difficulty=="medium" & viz=="map",   select=accuracy)))
t1.acc.m.det <- conf.interval(t.test(subset(s1.accuracy, difficulty=="medium" & viz=="chart", select=accuracy)))
t1.acc.h.det <- conf.interval(t.test(subset(s1.accuracy, difficulty=="hard"   & viz=="chart", select=accuracy)))
t1.acc.h.com <- conf.interval(t.test(subset(s1.accuracy, difficulty=="hard"   & viz=="dots",  select=accuracy)))
t1.acc.h.int <- conf.interval(t.test(subset(s1.accuracy, difficulty=="hard"   & viz=="map",   select=accuracy)))
# 
t.test(s1.actions$actions)
t.test(subset(s1.actions, viz=="dots" , select=actions))
t.test(subset(s1.actions, viz=="chart", select=actions))
t.test(subset(s1.actions, viz=="map"  , select=actions))
t.test(subset(s1.actions, difficulty=="easy"  , select=actions))
t.test(subset(s1.actions, difficulty=="medium", select=actions))
t.test(subset(s1.actions, difficulty=="hard"  , select=actions))
t1.act.e.com <- conf.interval(t.test(subset(s1.actions, difficulty=="easy"   & viz=="dots",  select=actions)))
t1.act.e.det <- conf.interval(t.test(subset(s1.actions, difficulty=="easy"   & viz=="chart", select=actions)))
t1.act.e.int <- conf.interval(t.test(subset(s1.actions, difficulty=="easy"   & viz=="map",   select=actions)))
t1.act.m.com <- conf.interval(t.test(subset(s1.actions, difficulty=="medium" & viz=="dots",  select=actions)))
t1.act.m.int <- conf.interval(t.test(subset(s1.actions, difficulty=="medium" & viz=="map",   select=actions)))
t1.act.m.det <- conf.interval(t.test(subset(s1.actions, difficulty=="medium" & viz=="chart", select=actions)))
t1.act.h.det <- conf.interval(t.test(subset(s1.actions, difficulty=="hard"   & viz=="chart", select=actions)))
t1.act.h.com <- conf.interval(t.test(subset(s1.actions, difficulty=="hard"   & viz=="dots",  select=actions)))
t1.act.h.int <- conf.interval(t.test(subset(s1.actions, difficulty=="hard"   & viz=="map",   select=actions)))
# 
t.test(s1.speed$speed)
t.test(subset(s1.speed, viz=="dots" , select=speed))
t.test(subset(s1.speed, viz=="chart", select=speed))
t.test(subset(s1.speed, viz=="map"  , select=speed))
t.test(subset(s1.speed, difficulty=="easy"  , select=speed))
t.test(subset(s1.speed, difficulty=="medium", select=speed))
t.test(subset(s1.speed, difficulty=="hard"  , select=speed))
t1.time.e.com <- conf.interval(t.test(subset(s1.speed, difficulty=="easy"   & viz=="dots",  select=speed)))
t1.time.e.det <- conf.interval(t.test(subset(s1.speed, difficulty=="easy"   & viz=="chart", select=speed)))
t1.time.e.int <- conf.interval(t.test(subset(s1.speed, difficulty=="easy"   & viz=="map",   select=speed)))
t1.time.m.com <- conf.interval(t.test(subset(s1.speed, difficulty=="medium" & viz=="dots",  select=speed)))
t1.time.m.int <- conf.interval(t.test(subset(s1.speed, difficulty=="medium" & viz=="map",   select=speed)))
t1.time.m.det <- conf.interval(t.test(subset(s1.speed, difficulty=="medium" & viz=="chart", select=speed)))
t1.time.h.det <- conf.interval(t.test(subset(s1.speed, difficulty=="hard"   & viz=="chart", select=speed)))
t1.time.h.com <- conf.interval(t.test(subset(s1.speed, difficulty=="hard"   & viz=="dots",  select=speed)))
t1.time.h.int <- conf.interval(t.test(subset(s1.speed, difficulty=="hard"   & viz=="map",   select=speed)))

##########
# Study 2
##########


t.test(s2.accuracy$accuracy)
t.test(subset(s2.accuracy, viz=="dots" , select=accuracy))
t.test(subset(s2.accuracy, viz=="chart", select=accuracy))
t.test(subset(s2.accuracy, viz=="series"  , select=accuracy))
t.test(subset(s2.accuracy, difficulty=="easy"  , select=accuracy))
t.test(subset(s2.accuracy, difficulty=="medium", select=accuracy))
t.test(subset(s2.accuracy, difficulty=="hard"  , select=accuracy))
t2.acc.e.com <- conf.interval(t.test(subset(s2.accuracy, difficulty=="easy"   & viz=="dots",  select=accuracy)))
t2.acc.e.det <- conf.interval(t.test(subset(s2.accuracy, difficulty=="easy"   & viz=="chart", select=accuracy)))
t2.acc.e.int <- conf.interval(t.test(subset(s2.accuracy, difficulty=="easy"   & viz=="series",   select=accuracy)))
t2.acc.m.com <- conf.interval(t.test(subset(s2.accuracy, difficulty=="medium" & viz=="dots",  select=accuracy)))
t2.acc.m.int <- conf.interval(t.test(subset(s2.accuracy, difficulty=="medium" & viz=="series",   select=accuracy)))
t2.acc.m.det <- conf.interval(t.test(subset(s2.accuracy, difficulty=="medium" & viz=="chart", select=accuracy)))
t2.acc.h.det <- conf.interval(t.test(subset(s2.accuracy, difficulty=="hard"   & viz=="chart", select=accuracy)))
t2.acc.h.com <- conf.interval(t.test(subset(s2.accuracy, difficulty=="hard"   & viz=="dots",  select=accuracy)))
t2.acc.h.int <- conf.interval(t.test(subset(s2.accuracy, difficulty=="hard"   & viz=="series",   select=accuracy)))

t.test(s2.actions$actions)
t.test(subset(s2.actions, viz=="dots" , select=actions))
t.test(subset(s2.actions, viz=="chart", select=actions))
t.test(subset(s2.actions, viz=="series"  , select=actions))
t.test(subset(s2.actions, difficulty=="easy"  , select=actions))
t.test(subset(s2.actions, difficulty=="medium", select=actions))
t.test(subset(s2.actions, difficulty=="hard"  , select=actions))
t2.act.e.com <- conf.interval(t.test(subset(s2.actions, difficulty=="easy"   & viz=="dots",  select=actions)))
t2.act.e.det <- conf.interval(t.test(subset(s2.actions, difficulty=="easy"   & viz=="chart", select=actions)))
t2.act.e.int <- conf.interval(t.test(subset(s2.actions, difficulty=="easy"   & viz=="series",   select=actions)))
t2.act.m.com <- conf.interval(t.test(subset(s2.actions, difficulty=="medium" & viz=="dots",  select=actions)))
t2.act.m.int <- conf.interval(t.test(subset(s2.actions, difficulty=="medium" & viz=="series",   select=actions)))
t2.act.m.det <- conf.interval(t.test(subset(s2.actions, difficulty=="medium" & viz=="chart", select=actions)))
t2.act.h.det <- conf.interval(t.test(subset(s2.actions, difficulty=="hard"   & viz=="chart", select=actions)))
t2.act.h.com <- conf.interval(t.test(subset(s2.actions, difficulty=="hard"   & viz=="dots",  select=actions)))
t2.act.h.int <- conf.interval(t.test(subset(s2.actions, difficulty=="hard"   & viz=="series",   select=actions)))

t.test(s2.speed$speed)
t.test(subset(s2.speed, viz=="dots" , select=speed))
t.test(subset(s2.speed, viz=="chart", select=speed))
t.test(subset(s2.speed, viz=="series"  , select=speed))
t.test(subset(s2.speed, difficulty=="easy"  , select=speed))
t.test(subset(s2.speed, difficulty=="medium", select=speed))
t.test(subset(s2.speed, difficulty=="hard"  , select=speed))
t2.time.e.com <- conf.interval(t.test(subset(s2.speed, difficulty=="easy"   & viz=="dots",  select=speed)))
t2.time.e.det <- conf.interval(t.test(subset(s2.speed, difficulty=="easy"   & viz=="chart", select=speed)))
t2.time.e.int <- conf.interval(t.test(subset(s2.speed, difficulty=="easy"   & viz=="series",   select=speed)))
t2.time.m.com <- conf.interval(t.test(subset(s2.speed, difficulty=="medium" & viz=="dots",  select=speed)))
t2.time.m.int <- conf.interval(t.test(subset(s2.speed, difficulty=="medium" & viz=="series",   select=speed)))
t2.time.m.det <- conf.interval(t.test(subset(s2.speed, difficulty=="medium" & viz=="chart", select=speed)))
t2.time.h.det <- conf.interval(t.test(subset(s2.speed, difficulty=="hard"   & viz=="chart", select=speed)))
t2.time.h.com <- conf.interval(t.test(subset(s2.speed, difficulty=="hard"   & viz=="dots",  select=speed)))
t2.time.h.int <- conf.interval(t.test(subset(s2.speed, difficulty=="hard"   & viz=="series",   select=speed)))
# 
# 
# ggplot(t1.acc.e.int, aes(x = c("intuitive","detailed", "compact"), y = mean)) + 
#     geom_pointrange(data = t1.acc.e.int, aes(x="Intuitive",y = mean, ymax = max, ymin = min), size=1, color = "#08519c") + 
#     geom_pointrange(data = t1.acc.e.det, aes(x="Detailed" ,y = mean, ymax = max, ymin = min), size=1, color = "#08519c") + 
#     geom_pointrange(data = t1.acc.e.com, aes(x="Compact"  ,y = mean, ymax = max, ymin = min), size=1, color = "#08519c") + 
#     scale_y_continuous(expand = c(0, 0), limits = c(0,1)) +
#     coord_flip() + 
#     theme_void()
# 
# last_plot()
# ggsave("s1.easy.accuracy.png", width=6, height=2, dpi=300, bg = "transparent")
# 
# ggplot(t1.acc.m.int, aes(x = c("intuitive","detailed", "compact"), y = mean)) + 
#     geom_pointrange(data = t1.acc.m.int, aes(x="Intuitive",y = mean, ymax = max, ymin = min), size=1, color = "#08519c") + 
#     geom_pointrange(data = t1.acc.m.det, aes(x="Detailed" ,y = mean, ymax = max, ymin = min), size=1, color = "#08519c") + 
#     geom_pointrange(data = t1.acc.m.com, aes(x="Compact"  ,y = mean, ymax = max, ymin = min), size=1, color = "#08519c") + 
#     scale_y_continuous(expand = c(0, 0), limits = c(0,1)) +
#     coord_flip() + 
#     theme_void()
# 
# last_plot()
# ggsave("s1.medium.accuracy.png", width=6, height=2, dpi=300, bg = "transparent")
# 
# ggplot(t1.acc.h.int, aes(x = c("intuitive","detailed", "compact"), y = mean)) + 
#     geom_pointrange(data = t1.acc.h.int, aes(x="Intuitive",y = mean, ymax = max, ymin = min), size=1, color = "#08519c") + 
#     geom_pointrange(data = t1.acc.h.det, aes(x="Detailed" ,y = mean, ymax = max, ymin = min), size=1, color = "#08519c") + 
#     geom_pointrange(data = t1.acc.h.com, aes(x="Compact"  ,y = mean, ymax = max, ymin = min), size=1, color = "#08519c") + 
#     scale_y_continuous(expand = c(0, 0), limits = c(0,1)) +
#     coord_flip() + 
#     theme_void()
# 
# last_plot()
# ggsave("s1.hard.accuracy.png", width=6, height=2, dpi=300, bg = "transparent")
# 
# ggplot(t1.act.e.int, aes(x = c("intuitive","detailed", "compact"), y = mean)) + 
#     geom_pointrange(data = t1.act.e.int, aes(x="Intuitive",y = mean, ymax = max, ymin = min), size=1, color = "#08519c") + 
#     geom_pointrange(data = t1.act.e.det, aes(x="Detailed" ,y = mean, ymax = max, ymin = min), size=1, color = "#08519c") + 
#     geom_pointrange(data = t1.act.e.com, aes(x="Compact"  ,y = mean, ymax = max, ymin = min), size=1, color = "#08519c") + 
#     scale_y_continuous(expand = c(0, 0), limits = c(0,15)) +
#     coord_flip() + 
#     theme_void()
# 
# last_plot()
# ggsave("s1.easy.actions.png", width=6, height=2, dpi=300, bg = "transparent")
# 
# ggplot(t1.act.m.int, aes(x = c("intuitive","detailed", "compact"), y = mean)) + 
#     geom_pointrange(data = t1.act.m.int, aes(x="Intuitive",y = mean, ymax = max, ymin = min), size=1, color = "#08519c") + 
#     geom_pointrange(data = t1.act.m.det, aes(x="Detailed" ,y = mean, ymax = max, ymin = min), size=1, color = "#08519c") + 
#     geom_pointrange(data = t1.act.m.com, aes(x="Compact"  ,y = mean, ymax = max, ymin = min), size=1, color = "#08519c") + 
#     scale_y_continuous(expand = c(0, 0), limits = c(0,15)) +
#     coord_flip() + 
#     theme_void()
# 
# last_plot()
# ggsave("s1.medium.actions.png", width=6, height=2, dpi=300, bg = "transparent")
# 
# ggplot(t1.act.h.int, aes(x = c("intuitive","detailed", "compact"), y = mean)) + 
#     geom_pointrange(data = t1.act.h.int, aes(x="Intuitive",y = mean, ymax = max, ymin = min), size=1, color = "#08519c") + 
#     geom_pointrange(data = t1.act.h.det, aes(x="Detailed" ,y = mean, ymax = max, ymin = min), size=1, color = "#08519c") + 
#     geom_pointrange(data = t1.act.h.com, aes(x="Compact"  ,y = mean, ymax = max, ymin = min), size=1, color = "#08519c") + 
#     scale_y_continuous(expand = c(0, 0), limits = c(0,15)) +
#     coord_flip() + 
#     theme_void()
# 
# last_plot()
# ggsave("s1.hard.actions.png", width=6, height=2, dpi=300, bg = "transparent")
# 
# 
# ggplot(t1.time.e.int, aes(x = c("intuitive","detailed", "compact"), y = mean)) + 
#     geom_pointrange(data = t1.time.e.int, aes(x="Intuitive",y = mean, ymax = max, ymin = min), size=1, color = "#08519c") + 
#     geom_pointrange(data = t1.time.e.det, aes(x="Detailed" ,y = mean, ymax = max, ymin = min), size=1, color = "#08519c") + 
#     geom_pointrange(data = t1.time.e.com, aes(x="Compact"  ,y = mean, ymax = max, ymin = min), size=1, color = "#08519c") + 
#     scale_y_continuous(expand = c(0, 0), limits = c(0,80)) +
#     coord_flip() + 
#     theme_void()
# 
# last_plot()
# ggsave("s1.easy.time.png", width=6, height=2, dpi=300, bg = "transparent")
# 
# ggplot(t1.time.m.int, aes(x = c("intuitive","detailed", "compact"), y = mean)) + 
#     geom_pointrange(data = t1.time.m.int, aes(x="Intuitive",y = mean, ymax = max, ymin = min), size=1, color = "#08519c") + 
#     geom_pointrange(data = t1.time.m.det, aes(x="Detailed" ,y = mean, ymax = 80, ymin = min), size=1, color = "#08519c") + 
#     geom_pointrange(data = t1.time.m.com, aes(x="Compact"  ,y = mean, ymax = max, ymin = min), size=1, color = "#08519c") + 
#     scale_y_continuous(expand = c(0, 0), limits = c(0,80)) +
#     coord_flip() + 
#     theme_void()
# 
# last_plot()
# ggsave("s1.medium.time.png", width=6, height=2, dpi=300, bg = "transparent")
# 
# ggplot(t1.time.h.int, aes(x = c("intuitive","detailed", "compact"), y = mean)) + 
#     geom_pointrange(data = t1.time.h.int, aes(x="Intuitive",y = mean, ymax = max, ymin = min), size=1, color = "#08519c") + 
#     geom_pointrange(data = t1.time.h.det, aes(x="Detailed" ,y = mean, ymax = max, ymin = min), size=1, color = "#08519c") + 
#     geom_pointrange(data = t1.time.h.com, aes(x="Compact"  ,y = mean, ymax = max, ymin = min), size=1, color = "#08519c") + 
#     scale_y_continuous(expand = c(0, 0), limits = c(0,80)) +
#     coord_flip() + 
#     theme_void()
# 
# last_plot()
# ggsave("s1.hard.time.png", width=6, height=2, dpi=300, bg = "transparent")
# 
# 
# 
# 
# 
# 
# ggplot(t2.acc.e.int, aes(x = c("intuitive","detailed", "compact"), y = mean)) + 
#     geom_pointrange(data = t2.acc.e.int, aes(x="Intuitive",y = mean, ymax = max, ymin = min), size=1, color = "#08519c") + 
#     geom_pointrange(data = t2.acc.e.det, aes(x="Detailed" ,y = mean, ymax = max, ymin = min), size=1, color = "#08519c") + 
#     geom_pointrange(data = t2.acc.e.com, aes(x="Compact"  ,y = mean, ymax = max, ymin = min), size=1, color = "#08519c") + 
#     scale_y_continuous(expand = c(0, 0), limits = c(0,1)) +
#     coord_flip() + 
#     theme_void()
# 
# last_plot()
# ggsave("s2.easy.accuracy.png", width=6, height=2, dpi=300, bg = "transparent")
# 
# ggplot(t2.acc.m.int, aes(x = c("intuitive","detailed", "compact"), y = mean)) + 
#     geom_pointrange(data = t2.acc.m.int, aes(x="Intuitive",y = mean, ymax = max, ymin = min), size=1, color = "#08519c") + 
#     geom_pointrange(data = t2.acc.m.det, aes(x="Detailed" ,y = mean, ymax = max, ymin = min), size=1, color = "#08519c") + 
#     geom_pointrange(data = t2.acc.m.com, aes(x="Compact"  ,y = mean, ymax = max, ymin = min), size=1, color = "#08519c") + 
#     scale_y_continuous(expand = c(0, 0), limits = c(0,1)) +
#     coord_flip() + 
#     theme_void()
# 
# last_plot()
# ggsave("s2.medium.accuracy.png", width=6, height=2, dpi=300, bg = "transparent")
# 
# ggplot(t2.acc.h.int, aes(x = c("intuitive","detailed", "compact"), y = mean)) + 
#     geom_pointrange(data = t2.acc.h.int, aes(x="Intuitive",y = mean, ymax = max, ymin = min), size=1, color = "#08519c") + 
#     geom_pointrange(data = t2.acc.h.det, aes(x="Detailed" ,y = mean, ymax = max, ymin = min), size=1, color = "#08519c") + 
#     geom_pointrange(data = t2.acc.h.com, aes(x="Compact"  ,y = mean, ymax = max, ymin = min), size=1, color = "#08519c") + 
#     scale_y_continuous(expand = c(0, 0), limits = c(0,1)) +
#     coord_flip() + 
#     theme_void()
# 
# last_plot()
# ggsave("s2.hard.accuracy.png", width=6, height=2, dpi=300, bg = "transparent")
# 
# ggplot(t2.act.e.int, aes(x = c("intuitive","detailed", "compact"), y = mean)) + 
#     geom_pointrange(data = t2.act.e.int, aes(x="Intuitive",y = mean, ymax = max, ymin = min), size=1, color = "#08519c") + 
#     geom_pointrange(data = t2.act.e.det, aes(x="Detailed" ,y = mean, ymax = max, ymin = min), size=1, color = "#08519c") + 
#     geom_pointrange(data = t2.act.e.com, aes(x="Compact"  ,y = mean, ymax = max, ymin = min), size=1, color = "#08519c") + 
#     scale_y_continuous(expand = c(0, 0), limits = c(0,15)) +
#     coord_flip() + 
#     theme_void()
# 
# last_plot()
# ggsave("s2.easy.actions.png", width=6, height=2, dpi=300, bg = "transparent")
# 
# ggplot(t2.act.m.int, aes(x = c("intuitive","detailed", "compact"), y = mean)) + 
#     geom_pointrange(data = t2.act.m.int, aes(x="Intuitive",y = mean, ymax = max, ymin = min), size=1, color = "#08519c") + 
#     geom_pointrange(data = t2.act.m.det, aes(x="Detailed" ,y = mean, ymax = max, ymin = min), size=1, color = "#08519c") + 
#     geom_pointrange(data = t2.act.m.com, aes(x="Compact"  ,y = mean, ymax = max, ymin = min), size=1, color = "#08519c") + 
#     scale_y_continuous(expand = c(0, 0), limits = c(0,15)) +
#     coord_flip() + 
#     theme_void()
# 
# last_plot()
# ggsave("s2.medium.actions.png", width=6, height=2, dpi=300, bg = "transparent")
# 
# ggplot(t2.act.h.int, aes(x = c("intuitive","detailed", "compact"), y = mean)) + 
#     geom_pointrange(data = t2.act.h.int, aes(x="Intuitive",y = mean, ymax = max, ymin = min), size=1, color = "#08519c") + 
#     geom_pointrange(data = t2.act.h.det, aes(x="Detailed" ,y = mean, ymax = max, ymin = min), size=1, color = "#08519c") + 
#     geom_pointrange(data = t2.act.h.com, aes(x="Compact"  ,y = mean, ymax = max, ymin = min), size=1, color = "#08519c") + 
#     scale_y_continuous(expand = c(0, 0), limits = c(0,15)) +
#     coord_flip() + 
#     theme_void()
# 
# last_plot()
# ggsave("s2.hard.actions.png", width=6, height=2, dpi=300, bg = "transparent")
# 
# 
# ggplot(t2.time.e.int, aes(x = c("intuitive","detailed", "compact"), y = mean)) + 
#     geom_pointrange(data = t2.time.e.int, aes(x="Intuitive",y = mean, ymax = max, ymin = min), size=1, color = "#08519c") + 
#     geom_pointrange(data = t2.time.e.det, aes(x="Detailed" ,y = mean, ymax = max, ymin = min), size=1, color = "#08519c") + 
#     geom_pointrange(data = t2.time.e.com, aes(x="Compact"  ,y = mean, ymax = max, ymin = min), size=1, color = "#08519c") + 
#     scale_y_continuous(expand = c(0, 0), limits = c(0,80)) +
#     coord_flip()
#     theme_void()
# 
# last_plot()
# ggsave("s2.easy.time.png", width=6, height=2, dpi=300, bg = "transparent")
# 
# ggplot(t2.time.m.int, aes(x = c("intuitive","detailed", "compact"), y = mean)) + 
#     geom_pointrange(data = t2.time.m.int, aes(x="Intuitive",y = mean, ymax = max, ymin = min), size=1, color = "#08519c") + 
#     geom_pointrange(data = t2.time.m.det, aes(x="Detailed" ,y = mean, ymax = max, ymin = min), size=1, color = "#08519c") + 
#     geom_pointrange(data = t2.time.m.com, aes(x="Compact"  ,y = mean, ymax = max, ymin = min), size=1, color = "#08519c") + 
#     scale_y_continuous(expand = c(0, 0), limits = c(0,80)) +
#     coord_flip() + 
#     theme_void()
# 
# last_plot()
# ggsave("s2.medium.time.png", width=6, height=2, dpi=300, bg = "transparent")
# 
# ggplot(t2.time.h.int, aes(x = c("intuitive","detailed", "compact"), y = mean)) + 
#     geom_pointrange(data = t2.time.h.int, aes(x="Intuitive",y = mean, ymax = max, ymin = min), size=1, color = "#08519c") + 
#     geom_pointrange(data = t2.time.h.det, aes(x="Detailed" ,y = mean, ymax = max, ymin = min), size=1, color = "#08519c") + 
#     geom_pointrange(data = t2.time.h.com, aes(x="Compact"  ,y = mean, ymax = max, ymin = min), size=1, color = "#08519c") + 
#     scale_y_continuous(expand = c(0, 0), limits = c(0,80)) +
#     coord_flip() + 
#     theme_void()
# 
# last_plot()
# ggsave("s2.hard.time.png", width=6, height=2, dpi=300, bg = "transparent")
# 
# 
