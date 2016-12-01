library(Hmisc)
require(ggplot2)

#### First Study
s1.qf.charts    <- read.csv("data/qf1_charts.csv")
s1.qf.dots      <- read.csv("data/qf1_dots.csv")
s1.qf.series    <- read.csv("data/qf1_maps.csv")

s1.compact.easy    <- read.csv("data/likert_s1/compact_easy.csv"  )
s1.compact.medium  <- read.csv("data/likert_s1/compact_hard.csv"  )
s1.compact.hard    <- read.csv("data/likert_s1/compact_medium.csv")
s1.detailed.easy   <- read.csv("data/likert_s1/detailed_easy.csv" )
s1.detailed.medium <- read.csv("data/likert_s1/detailed_hard.csv" )
s1.detailed.hard   <- read.csv("data/likert_s1/detailed_medium.csv" )
s1.intuitive.easy  <- read.csv("data/likert_s1/intuitive_easy.csv"  )
s1.intuitive.medium<- read.csv("data/likert_s1/intuitive_hard.csv"  )
s1.intuitive.hard  <- read.csv("data/likert_s1/intuitive_medium.csv")
s1.all             <- read.csv("data/likert_s1/study1All.csv")
s1.intuitive       <- read.csv("data/likert_s1/s1_compact.csv")
s1.detailed        <- read.csv("data/likert_s1/s1_detailed.csv")
s1.compact         <- read.csv("data/likert_s1/s1_intuitive.csv")


#### Second Study
s2.qf.charts    <- read.csv("data/qf2_charts.csv")
s2.qf.dots      <- read.csv("data/qf2_dots.csv")
s2.qf.series    <- read.csv("data/qf2_series.csv")

s2.compact.easy    <- read.csv("data/likert_s2/compact_easy.csv"  )
s2.compact.medium  <- read.csv("data/likert_s2/compact_hard.csv"  )
s2.compact.hard    <- read.csv("data/likert_s2/compact_medium.csv")
s2.detailed.easy   <- read.csv("data/likert_s2/detailed_easy.csv" )
s2.detailed.medium <- read.csv("data/likert_s2/detailed_hard.csv" )
s2.detailed.hard   <- read.csv("data/likert_s2/detailed_medium.csv" )
s2.intuitive.easy  <- read.csv("data/likert_s2/intuitive_easy.csv"  )
s2.intuitive.medium<- read.csv("data/likert_s2/intuitive_hard.csv"  )
s2.intuitive.hard  <- read.csv("data/likert_s2/intuitive_medium.csv")
s2.all             <- read.csv("data/likert_s2/study2All.csv")
s2.intuitive       <- read.csv("data/likert_s2/s2_compact.csv")
s2.detailed        <- read.csv("data/likert_s2/s2_detailed.csv")
s2.compact         <- read.csv("data/likert_s2/s2_intuitive.csv")

s1.compact.ease   <- head(s1.qf.dots$vq1, 75)
s1.compact.appeal <- head(s1.qf.dots$vq3, 75)
s1.compact.expect <- head(s1.qf.dots$vq15,75)
s1.compact.trust  <- head(s1.qf.dots$vq6, 75)
s1.compact.conf   <- head(s1.qf.dots$vq8, 75)
s1.compact.cred   <- head(s1.qf.dots$vq7, 75)

s2.compact.ease   <- head(s2.qf.dots$vq1, 75)
s2.compact.appeal <- head(s2.qf.dots$vq3, 75)
s2.compact.expect <- head(s2.qf.dots$vq15,75)
s2.compact.trust  <- head(s2.qf.dots$vq6, 75)
s2.compact.conf   <- head(s2.qf.dots$vq8, 75)
s2.compact.cred   <- head(s2.qf.dots$vq7, 75)

s1.detailed.ease   <- head(s1.qf.charts$vq1, 75)
s1.detailed.appeal <- head(s1.qf.charts$vq3, 75)
s1.detailed.expect <- head(s1.qf.charts$vq15,75)
s1.detailed.trust  <- head(s1.qf.charts$vq6, 75)
s1.detailed.conf   <- head(s1.qf.charts$vq8, 75)
s1.detailed.cred   <- head(s1.qf.charts$vq7, 75)

s2.detailed.ease   <- head(s2.qf.charts$vq1, 75)
s2.detailed.appeal <- head(s2.qf.charts$vq3, 75)
s2.detailed.expect <- head(s2.qf.charts$vq15,75)
s2.detailed.trust  <- head(s2.qf.charts$vq6, 75)
s2.detailed.conf   <- head(s2.qf.charts$vq8, 75)
s2.detailed.cred   <- head(s2.qf.charts$vq7, 75)

s1.intuitive.ease   <- head(s1.qf.series$vq1, 75)
s1.intuitive.appeal <- head(s1.qf.series$vq3, 75)
s1.intuitive.expect <- head(s1.qf.series$vq15,75)
s1.intuitive.trust  <- head(s1.qf.series$vq6, 75)
s1.intuitive.conf   <- head(s1.qf.series$vq8, 75)
s1.intuitive.cred   <- head(s1.qf.series$vq7, 75)

s2.intuitive.ease   <- head(s2.qf.series$vq1, 75)
s2.intuitive.appeal <- head(s2.qf.series$vq3, 75)
s2.intuitive.expect <- head(s2.qf.series$vq15,75)
s2.intuitive.trust  <- head(s2.qf.series$vq6, 75)
s2.intuitive.conf   <- head(s2.qf.series$vq8, 75)
s2.intuitive.cred   <- head(s2.qf.series$vq7, 75)

df.ease <- data.frame(
    s1.intuitive.ease,  
    s2.intuitive.ease,  
    s1.detailed.ease,  
    s2.detailed.ease,  
    s1.compact.ease,  
    s2.compact.ease)

df.appeal <- data.frame(
    s1.intuitive.appeal,
    s2.intuitive.appeal,
    s1.detailed.appeal,
    s2.detailed.appeal,
    s1.compact.appeal,
    s2.compact.appeal)

df.expect <- data.frame(
    s1.intuitive.expect,
    s2.intuitive.expect,
    s1.detailed.expect,
    s2.detailed.expect,
    s1.compact.expect,
    s2.compact.expect)

df.trust <- data.frame(
    s1.intuitive.trust, 
    s2.intuitive.trust, 
    s1.detailed.trust, 
    s2.detailed.trust,
    s1.compact.trust, 
    s2.compact.trust)

df.conf <- data.frame(
    s1.intuitive.conf,  
    s2.intuitive.conf,  
    s1.detailed.conf,  
    s2.detailed.conf, 
    s1.compact.conf,  
    s2.compact.conf)  
    
df.cred <- data.frame(
    s1.intuitive.cred,
    s2.intuitive.cred,
    s1.detailed.cred,
    s2.detailed.cred,
    s1.compact.cred,
    s2.compact.cred)

#ggplot(stack(df.ease),   aes(x = ind, y = values)) + geom_boxplot() + stat_summary(fun.y=mean, geom="point", shape="+", size=7)
#ggplot(stack(df.appeal), aes(x = ind, y = values)) + geom_boxplot() + stat_summary(fun.y=mean, geom="point", shape="+", size=7)
#ggplot(stack(df.expect), aes(x = ind, y = values)) + geom_boxplot() + stat_summary(fun.y=mean, geom="point", shape="+", size=7)
#ggplot(stack(df.trust),  aes(x = ind, y = values)) + geom_boxplot() + stat_summary(fun.y=mean, geom="point", shape="+", size=7)
#ggplot(stack(df.conf),   aes(x = ind, y = values)) + geom_boxplot() + stat_summary(fun.y=mean, geom="point", shape="+", size=7)
#ggplot(stack(df.cred),   aes(x = ind, y = values)) + geom_boxplot() + stat_summary(fun.y=mean, geom="point", shape="+", size=7)

s1.variance.intuitive <- mean(c(s1.intuitive.easy$ls.accuracy, s1.intuitive.medium$ls.accuracy, s1.intuitive.hard$ls.accuracy))
s1.variance.detailed  <- mean(c(s1.detailed.easy$ls.accuracy, s1.detailed.medium$ls.accuracy, s1.detailed.hard$ls.accuracy))
s1.variance.compact   <- mean(c(s1.compact.easy$ls.accuracy, s1.compact.medium$ls.accuracy, s1.compact.hard$ls.accuracy))

# Using the variance:
#ggplot(s1.intuitive.easy,   aes(x=trust, y=ls.accuracy-s1.variance.intuitive)) + geom_point(shape=16, size=3, alpha=1/5) + geom_smooth(method=lm) + ylab("intuitive.easy  ")
#ggplot(s1.intuitive.medium, aes(x=trust, y=ls.accuracy-s1.variance.intuitive)) + geom_point(shape=16, size=3, alpha=1/5) + geom_smooth(method=lm) + ylab("intuitive.medium")
#ggplot(s1.intuitive.hard,   aes(x=trust, y=ls.accuracy-s1.variance.intuitive)) + geom_point(shape=16, size=3, alpha=1/5) + geom_smooth(method=lm) + ylab("intuitive.hard  ")
#ggplot(s1.detailed.easy,    aes(x=trust, y=ls.accuracy-s1.variance.detailed))  + geom_point(shape=16, size=3, alpha=1/5) + geom_smooth(method=lm) + ylab("detailed.easy   ")
#ggplot(s1.detailed.medium,  aes(x=trust, y=ls.accuracy-s1.variance.detailed))  + geom_point(shape=16, size=3, alpha=1/5) + geom_smooth(method=lm) + ylab("detailed.medium ")
#ggplot(s1.detailed.hard,    aes(x=trust, y=ls.accuracy-s1.variance.detailed))  + geom_point(shape=16, size=3, alpha=1/5) + geom_smooth(method=lm) + ylab("detailed.hard   ")
#ggplot(s1.compact.easy,     aes(x=trust, y=ls.accuracy-s1.variance.compact))   + geom_point(shape=16, size=3, alpha=1/5) + geom_smooth(method=lm) + ylab("compact.easy    ")
#ggplot(s1.compact.medium,   aes(x=trust, y=ls.accuracy-s1.variance.compact))   + geom_point(shape=16, size=3, alpha=1/5) + geom_smooth(method=lm) + ylab("compact.medium  ")
#ggplot(s1.compact.hard,     aes(x=trust, y=ls.accuracy-s1.variance.compact))   + geom_point(shape=16, size=3, alpha=1/5) + geom_smooth(method=lm) + ylab("compact.hard    ")

# Using just the accuracy:
ggplot(s1.intuitive.easy,   aes(x=trust, y=ls.accuracy)) + geom_point(shape=16, size=3, alpha=1/5) + geom_smooth(method=lm) + ylab("intuitive.easy  ")
ggplot(s1.intuitive.medium, aes(x=trust, y=ls.accuracy)) + geom_point(shape=16, size=3, alpha=1/5) + geom_smooth(method=lm) + ylab("intuitive.medium")
ggplot(s1.intuitive.hard,   aes(x=trust, y=ls.accuracy)) + geom_point(shape=16, size=3, alpha=1/5) + geom_smooth(method=lm) + ylab("intuitive.hard  ")
ggplot(s1.detailed.easy,    aes(x=trust, y=ls.accuracy)) + geom_point(shape=16, size=3, alpha=1/5) + geom_smooth(method=lm) + ylab("detailed.easy   ")
ggplot(s1.detailed.medium,  aes(x=trust, y=ls.accuracy)) + geom_point(shape=16, size=3, alpha=1/5) + geom_smooth(method=lm) + ylab("detailed.medium ")
ggplot(s1.detailed.hard,    aes(x=trust, y=ls.accuracy)) + geom_point(shape=16, size=3, alpha=1/5) + geom_smooth(method=lm) + ylab("detailed.hard   ")
ggplot(s1.compact.easy,     aes(x=trust, y=ls.accuracy)) + geom_point(shape=16, size=3, alpha=1/5) + geom_smooth(method=lm) + ylab("compact.easy    ")
ggplot(s1.compact.medium,   aes(x=trust, y=ls.accuracy)) + geom_point(shape=16, size=3, alpha=1/5) + geom_smooth(method=lm) + ylab("compact.medium  ")
ggplot(s1.compact.hard,     aes(x=trust, y=ls.accuracy)) + geom_point(shape=16, size=3, alpha=1/5) + geom_smooth(method=lm) + ylab("compact.hard    ")
ggplot(s1.all,              aes(x=trust, y=ls.accuracy)) + geom_point(shape=16, size=3, alpha=1/5) + geom_smooth(method=lm) + ylab("s1.all          ")
ggplot(s1.intuitive,        aes(x=trust, y=ls.accuracy)) + geom_point(shape=16, size=3, alpha=1/5) + geom_smooth(method=lm) + ylab("s1.intuitive    ")
ggplot(s1.detailed,         aes(x=trust, y=ls.accuracy)) + geom_point(shape=16, size=3, alpha=1/5) + geom_smooth(method=lm) + ylab("s1.detailed     ")

ggplot(s1.compact,          aes(x=appeal, y=ls.accuracy)) + geom_point(shape=16, size=3, alpha=1/5) + geom_smooth(method=lm) + ylab("s1.compact      ")
ggplot(s1.intuitive.hard,   aes(x=appeal, y=ls.accuracy)) + geom_point(shape=16, size=3, alpha=1/5) + geom_smooth(method=lm) + ylab("intuitive.hard  ")
ggplot(s1.intuitive.hard,   aes(x=expect, y=ls.accuracy)) + geom_point(shape=16, size=3, alpha=1/5) + geom_smooth(method=lm) + ylab("intuitive.hard  ")
ggplot(s1.detailed.easy,    aes(x=trust,  y=ls.accuracy)) + geom_point(shape=16, size=3, alpha=1/5) + geom_smooth(method=lm) + ylab("s1.detailed.easy")

rcorr(as.matrix(subset(s1.all,             select = c(ls.speed, ls.actions, ls.accuracy, ease, understand, appeal, engage, likeit, trust, credible, confidence, operation, suitable, importance, comprehensible, easytasks, opacity, expect))), type="spearman") 
rcorr(as.matrix(subset(s1.intuitive,       select = c(ls.speed, ls.actions, ls.accuracy, ease, understand, appeal, engage, likeit, trust, credible, confidence, operation, suitable, importance, comprehensible, easytasks, opacity, expect))), type="spearman") 
rcorr(as.matrix(subset(s1.detailed,        select = c(ls.speed, ls.actions, ls.accuracy, ease, understand, appeal, engage, likeit, trust, credible, confidence, operation, suitable, importance, comprehensible, easytasks, opacity, expect))), type="spearman") 
rcorr(as.matrix(subset(s1.compact,         select = c(ls.speed, ls.actions, ls.accuracy, ease, understand, appeal, engage, likeit, trust, credible, confidence, operation, suitable, importance, comprehensible, easytasks, opacity, expect))), type="spearman") 
rcorr(as.matrix(subset(s1.intuitive.easy,  select = c(ls.speed, ls.actions, ls.accuracy, ease, understand, appeal, engage, likeit, trust, credible, confidence, operation, suitable, importance, comprehensible, easytasks, opacity, expect))), type="spearman") 
rcorr(as.matrix(subset(s1.intuitive.medium,select = c(ls.speed, ls.actions, ls.accuracy, ease, understand, appeal, engage, likeit, trust, credible, confidence, operation, suitable, importance, comprehensible, easytasks, opacity, expect))), type="spearman") 
rcorr(as.matrix(subset(s1.intuitive.hard,  select = c(ls.speed, ls.actions, ls.accuracy, ease, understand, appeal, engage, likeit, trust, credible, confidence, operation, suitable, importance, comprehensible, easytasks, opacity, expect))), type="spearman") 
rcorr(as.matrix(subset(s1.detailed.easy,   select = c(ls.speed, ls.actions, ls.accuracy, ease, understand, appeal, engage, likeit, trust, credible, confidence, operation, suitable, importance, comprehensible, easytasks, opacity, expect))), type="spearman") 
rcorr(as.matrix(subset(s1.detailed.medium, select = c(ls.speed, ls.actions, ls.accuracy, ease, understand, appeal, engage, likeit, trust, credible, confidence, operation, suitable, importance, comprehensible, easytasks, opacity, expect))), type="spearman") 
rcorr(as.matrix(subset(s1.detailed.hard,   select = c(ls.speed, ls.actions, ls.accuracy, ease, understand, appeal, engage, likeit, trust, credible, confidence, operation, suitable, importance, comprehensible, easytasks, opacity, expect))), type="spearman") 
rcorr(as.matrix(subset(s1.compact.easy,    select = c(ls.speed, ls.actions, ls.accuracy, ease, understand, appeal, engage, likeit, trust, credible, confidence, operation, suitable, importance, comprehensible, easytasks, opacity, expect))), type="spearman") 
rcorr(as.matrix(subset(s1.compact.medium,  select = c(ls.speed, ls.actions, ls.accuracy, ease, understand, appeal, engage, likeit, trust, credible, confidence, operation, suitable, importance, comprehensible, easytasks, opacity, expect))), type="spearman") 
rcorr(as.matrix(subset(s1.compact.hard,    select = c(ls.speed, ls.actions, ls.accuracy, ease, understand, appeal, engage, likeit, trust, credible, confidence, operation, suitable, importance, comprehensible, easytasks, opacity, expect))), type="spearman") 

rcorr(as.matrix(subset(s2.all,             select = c(ls.speed, ls.actions, ls.accuracy,  ease, appeal, expect, trust, confidence, credible))), type="spearman") 
rcorr(as.matrix(subset(s2.intuitive,       select = c(ls.speed, ls.actions, ls.accuracy,  ease, appeal, expect, trust, confidence, credible))), type="spearman") 
rcorr(as.matrix(subset(s2.detailed,        select = c(ls.speed, ls.actions, ls.accuracy,  ease, appeal, expect, trust, confidence, credible))), type="spearman") 
rcorr(as.matrix(subset(s2.compact,         select = c(ls.speed, ls.actions, ls.accuracy,  ease, appeal, expect, trust, confidence, credible))), type="spearman") 
rcorr(as.matrix(subset(s2.intuitive.easy,  select = c(ls.speed, ls.actions, ls.accuracy,  ease, appeal, expect, trust, confidence, credible))), type="spearman") 
rcorr(as.matrix(subset(s2.intuitive.medium,select = c(ls.speed, ls.actions, ls.accuracy,  ease, appeal, expect, trust, confidence, credible))), type="spearman") 
rcorr(as.matrix(subset(s2.intuitive.hard,  select = c(ls.speed, ls.actions, ls.accuracy,  ease, appeal, expect, trust, confidence, credible))), type="spearman") 
rcorr(as.matrix(subset(s2.detailed.easy,   select = c(ls.speed, ls.actions, ls.accuracy,  ease, appeal, expect, trust, confidence, credible))), type="spearman") 
rcorr(as.matrix(subset(s2.detailed.medium, select = c(ls.speed, ls.actions, ls.accuracy,  ease, appeal, expect, trust, confidence, credible))), type="spearman") 
rcorr(as.matrix(subset(s2.detailed.hard,   select = c(ls.speed, ls.actions, ls.accuracy,  ease, appeal, expect, trust, confidence, credible))), type="spearman") 
rcorr(as.matrix(subset(s2.compact.easy,    select = c(ls.speed, ls.actions, ls.accuracy,  ease, appeal, expect, trust, confidence, credible))), type="spearman") 
rcorr(as.matrix(subset(s2.compact.medium,  select = c(ls.speed, ls.actions, ls.accuracy,  ease, appeal, expect, trust, confidence, credible))), type="spearman") 
rcorr(as.matrix(subset(s2.compact.hard,    select = c(ls.speed, ls.actions, ls.accuracy,  ease, appeal, expect, trust, confidence, credible))), type="spearman") 






