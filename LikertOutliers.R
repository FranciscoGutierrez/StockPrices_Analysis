require(plyr)
require(devtools)

s1.intuitive  <- head(read.csv("data/likert_outliers/s1.intuitive.outliers.csv"),43)
s1.detailed   <- head(read.csv("data/likert_outliers/s1.detailed.outliers.csv" ),43)
s1.compact    <- head(read.csv("data/likert_outliers/s1.compact.outliers.csv"  ),43)

s2.intuitive  <- head(read.csv("data/likert_outliers/s2.intuitive.outliers.csv"),18)
s2.detailed   <- head(read.csv("data/likert_outliers/s2.detailed.outliers.csv" ),18)
s2.compact    <- head(read.csv("data/likert_outliers/s2.compact.outliers.csv"  ),18)


s1.ease            <- data.frame(s1.intuitive$ease          , s1.detailed$ease          , s1.compact$ease          )
s1.understand      <- data.frame(s1.intuitive$understand    , s1.detailed$understand    , s1.compact$understand    )
s1.appeal          <- data.frame(s1.intuitive$appeal        , s1.detailed$appeal        , s1.compact$appeal        )
s1.engage          <- data.frame(s1.intuitive$engage        , s1.detailed$engage        , s1.compact$engage        )
s1.likeit          <- data.frame(s1.intuitive$likeit        , s1.detailed$likeit        , s1.compact$likeit        )
s1.trust           <- data.frame(s1.intuitive$trust         , s1.detailed$trust         , s1.compact$trust         )
s1.credible        <- data.frame(s1.intuitive$credible      , s1.detailed$credible      , s1.compact$credible      )
s1.confidence      <- data.frame(s1.intuitive$confidence    , s1.detailed$confidence    , s1.compact$confidence    )
s1.operation       <- data.frame(s1.intuitive$operation     , s1.detailed$operation     , s1.compact$operation     )
s1.suitable        <- data.frame(s1.intuitive$suitable      , s1.detailed$suitable      , s1.compact$suitable      )
s1.importance      <- data.frame(s1.intuitive$importance    , s1.detailed$importance    , s1.compact$importance    )
s1.comprehensible  <- data.frame(s1.intuitive$comprehensible, s1.detailed$comprehensible, s1.compact$comprehensible)
s1.easytasks       <- data.frame(s1.intuitive$easytasks     , s1.detailed$easytasks     , s1.compact$easytasks     )
s1.opacity         <- data.frame(s1.intuitive$opacity       , s1.detailed$opacity       , s1.compact$opacity       )
s1.expect          <- data.frame(s1.intuitive$expect        , s1.detailed$expect        , s1.compact$expect        )

s2.ease            <- data.frame(s2.intuitive$ease          , s2.detailed$ease          , s2.compact$ease          )
s2.understand      <- data.frame(s2.intuitive$understand    , s2.detailed$understand    , s2.compact$understand    )
s2.appeal          <- data.frame(s2.intuitive$appeal        , s2.detailed$appeal        , s2.compact$appeal        )
s2.engage          <- data.frame(s2.intuitive$engage        , s2.detailed$engage        , s2.compact$engage        )
s2.likeit          <- data.frame(s2.intuitive$likeit        , s2.detailed$likeit        , s2.compact$likeit        )
s2.trust           <- data.frame(s2.intuitive$trust         , s2.detailed$trust         , s2.compact$trust         )
s2.credible        <- data.frame(s2.intuitive$credible      , s2.detailed$credible      , s2.compact$credible      )
s2.confidence      <- data.frame(s2.intuitive$confidence    , s2.detailed$confidence    , s2.compact$confidence    )
s2.operation       <- data.frame(s2.intuitive$operation     , s2.detailed$operation     , s2.compact$operation     )
s2.suitable        <- data.frame(s2.intuitive$suitable      , s2.detailed$suitable      , s2.compact$suitable      )
s2.importance      <- data.frame(s2.intuitive$importance    , s2.detailed$importance    , s2.compact$importance    )
s2.comprehensible  <- data.frame(s2.intuitive$comprehensible, s2.detailed$comprehensible, s2.compact$comprehensible)
s2.easytasks       <- data.frame(s2.intuitive$easytasks     , s2.detailed$easytasks     , s2.compact$easytasks     )
s2.opacity         <- data.frame(s2.intuitive$opacity       , s2.detailed$opacity       , s2.compact$opacity       )
s2.expect          <- data.frame(s2.intuitive$expect        , s2.detailed$expect        , s2.compact$expect        )

kruskal.test(stack(s1.ease          )$values, stack(s1.ease          )$ind)
kruskal.test(stack(s1.understand    )$values, stack(s1.understand    )$ind)
kruskal.test(stack(s1.appeal        )$values, stack(s1.appeal        )$ind)
kruskal.test(stack(s1.engage        )$values, stack(s1.engage        )$ind)
kruskal.test(stack(s1.likeit        )$values, stack(s1.likeit        )$ind)
kruskal.test(stack(s1.trust         )$values, stack(s1.trust         )$ind)
kruskal.test(stack(s1.credible      )$values, stack(s1.credible      )$ind)
kruskal.test(stack(s1.confidence    )$values, stack(s1.confidence    )$ind)
kruskal.test(stack(s1.operation     )$values, stack(s1.operation     )$ind)
kruskal.test(stack(s1.suitable      )$values, stack(s1.suitable      )$ind)
kruskal.test(stack(s1.importance    )$values, stack(s1.importance    )$ind)
kruskal.test(stack(s1.comprehensible)$values, stack(s1.comprehensible)$ind)
kruskal.test(stack(s1.easytasks     )$values, stack(s1.easytasks     )$ind)
kruskal.test(stack(s1.opacity       )$values, stack(s1.opacity       )$ind)
kruskal.test(stack(s1.expect        )$values, stack(s1.expect        )$ind)


kw.ease          <- kruskal.test(stack(s2.ease          )$values, stack(s2.ease          )$ind)$p.value
kw.understand    <- kruskal.test(stack(s2.understand    )$values, stack(s2.understand    )$ind)$p.value
kw.appeal        <- kruskal.test(stack(s2.appeal        )$values, stack(s2.appeal        )$ind)$p.value
kw.engage        <- kruskal.test(stack(s2.engage        )$values, stack(s2.engage        )$ind)$p.value
kw.likeit        <- kruskal.test(stack(s2.likeit        )$values, stack(s2.likeit        )$ind)$p.value
kw.trust         <- kruskal.test(stack(s2.trust         )$values, stack(s2.trust         )$ind)$p.value
kw.credible      <- kruskal.test(stack(s2.credible      )$values, stack(s2.credible      )$ind)$p.value
kw.confidence    <- kruskal.test(stack(s2.confidence    )$values, stack(s2.confidence    )$ind)$p.value
kw.operation     <- kruskal.test(stack(s2.operation     )$values, stack(s2.operation     )$ind)$p.value
kw.suitable      <- kruskal.test(stack(s2.suitable      )$values, stack(s2.suitable      )$ind)$p.value
kw.importance    <- kruskal.test(stack(s2.importance    )$values, stack(s2.importance    )$ind)$p.value
kw.comprehensible<- kruskal.test(stack(s2.comprehensible)$values, stack(s2.comprehensible)$ind)$p.value
kw.easytasks     <- kruskal.test(stack(s2.easytasks     )$values, stack(s2.easytasks     )$ind)$p.value
kw.opacity       <- kruskal.test(stack(s2.opacity       )$values, stack(s2.opacity       )$ind)$p.value
kw.expect        <- kruskal.test(stack(s2.expect        )$values, stack(s2.expect        )$ind)$p.value

r <- data.frame(c(
kw.ease,
kw.understand    ,
kw.appeal        ,
kw.engage        ,
kw.likeit        ,
kw.trust         ,
kw.credible      ,
kw.confidence    ,
kw.operation     ,
kw.suitable      ,
kw.importance    ,
kw.comprehensible,
kw.easytasks     ,
kw.opacity       ,
kw.expect))       

wilcox.test(s1.confidence$s1.intuitive.confidence, s1.confidence$s1.detailed.confidence )
wilcox.test(s1.confidence$s1.detailed.confidence,  s1.confidence$s1.compact.confidence  )
wilcox.test(s1.confidence$s1.compact.confidence,   s1.confidence$s1.intuitive.confidence)
wilcox.test(s1.suitable$s1.intuitive.suitable,     s1.suitable$s1.detailed.suitable     )
wilcox.test(s1.suitable$s1.detailed.suitable,      s1.suitable$s1.compact.suitable      )
wilcox.test(s1.suitable$s1.compact.suitable,       s1.suitable$s1.intuitive.suitable    )
wilcox.test(s1.easytasks$s1.intuitive.easytasks,   s1.easytasks$s1.detailed.easytasks   )
wilcox.test(s1.easytasks$s1.detailed.easytasks,    s1.easytasks$s1.compact.easytasks    )
wilcox.test(s1.easytasks$s1.compact.easytasks,     s1.easytasks$s1.intuitive.easytasks  )


r <- c(
wilcox.test(s1.ease$s1.intuitive.ease,                     s1.ease$s1.detailed.ease                     )$p.value,
wilcox.test(s1.ease$s1.detailed.ease,                      s1.ease$s1.compact.ease                      )$p.value,
wilcox.test(s1.ease$s1.compact.ease,                       s1.ease$s1.intuitive.ease                    )$p.value,
wilcox.test(s1.understand$s1.intuitive.understand,         s1.understand$s1.detailed.understand         )$p.value,
wilcox.test(s1.understand$s1.detailed.understand,          s1.understand$s1.compact.understand          )$p.value,
wilcox.test(s1.understand$s1.compact.understand,           s1.understand$s1.intuitive.understand        )$p.value,
wilcox.test(s1.engage$s1.intuitive.engage,                 s1.engage$s1.detailed.engage                 )$p.value,
wilcox.test(s1.engage$s1.detailed.engage,                  s1.engage$s1.compact.engage                  )$p.value,
wilcox.test(s1.engage$s1.compact.engage,                   s1.engage$s1.intuitive.engage                )$p.value,
wilcox.test(s1.likeit$s1.intuitive.likeit,                 s1.likeit$s1.detailed.likeit                 )$p.value,
wilcox.test(s1.likeit$s1.detailed.likeit,                  s1.likeit$s1.compact.likeit                  )$p.value,
wilcox.test(s1.likeit$s1.compact.likeit,                   s1.likeit$s1.intuitive.likeit                )$p.value,
wilcox.test(s1.trust$s1.intuitive.trust,                   s1.trust$s1.detailed.trust                   )$p.value,
wilcox.test(s1.trust$s1.detailed.trust,                    s1.trust$s1.compact.trust                    )$p.value,
wilcox.test(s1.trust$s1.compact.trust,                     s1.trust$s1.intuitive.trust                  )$p.value,
wilcox.test(s1.confidence$s1.intuitive.confidence,         s1.confidence$s1.detailed.confidence         )$p.value,
wilcox.test(s1.confidence$s1.detailed.confidence,          s1.confidence$s1.compact.confidence          )$p.value,
wilcox.test(s1.confidence$s1.compact.confidence,           s1.confidence$s1.intuitive.confidence        )$p.value,
wilcox.test(s1.operation$s1.intuitive.operation,           s1.operation$s1.detailed.operation           )$p.value,
wilcox.test(s1.operation$s1.detailed.operation,            s1.operation$s1.compact.operation            )$p.value,
wilcox.test(s1.operation$s1.compact.operation,             s1.operation$s1.intuitive.operation          )$p.value,
wilcox.test(s1.suitable$s1.intuitive.suitable,             s1.suitable$s1.detailed.suitable             )$p.value,
wilcox.test(s1.suitable$s1.detailed.suitable,              s1.suitable$s1.compact.suitable              )$p.value,
wilcox.test(s1.suitable$s1.compact.suitable,               s1.suitable$s1.intuitive.suitable            )$p.value,
wilcox.test(s1.importance$s1.intuitive.importance,         s1.importance$s1.detailed.importance         )$p.value,
wilcox.test(s1.importance$s1.detailed.importance,          s1.importance$s1.compact.importance          )$p.value,
wilcox.test(s1.importance$s1.compact.importance,           s1.importance$s1.intuitive.importance        )$p.value,
wilcox.test(s1.comprehensible$s1.intuitive.comprehensible, s1.comprehensible$s1.detailed.comprehensible )$p.value,
wilcox.test(s1.comprehensible$s1.detailed.comprehensible,  s1.comprehensible$s1.compact.comprehensible  )$p.value,
wilcox.test(s1.comprehensible$s1.compact.comprehensible,   s1.comprehensible$s1.intuitive.comprehensible)$p.value,
wilcox.test(s1.easytasks$s1.intuitive.easytasks,           s1.easytasks$s1.detailed.easytasks           )$p.value,
wilcox.test(s1.easytasks$s1.detailed.easytasks,            s1.easytasks$s1.compact.easytasks            )$p.value,
wilcox.test(s1.easytasks$s1.compact.easytasks,             s1.easytasks$s1.intuitive.easytasks          )$p.value,
wilcox.test(s1.opacity$s1.intuitive.opacity,               s1.opacity$s1.detailed.opacity               )$p.value,
wilcox.test(s1.opacity$s1.detailed.opacity,                s1.opacity$s1.compact.opacity                )$p.value,
wilcox.test(s1.opacity$s1.compact.opacity,                 s1.opacity$s1.intuitive.opacity              )$p.value
)



cr <- c(
    wilcox.test(s1.understand$s1.intuitive.understand,         s1.understand$s1.detailed.understand         )$statistic,
    wilcox.test(s1.understand$s1.detailed.understand,          s1.understand$s1.compact.understand          )$statistic,
    wilcox.test(s1.understand$s1.compact.understand,           s1.understand$s1.intuitive.understand        )$statistic,
    wilcox.test(s1.engage$s1.intuitive.engage,                 s1.engage$s1.detailed.engage                 )$statistic,
    wilcox.test(s1.engage$s1.detailed.engage,                  s1.engage$s1.compact.engage                  )$statistic,
    wilcox.test(s1.engage$s1.compact.engage,                   s1.engage$s1.intuitive.engage                )$statistic,
    wilcox.test(s1.credible$s1.intuitive.credible,             s1.credible$s1.detailed.credible             )$statistic,
    wilcox.test(s1.credible$s1.detailed.credible,              s1.credible$s1.compact.credible              )$statistic,
    wilcox.test(s1.credible$s1.compact.credible,               s1.credible$s1.intuitive.credible            )$statistic,
    wilcox.test(s1.comprehensible$s1.intuitive.comprehensible, s1.comprehensible$s1.detailed.comprehensible )$statistic,
    wilcox.test(s1.comprehensible$s1.detailed.comprehensible,  s1.comprehensible$s1.compact.comprehensible  )$statistic,
    wilcox.test(s1.comprehensible$s1.compact.comprehensible,   s1.comprehensible$s1.intuitive.comprehensible)$statistic
)

