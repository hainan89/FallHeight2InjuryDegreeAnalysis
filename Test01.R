
rm(list = ls())

d <- read.csv("case_analysis_for_fall_height.csv")
d <- d[which(d$Degree != "NULL"), ]
d <- d[which(d$FallDist != 0), ]
d <- d[which(d$FallHt != 0), ]

d$FallDist <- (d$FallDist - min(d$FallDist)) / abs(max(d$FallDist) - min(d$FallDist))
d$FallHt <- (d$FallHt - min(d$FallHt)) / abs(max(d$FallHt) - min(d$FallHt))

# # 1 nnet
# library(nnet)
# fit <- nnet(Degree~ FallHt + FallDist, data = d, size = 3)
# 
# r <- predict(fit, data = d)
# 
# correct_record = 0
# for(i in seq(1, dim(r)[1])){
#     r.name <- names(which(r[i, ] == max(r[i, ])))
#     if(r.name == d[i, 1]){
#         correct_record = correct_record + 1
#     }
# }
# 
# correct_ratio <- correct_record / dim(r)[1]

# # 2 k-means
# library(stats)
# d <- cbind(d, DegreeVal = rep(0, dim(d)[1]))
# d[which(d$Degree == "Fatality"), 4] <- 3
# d[which(d$Degree == "Hospitalized injury"), 4] <- 2
# d[which(d$Degree == "Non Hospitalized injury"), 4] <- 1
# 
# d.frame <- data.frame(FallDist = d$FallDist, FallHt = d$FallHt, DegreeVal = d$DegreeVal)
# fit <- kmeans(d.frame, 3)
# 
# # table(d[which(fit$cluster == 1), 1])
# # table(d[which(fit$cluster == 2), 1])
# # table(d[which(fit$cluster == 3), 1])

# 3 svm
library(e1071)
fit <- svm(Degree ~ FallDist + FallHt, data = d)
table(d$Degree, fit$decision.values)




