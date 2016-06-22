
rm(list = ls())

dd <- read.csv("case_analysis_for_fall_height.csv")
r.d <- dd[which(dd$Degree != "NULL"), ]
r.d <- r.d[which(r.d$FallDist != 0), ]
r.d <- r.d[which(r.d$FallHt != 0), ]

mdist <- function(x){
    t <- as.matrix(x)
    p <- dim(t)[2]
    m <- apply(t, 2, mean)
    s <- var(t)
    mahalanobis(t, m, s)
}

md <- mdist(r.d[, -c(1)])
c <- qchisq(0.90, df = 2)
d <- r.d[md < c, ]

###################################################
###################################################

# raw data description
# plot(d$FallDist, d$FallHt, type = "n", 
#      xlab = "Fall Distance", 
#      ylab="Fall Height")
# 
# for(i in seq(1, dim(d)[1])){
#     if(d[i, 1] == "Non Hospitalized injury")
#         colVal = "green"
#     if(d[i, 1] == "Hospitalized injury")
#         colVal = "blue"
#     if(d[i, 1] == "Fatality")
#         colVal = "red"
#     points(d[i, 2], d[i, 3], col = colVal)
# }
# legend("bottomright", c("Non Hospitalized", "Hospitalized", "Fatality"),
#        pch =rep(1, 3),
#        col = c("green", "blue", "red"))

# boxplot(FallHt ~ Degree, data = d,
#         main = "Injury Degree VS. Fall Height",
#         xlab = "Injury Degree",
#         ylab = "Fall Height")
# 
# boxplot(FallDist ~ Degree, data = d,
#         main = "Injury Degree VS. Fall Distance",
#         xlab = "Injury Degree",
#         ylab = "Fall Distance")

###################################################
###################################################

d$FallDist <- (d$FallDist - min(d$FallDist)) / abs(max(d$FallDist) - min(d$FallDist))
d$FallHt <- (d$FallHt - min(d$FallHt)) / abs(max(d$FallHt) - min(d$FallHt))

# 50% 70% 90%
train.sample = sample(1:dim(d)[1], dim(d)[1] * 0.7)
d.train = d[train.sample, ]
d.test = d[-train.sample, ]

# # 1 nnet
# library(nnet)
# fit <- nnet(Degree~ FallHt + FallDist, data = d.train, size = 3)
# 
# r <- predict(fit, d.test)
# 
# correct_record = 0
# for(i in seq(1, dim(r)[1])){
#     r.name <- names(which(r[i, ] == max(r[i, ])))
#     if(r.name == d.test[i, 1]){
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

# # 3 svm
# library(e1071)
# fit <- svm(Degree ~ FallDist + FallHt, data = d.train)
# r <- predict(fit, d.test)
# print(table(d.test$Degree, r))



ann = c(0.6341954, 0.6374521, 0.6149425)
svm = c(0.625952, 0.6385017, 0.6394597)
plot(0:1, 0:1, type = "n", 
     xlab = "Sample Ratio", 
     ylab="Predicting Correct Ratio",
     xlim = c(0.4, 1),
     ylim = c(0.6, 0.7))
x = c(0.5 , 0.7, 0.9)
points(x, ann, type = "b", lty=6, pch=0, col="red")
points(x, svm, type = "b", lty=5, pch=1, col="blue")
legend("topright", c("ANN", "SVM"), col = c("red", "blue"), pch = c(0,1))








