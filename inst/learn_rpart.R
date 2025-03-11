
# ?rpart::rpart
library(rpart)
set.seed(1314); m1 = rpart(Kyphosis ~ Age + Number + Start, data = kyphosis)
set.seed(1314); m1a = rpart(Kyphosis ~ Age + Number + Start, data = kyphosis)
set.seed(125); m2 = rpart(Kyphosis ~ Age + Number + Start, data = kyphosis)

stopifnot(
  identical(m1, m1a),
  !identical(m1, m2)
) # ?rpart::rpart used random seed!!!!

