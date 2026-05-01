
# ?rpart::rpart
library(rpart)
set.seed(14); m1 = rpart(Kyphosis ~ Age + Number + Start, data = kyphosis)
set.seed(14); m1a = rpart(Kyphosis ~ Age + Number + Start, data = kyphosis)
set.seed(25); m2 = rpart(Kyphosis ~ Age + Number + Start, data = kyphosis)

stopifnot(
  identical(m1, m1a),
  !identical(m1, m2)
) # ?rpart::rpart used random seed!!!!

