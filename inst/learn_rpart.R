
?rpart::rpart
library(rpart)
set.seed(1314); (x1 = rnorm(3))
{
  set.seed(1314);
  fit = rpart(Kyphosis ~ Age + Number + Start, data = kyphosis)
  (x2 = rnorm(3))
}
stopifnot(!identical(x1, x2)) # !!!!
# ?rpart::rpart used random seed!!!!

