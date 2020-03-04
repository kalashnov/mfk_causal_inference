# generate data

n <- 10000
ability <- runif(n, 0, 2)
college <- rbinom(n, 1, 0.5)
epsilon1 <- rnorm(n, 0, 1)
epsilon2 <- rnorm(n, 0, 1)

w0 <- epsilon1 > 0
w1 <- ifelse(ability > 1, 1, epsilon1 > 0)

y0 <- 3*ability + epsilon2
y1 <- 1 + y0

w <- ifelse(college == 1, w1, w0)
y <- ifelse(college == 1, y1, y0)

# estimate effect

mean(y[college==1]) - mean(y[college==0])

mean(ability[college==1]) - mean(ability[college==0])

model <- lm(y ~ college)
summary(model)

model <- lm(y ~ college + w)
summary(model)


model <- lm(y ~ college + w + ability)
summary(model)

# check
