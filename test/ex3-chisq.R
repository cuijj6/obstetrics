library(data.table)
library(magrittr)

n <- 100 # people
age <- runif(n, 10, 80) %>% floor()
gender <- rbinom(n, 1, 0.5) %>% factor(label = c("female", "male"))
value  <- rnorm(100, 20, 10)

df <- data.table(value, gender, age)
df$group <- 1 # default

## q1: if gender = female, set group = 2

group1 <- data.table(df$gender = female)
## q2: if gender = female and age > 30, set group = 3


## add

# q3: cal z = x + y in d
# q4: cal z = x/y in d
# q5: for each row, whether x > y
d <- data.table(x = age, y = value)

name <- c("jack", "jill", "jenny")
ages <- c(12, 13, 11)
states <- c("UA", "CA", "NA")
d1 <- data.frame(name, ages, stringsAsFactors=FALSE)
d2 <- data.frame(name, states, stringsAsFactors=FALSE)

