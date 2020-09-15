library(tidyverse)
summary(diamonds)
head(diamonds)

ggplot(diamonds) +
  geom_bar(aes(cut))
ggplot(diamonds) +
  geom_bar(aes(clarity))
ggplot(diamonds) +
  geom_bar(aes(color))

diamonds %>%
  count(cut)
ggplot(diamonds, aes(x=carat)) +
  geom_histogram(binwidth = 0.5)
ggplot(diamonds, aes(x=carat)) +
  geom_histogram(binwidth = 0.1)
ggplot(diamonds, aes(x=carat)) +
  geom_histogram(binwidth = 0.01)

smaller <- diamonds %>%
  filter(carat < 3)
ggplot(smaller, aes(x=carat)) +
  geom_histogram(binwidth = 0.01)
ggplot(smaller, aes(x=x)) +
  geom_histogram()
ggplot(smaller, aes(x=y)) +
  geom_histogram()
ggplot(smaller, aes(x=z)) +
  geom_histogram()


ggplot(diamonds, aes(x=x)) +
  geom_histogram(binwidth = 0.5) +
  coord_cartesian(ylim = c(0, 50))
ggplot(diamonds, aes(x=y)) +
  geom_histogram(binwidth = 0.5) +
  coord_cartesian(ylim = c(0, 50))
ggplot(diamonds, aes(x=z)) +
  geom_histogram(binwidth = 0.5) +
  coord_cartesian(ylim = c(0, 50))

unusual1 <- diamonds %>%
  filter(x == 0) %>%
  arrange(x)
unusual1

unusual2 <- diamonds %>%
  filter(y < 3 | y > 20) %>%
  arrange(y)
unusual2

unusual3 <- diamonds %>%
  filter(z == 0 | z > 20) %>%
  arrange(z)
unusual3
tail(unusual3)

ggplot(diamonds, aes(x=price)) +
  geom_histogram(binwidth = 50)

ggplot(diamonds, aes(x=price)) +
  geom_histogram(binwidth = 50) +
  coord_cartesian(xlim = c(1400, 1600), ylim = c(0, 500))
# There is no diamond with a price between 1475 to 1525.

diamonds %>%
  filter(carat == 0.99) %>%
  count()

diamonds %>%
  filter(carat == 1) %>%
  count()
# People tend to produce and buy diamonds in full carats.

diamonds2 <- diamonds %>%
  mutate(y = ifelse(y < 3 | y > 20, NA, y))

ggplot(diamonds2, aes(x = y)) +
  geom_histogram()

ggplot(diamonds2, aes(x = x, y = y)) +
  geom_point()

mean(diamonds2$y)
sum(diamonds2$y)
mean(diamonds2$y, na.rm = TRUE)
sum(diamonds2$y, na.rm = TRUE)

