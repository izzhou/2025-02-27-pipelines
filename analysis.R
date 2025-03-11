library(tidyverse)
library(janitor)
library(broom)

# load data

data <- read_csv("data/titanic.csv")

data

# clean data
data <- janitor::clean_names(data)

data

# eda

ggplot(data = data, aes(x = pclass)) +
  geom_bar()

ggplot(data, aes(x = survived)) +
  geom_bar()


# model

model <- glm(survived ~ as.factor(pclass) + sex + age + fare, data = data, family = "binomial")

summary(model)

# results

coef <- broom::tidy(model)
coef

# process results

coef <- coef |>
  dplyr::mutate(or = exp(estimate))

coef

# plot results


ggplot(coef |> dplyr::filter(term != "(Intercept)"), aes(x = term, y = or)) +
  geom_point() +
  coord_flip() +
  geom_hline(yintercept = 1)