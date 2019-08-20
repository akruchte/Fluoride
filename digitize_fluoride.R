library(tidyverse)
library(splines)

theme_set(theme_classic())

colors <- scale_color_manual(values = c(male = 'blue', female = 'orange'))

male <- read_csv('Male.csv', col_names = FALSE) %>%
    rename(fluoride = X1, fsiq = X2) %>%
    mutate(sex = 'male')

female <- read_csv('Female.csv', col_names = FALSE) %>%
    rename(fluoride = X1, fsiq = X2) %>%
    mutate(sex = 'female')

data <- bind_rows(male, female)

#not significant, almost certainly due to loss of data in digitizing
summary(lm(fsiq ~ fluoride, data = data))
summary(lm(fsiq ~ fluoride * sex, data = data))


## iq score density estimates
ggplot(data, aes(fsiq)) +
    geom_density(aes(fill = sex), alpha = 0.9)


## significant sex difference in IQ
summary(lm(fsiq ~ sex, data = data))

## significant after removing two male outliers
summary(lm(fsiq ~ sex, data = data %>% filter(fsiq >= 60)))

## still significant after narrowing to range between intellectual disability and 'gifted'
summary(lm(fsiq ~ sex, data = data %>% filter(fsiq >= 70, fsiq <= 130)))


base_fit <- lm(fsiq ~ sex, data = data)
lm(fsiq ~ ns(fluoride, 4) + sex, data = data) %>% anova(base_fit)
lm(fsiq ~ ns(fluoride, 4) * sex, data = data) %>% anova(base_fit)
               
ggplot(data, aes(x = fluoride, y = fsiq, color = sex)) +
    geom_point() +
    geom_smooth() +
    colors


standard_range <- data %>% filter(fsiq >= 70, fsiq <= 130)
ggplot(standard_range, aes(x = fluoride, y = fsiq, color = sex)) +
    geom_point() +
    geom_smooth() +
    colors


