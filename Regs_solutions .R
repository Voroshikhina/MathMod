# Регрессионный анализ

# установим рабочую папку я загрузим данные
setwd("~/Desktop/ECO_tasks/")
data <- read.csv("data.csv")

# посмотрим свод по данным
summary(data)

# сделаем подвыборку по своему варианту
# также отберем только значимые колонки
data_variant <- na.omit(data[data$year == 2019 & 
                       data$doy >= 152 & 
                       data$doy <= 243 & 
                       data$hour > 7 & 
                       data$hour < 19 &
                       (data$age_group_index == "III" | data$age_group_index == "IV"), 
                       c("year", "doy", "hour", "Species", "Flux",
                         "age_group_index", "in_site_antrop_load", "VTA_score")])

# проверим, что возраст не более V
unique(data_variant$age_group_index)

# првоверим данные
head(data_variant)
tail(data_variant)

# построим корр матрицу
(corr <- cor(data_variant[, -c(1, 4, 6, 7)]))

# визуализируем корр матрицу
library(ggcorrplot)
ggcorrplot(corr,
           type = "lower",
           insig = "blank",
           lab = TRUE,
           digits = 3
)

# построим модель от всех имеющихся переменных
model <- lm(data = data_variant, Flux ~ .)
summary(model)

# с помощью ANOVA отметим значимые переменные
anova(model)

# уберем из регрессии незначимые переменные и оцениваем еще раз
model_corrected <- lm(data = data_variant, Flux ~ hour + Species + age_group_index + VTA_score)
summary(model_corrected)

# теперь все коэффициенты значимы
# согласно выводу, скорость сокотечения положительно зависит от часа дня, вида дерева, возраста 
# и отрицательно от визуальной оценки состояния

anova(model_corrected)

# визуализиуем полученную модель
qplot(hour, Flux, data = data_variant, alpha = I(1/10), color = Species) + 
  theme_bw() + 
  geom_line(aes(y = predict(lm(Flux ~ hour + Species + age_group_index + VTA_score, data = data_variant)))) +
  lims(x = c(8.5, 18), y = c(0, 10))
