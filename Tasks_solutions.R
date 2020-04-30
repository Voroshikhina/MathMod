# Tasks 

# Введение в R

library(dplyr)
library(ggplot2)

# загрузим данные
data(iris)

# 1
# Среднее по колонкам можно посчитать функцией colMeans()
# чтобы из вектора сделать список, используем функцию as.list()
as.list(colMeans(iris[, 1:4]))

# 2
# Среднее по рядам (строкам) можно найти с rowMeans()
rowMeans(iris[, 1:4])

# 3
# для начала создадим вектор с названиями нуклеотидов ДНК
nucl <- c("A", "G", "C", "T")
# сдлаем случайную выборку на 100 элементов
DNA <- sample(nucl, size = 1000, replace = TRUE)
# найдем и выведем долю кислот А и Т
dna_at <- (sum(DNA == "A") + sum(DNA == "T"))/length(DNA)
dna_at

# 4
# из вектора letters создадим случайный вектор на 20000 символов
let_vector <- sample(letters, size = 20000, replace = TRUE)
# создадим вектор гласных (y считаем за согласную)
vowels <- c("a", "e", "i", "o", "u")
# найдем количество гласных
sum(let_vector %in% vowels)

# 5
# с помощью пакета dplyr сгруппируем данные по видам
# найдем среднее по группе и вытащим вектор
species <- iris %>% 
  group_by(Species) %>% 
  summarise(m = mean(Petal.Length)) %>% 
  arrange(m) %>% 
  pull(Species)

# получаем факторный вектор
species
class(species)
levels(species)

# 6
# функция нахождения медианы
med <- function(x) {
  x_sort <- sort(x)
  # для четного количества позиций в векторе - среднее между центральными элементам
  if (length(x_sort) %% 2 == 0) {
    return(mean(c(x_sort[length(x_sort) %/% 2], 
                x_sort[length(x_sort) %/% 2 + 1])))
  }
  # для нечетного - сам центральный элемент
  else return(x_sort[length(x_sort) %/% 2 + 1])
}

# проверка
x <- sample(1:20, 10, replace = TRUE)
median(x)
med(x)

y <- sample(1:20, 11, replace = TRUE)
median(y)
med(y)

# 7
# график строим при помощи ggplot
ggplot(data = iris) +
  geom_point(aes(x = Petal.Length, y = Sepal.Length, color = Species))

# 8
# вычисления проводим при помощи dplyr
diamonds %>% 
  filter(price > 1000) %>% # фильтруем по цене
  group_by(clarity) %>% # группируем по яркости
  summarise(m_price = mean(price/carat)) # средняя цена/карат
