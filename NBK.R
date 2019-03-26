setwd("C://Users/DNS/Desktop/StatAd")
getwd()
# Прочитаем файл с данными в переменную winedata
winedata <- read.csv("datawine.txt", sep=",")

# Узнаем объём выборки и число признаков
dims = dim(winedata) # n x m
n = dims[1] # объём выборки # 178
m = dims[2] # количество факторов # 14 (13+1 переменная отклика) 

# Часть выборки, например, 120 записей, будем использовать как обучающую выборку
n_train = 120



# Подсчитаем частоты встречаемости классов
# в исходной выборке
# (выразим эти частоты в %, округлив до десятых)
round(prop.table(table(winedata[1]))*100, digits = 1) # априорные вероятности гипотез (классов) (p(Hk))


# Чтобы обеспечить репрезентативность выборки,перемешаем её
set.seed(12345) # Псевдослучайное число
winedata_mixed=winedata[order(runif(n)),]


# Сортируем данные в случайном порядке
# Выберем обучающую выборку
train_data = winedata_mixed[1:n_train,]



# Сохраним номера классов для строк обучающей выборки в переменной train_data_labels
train_data_labels = train_data[,1]


# Необходимо использовать функцию factor для формирования меток соответствующих уровней значения переменной отклика
train_data_labels<-factor(train_data_labels)
# Подсчитаем частоту присутствия каждого класса в
# обучающей выборке
# и сравним с соответствующими частотами в
# исходной выбоке



#вырежем данные для будущего предсказания
  
new_predict<-winedata[c(1:2),]
winedata[-c(1,2),]
  
round(prop.table(table(train_data[1]))*100,
        digits = 1)

# Оставшуюся часть "перемешанной выборки" будем использовать как тестовую выборку
test_data = winedata_mixed[(n_train+1):n, ]
test_data_labels = test_data[,1]

  
# Перед вызовом наивного Байесовского классификатора удалим номера классов из обучающей и тестовой выборок
train_data = train_data[-1] # "Метки" классов мы передадим классификатору отдельно
test_data = test_data[-1] # "Метки" классов тестовой выборки передавать классификатору не будем!
    

# Установим (при необходимости) и подключим пакет e1071:
    
#install.packages("e1071")

library(e1071)

# Вызовем наивный Байесовский классификатор:
my_classifier <- naiveBayes(train_data,
                              train_data_labels)
# Передаём обучающую выборку: факторы (train_data) и метки классов (train_data_labels)
# Определим ("спрогнозируем") метки для тестовых
# данных  Передаём классификатор и тестовую выборку
data_test_pred <- predict(my_classifier,
                            test_data) 

  #Передаём классификатор и тестовую выборку 
  # (только факторы, без меток)
  # Для оценки качества прогноза
  # подключим (если не подключали ранее) библиотеку gmodels:

   # install.packages("gmodels")
    library("gmodels")
  # и построим кросс-валидационную таблицу:
CrossTable(x = test_data_labels,
             y = data_test_pred,
             prop.chisq=TRUE)
  
new_predict
data_test_pred<-predict(my_classifier,new_predict)
data_test_pred
  