# install.packages("caret")
library(caret)
data(GermanCredit, package = "caret")

## Обнаружение и удаление ненужных предикторов

# Создадим копию данных без столбца с откликом Class:
gcred = GermanCredit[, -10]
# Функция nearZeroVar() возращает вектор номеров переменных,
# обладающих околонулевой дисперсией:
nz = nearZeroVar(gcred)

# имена этих переменных 
names(gcred)[nz]

## Удаляем предикторы с околонулевой дисперсией:
gcred.clean = gcred[, -nz]

# Функция findCorrelation() возвращает вектор 
# номеров переменных с высокой корреляцией:
(highCor = findCorrelation(cor(gcred.clean), cutoff = 0.75))

print("Имена этих переменных:"); names( gcred.clean)[highCor]

# Удаляем эти переменные:
gcred.clean =  gcred.clean[, -highCor]

## Ищем линейные зависимости между переменными
(linCombo <- findLinearCombos(gcred.clean))
# Удаляем эти переменные:
gcred.clean =  gcred.clean[, -linCombo$remove]
temp =  gcred.clean[, linCombo$remove]
dim(gcred.clean)

# names(gcred.clean)[linCombo$remove]
# plot(gcred$Personal.Male.Divorced.Seperated~gcred$Job.Management.SelfEmp.HighlyQualified)

## Предварительная обработка данных 
TransPred <- c("Duration", "Amount", "Age")
preVar <- preProcess(GermanCredit[, TransPred])
TransVar = predict(preVar, GermanCredit[, TransPred])
print("До преобразования:")
summary(GermanCredit[, TransPred])
print("После преобразования:")
summary(TransVar) 
# install.packages("e1071")
# library(e1071)
preBox <- preProcess(GermanCredit[, TransPred], method = "BoxCox")

# Посмотри графически, как изменилась переменная Amount
BoxVar <- predict(preBox,GermanCredit[, TransPred]) 
# Инициализируем датафрейм с ответами
y <- factor(GermanCredit$Class)
# хз зачем, библиотека lettice
# trellis.par.set(theme = col.whitebg(), warn = FALSE)
# было
featurePlot(GermanCredit$Amount, y, "density", labels = c("Amount", ""))
# стало
featurePlot(BoxVar[, 2], y, "density", labels = c("Box-Amount", ""))

"Попробуем сравнить точность прогноза двух моделей 
логистической регрессии с использованием функции train(), 
которая будет предметом нашего подробного рассмотрения ниже. 
Для тестирования моделей будем многократно (times = 100) 
случайным образом делить всю выборку на обучающую (800 объектов или 80%) 
и контрольную (200 объектов или 20%), 
для чего с помощью функции createDataPartition() 
создадим соответствующую “заготовку” train.index. 
Метод тестирования method = LGOCV 
(многократное разбиение на обучающую и контрольную выборки) 
и train.index определим в специальном объекте trControl. 
На функцию train() подадим данные для построения модели, 
тип модели и условия тестирования:"


train.index <- createDataPartition(y, p = .8, times = 100)
trControl = trainControl(method = "LGOCV", index = train.index)
print("Модель на основе исходного набора из 43 предикторов")
modSource <- train(gcred.clean, y, "glm", family = binomial, trControl = trControl)
# Оценка важности предиктора
plot(varImp(modSource, scale = FALSE))

## Работа с пропущенными значениями
# Удаление пропущенных значени
# Замена на медианное или среднее значение
# Восстановление через линейную зависимость
# Восстановление через метод knn

##
# получить список линейных моделей
# ls(getModelInfo(model = "lm"))
library(DMwR)
data(algae)

# Заполним пропуски в данных на основе алгоритма бэггинга
pPbI <- preProcess(algae[, 4:11], method = 'bagImpute')
algae[, 4:11] <- predict(pPbI, algae[, 4:11])

lm.a1.cv <- train(a1 ~ ., data = algae[, 1:12], method = 'lm',
                  trControl = trainControl(method = "cv"))

lm_step.a1.cv <- train(a1 ~ size + mxPH + mnO2 + NO3 + NH4 + PO4, 
                       data = algae[, 1:12], method = 'lm',
                       trControl = trainControl(method = "cv"))

anova(lm.a1.cv, lm_step.a1.cv)
