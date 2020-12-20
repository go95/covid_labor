library(dplyr)
library(plm)
library(coefplot)
library(stargazer)

# разницы
#

data <- read.csv('final.csv')
data <- data[!is.na(data['infections']),]
data <- data[data$region != 'Чукотский АО',]
data <- data[data$oced != 'ПРЕДОСТАВЛЕНИЕ ПРОЧИХ ВИДОВ УСЛУГ',]
data$unemp_rate <- data$unemployed/data$employed_in_industry_jan
data$unempl_population <- data$unemployed/data$population
data$infect_rate <- data$infections/data$population
data$infect_rate_tst <- data$infections/data$tests
data$level_binary <- as.integer(data$level <= 1)

levels(data$oced) <- c(
  'Отходы, водоснабжение',
  'Государственное управление',
  'Административная деятельность',
  'Здравоохранение',
  'ИТ, связь',
  'Культра, развлечения, спорт',
  'Гостиницы, рестораны',
  'Наука, Техника',
  'Финансы, страхование',
  'Ископаемые',
  'Энергетика',
  'Обрабатывающие производства',
  'Образование',
  'Прочее',
  'Сельское хозяйство, рыболовство',
  'Строительство',
  'Оптовая, розничная торговля',
  'Транспорт, хранение'
)

data$industry <- data$oced


model <- lm(log(unemp_rate) ~ log(infections):C(industry) + Rt + level_binary:C(industry) + C(industry) + C(region) + C(week), data) # ТАК ВСЕ НАОБОРОТ. ПОЧЕМУ?
# model_tests <- lm(log(unemp_rate) ~ log(infect_rate):C(industry) + tests + Rt + level_binary:C(industry) + C(industry) + C(region) + C(week), data[!(data$region %in% c('Кировская', 'ХМАО')),]) # ТАК ВСЕ НАОБОРОТ. ПОЧЕМУ?

coefplot(model, predictors='level_binary', sort='magnitude', decreasing=FALSE, title='Модель по классификатору ОКВЭД', xlab='% Влияние карантина на заявки по безработице')

# , sort='magnitude'

# legend
# rows


data <- read.csv('final_other_class.csv')
data <- data[!is.na(data['infections']),]
data <- data[data$region != 'Чукотский АО',]
data <- data[data$oced != 'None',]
data$unemp_rate <- data$unemployed/data$employed_in_industry_jan
data$unempl_population <- data$unemployed/data$population
data$infect_rate <- data$infections/data$population
data$infect_rate_tst <- data$infections/data$tests
data$level_binary <- as.integer(data$level <= 1)

data <- data %>% group_by(region, oced) %>% arrange(week) %>% mutate(lagged_level_binary=lag(level_binary))

model2 <- lm(log(unemployed) ~ log(infections):C(oced) + Rt + C(oced):level_binary + C(oced) + C(region) + C(week), data)
model <- lm(log(unemployed) ~ log(infections):C(oced) + Rt + C(oced):level_binary + C(oced)*C(region) + C(week), data)
model_lagged <- lm(log(unemployed) ~ log(infections):C(oced) + Rt + C(oced):lagged_level_binary + C(oced)*C(region) + C(week), data)
model_with_fe <- model2
model_with_interactive_fe <- model
model_with_lag <- model_lagged
multiplot(model_with_interactive_fe, model_with_fe, predictors='level_binary', sort='magnitude', decreasing=FALSE, xlab='% Влияние карантина на заявки по безработице', title='Модели по справочнику ИАС ОБВ «Работа в России»')
coefplot(model_with_lag, predictors='lagged_level_binary', sort='magnitude', decreasing=FALSE, xlab='% Влияние карантина на заявки по безработице (с лагом)', title='Модели по справочнику ИАС ОБВ «Работа в России»')

# heteroskedasticity + ?
stargazer(model, model2, model_lagged, keep='.*oced.*')

model_men <- lm(log(cv_gender) ~ log(infections):C(oced) + Rt + C(oced):level_binary + C(oced) + C(region) + C(week), data[data$cv_gender!=0,])
model_women <- lm(log(unemployed - cv_gender) ~ log(infections):C(oced) + Rt + C(oced):level_binary + C(oced) + C(region) + C(week), data[data$cv_gender!=data$unemployed,])
multiplot(model_men, model_women, predictors='level_binary', sort='magnitude', decreasing=FALSE, xlab='% Влияние карантина на заявки по безработице', title='Модели по справочнику ИАС ОБВ «Работа в России»')

younger_30 <- lm(log(cv_birthday_1990) ~ log(infections):C(oced) + Rt + C(oced):level_binary + C(oced) + C(region) + C(week), data[data$cv_birthday_1990 != 0,])
older_30 <- lm(log(unemployed - cv_birthday_1990) ~ log(infections):C(oced) + Rt + C(oced):level_binary + C(oced) + C(region) + C(week), data[data$unemployed != data$cv_birthday_1990,])

multiplot(younger_30, older_30, predictors='level_binary', sort='magnitude', decreasing=FALSE, xlab='% Влияние карантина на заявки по безработице', title='Модели по справочнику ИАС ОБВ «Работа в России»')

age_20 <- lm(log(cv_birthday_1990) ~ log(infections):C(oced) + Rt + level_binary + C(oced) + C(region) + C(week), data[data$cv_birthday_1990 != 0,])
age_30 <- lm(log(cv_birthday_1980 - cv_birthday_1990) ~ log(infections):C(oced) + Rt + level_binary + C(oced) + C(region) + C(week), data[data$cv_birthday_1980 != data$cv_birthday_1990,])
age_40 <- lm(log(cv_birthday_1970 - cv_birthday_1980) ~ log(infections):C(oced) + Rt + level_binary + C(oced) + C(region) + C(week), data[data$cv_birthday_1970 != data$cv_birthday_1980,])
age_50 <- lm(log(cv_birthday_1960 - cv_birthday_1970) ~ log(infections):C(oced) + Rt + level_binary + C(oced) + C(region) + C(week), data[data$cv_birthday_1960 != data$cv_birthday_1970,])
age_60 <- lm(log(unemployed - cv_birthday_1960) ~ log(infections):C(oced) + Rt + level_binary + C(oced) + C(region) + C(week), data[data$unemployed != data$cv_birthday_1960,])
multiplot(age_20, age_30, age_40, age_50, age_60, predictors='level_binary', sort='magnitude', decreasing=FALSE, xlab='% Влияние карантина на заявки по безработице', title='Модели по справочнику ИАС ОБВ «Работа в России»')




# horovitz-tompson
# controls:
#   + bartik style
# bartik style interactions
# log + norm
# other identification things
# double robustness