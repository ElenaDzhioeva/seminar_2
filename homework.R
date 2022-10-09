install.packages("readxl")
install.packages("rpivotTable")
install.packages("dplyr")

library(readxl)
library(rpivotTable)
library(dplyr)

pb <- readxl::read_xlsx("problems_1.xlsx")

View(pb)


#расчитать среднюю сумму начисленных средств(TotalAmount) в разрезе административных округов с детализацией по 2016, 2017, 2018, 2019 гг.

pb2016 <- pb[pb$Year==2016:2019,]

View(pb2016)

rpivotTable::rpivotTable(pb2016, rows = c("AdmArea", "Year"), aggregatorName = "Average", vals = "TotalAmount")


#рассчитать суммарные помесячные начисления за 2021 год.

pb2021 <- pb[pb$Year==2021,]

View(pb2021)

rpivotTable::rpivotTable(pb2021, rows="Month", cols="Year", aggregatorName = "Sum",vals="TotalAmount")


#построить график средних ежегодных начислений по всем административным округам.

rpivotTable::rpivotTable(pb, rows="AdmArea", aggregatorName ="Average", vals="TotalAmount", rendererName = "Bar Chart")


#из исходных данных отобрать только центральный административный округ, создав из него новый датафрейм.

central <- pb[pb$AdmArea=="Центральный административный округ",]

View(central)

write.csv(central, file = "central.csv", fileEncoding = "UTF-8")

#в созданном датафрейме отобрать только те месяца, начисления в которых оказались выше среднего уровня начислений в течение соответствующего года.

central$TotalAmount <- as.numeric(central$TotalAmount)
central$Year <- as.numeric(central$Year)

central_average <- central %>% group_by(AdmArea, Year) %>% 
  summarise(average_sum = mean(TotalAmount))

View(central_average)

central <- central %>% mutate(average_sum = recode(Year, 
'2016'="934177571", '2017'="1083549356", '2018' ="1240896844",
'2019' = "1219241088", '2020' = "1300439017", '2021' = "1393470895"
))

central$average_sum <- as.numeric(central$average_sum)

central_months <- central[central$TotalAmount > central$average_sum,]

View(central_months)

write.csv(central_months, file = "central_months.csv", fileEncoding = "UTF-8")
