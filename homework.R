install.packages("xlsx", dep = T)
install.packages("rpivotTable")
install.packages("dplyr")

library("xlsx")
library("rpivotTable")
library("dplyr")

data <- read.xlsx("problems_1.xlsx", sheetIndex = "0", )
df <- data.frame(data)
View(df)

df$TotalAmount <- as.numeric(df$TotalAmount)
df$Year <- as.numeric(df$Year)

# 1 - Средняя сумма начисленных средств в разрезе административных округов
first <- df %>% group_by(AdmArea, Year) %>% 
  summarise(average_sum = mean(TotalAmount))
View(first)
rpivotTable::rpivotTable(df,cols=c("Year"), rows=c("AdmArea"), aggregatorName = "Average",vals="TotalAmount", rendererName = "Bar Chart")

# 2 - Суммарные помесячные начисления
rpivotTable::rpivotTable(df, cols=c("Year"), rows=c("Month"), aggregatorName = "Sum",vals="TotalAmount", rendererName = "Bar Chart")
# 2 - Суммарные помесячные начисления за 2021 г.
rpivotTable::rpivotTable(df, cols=c("Year" == 2021), rows=c("Month"), aggregatorName = "Sum",vals="TotalAmount", rendererName = "Bar Chart")

# 3
#сумма ежегодных начислений по всем округам
third <- df %>% group_by(AdmArea, Year) %>% 
  summarise(sum_year = sum(TotalAmount))
View(third) 
#среднегодовые начисления по округам
rpivotTable::rpivotTable(third, rows=c("AdmArea"), aggregatorName = "Average",vals="sum_year", rendererName = "Bar Chart") 

# ЦАО
cad <- df[df$AdmArea == "Центральный административный округ",]
View(cad)
write.csv(cad, file = "cad.csv")

#Среднемесячные начисления ЦАО
cad_average <- first[first$AdmArea == "Центральный административный округ",]
View(cad_average)

cad <- cad %>% mutate(average_sum = recode(Year, 
                                           '2016'="средняя сумма",
                                           '2017'="1083549356",
                                           '2018' ="1240896844",
                                           '2019' = "1219241088",
                                           '2020' = "1300439017",
                                           '2021' = "1393470895"
))
View(cad)

cad$average_sum <- as.numeric(cad$average_sum)
cad$TotalAmount <- as.numeric(cad$TotalAmount)

cad_2 <- cad[cad$TotalAmount > cad$average_sum, ]
View(cad_2)
write.csv(cad_2, file = "cad_2.csv")

