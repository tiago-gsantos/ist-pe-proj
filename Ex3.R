library(ggplot2)
library(readxl)

url <- "dataset_path.xlsx"

data <- read_excel(url)

data$share <- as.numeric(data$share)
data$DATE <- as.Date(paste(data$YEAR, data$MONTH, "01", sep = "-"))
data$COUNTRY <- as.factor(data$COUNTRY)

countries <- c("IEA Total", "Italy", "Latvia")

data |>
  subset(YEAR >= 2015 & PRODUCT == "Renewables" & COUNTRY %in% countries) |>
  ggplot() +
  geom_point(aes(x = DATE, y = share*100, color = COUNTRY), size = 1) +
  geom_line(aes(x = DATE, y = share*100, color = COUNTRY)) +
  labs(title = "Monthly Evolution of Renewable Electricity Produced since 2015", 
       x = "Date (monthly)",
       y = "Renewable Electric Energy Produced (%)") +
  ylim(0, 100)