library(ggplot2)

url <- "dataset_url.csv"
data <- read.csv(url, check.names = FALSE)

data |>
  subset(year == 1986 & age == "25-34 years") |>
  ggplot() +
  geom_boxplot(aes(sex, `suicides/100k pop`, fill = sex)) +
  labs(title = "Suicide Rates per 100k Inhabitants between Sex in the 25-34 Age Group in 1986", 
       x = "Sex",
       y = "Suicides per 100k inhabitants")
