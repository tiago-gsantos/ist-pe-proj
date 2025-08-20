library(ggplot2)

url <- 'dataset_url.csv'
data <- read.csv(url)

continents <- c("Europe", "Americas")
countries <- c("Lithuania", "Iceland", "United States" ,"Saint Lucia")

data |>
  subset(Continent %in% continents) |>
  ggplot() + 
  geom_point(aes(GDP, HCI,color = Continent), size = 2.5) + 
  geom_text(data = subset(data, Country %in% countries), 
            aes(GDP, HCI, label = paste0("←", Country)), 
            size = 3.6,
            hjust = -0.05,
            vjust = 0.3) +
  scale_x_log10() +
  labs(title = "Human Capital Index vs GDP per capita",
       x = "GDP per capita (international dollars)",
       y = "HCI")