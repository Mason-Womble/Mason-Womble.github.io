owidall = read.csv("https://github.com/owid/covid-19-data/blob/master/public/data/owid-covid-data.csv?raw=true")
# Deselect cases/rows
owidall = owidall[!grepl("^OWID", owidall$iso_code), ]
# Subset by continent: Europe
owideu = subset(owidall, continent=="Europe")

# Load necessary libraries
library(ggplot2)

# Scatterplot using data
ggplot(owideu, aes(x = year_month, y = new_deaths)) +
  geom_point(alpha = 0.6, color = "magenta") +
  geom_text(data = label_data, aes(label = location), vjust = -1, hjust = 1.5, size = 3) +
  labs(title = "Daily COVID-19 Deaths in Europe Over Time",
       x = "Date",
       y = "COVID Deaths in Europe (Daily)") +
  scale_y_continuous(limits = c(0, 6000), breaks = seq(0, 6000, by = 1000)) +
  scale_x_date(limits = c(as.Date("2020-01-01"), as.Date("2023-08-01")),
               date_labels = "%Y-%m", date_breaks = "2 months") +
  theme_minimal(base_size = 15) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))