library(tidyr)
library(dplyr)
library(readxl)
library(stringr)
library(ggplot2)

energy <- read_xlsx("total-final-energy-consumption-tidy.xlsx", sheet = "Elec Domestic")
arealoc <- read_xlsx("london-borough-profiles.xlsx", sheet = "Data")
population <- read_xlsx("london-borough-atlas-tidy.xlsx", sheet = "population")
areahectares <- read_xlsx("london-borough-atlas-tidy.xlsx", sheet = "hectares")

london <- arealoc[2:34, 3:4]
names(london) <- c("area", "location")
london1 <- left_join(london, areahectares, by = "area")
londonpop <- left_join(london1, population, by = "area")
londonpop <- subset(londonpop, select = -c(0, 4:9))
londonpop <- subset(londonpop, select = -c(0, 14))
newlondonpop <- gather(data = londonpop, key = "year", value = "pop", 4:13)
names(newlondonpop) <- c("areaname", "location", "inland_hectares", "year", "pop")

elec <- gather(data = energy, key = "year", value = "electricity_Gwh", 2:12)
london_consumption <- left_join(newlondonpop, elec, by = c("areaname", "year"))
london_consumption$year <- as.integer(london_consumption$year)
london_consumption <- mutate(london_consumption, kWh_per_cap = electricity_Gwh*10^6/pop, pop_per_hec = pop/inland_hectares)

ggplot(data = london_consumption, aes(x = pop_per_hec, y = kWh_per_cap)) + geom_line() + xlim(10, 150) + ylim(1000, 4000) + facet_wrap(~areaname, nrow = 7)


library(gapminder)
library(ggplot2)
library(gganimate)

ggplot(data = london_consumption, aes(x = pop_per_hec, y = kWh_per_cap, size = pop, color = location)) +
    scale_size("population", limits = range(london_consumption$pop)) + xlim(10, 150) + ylim(1000, 4000) +
    geom_point(alpha = 0.5) + theme_bw() + transition_time(year) +
    labs(title = 'Year: {frame_time}', x = 'Population density (per hectare)', y = 'Domestic electricity consumption per cap (kWh)') + 
    shadow_wake(wake_length = 0.3, alpha = FALSE) + ease_aes('linear')

anim_save("london-electricity -consumption-wake.gif")

ggplot(data = london_consumption, aes(x = pop_per_hec, y = kWh_per_cap, size = pop, color = location)) +
  scale_size("population", limits = range(london_consumption$pop)) + xlim(10, 150) + ylim(1000, 4000) +
  geom_point(alpha = 0.5) + theme_bw() + transition_time(year) +
  labs(title = 'Year: {frame_time}', x = 'Population density (per hectare)', y = 'Domestic electricity consumption per cap (kWh)') + 
  shadow_mark(alpha = 0.3, size = 0.5) + ease_aes('linear')

anim_save("london-electricity -consumption-mark.gif")


ggplot(data = london_consumption, aes(x = pop_per_hec, y = electricity_Gwh, size = pop, color = location)) +
  scale_size("population", limits = range(london_consumption$pop)) + xlim(10, 150) + ylim(20, 700) +
  geom_point(alpha = 0.5) + theme_bw() + transition_time(year) +
  labs(title = 'Year: {frame_time}', x = 'Population density (per hectare)', y = 'Domestic electricity consumption (GWh)') + 
  shadow_mark(alpha = 0.3, size = 0.5) + ease_aes('linear')

anim_save("london-electricity-consumption-area.gif")


