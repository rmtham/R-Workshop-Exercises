library("tidyr")
library("dplyr")
library("plyr")
library("knitr")
library("ggplot2")

dat <- read.csv(file = "C:/Users/rmtham/Desktop/data/gapminder_data.csv")
str(gapminder)
gap_wide <- read.csv("C:/Users/rmtham/Desktop/data/gapminder_wide.csv", stringsAsFactors = FALSE)
str(gap_wide)
gap_long <- gap_wide %>% 
  pivot_longer(
    cols = c(starts_with('pop'), starts_with('lifeExp'), starts_with('gdpPercap')),
    names_to = "obstype_year", values_to = "obs_values"
  )

str(gap_long)
gap_long <- gap_wide %>% 
  pivot_longer(
    cols = c(-continent, -country),
    names_to = "obstype_year", values_to = "obs_values"
  )
str(gap_long)
gap_long <- gap_long %>% separate(obstype_year, into = c("obs_type", "year"), sep = "_")
gap_long$year <- as.integer(gap_long$year)
gap_normal <- gap_long %>% 
  pivot_wider(names_from = obs_type, values_from = obs_values)
dim(gap_normal)
dim(gapminder)
names(gap_normal)
names(gapminder)
gap_normal <- gap_normal[, names(gapminder)]
all.equal(gap_normal, gapminder)
head(gap_normal)
head(gapminder)
gap_normal <- gap_normal %>% arrange(country, year)
all.equal(gap_normal, gapminder)
head(gap_normal)
head(gapminder)
gap_temp <- gap_long %>% unite(varID, continent, country, sep="_")
str(gap_temp)
gap_temp <- gap_long %>% 
  unite(ID_var, continent, country, sep = "_") %>%
  unite(var_names, obs_type, year, sep = "_")
str(gap_temp)
gap_wide_new <- gap_long %>% 
  unite(ID_var, continent, country, sep = "_") %>%
  unite(var_names, obs_type, year, sep = "_") %>%
  pivot_wider(names_from = var_names, values_from = obs_values) %>%
  separate(ID_var, c("continent", "country"), sep = "_")
gap_wide_new

str(gap_long)

mean_le_pop_gdp <- gap_long %>%
  group_by(continent) %>%
  summarize(mean_le = mean(lifeExp),
            mean_pop = mean(pop),
            mean_gdp = mean(gdpPercap)
            )

gap_ludicrously_wide <- gap_long %>% 
  unite(ID_var, country, sep = "_") %>%
  unite(var_names, obs_type, year, sep = "_") %>%
  pivot_wider(names_from = var_names, values_from = obs_values) %>%
  separate(ID_var, c("country"), sep = "_")
gap_ludicrously_wide

