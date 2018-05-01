# Read crime data from Open Baltimore
crime.endpoint <- "https://data.baltimorecity.gov/resource/4ih5-d5d5.json"
crime.data <- RSocrata::read.socrata(crime.endpoint)

# loc <- rwunderground::set_location(zip_code = 21202)
# weather.data <- history_range(loc, 
#                               date_start = "20120101", 
#                               date_end = str_replace_all(today(), "-", ""))

weekly.shooting <- crime.data %>% 
  mutate_at("crimedate", as.Date) %>%
  mutate(week.start = floor_date(crimedate, "week")) %>%
  group_by(week.start) %>%
  summarise(nfs = sum(description == "SHOOTING"),
            hom = sum(description == "HOMICIDE"),
            all.shootings = nfs + hom) %>%
  mutate(uprising = ifelse(week.start <= "2015-05-01", "Before May 2015", "May 2015 +"))

weather <- read.csv("1325092.csv") %>%
  bind_rows(read.csv("1325101.csv")) %>%
  filter(NAME == "MARYLAND SCIENCE CENTER, MD US") %>%
  mutate_at("DATE", as.Date)

weekly.weather <- weather %>%
  mutate(week.start = floor_date(DATE, "week")) %>%
  group_by(week.start) %>%
  summarise(mean.max.temp = mean(TMAX, na.rm = T),
            mean.min.temp = mean(TMIN, na.rm = T),
            days.precip = sum(PRCP > 0.05), # precip threshold decent?
            total.precip = sum(PRCP)) 

weekly.data <- weekly.shooting %>%
  left_join(weekly.weather, by = c("week.start"))

write_rds(weekly.data, "cache.Rdata")

