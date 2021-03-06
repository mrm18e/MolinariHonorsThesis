
## Making a Deaths dataset
deaths <- dat %>%
  dplyr::select(GEOID:CTYNAME,
                DEATHS2010:DEATHS2020) %>%
  pivot_longer(cols = c(DEATHS2010:DEATHS2020),
               names_to = "deathyears",
               values_to = "deaths") %>%
  mutate(year = substr(deathyears,7,11)) %>%
  filter(COUNTY != "000",
         year != "2010") %>%
  group_by(year, GEOID, STATE, COUNTY, STNAME, CTYNAME) %>% # Grouping by County, County name, and Year
  summarise(deaths = sum(deaths)) %>%
  group_by(GEOID, CTYNAME) %>%
  mutate(perdrop = (deaths - lag(deaths))/abs(lag(deaths))) %>%
  # mutate(perdrop = if_else(lag(deaths)<0,abs(perdrop), perdrop)) %>%
  I()

deaths$perdrop[is.nan(deaths$perdrop)] <- NA # some values are 0/0 or 0/1 or 1/0. We set those to NA
deaths[is.na(deaths)] <- 1 # we set all NA values to = 1.0

jenks_deaths <-  deaths %>%
  filter(year == 2020)

# Some values are Inf and -Inf. We drop them for the Jenks calculations.
jenks_deaths <- jenks_deaaths[!is.infinite(jenks_deaths$perdrop),]
getJenksBreaks(jenks_deaths$perdrop, 5)
deaths <- deaths %>%
  filter(year == 2020) # we only want the 2020 change

z <- deaths[which(deaths$GEOID == "01001"),] %>% 
  filter(GEOID == "01001")
getJenksBreaks(deaths$perdrop, 6)
deaths <- deaths %>%
  dplyr::select(GEOID, perdrop) %>% # we select just our county ID and the percentage drop
  mutate(groups_perdrop = case_when( # we classify our percentage drops into given categories
    perdrop <= -0.5 ~ "< -50%",
    perdrop < -0.25 ~ "< -25%",
    perdrop < 0 ~ "< 0%",
    perdrop < 0.25 ~ "< 25%",
    perdrop <= 2 ~ "> 100%"
  )) %>%
  I()

# We need to convert the categories into a leveled factor. If we don't do this, the order is wrong.
deaths$groups_perdrop = factor(deaths$groups_perdrop,
                               levels = c("< -50%", "< -25%", "< 0%", "< 25%", "> 100%"))
# Using colorbrewer, we create an RGB color scheme.
deaths$rgb <- "#999999" # we have to initialize the variable first.
deaths$rgb[which(deaths$groups_perdrop == levels(deaths$groups_perdrop)[1])] <- "#e66101"
deaths$rgb[which(deaths$groups_perdrop == levels(deaths$groups_perdrop)[2])] <- "#fdb863"
deaths$rgb[which(deaths$groups_perdrop == levels(deaths$groups_perdrop)[3])] <- "#eaf322"
deaths$rgb[which(deaths$groups_perdrop == levels(deaths$groups_perdrop)[4])] <- "#b2abd2"
deaths$rgb[which(deaths$groups_perdrop == levels(deaths$groups_perdrop)[5])] <- "#5e3c99"


# Joining our birth data with our shapefile
countydat <- left_join(shape, deaths)

write_sf(countydat, "./R/DATA-PROCESSED/deathshapefile.shp")

pal2 <- c( "#e66101",  "#fdb863", "#eaf322",  "#b2abd2", "#5e3c99")

# Making our map
map_deaths <- 
  ggplot(data = countydat) +
  geom_sf(aes(fill = groups_perdrop), color = NA) + # we set the fill to equal the raw color code
  geom_sf(data = states, fill=NA, color = "black") +
  scale_fill_manual(values = pal2, na.value = "#999999") +
  theme_bw() +
  coord_sf(datum=NA) +
  theme(legend.position = "right") +
  labs(fill = "% Change in Deaths")

countydat %>%
  ggplot(aes(fill = rgb)) +
  geom_sf(color = NA)



