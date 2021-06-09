
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
  mutate(perdrop = deaths/lag(deaths)) %>%
  I()

deaths$perdrop[is.nan(deaths$perdrop)] <- NA # some values are 0/0 or 0/1 or 1/0. We set those to NA
deaths[is.na(deaths)] <- 1 # we set all NA values to = 1.0

deaths <- deaths %>%
  filter(year == 2020) # we only want the 2020 change

getJenksBreaks(deaths$perdrop, 6)
deaths <- deaths %>%
  dplyr::select(GEOID, perdrop) %>% # we select just our county ID and the percentage drop
  mutate(groups_perdrop = case_when( # we classify our percentage drops into given categories
    # perdrop <= 0.88 ~ "< 0.85",
    perdrop < 1 ~ "< 1",
    perdrop < 1.25 ~ "< 1.15",
    perdrop < 1.5 ~ "< 1.5",
    perdrop < 2 ~ "< 2",
    perdrop >= 2 ~ "<6"
  )) %>%
  I()

# We need to convert the categories into a levelled factor. If we don't do this, the order is wrong.
deaths$groups_perdrop = factor(deaths$groups_perdrop,
                               levels = c("< 1", "< 1.15", "< 1.5", "< 2", "<6"))
# Using colorbrewer, we create an RGB color scheme.
deaths$rgb <- "#999999" # we have to initialize the variable first.
deaths$rgb[which(deaths$groups_perdrop == levels(deaths$groups_perdrop)[1])] <- "#2b83ba"
deaths$rgb[which(deaths$groups_perdrop == levels(deaths$groups_perdrop)[2])] <- "#ffffbf"
deaths$rgb[which(deaths$groups_perdrop == levels(deaths$groups_perdrop)[3])] <- "#fee090"
deaths$rgb[which(deaths$groups_perdrop == levels(deaths$groups_perdrop)[4])] <- "#fc8d59"
deaths$rgb[which(deaths$groups_perdrop == levels(deaths$groups_perdrop)[5])] <- "#d73027"

# Joining our birth data with our shapefile
countydat <- left_join(shape, deaths)

# Making our map
map_deaths <- 
  ggplot() +
  geom_sf(data = countydat, fill = countydat$rgb, color = NA) + # we set the fill to equal the raw color code
  geom_sf(data = states, fill=NA, color = "black") +
  theme_bw() +
  coord_sf(datum=NA) +
  theme(legend.position = "right")