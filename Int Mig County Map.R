
## Making an International Migrations dataset
internationalmig <- dat %>%
  dplyr::select(GEOID:CTYNAME,
                INTERNATIONALMIG2010:INTERNATIONALMIG2020) %>%
  pivot_longer(cols = c(INTERNATIONALMIG2010:INTERNATIONALMIG2020),
               names_to = "internationalmigyears",
               values_to = "internationalmig") %>%
  mutate(year = substr(internationalmigyears,17,20)) %>%
  filter(COUNTY != "000",
         year != "2010") %>%
  group_by(year, GEOID, STATE, COUNTY, STNAME, CTYNAME) %>% # Grouping by County, County name, and Year
  summarise(internationalmig = sum(internationalmig)) %>%
  group_by(GEOID, CTYNAME) %>%
  mutate(perdrop = internationalmig/lag(internationalmig)) %>%
  I()

internationalmig$perdrop[is.nan(internationalmig$perdrop)] <- NA # some values are 0/0 or 0/1 or 1/0. We set those to NA
internationalmig[is.na(internationalmig)] <- 1 # we set all NA values to = 1.0

internationalmig <- internationalmig %>%
  filter(year == 2020) # we only want the 2020 change

getJenksBreaks(internationalmig$perdrop, 6)
internationalmig <- internationalmig %>%
  dplyr::select(GEOID, perdrop) %>% # we select just our county ID and the percentage drop
  mutate(groups_perdrop = case_when( # we classify our percentage drops into given categories
    # perdrop <= 0.88 ~ "< 0.85",
    perdrop < 1 ~ "< 1",
    perdrop < 3 ~ "< 3",
    perdrop < 6 ~ "< 6",
    perdrop < 9 ~ "> 9",
    perdrop <= Inf ~ "Inf"
  )) %>%
  I()

# We need to convert the categories into a leveled factor. If we don't do this, the order is wrong.
internationalmig$groups_perdrop = factor(internationalmig$groups_perdrop,
                                    levels = c("< 1", "< 3", "< 6", "> 9", "Inf"))
# Using colorbrewer, we create an RGB color scheme.
internationalmig$rgb <- "#999999" # we have to initialize the variable first.
internationalmig$rgb[which(internationalmig$groups_perdrop == levels(internationalmig$groups_perdrop)[1])] <- "#a6611a"
internationalmig$rgb[which(internationalmig$groups_perdrop == levels(internationalmig$groups_perdrop)[2])] <- "#dfc27d"
internationalmig$rgb[which(internationalmig$groups_perdrop == levels(internationalmig$groups_perdrop)[3])] <- "#f5f5f5"
internationalmig$rgb[which(internationalmig$groups_perdrop == levels(internationalmig$groups_perdrop)[4])] <- "#80cdc1"
internationalmig$rgb[which(internationalmig$groups_perdrop == levels(internationalmig$groups_perdrop)[5])] <- "#018571"

# Joining our birth data with our shapefile
countydat <- left_join(shape, internationalmig)

# Making our map
map_internationalmig <- 
  ggplot() +
  geom_sf(data = countydat, fill = countydat$rgb, color = NA) + # we set the fill to equal the raw color code
  geom_sf(data = states, fill=NA, color = "black") +
  theme_bw() +
  coord_sf(datum=NA) +
  theme(legend.position = "right")

map_counties <- plot_grid(map_births, map_deaths,
                     map_domesticmig, map_internationalmig,
                     ncol = 2)