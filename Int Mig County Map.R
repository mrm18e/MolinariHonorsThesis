
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
    perdrop < 1.25 ~ "< 1.15",
    perdrop < 1.5 ~ "< 1.5",
    perdrop < 2 ~ "< 2",
    perdrop >= 2 ~ "<6"
  )) %>%
  I()

# We need to convert the categories into a leveled factor. If we don't do this, the order is wrong.
internationalmig$groups_perdrop = factor(internationalmig$groups_perdrop,
                                    levels = c("< 1", "< 1.15", "< 1.5", "< 2", "< 6"))
# Using colorbrewer, we create an RGB color scheme.
internationalmig$rgb <- "#999999" # we have to initialize the variable first.
internationalmig$rgb[which(internationalmig$groups_perdrop == levels(internationalmig$groups_perdrop)[1])] <- "#fee5d9"
internationalmig$rgb[which(internationalmig$groups_perdrop == levels(internationalmig$groups_perdrop)[2])] <- "#fcae91"
internationalmig$rgb[which(internationalmig$groups_perdrop == levels(internationalmig$groups_perdrop)[3])] <- "#fb6a4a"
internationalmig$rgb[which(internationalmig$groups_perdrop == levels(internationalmig$groups_perdrop)[4])] <- "#de2d26"
internationalmig$rgb[which(internationalmig$groups_perdrop == levels(internationalmig$groups_perdrop)[5])] <- "#a50f15"

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