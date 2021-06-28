
## Making a Domestic Migrations dataset
domesticmig <- dat %>%
  dplyr::select(GEOID:CTYNAME,
                DOMESTICMIG2010:DOMESTICMIG2020) %>%
  pivot_longer(cols = c(DOMESTICMIG2010:DOMESTICMIG2020),
               names_to = "domesticmigyears",
               values_to = "domesticmig") %>%
  mutate(year = substr(domesticmigyears,12,15)) %>%
  filter(COUNTY != "000",
         year != "2010") %>%
  group_by(year, GEOID, STATE, COUNTY, STNAME, CTYNAME) %>% # Grouping by County, County name, and Year
  summarise(domesticmig = sum(domesticmig)) %>%
  group_by(GEOID, CTYNAME) %>%
  mutate(perdrop = (domesticmig-lag(domesticmig))) %>%
  I()

z <- domesticmig[which(domesticmig$GEOID == "01001"),] %>% 
  filter(GEOID == "01001")

domesticmig$perdrop[is.nan(domesticmig$perdrop)] <- NA # some values are 0/0 or 0/1 or 1/0. We set those to NA
domesticmig[is.na(domesticmig)] <- 1 # we set all NA values to = 1.0

domesticmig <- domesticmig %>%
  filter(year == 2020) # we only want the 2020 change

getJenksBreaks(domesticmig$perdrop, 6)
domesticmig <- domesticmig %>%
  dplyr::select(GEOID, perdrop) %>% # we select just our county ID and the percentage drop
  mutate(groups_perdrop = case_when( # we classify our percentage drops into given categories
    # perdrop <= 0.88 ~ "< 0.85",
    perdrop < -10000 ~ "< -10000",
    perdrop < -1000 ~ "< -1000",
    perdrop < 0 ~ "< 0",
    perdrop < 3000 ~ "< 3000",
    perdrop <= 15000 ~ "> 15000"
  )) %>%
  I()

# We need to convert the categories into a leveled factor. If we don't do this, the order is wrong.
domesticmig$groups_perdrop = factor(domesticmig$groups_perdrop,
                               levels = c("< -10000", "< -1000", "< 0", "< 3000", "> 15000"))
# Using colorbrewer, we create an RGB color scheme.
domesticmig$rgb <- "#999999" # we have to initialize the variable first.
domesticmig$rgb[which(domesticmig$groups_perdrop == levels(domesticmig$groups_perdrop)[1])] <- "#ca0020"
domesticmig$rgb[which(domesticmig$groups_perdrop == levels(domesticmig$groups_perdrop)[2])] <- "#f4a582"
domesticmig$rgb[which(domesticmig$groups_perdrop == levels(domesticmig$groups_perdrop)[3])] <- "#f7f7f7"
domesticmig$rgb[which(domesticmig$groups_perdrop == levels(domesticmig$groups_perdrop)[4])] <- "#92c5de"
domesticmig$rgb[which(domesticmig$groups_perdrop == levels(domesticmig$groups_perdrop)[5])] <- "#0571b0"

# Joining our birth data with our shapefile
countydat <- left_join(shape, domesticmig)

# Making our map
map_domesticmig <- 
  ggplot() +
  geom_sf(data = countydat, fill = countydat$rgb, color = NA) + # we set the fill to equal the raw color code
  geom_sf(data = states, fill=NA, color = "black") +
  theme_bw() +
  coord_sf(datum=NA) +
  theme(legend.position = "right")

#We need to use the numeric amount of migrations