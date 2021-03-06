
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
  mutate(perdrop = (internationalmig - lag(internationalmig))/abs(lag(internationalmig))) %>%
  # mutate(perdrop = if_else(lag(internationalmig)<0,abs(perdrop), perdrop)) %>%
  I()

internationalmig$perdrop[is.nan(internationalmig$perdrop)] <- NA # some values are 0/0 or 0/1 or 1/0. We set those to NA
internationalmig[is.na(internationalmig)] <- 1 # we set all NA values to = 1.0

jenks_internationalmig <-  internationalmig %>%
  filter(year == 2020)

# Some values are Inf and -Inf. We drop them for the Jenks calculations.
jenks_internationalmig <- jenks_internationalmig[!is.infinite(jenks_internationalmig$perdrop),]
getJenksBreaks(jenks_internationalmig$perdrop, 5)
internationalmig <- internationalmig %>%
  filter(year == 2020) # we only want the 2020 change

z <- internationalmig[which(internationalmig$GEOID == "01001"),] %>% 
  filter(GEOID == "01001")
getJenksBreaks(internationalmig$perdrop, 6)
internationalmig <- internationalmig %>%
  dplyr::select(GEOID, perdrop) %>% # we select just our county ID and the percentage drop
  mutate(groups_perdrop = case_when( # we classify our percentage drops into given categories
    perdrop <= -1 ~ "< -100%",
    perdrop < -0.5 ~ "< -50%",
    perdrop < 0 ~ "< 0%",
    perdrop < 0.5 ~ "< 50%",
    perdrop <= 1000 ~ "> 50%"
  )) %>%
  I()

# We need to convert the categories into a leveled factor. If we don't do this, the order is wrong.
internationalmig$groups_perdrop = factor(internationalmig$groups_perdrop,
                                    levels = c("< -100%", "< -50%", "< 0%", "< 50%", "> 50%"))
# Using colorbrewer, we create an RGB color scheme.
internationalmig$rgb <- "#999999" # we have to initialize the variable first.
internationalmig$rgb[which(internationalmig$groups_perdrop == levels(internationalmig$groups_perdrop)[1])] <- "#a6611a"
internationalmig$rgb[which(internationalmig$groups_perdrop == levels(internationalmig$groups_perdrop)[2])] <- "#dfc27d"
internationalmig$rgb[which(internationalmig$groups_perdrop == levels(internationalmig$groups_perdrop)[3])] <- "#f3deba"
internationalmig$rgb[which(internationalmig$groups_perdrop == levels(internationalmig$groups_perdrop)[4])] <- "#80cdc1"
internationalmig$rgb[which(internationalmig$groups_perdrop == levels(internationalmig$groups_perdrop)[5])] <- "#018571"

# Joining our birth data with our shapefile
countydat <- left_join(shape, internationalmig) %>%
  rationalize()


write_sf(countydat, "./R/DATA-PROCESSED/intmigshapefile.shp")

pal2 <- c( "#a6611a",  "#dfc27d", "#f3deba",  "#80cdc1", "#018571")

# Making our map
map_internationalmig <- 
  ggplot(data = countydat) +
  geom_sf(aes(fill = groups_perdrop), color = NA) + # we set the fill to equal the raw color code
  geom_sf(data = states, fill=NA, color = "black") +
  scale_fill_manual(values = pal2, na.value = "#999999") +
  theme_bw() +
  coord_sf(datum=NA) +
  theme(legend.position = "right") +
  labs(fill = "% Change in Inter. Mig.")

countydat %>%
  ggplot(aes(fill = rgb)) +
  geom_sf(color = NA)

map_counties <- plot_grid(map_births, map_deaths,
                     map_domesticmig, map_internationalmig,
                     ncol = 2)

#ONE PARAGRAPH PER EACH PANEL (1 FOR SIMILARITIES 'SPECULATION' - LACKING IN IRS DATA AND 1 FOR DIFFERENCES)

#COUNTIES WITH -INF MEANS A SIGNIFICANT DROP IN INT MIG AND INF MEANS AN ORIGINALLY NEGLIGIBLE AMOUNT OF INT MIG AND NOW A BIT OF INT MIG

