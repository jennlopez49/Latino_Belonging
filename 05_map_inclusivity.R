# Load U.S. states shapefile
states <- tigris::states(cb = TRUE, resolution = "20m", class = "sf")

# Filter for the 50 states (exclude territories)
states <- states[!states$STUSPS %in% c("HI", "AK", "GU", "PR", "VI"), ]
states$state 

# merge 
states <- merge(states, inclusivity_2016, by.x = "STUSPS", by.y = "State")
states$ICI_Score_2016 <- as.numeric(states$ICI_Score_2016) 
states$ICI_2016_col <- ifelse(states$ICI_Score_2016 < -86, 0, 
                              ifelse(states$ICI_Score_2016 > -86 & states$ICI_Score_2016 <= -1, .5, 
                                     ifelse(states$ICI_Score_2016 > -1, 1, NA))) 

### trying another way 

midpoint <- (max(states$ICI_Score_2016, na.rm = TRUE) + min(states$ICI_Score_2016, na.rm = TRUE)) / 2
centered_values <- states$ICI_Score_2016 - midpoint

# Normalize using the max absolute deviation from the midpoint
max_abs_deviation <- max(abs(max(states$ICI_Score_2016, na.rm = TRUE) - midpoint), abs(min(states$ICI_Score_2016, na.rm = TRUE) - midpoint))
states$normalized_var <- centered_values / max_abs_deviation

states$bucketed_index <- case_when(
  states$normalized_var >= -1 & states$normalized_var < -0.5 ~ -1,
  states$normalized_var >= -0.5 & states$normalized_var < 0 ~ -0.5,
  states$normalized_var >= 0 & states$normalized_var < 0.3 ~ 0,
  states$normalized_var >= 0.3 & states$normalized_var < 0.7 ~ 0.5,
  states$normalized_var >= 0.7 & states$normalized_var <= 1 ~ 1,
  TRUE ~ NA_real_ # Handle unexpected values
)

states$bucketed_index_alt <- case_when(
  states$ICI_Score_2016 >= -355 & states$ICI_Score_2016 < -100 ~ -1,
  states$ICI_Score_2016 >= -100 & states$ICI_Score_2016 < -60 ~ -.5,
  states$ICI_Score_2016 >= -60 & states$ICI_Score_2016 < 0 ~ 0,
  states$ICI_Score_2016 >= 0 & states$ICI_Score_2016 < 50 ~ .5,
  states$ICI_Score_2016 >= 50 & states$ICI_Score_2016 <= 164 ~ 1,
  TRUE ~ NA_real_ # Handle unexpected values
)

states$ICI_2016_col_index <- as.numeric(as.character(states$bucketed_index_alt))

ggplot(data = states) +
  geom_sf(aes(fill = ICI_Score_2016)) +
  scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0,
limits = c(-355, 164)) +
  theme_minimal() +
  labs(
    title = "Immigrant Climate by State in 2016",
    caption = "Data Source: Pham and Van (2016)"
  ) +
  theme(
    axis.text = element_blank(),  # Removes lat/long labels
    axis.ticks = element_blank(), # Removes axis ticks
    panel.grid = element_blank(), 
    legend.position = "bottom",
    plot.title = element_text(size = 16, face = "bold"),
    plot.title.position = "panel"
  )
# collapsed version
map_2016 <- ggplot(data = states) +
  geom_sf(aes(fill = ICI_2016_col_index)) +
  scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0,
                       limits = c(-1, 1)) +
  theme_minimal() +
  labs(
    title = "Immigrant Climate by State in 2016",
    caption = "Data Source: Pham and Van (2016)"
  ) +
  theme(
    axis.text = element_blank(),  # Removes lat/long labels
    axis.ticks = element_blank(), # Removes axis ticks
    panel.grid = element_blank(), 
    legend.position = "bottom",
    plot.title = element_text(size = 16, face = "bold"),
    plot.title.position = "panel"
  )

ggsave(
  filename = "state_climate_2016.png",  # File name with extension
  plot = map_2016,                             # Plot object
  width = 10,                             # Width in inches
  height = 8,                             # Height in inches
  dpi = 300                               # Resolution in dots per inch
)

### 2020 version 
inclusivity$STUSPS <- inclusivity$State_abb
states <- merge(states, inclusivity, by = "STUSPS")
states$ICI_2020_col <- as.numeric(states$inclusivity_varied)

map_2020 <- ggplot(data = states) +
  geom_sf(aes(fill = ICI_2020_col)) +
  scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0,
                       limits = c(-1, 1)) +
  theme_minimal() +
  labs(
    title = "Immigrant Climate by State in 2020",
    caption = "Data Source: Pham and Van (2020)"
  ) +
  theme(
    axis.text = element_blank(),  # Removes lat/long labels
    axis.ticks = element_blank(), # Removes axis ticks
    panel.grid = element_blank(), 
    legend.position = "bottom",
    plot.title = element_text(size = 16, face = "bold"),
    plot.title.position = "panel"
  )

ggsave(
  filename = "state_climate_2020.png",  # File name with extension
  plot = map_2020,                             # Plot object
  width = 10,                             # Width in inches
  height = 8,                             # Height in inches
  dpi = 300                               # Resolution in dots per inch
)
