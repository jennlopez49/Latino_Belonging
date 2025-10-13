# Importing Data
#inclusivity <- readxl::read_xlsx("inclusivity_scores_2009_16.xlsx")

final_scores <- read.csv("scores_final.csv")
scores_subperiod <- read.csv("scores_final_subperiods.csv")
# Load U.S. states shapefile
states <- tigris::states(cb = TRUE, resolution = "20m", class = "sf")

# Filter for the 50 states (exclude territories)
states <- states[!states$STUSPS %in% c("HI", "AK", "GU", "PR", "VI"), ]
states$state 

# merge 
states <- merge(states, scores_final_subperiods, 
                by.x="STUSPS", by.y="State")
# states$ICI_Score_2016 <- as.numeric(states$ICI_Score_2016) 
# states$ICI_2016_col <- ifelse(states$ICI_Score_2016 < -86, 0, 
#                               ifelse(states$ICI_Score_2016 > -86 & states$ICI_Score_2016 <= -1, .5, 
#                                      ifelse(states$ICI_Score_2016 > -1, 1, NA))) 
# 
# ### trying another way 
# 
# midpoint <- (max(states$ICI_Score_2016, na.rm = TRUE) + min(states$ICI_Score_2016, na.rm = TRUE)) / 2
# centered_values <- states$ICI_Score_2016 - midpoint
# 
# # Normalize using the max absolute deviation from the midpoint
# max_abs_deviation <- max(abs(max(states$ICI_Score_2016, na.rm = TRUE) - midpoint), abs(min(states$ICI_Score_2016, na.rm = TRUE) - midpoint))
# states$normalized_var <- centered_values / max_abs_deviation
# 
# states$bucketed_index <- case_when(
#   states$normalized_var >= -1 & states$normalized_var < -0.5 ~ -1,
#   states$normalized_var >= -0.5 & states$normalized_var < 0 ~ -0.5,
#   states$normalized_var >= 0 & states$normalized_var < 0.3 ~ 0,
#   states$normalized_var >= 0.3 & states$normalized_var < 0.7 ~ 0.5,
#   states$normalized_var >= 0.7 & states$normalized_var <= 1 ~ 1,
#   TRUE ~ NA_real_ # Handle unexpected values
# )
# 
# states$bucketed_index_alt <- case_when(
#   states$ICI_Score_2016 >= -355 & states$ICI_Score_2016 < -100 ~ -1,
#   states$ICI_Score_2016 >= -100 & states$ICI_Score_2016 < -60 ~ -.5,
#   states$ICI_Score_2016 >= -60 & states$ICI_Score_2016 < 0 ~ 0,
#   states$ICI_Score_2016 >= 0 & states$ICI_Score_2016 < 50 ~ .5,
#   states$ICI_Score_2016 >= 50 & states$ICI_Score_2016 <= 164 ~ 1,
#   TRUE ~ NA_real_ # Handle unexpected values
# )
# 
# states$ICI_2016_col_index <- as.numeric(as.character(states$bucketed_index_alt))

### Standardizing 
states <- states %>% mutate(
  # sym_lat_index_16 = case_when(
  # latino_sym_16 < -10 ~ -1,
  # latino_sym_16 >= -10 & latino_sym_16 < -5 ~ -0.5,
  # latino_sym_16 >= -5 & latino_sym_16 <= 5 ~ 0,
  # latino_sym_16 > 5 & latino_sym_16 <= 10 ~ 0.5,
  # latino_sym_16 > 10 ~ 1),
  # conc_lat_index_16 = case_when(
  #   latino_conc_16 < -10 ~ -1,
  #   latino_conc_16 >= -10 & latino_conc_16 < -5 ~ -0.5,
  #   latino_conc_16 >= -5 & latino_conc_16 <= 5 ~ 0,
  #   latino_conc_16 > 5 & latino_conc_16 <= 10 ~ 0.5,
  #   latino_conc_16 > 10 ~ 1),
  # sym_lat_index_20 = case_when(
  #   latino_sym_20 < -10 ~ -1,
  #   latino_sym_20 >= -10 & latino_sym_20 < -5 ~ -0.5,
  #   latino_sym_20 >= -5 & latino_sym_20 <= 5 ~ 0,
  #   latino_sym_20 > 5 & latino_sym_20 <= 10 ~ 0.5,
  #   latino_sym_20 > 10 ~ 1),
  # conc_lat_index_20 = case_when(
  #   latino_conc_20 < -10 ~ -1,
  #   latino_conc_20 >= -10 & latino_conc_20 < -5 ~ -0.5,
  #   latino_conc_20 >= -5 & latino_conc_20 <= 5 ~ 0,
  #   latino_conc_20 > 5 & latino_conc_20 <= 10 ~ 0.5,
  #   latino_conc_20 > 10 ~ 1),
  conc_lat_index_14_16 = case_when(
    class.conc_lat_14_16 < -10 ~ -1,
    class.conc_lat_14_16 >= -10 & class.conc_lat_14_16 < -5 ~ -0.5,
    class.conc_lat_14_16 >= -5 & class.conc_lat_14_16 <= 5 ~ 0,
    class.conc_lat_14_16 > 5 & class.conc_lat_14_16 <= 10 ~ 0.5,
    class.conc_lat_14_16 > 10 ~ 1),
  sym_lat_index_14_16 = case_when(
    class.sym_lat_14_16 < -10 ~ -1,
    class.sym_lat_14_16 >= -10 & class.sym_lat_14_16 < -5 ~ -0.5,
    class.sym_lat_14_16 >= -5 & class.sym_lat_14_16 <= 5 ~ 0,
    class.sym_lat_14_16 > 5 & class.sym_lat_14_16 <= 10 ~ 0.5,
    class.sym_lat_14_16 > 10 ~ 1)
)

sym_16_map <- ggplot(data = states) +
  geom_sf(aes(fill = sym_lat_index_16)) +
  scale_fill_gradient2(low = "red", mid = "white", high = "lightblue", midpoint = 0,
limits = c(-1, 1)) +
  theme_minimal() +
  labs(
    title = "Immigrant Climate by State 2012-2016",
    subtitle = "All Policies",
    caption = "Data Source: Original Data."
  ) +
  theme(
    axis.text = element_blank(),  # Removes lat/long labels
    axis.ticks = element_blank(), # Removes axis ticks
    panel.grid = element_blank(), 
    legend.position = "bottom",
    plot.title = element_text(size = 16, face = "bold"),
    plot.title.position = "plot"
  )

conc_16_map <- ggplot(data = states) +
  geom_sf(aes(fill = conc_lat_index_16)) +
  scale_fill_gradient2(low = "red", mid = "white", high = "lightblue", midpoint = 0,
                       limits = c(-1, 1)) +
  theme_minimal() +
  labs(
    title = "Immigrant Climate by State 2012-2016",
    subtitle = "Concrete Policies",
    caption = "Data Source: Original Data."
  ) +
  theme(
    axis.text = element_blank(),  # Removes lat/long labels
    axis.ticks = element_blank(), # Removes axis ticks
    panel.grid = element_blank(), 
    legend.position = "bottom",
    plot.title = element_text(size = 16, face = "bold"),
    plot.title.position = "plot"
  )

sym_20_map <- ggplot(data = states) +
  geom_sf(aes(fill = sym_lat_index_20)) +
  scale_fill_gradient2(low = "red", mid = "white", high = "lightblue", midpoint = 0,
                       limits = c(-1, 1)) +
  theme_minimal() +
  labs(
    title = "Immigrant Climate by State 2016-2020",
    subtitle ="All Policies",
    caption = "Data Source: Original Data."
  ) +
  theme(
    axis.text = element_blank(),  # Removes lat/long labels
    axis.ticks = element_blank(), # Removes axis ticks
    panel.grid = element_blank(), 
    legend.position = "bottom",
    plot.title = element_text(size = 16, face = "bold"),
    plot.title.position = "plot"
  )

conc_20_map <- ggplot(data = states) +
  geom_sf(aes(fill = conc_lat_index_20)) +
  scale_fill_gradient2(low = "red", mid = "white", high = "lightblue", midpoint = 0,
                       limits = c(-1, 1)) +
  theme_minimal() +
  labs(
    title = "Immigrant Climate by State (2016-2020)",
    subtitle = "Concrete Policies",
    caption = "Data Source: Original Data."
  ) +
  theme(
    axis.text = element_blank(),  # Removes lat/long labels
    axis.ticks = element_blank(), # Removes axis ticks
    panel.grid = element_blank(), 
    legend.position = "bottom",
    plot.title = element_text(size = 16, face = "bold"),
    plot.title.position = "plot"
  )

#### Saving the new graphs --------- 
ggsave(
  filename = "state_climate_2016_Full.png",  # File name with extension
  plot = sym_16_map,                             # Plot object
  width = 10,                             # Width in inches
  height = 8,                             # Height in inches
  dpi = 300                               # Resolution in dots per inch
)

ggsave(
  filename = "state_climate_2016_Conc.png",  # File name with extension
  plot = conc_16_map,                             # Plot object
  width = 10,                             # Width in inches
  height = 8,                             # Height in inches
  dpi = 300                               # Resolution in dots per inch
)

ggsave(
  filename = "state_climate_2020_Full.png",  # File name with extension
  plot = sym_20_map,                             # Plot object
  width = 10,                             # Width in inches
  height = 8,                             # Height in inches
  dpi = 300                               # Resolution in dots per inch
)

ggsave(
  filename = "state_climate_2020_Conc.png",  # File name with extension
  plot = conc_20_map,                             # Plot object
  width = 10,                             # Width in inches
  height = 8,                             # Height in inches
  dpi = 300                               # Resolution in dots per inch
)

# collapsed version
map_2016 <- ggplot(data = states) +
  geom_sf(aes(fill = ICI_2016_col_index)) +
  scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0,
                       limits = c(-1, 1)) +
  theme_minimal() +
  labs(
    title = "Immigrant Climate by State in 2017",
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

inclusivity <- readxl::read_xlsx("inclusivity_scores_2009_16.xlsx")
inclusivity$STUSPS <- inclusivity$State

inclusivity_20 <- readxl::read_xlsx("~/Desktop/COIi work/Latino_Imm_Enf/Latino_Proj/inclusive.xlsx")
colnames(inclusivity_20) <- c("State","inclusivity", "inclusivity_varied")
inclusive_full <- merge(inclusivity, inclusivity_20, by = "State")


inclusive_full <- inclusive_full %>% mutate(
  bucketed_index_alt_11 = inclusivity_varied,
  bucketed_index_alt_09 = case_when(
  inclusive_full$ICI_Score_2009 < -30 ~ -1,
  inclusive_full$ICI_Score_2009 >= -30 & inclusive_full$ICI_Score_2009 < -10 ~ -0.5,
  inclusive_full$ICI_Score_2009 >= -10 & inclusive_full$ICI_Score_2009 < 0 ~ 0,
  inclusive_full$ICI_Score_2009 >= 1 & inclusive_full$ICI_Score_2009 < 10 ~ 0.5,
  inclusive_full$ICI_Score_2009 >= 10 ~ 1,
  TRUE ~ NA_real_ ,
))

inclusive_full <- inclusive_full %>% mutate(
  index_11 = as.numeric(as.character(inclusive_full$bucketed_index_alt_11)),
  index_09 = as.numeric(as.character(inclusive_full$bucketed_index_alt_09)),
  ICI_Score_2011 = as.numeric(ICI_Score_2011),
  ICI_Score_2009 = as.numeric(ICI_Score_2009)
  )



states <- merge(states, inclusive_full, by.x = "STUSPS", by.y ="State")
# states$ICI_2020_col <- as.numeric(states$inclusivity_varied)




map_2011 <- ggplot(data = states) +
  geom_sf(aes(fill = ICI_Score_2011)) +
  scale_fill_gradient2(
    name = "ICI Index (2007-2011)",  # Set legend title
    low = "red", mid = "white", high = "blue",midpoint = 0
  ) +
  theme_minimal() +
  labs(
    title = "Immigrant Climate by State in 2011",
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

map_2009 <- ggplot(data = states) +
  geom_sf(aes(fill = ICI_Score_2009)) +
  scale_fill_gradient2(
    name = "ICI Index (2007-2009)",  # Set legend title
    low = "red", mid = "white", high = "blue", midpoint = 0
  ) +
  theme_minimal() +
  labs(
    title = "Immigrant Climate by State in 2009",
    caption = "Data Source: Pham and Van (2013)"
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
  filename = "state_climate_2011.png",  # File name with extension
  plot = map_2011,                             # Plot object
  width = 10,                             # Width in inches
  height = 8,                             # Height in inches
  dpi = 300                               # Resolution in dots per inch
)

ggsave(
  filename = "state_climate_2009.png",  # File name with extension
  plot = map_2009,                             # Plot object
  width = 10,                             # Width in inches
  height = 8,                             # Height in inches
  dpi = 300                               # Resolution in dots per inch
)

states_287g <- merge(states, state_exp_287g, by.x = "STUSPS", by.y = "State", all.y = TRUE,
                     all.x = TRUE)

full_hist_287g <- read.csv("~/Desktop/COIi work/State_Laws/full_hist_287g.csv")

full_hist_287g <- full_hist_287g %>% mutate(
  active_2010 = ifelse(Year_Signed <= 2008 & Last_Year >= 2012, 1, 0),
  active_2016 = ifelse(Year_Signed <= 2016 & Last_Year >= 2012, 1, 0),
  active_2020 = ifelse(Year_Signed <= 2020 & Last_Year >= 2016, 1, 0),
  active_2025 = ifelse(Year_Signed <= 2025 & Last_Year >= 2020, 1, 0)
)

agreements_per_state <- full_hist_287g %>%
  group_by(State) %>%
  summarise(
    total_agreements = n(),
    active_2010 = sum(active_2010, na.rm = TRUE),
    active_2016 = sum(active_2016, na.rm = TRUE),
    active_2020 = sum(active_2020, na.rm = TRUE),
    active_2025 = sum(active_2025, na.rm = TRUE)
  ) 

agreements <- states %>%
  left_join(agreements_per_state, by = c("STUSPS" = "State")) %>%
  mutate(
    total_agreements = replace_na(total_agreements, 0),
    active_2010 = replace_na(active_2010, 0),
    active_2016 = replace_na(active_2016, 0),
    active_2020 = replace_na(active_2020, 0),
    active_2025 = replace_na(active_2025, 0)
  )

ggplot(states_287g) +
  geom_sf(aes(fill = abs(total_exp_lat_2010))) +
  scale_fill_viridis_c(
    # colors = c("white", "yellow", "orange","red","firebrick", "purple"),
    name = "Impact of 287(g) Agreements",
    trans = "sqrt",
    direction = -1
  ) +
  theme_minimal() +
  labs(
    title = "287(g) Agreement Scores by State by 2010"
  ) +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    legend.position = "bottom",
    plot.title = element_text(size = 14, face = "bold")
  )


ggplot(states_287g) +
  geom_sf(aes(fill = abs(total_exp_lat_2016))) +
  scale_fill_viridis_c(
    # colors = c("white", "yellow", "orange","red","firebrick", "purple"),
    name = "Impact of 287(g) Agreements",
    trans = "sqrt",
    direction = -1
  ) +
  theme_minimal() +
  labs(
    title = "287(g) Agreement Scores by State by 2016"
  ) +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    legend.position = "bottom",
    plot.title = element_text(size = 14, face = "bold")
  )


ggplot(states_287g) +
  geom_sf(aes(fill = abs(total_exp_lat_2020))) +
  scale_fill_viridis_c(
    # colors = c("white", "yellow", "orange","red","firebrick", "purple"),
    name = "Impact of 287(g) Agreements",
    trans = "sqrt",
    direction = -1
  ) +
  theme_minimal() +
  labs(
    title = "287(g) Agreement Scores by State by 2020",
  ) +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    legend.position = "bottom",
    plot.title = element_text(size = 14, face = "bold")
  )


ggplot(states_287g) +
  geom_sf(aes(fill = abs(total_exp_lat_2025))) +
  scale_fill_viridis_c(
    # colors = c("white", "yellow", "orange","red","firebrick", "purple"),
    name = "Impact of 287(g) Agreements",
    trans = "sqrt",
    direction = -1
  ) +
  theme_minimal() +
  labs(
    title = "287(g) Agreement Scores by State by 2025"
  ) +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    legend.position = "bottom",
    plot.title = element_text(size = 14, face = "bold")
  )

active10 <- ggplot(agreements) +
  geom_sf(aes(fill = active_2010)) +
  scale_fill_viridis_c(
    # colors = c("white", "yellow", "orange","red","firebrick", "purple"),
    name = "# of 287(g) Agreements",
    trans = "sqrt",
    direction = -1,
    option = "inferno"
  ) +
  theme_minimal() +
  labs(
    title = "287(g) Agreements in Each State by 2010"
  ) +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    legend.position = "bottom",
    plot.title = element_text(size = 14, face = "bold")
  ) +  guides(
    fill = guide_colorbar(
      barwidth = 15,  # wider bar
      barheight = 0.5, 
      title.position = "top"
    )
  )

active16 <- ggplot(agreements) +
  geom_sf(aes(fill = active_2016)) +
  scale_fill_viridis_c(
    # colors = c("white", "yellow", "orange","red","firebrick", "purple"),
    name = "# of 287(g) Agreements",
    trans = "sqrt",
    direction = -1,
    breaks = c(0, 2, 4, 6),
    labels = c("0", "2", "4", "6+"),
    option = "inferno"
  ) +
  theme_minimal() +
  labs(
    title = "287(g) Agreements in Each State by 2016"
  ) +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    legend.position = "bottom",
    plot.title = element_text(size = 14, face = "bold")
  ) +  guides(
    fill = guide_colorbar(
      barwidth = 15,  # wider bar
      barheight = 0.5, 
      title.position = "top"
    )
  )

active20 <- ggplot(agreements) +
  geom_sf(aes(fill = active_2020)) +
  scale_fill_viridis_c(
    # colors = c("white", "yellow", "orange","red","firebrick", "purple"),
    name = "# of 287(g) Agreements",
    trans = "sqrt",
    direction = -1,
    option = "inferno",
    breaks = c(0, 1, 2, 5, 10, 20, 30, 40),
    labels = c("0", "1", "2", "5", "10","20","30", "40+")
  ) +
  theme_minimal() +
  labs(
    title = "287(g) Agreements in Each State by 2020"
  ) +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    legend.position = "bottom",
    plot.title = element_text(size = 14, face = "bold")
  ) +  guides(
    fill = guide_colorbar(
      barwidth = 15,  # wider bar
      barheight = 0.5, 
      title.position = "top"
    )
  )

active25 <- ggplot(agreements) +
  geom_sf(aes(fill = active_2025)) +
  scale_fill_viridis_c(
    # colors = c("white", "yellow", "orange","red","firebrick", "purple"),
    name = "# of 287(g) Agreements",
    trans = "sqrt",
    direction = -1,
    breaks = c(0, 1, 5, 10, 20, 40, 60, 150),
    labels = c("0", "1", "5", "10", "20", "40", "60", "150+"),
    option = "inferno"
  ) +
  theme_minimal() +
  labs(
    title = "287(g) Agreements in Each State by 2025"
  ) +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    legend.position = "bottom",
    plot.title = element_text(size = 14, face = "bold")
  ) + guides(
    fill = guide_colorbar(
      barwidth = 15,  # wider bar
      barheight = 0.5, 
      title.position = "top"
    )
  )

maps2010s <- active10 + active16

maps2020s <- active20 + active25

ggsave(
  filename = "287g_maps_2010s.png",  # File name with extension
  plot = maps2010s,                             # Plot object
  width = 10,                             # Width in inches
  height = 8,                             # Height in inches
  dpi = 300                               # Resolution in dots per inch
)

ggsave(
  filename = "287g_maps_2020s.png",  # File name with extension
  plot = maps2020s,                             # Plot object
  width = 10,                             # Width in inches
  height = 8,                             # Height in inches
  dpi = 300                               # Resolution in dots per inch
)



conc_16_map <- ggplot(data = states) +
  geom_sf(aes(fill = conc_lat_index_14_16)) +
  scale_fill_gradient2(low = "red", mid = "white", high = "lightblue", midpoint = 0,
                       limits = c(-1, 1)) +
  theme_minimal() +
  labs(
    title = "Immigrant Climate by State 2014-2016",
    subtitle = "Concrete Policies",
    caption = "Data Source: Original Data."
  ) +
  theme(
    axis.text = element_blank(),  # Removes lat/long labels
    axis.ticks = element_blank(), # Removes axis ticks
    panel.grid = element_blank(), 
    legend.position = "bottom",
    plot.title = element_text(size = 16, face = "bold"),
    plot.title.position = "plot"
  )

sym_16_map <- ggplot(data = states) +
  geom_sf(aes(fill = sym_lat_index_14_16)) +
  scale_fill_gradient2(low = "red", mid = "white", high = "lightblue", midpoint = 0,
                       limits = c(-1, 1)) +
  theme_minimal() +
  labs(
    title = "Immigrant Climate by State 2014-2016",
    subtitle = "Concrete Policies",
    caption = "Data Source: Original Data."
  ) +
  theme(
    axis.text = element_blank(),  # Removes lat/long labels
    axis.ticks = element_blank(), # Removes axis ticks
    panel.grid = element_blank(), 
    legend.position = "bottom",
    plot.title = element_text(size = 16, face = "bold"),
    plot.title.position = "plot"
  )

ggsave(
  filename = "conc_map14_16.png",  # File name with extension
  plot = conc_16_map,                             # Plot object
  width = 10,                             # Width in inches
  height = 8,                             # Height in inches
  dpi = 300                               # Resolution in dots per inch
)
