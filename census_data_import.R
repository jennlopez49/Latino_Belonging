### downloading census data to get % Latino for each state --------------------

#### 2016 Vars & API codes ------------
### Total pop - B01003_001E
### Latino pop by specific origin (total)-- B03001_003E

## enter API key
census_api_key("INSERT KEY HERE", install = TRUE)

## getting the 2016 vars
acs_2016 <- get_acs(geography = "state", 
              variables = c(totalpop = "B01003_001E", latinopop = "B03001_003E"), 
              year = 2016)
### getting the 2020 vars 


dcs_2020 <- get_decennial(geography = "state", 
                    variables = c(totalpop = "P1_001N", latinopop = "P9_002N"), 
                    year = 2020,
                    sumfile = "dhc")
data_2016_wide <- acs_2016 %>%
  select(GEOID, NAME, variable, estimate) %>%  # Keep relevant columns
  pivot_wider(names_from = variable, values_from = estimate) %>% 
  mutate(percent.latino.2016 = (B03001_003/B01003_001)*100)

data_2020_wide <- dcs_2020 %>%
  select(GEOID, NAME, variable, value) %>%  # Keep relevant columns
  pivot_wider(names_from = variable, values_from = value) %>% 
  mutate(percent.latino.2020 = (latinopop/totalpop)*100)

data_2016 <- data_2016_wide %>% select(NAME, percent.latino.2016)
data_2020 <- data_2020_wide %>% select(NAME, percent.latino.2020)

latino_pop_20162020 <- left_join(data_2016, data_2020)
# saving
write.csv(latino_pop_20162020, "latino_pop.csv")
