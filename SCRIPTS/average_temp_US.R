library(dplyr)
library(readr)
library(tidyr)
# https://www2.census.gov/geo/docs/reference/state.txt?#
statefips <- read_delim(
  "/Users/selahmitchell/Downloads/state.txt",
  "|",
  escape_double = FALSE,
  trim_ws = TRUE,
  col_types = cols(
    STATE = col_character(),
    STUSAB = col_character(),
    STATE_NAME = col_character(),
    STATENS = col_character()
  )
)
noaastate <-
  read_csv("/Users/selahmitchell/Downloads/NOAA_State.txt") %>%
  left_join(statefips, by=c("State_Name"="STATE_NAME"))

county_filepath <- "/Users/selahmitchell/Downloads/climdiv-tmpccy-v1.0.0-20240906.txt"
county_percep_filepath <- "/Users/selahmitchell/Downloads/climdiv-pcpncy-v1.0.0-20240906.txt"

climdiv_county <-
  read_fwf(
    county_filepath,
    fwf_widths(
      c(2, 3, 2, 4, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7),
      c("noaa_state_order", "countyfips", "code", "year", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
    ),
    col_types = cols(
      noaa_state_order = col_character(),
      countyfips = col_character(),
      code = col_character(),
      year = col_integer(),
      Jan = col_double(),
      Feb = col_double(),
      Mar = col_double(),
      Apr = col_double(),
      May = col_double(),
      Jun = col_double(),
      Jul = col_double(),
      Aug = col_double(),
      Sep = col_double(),
      Oct = col_double(),
      Nov = col_double(),
      Dec = col_double()
    )
  ) %>%
  filter(noaa_state_order <= 48) %>% # takes Alaska out
  left_join(noaastate, by="noaa_state_order") %>%
  mutate(fips = paste0(STATE, countyfips)) %>%
  dplyr::select(-code, -STATE, -STUSAB, -STATENS, -noaa_state_order, -countyfips, -State_Name) %>%
  gather("month", "temp", -fips, -year) %>%
  mutate(temp=as.numeric(temp), fips=case_when(fips=="24511"~"11001", TRUE~fips))

print(tail(climdiv_county))


climdivp_county <-
  read_fwf(
    county_percep_filepath,
    fwf_widths(
      c(2, 3, 2, 4, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7),
      c("noaa_state_order", "countyfips", "code", "year", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
    ),
    col_types = cols(
      noaa_state_order = col_character(),
      countyfips = col_character(),
      code = col_character(),
      year = col_integer(),
      Jan = col_double(),
      Feb = col_double(),
      Mar = col_double(),
      Apr = col_double(),
      May = col_double(),
      Jun = col_double(),
      Jul = col_double(),
      Aug = col_double(),
      Sep = col_double(),
      Oct = col_double(),
      Nov = col_double(),
      Dec = col_double()
    )
  ) %>%
  filter(noaa_state_order <= 48) %>% # takes Alaska out
  left_join(noaastate, by="noaa_state_order") %>%
  mutate(fips = paste0(STATE, countyfips)) %>%
  dplyr::select(-code, -STATE, -STUSAB, -STATENS, -noaa_state_order, -countyfips, -State_Name) %>%
  gather("month", "percip", -fips, -year) %>%
  mutate(percip =as.numeric(percip), fips=case_when(fips=="24511"~"11001", TRUE~fips))

print(tail(climdivp_county))



# Joining the two tibbles
climate_data <- climdivp_county %>%
  inner_join(climdiv_county, by = c("year", "fips", "month"))

# View the combined data
print(tail(climate_data))



countynames <- read_csv("/Users/selahmitchell/Downloads/US_FIPS.csv") %>% 
  mutate(fips=paste0(FIPS_State, FIPS_County)) %>%
  dplyr::select(fips,County_Name,State)

print(head(countynames))

climate_data_all <- climate_data %>%
  left_join(countynames, by = "fips") %>%
  select(year, fips, month, temp, County_Name, State,percip)  # Select relevant columns

print(head(climate_data_all))

# Assuming average_temp_data is a data frame with a 'year' column
climate_data_all <- subset(climate_data_all, year >= 2003 & year <= 2023)

print(head(climate_data_all))


# Print the last few entries of the filtered dataset
head(climate_data_all)
write_csv(climate_data_all, "/Users/selahmitchell/Downloads/average_temp_data_23.csv")
















