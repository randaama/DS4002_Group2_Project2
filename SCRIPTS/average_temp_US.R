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

countynames <- read_csv("/Users/selahmitchell/Downloads/US_FIPS.csv") %>% 
  mutate(fips=paste0(FIPS_State, FIPS_County)) %>%
  dplyr::select(fips,County_Name,State)

print(head(countynames))

average_temp_data <- climdiv_county %>%
  left_join(countynames, by = "fips") %>%
  select(year, fips, month, temp, County_Name, State)  # Select relevant columns
# Print the combined data to check
print(tail(average_temp_data))


write_csv(average_temp_data, "/Users/selahmitchell/Downloads/average_temp_data.csv")


