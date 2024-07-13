# Function to read and transform a CSV file to long format
read_and_transform <- function(file_path) {
  # Extract the base name of the file to use as a value name
  file_name <- tools::file_path_sans_ext(basename(file_path))
  
  # Read the CSV file
  data <- read_csv(file_path)
  
  # Ensure all year columns are of type character
  data <- data %>%
    mutate(across(where(is.numeric), as.character))
  
  # Convert to long format
  data_long <- data %>%
    pivot_longer(cols = -country, names_to = "year", values_to = file_name)
  
  return(data_long)
}

# Get a list of all CSV files in the data/ directory
file_list <- list.files(path = "data/", pattern = "*.csv", full.names = TRUE)

# Read and transform all CSV files
data_list <- lapply(file_list, read_and_transform)

# Join all data frames by 'country' and 'year'
combined_data <- reduce(data_list, full_join, by = c("country", "year"))

combined_data <- combined_data %>%
  mutate(
    child_mortality_num = as.numeric(child_mortality),
    lex_num = as.numeric(lex),
    mincpcap_num = as.numeric(mincpcap)
  )

convert_values <- function(value) {
  if (is.na(value)) {
    return(NA)
  } else if (str_detect(value, "k")) {
    return(as.numeric(str_replace(value, "k", "")) * 1000)
  } else if (str_detect(value, "M")) {
    return(as.numeric(str_replace(value, "M", "")) * 1000000)
  } else if (str_detect(value, "B")) {
    return(as.numeric(str_replace(value, "B", "")) * 1000000000)
  } else {
    return(as.numeric(value))
  }
}

combined_data <- combined_data %>%
  mutate(gdp_pcap_num = sapply(gdp_pcap, convert_values),
         pop_num = sapply(pop, convert_values))

combined_data <- combined_data %>%
  mutate(
    iso3c = countrycode(country, origin = "country.name", destination = "iso3c")
  ) %>% 
  select(country, iso3c, year, child_mortality_num, lex_num, mincpcap_num, gdp_pcap_num, pop_num)

# Download country polygons
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

# Join the country polygons with your dataset by ISO3 codes
world_data <- left_join(world, combined_data, by = c("iso_a3" = "iso3c"))

