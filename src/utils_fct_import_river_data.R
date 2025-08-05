#' @title River conditions data import function
#' @description Import river conditions data from SacPAS for Tillotson Model: includes Freeport (FPT) & Vernalis (VNS) river flow (cfs), OMR USGS tidally filtered flow, and Export (pumping discharge flow, cfs) at Tracy (TRP) and Harvey O. Banks (HRO) sites
#' @param sites character vector of site codes to import data from: "FPT", "VNS", "MAL", "TRP", "HRO"
#' @param years numeric vector of water years to import data from
#' @param metrics character vector of metrics to import data for: "Flow", "WaterTemperature", "PumpingDischarge"
#' @return  combined data frame for river conditions queried for the metrics, years and sites specified
require(data.table)
require(tidyverse)


fct_import_DART_river_conditions_query <- function(sites, years, metrics) {

  # Create an empty list to store wrangled data
  wrangled_data_list <- list()

  for (site in sites) {
      # Generate URL with the specified code, years, and metrics: uses Download CSV Only [single data pt/row]
      url <- paste0("https://www.cbr.washington.edu/dart/cs/php/rpt/mg.php?
                    sc=1
                    &mgconfig=river
                    &outputFormat=csvSingle&",
                    paste0("year%5B%5D=", paste(years, collapse = "&year%5B%5D=")),
                    "&loc%5B%5D=", paste(site, collapse = "&loc%5B%5D="),
                    "&data%5B%5D=", paste(metrics, collapse = "&data%5B%5D="),
                    "&tempUnit=C&startdate=1%2F1&enddate=12%2F31&avgyear=0&consolidate=1&grid=1&y1min=&y1max=&y2min=&y2max=&size=large")


      # Read data using fread
      raw_data <- data.table::fread(url)

      # wrangle data
      wrangled_data <- raw_data %>%
        filter(!is.na(value)) %>% # exclude rows where value is NA
        separate("mm-dd", c("mm", "dd")) %>%
        mutate(
          year = as.numeric(year),
          mm = as.numeric(mm),
          dd = as.numeric(dd)
        ) %>%
        mutate(YMD = lubridate::ymd(paste(year, mm, dd, sep = "-"))) %>%
        mutate(DOY = lubridate::yday(YMD))

      # Check if wrangled data is empty
      if (nrow(wrangled_data) == 0) {
        next  # Skip to the next site code
      }

      # Save wrangled data to the list
      wrangled_data_list[[site]] <- wrangled_data

  }

  # Bind all wrangled dataframes together
  combined_data <- bind_rows(wrangled_data_list)

  # # save combined file
  # write.csv(combined_data, file = here::here("data-raw", "SacPAS_river_covariates_combined_sites.csv"), row.names = FALSE)

  return(combined_data)
}

site<- "LWG"
#example use
fct_import_DART_river_conditions_query(
  sites = c("LWG", "BON"),
  years = 2000:2006,
  metrics = c("Inflow", "Spill+Percent", "Temp+%WQM%29")
)


years <- 2021:2023
site <- c("BON", "LWG")
metrics <- c("Inflow", "Spill Percent")

# Encode each metric for safe URL use
metrics_enc <- URLencode(metrics, reserved = TRUE)

url <- paste0(
  "https://www.cbr.washington.edu/dart/cs/php/rpt/mg.php?sc=1&mgconfig=river&outputFormat=csvSingle",
  "&", paste0("year%5B%5D=", paste(years, collapse = "&year%5B%5D=")),
  "&", paste0("loc%5B%5D=", paste(site, collapse = "&loc%5B%5D=")),
  "&", paste0("data%5B%5D=", paste(metrics_enc, collapse = "&data%5B%5D=")),
  "&startdate=1%2F1&enddate=12%2F31&avgyear=0&consolidate=1&grid=1",
  "&y1min=0&y1max=&y2min=&y2max=&size=large"
)






