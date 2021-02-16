source(here::here("data-raw", "load_data.R"))

start = as.Date("2020-02-24")
end   = today()

covid_url = 'https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/'

provinces_url = 'dati-province/dpc-covid19-ita-province-'
provinces_df <- load_data(start, end, covid_url, provinces_url)

regions_url = 'dati-regioni/dpc-covid19-ita-regioni-'
regions_df <- load_data(start, end, covid_url, regions_url)

country_url = 'dati-andamento-nazionale/dpc-covid19-ita-andamento-nazionale-'
country_df <- load_data(start, end, covid_url, country_url)

provinces_df <- provinces_df %>% 
  select(- colnames(provinces_df)[grepl("note|nuts", colnames(provinces_df))])
regions_df   = regions_df %>% 
  select(- colnames(regions_df)[grepl("note|nuts", colnames(regions_df))])
country_df   = country_df %>% 
  select(- colnames(country_df)[grepl("note", colnames(country_df))])

colnames(provinces_df) <- c('date', 'country', 'region_code', 'region_name',
                            'province_code', 'province_name', 'province_short_name',
                            'latitude', 'longitude', 'total_cases')

colnames(regions_df) <- c('date', 'country', 'region_code', 'region_name',
                         'latitude', 'longitude', 'hospitalized_symptoms',
                         'intensive_care', 'total_hospitalized', 'home_isolation',
                         'total_positive', 'change_total_positive', 'new_positive',
                         'hospital_discharged', 'deceased_patients', 
                         'suspected_diagnostic_cases', 'screening_cases',
                         'total_cases', 'swabs', 'test_cases', 
                         'admission_intensive_care', 'total_positives_molecular_test',
                         'total_positives_antigen_rapid_test', 
                         'swabs_molecular_test', 'swabs_antigen_rapid_test')

colnames(country_df) <- c('date', 'country', 'hospitalized_symptoms',
                          'intensive_care', 'total_hospitalized',
                          'home_isolation', 'total_positive', 'change_total_positive',
                          'new_positive', 'hospital_discharged', 'deceased_patients',
                          'suspected_diagnostic_cases', 'screening_cases',
                          'total_cases', 'swabs', 'test_cases', 
                          'admission_intensive_care', 'total_positives_molecular_test',
                          'total_positives_antigen_rapid_test', 
                          'swabs_molecular_test', 'swabs_antigen_rapid_test')


provinces_df$date <- as.Date(provinces_df$date)
regions_df$date   <- as.Date(regions_df$date)
country_df$date   <- as.Date(country_df$date)

country_df$region_name <- rep("Italy", nrow(country_df))

inhabitants <- readxl::read_excel(here::here("data-raw", "inhabitants.xlsx"))

covid19 <- bind_rows(regions_df, country_df) %>% 
  left_join(inhabitants, by = "region_name")

saveRDS(provinces_df, file = here::here("data-raw", "provinces.rds"))
saveRDS(regions_df, file = here::here("data-raw", "regions.rds"))
saveRDS(country_df, file = here::here("data-raw", "country.rds"))

saveRDS(covid19, file = here::here("data-raw", "covid19.rds"))


