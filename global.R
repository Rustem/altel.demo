library(plyr)
library(dplyr)
pj <- function(...) { paste(..., sep='/') }
data_dir <- pj(getwd(), 'data')
COLORS <- c('#FF0000', '#FFFF00', '#33FF66')
COLOR_NAMES <- c('bad', 'normal', 'ok')
KZ_CNTR <- c(48.005284, 66.9045435)
KZ_BND_UPPERLEFT <- c(54.329912, 46.426028)
KZ_BND_DOWNRIGHT <- c(40.930859, 85.625246)
checkStatus <- function(statuses) {
  
  chk <- function(st) {
    val <- NA
    if(st == 'ok') {
      val <- COLORS[3]
    } else if(st == 'off_air') {
      val <- COLORS[1]
    } else if(as.integer(as.character(st)) >= 50 & as.integer(as.character(st)) <= 100) {
      val <- COLORS[2]
    } else {
      val <- COLORS[1]
    }
    return(val)
  }
  return(unlist(Map(chk, statuses)))
}

raw_csv <- read.csv(pj(data_dir, 'cities.csv'))
cities <- raw_csv %>%
  select(
    city = RegionName,
    lat = Latitude,
    lon = Longitude
  )
cities <- mutate(cities, zoom=8)
cities <- rbind(data.frame(city='Kazakhstan', lat=KZ_CNTR[1], lon=KZ_CNTR[2], zoom=5), cities)


city_list <- as.character(cities$city)
names(city_list) <- city_list

raw_csv <- read.csv(pj(data_dir, 'base_stations.csv'))
base_stations <- raw_csv %>%
  select(
    id = BaseStationId,
    city = City,
    model = Model,
    manufacturer = Manufacturer,
    lat = Latitude,
    lon = Longitude,
    status = Status,
    connAbons = ConnAbons,
    technology = Technology
  )
base_stations <- mutate(base_stations, color=checkStatus(status))

kpi_transform <- function(data) {
  rv <- data %>%
    select(
      id = BaseStationId,
      year = year,
      month = month,
      day = day,
      success_call_rate = success_call_rate,
      fail_rate = fail_rate,
      cell_avail = cell_avail,
      max_active_abon = max_active_abon,
      overload_rate = overload_rate
    )
  return(rv)
}

kpi_2g <- kpi_transform(read.csv(pj(data_dir, 'kpi_2g.csv')))
kpi_3g <- kpi_transform(read.csv(pj(data_dir, 'kpi_3g.csv')))
kpi_lte <- kpi_transform(read.csv(pj(data_dir, 'kpi_lte.csv')))