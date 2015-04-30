#BaseStationID,
#City,
#Latitude,
#Longitude,
#Status,
#ConnAbons,
#Technology
#Manufacturer
#Model
library(uuid)
pj <- function(...) { return(paste(..., sep='/')) }
data_dir <- pj(getwd(), 'data')
raw_csv <- read.csv(pj(data_dir, 'cities.csv'))
cities <- raw_csv %>%
  select(
    city = RegionName,
    lat = Latitude,
    lon = Longitude
  )
cities <- mutate(cities, zoom=8)

STATUSES <- 0:100
TECH <- c('2g', '3g', 'lte')
MODELS <- c('X-100', 'Y-200', 'X-250', 'Y-300')
MANUFACTURERS <- 'HUAWEI'

bs <- data.frame(
  BaseStationID=character(),
  City=character(),
  Latitude=double(),
  Longitude=double(),
  Status=character(),
  ConnAbons=integer(),
  Technology=character(),
  Manufactuer=character(),
  Model=character())

for(from in seq(1, nrow(cities))) {
  curCity <- cities[from,]
  if(curCity$city == 'Kazakhstan')
    next
  curBSLats <- replicate(5, jitter(curCity$lat, factor=0.6))
  curBSLons <- replicate(5, jitter(curCity$lon, factor=0.6))
  for(i in seq(1, length(curBSLats))) {
    curLat <- curBSLats[i]
    curLon <- curBSLons[i]
    sts <- sample_n(data.frame(status=STATUSES), 1, weight=c(rep(1, 70), seq(1, 31)))$status
    if(sts == 0) {
      sts = 'off_air'
    }
    if(sts > 80) {
      sts = 'ok'
    }
    curBS <- data.frame(
      BaseStationId=UUIDgenerate(),
      City=curCity$city,
      Latitude=curLat,
      Longitude=curLon,
      Status=as.character(sts),
      ConnAbons=sample(5000:10000, 1),
      Technology=sample(TECH, 1),
      Manufacturer=MANUFACTURERS,
      Model=sample(MODELS, 1)
    )
    bs <- rbind(bs, curBS)
  }
}
write.csv(bs, pj(data_dir, 'base_stations_2.csv'), quote=FALSE, row.names=FALSE)

SUCCESS_CALL_RNG <- data.frame(n=seq(85, 100, by=0.5))
FAIL_RATE_RNG <- data.frame(n=seq(0, 5, by=0.12))
CELL_AVAIL_RNG <- data.frame(n=seq(85, 100, by=0.5))
MAX_ACTIVE_ABON_RNG <- data.frame(n=seq(50, 10000, by=25))
OVERLOAD_RATE_RNG <- data.frame(n=seq(0, 10, by=0.18))
YEAR = 2015
MONTHS = 1:2
DAYS = 1:10

kpi2g <- data.frame(
  BaseStationId=character(),
  year=integer(),
  month=integer(),
  day=integer(),
  success_call_rate=double(),
  fail_rate=double(),
  cell_avail=double(),
  max_active_abon=integer(),
  overload_rate=double()
)
kpi3g <- data.frame(
  BaseStationId=character(),
  year=integer(),
  month=integer(),
  day=integer(),
  success_call_rate=double(),
  fail_rate=double(),
  cell_avail=double(),
  max_active_abon=integer(),
  overload_rate=double()
)
kpilte <- data.frame(
  BaseStationId=character(),
  year=integer(),
  month=integer(),
  day=integer(),
  success_call_rate=double(),
  fail_rate=double(),
  cell_avail=double(),
  max_active_abon=integer(),
  overload_rate=double()
)

rawbsIds <- distinct(select(bs, BaseStationId), BaseStationId)$BaseStationId
print(length(rawbsIds))
for(from in seq(1, length(rawbsIds))) {
  curBSId <- rawbsIds[from]
  for(mon in MONTHS) {
    for(d in DAYS) {
      kpi2g <- rbind(kpi2g, data.frame(
        BaseStationId=curBSId,
        year=YEAR,
        month=mon,
        day=d,
        success_call_rate=sample_n(SUCCESS_CALL_RNG, 1, weight = 1:nrow(SUCCESS_CALL_RNG))$n,
        fail_rate=sample_n(FAIL_RATE_RNG, 1, weight = 1:nrow(FAIL_RATE_RNG))$n,
        cell_avail=sample_n(CELL_AVAIL_RNG, 1, weight = 1:nrow(CELL_AVAIL_RNG))$n,
        max_active_abon=sample_n(MAX_ACTIVE_ABON_RNG, 1, weight = 1:nrow(MAX_ACTIVE_ABON_RNG))$n,
        overload_rate=sample_n(OVERLOAD_RATE_RNG, 1, weight = 1:nrow(OVERLOAD_RATE_RNG))$n
      ))
      kpi3g <- rbind(kpi3g, data.frame(
        BaseStationId=curBSId,
        year=YEAR,
        month=mon,
        day=d,
        success_call_rate=sample_n(SUCCESS_CALL_RNG, 1, weight = 1:nrow(SUCCESS_CALL_RNG))$n,
        fail_rate=sample_n(FAIL_RATE_RNG, 1, weight = 1:nrow(FAIL_RATE_RNG))$n,
        cell_avail=sample_n(CELL_AVAIL_RNG, 1, weight = 1:nrow(CELL_AVAIL_RNG))$n,
        max_active_abon=sample_n(MAX_ACTIVE_ABON_RNG, 1, weight = 1:nrow(MAX_ACTIVE_ABON_RNG))$n,
        overload_rate=sample_n(OVERLOAD_RATE_RNG, 1, weight = 1:nrow(OVERLOAD_RATE_RNG))$n
      ))
      kpilte <- rbind(kpilte, data.frame(
        BaseStationId=curBSId,
        year=YEAR,
        month=mon,
        day=d,
        success_call_rate=sample_n(SUCCESS_CALL_RNG, 1, weight = 1:nrow(SUCCESS_CALL_RNG))$n,
        fail_rate=sample_n(FAIL_RATE_RNG, 1, weight = 1:nrow(FAIL_RATE_RNG))$n,
        cell_avail=sample_n(CELL_AVAIL_RNG, 1, weight = 1:nrow(CELL_AVAIL_RNG))$n,
        max_active_abon=sample_n(MAX_ACTIVE_ABON_RNG, 1, weight = 1:nrow(MAX_ACTIVE_ABON_RNG))$n,
        overload_rate=sample_n(OVERLOAD_RATE_RNG, 1, weight = 1:nrow(OVERLOAD_RATE_RNG))$n
      ))
    }
  }
}

write.csv(kpi2g, pj(data_dir, 'kpi_2g.csv'), quote=FALSE, row.names=FALSE)
write.csv(kpi3g, pj(data_dir, 'kpi_3g.csv'), quote=FALSE, row.names=FALSE)
write.csv(kpilte, pj(data_dir, 'kpi_lte.csv'), quote=FALSE, row.names=FALSE)

# replicate(100, jitter(c(42.5555, 72.34), factor=0.6))