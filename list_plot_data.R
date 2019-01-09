update_data = TRUE
r1_col = "orange"
r2_col = "steelblue3"

urls_of_data <- NULL
if(update_data) {
  urls_of_data <- nhsAEscraper::getAEdata_urls_monthly()
  urls_of_Scotland_data <- nhsAEscraperScotland::getAEdata_urls_monthly()
}
AE_Data <- nhsAEscraper::getAE_data(update_data = update_data, directory = 'data-raw')
AE_Data <- clean_region_col(AE_Data)
AE_Data_Scot <- nhsAEscraperScotland::getAE_data(update_data = update_data, directory = 'data-raw')
AE_Data_Scot <- standardise_data(AE_Data_Scot)

AE_Data <- merge(AE_Data, AE_Data_Scot, all = T)

level <- 'Provider'
measure <- 'All'

provLookup <- AE_Data[!duplicated(AE_Data[,c('Prov_Code')]),c('Prov_Code','Prov_Name','Reg_Code','Region','Nat_Code','Country')]
provLookup <- provLookup %>% arrange(Prov_Name) 
orgNamesScot <- provLookup[which(provLookup$Country == "Scotland"),'Prov_Name']

perf.start.date <- "2015-07-01"
perf.end.date <- lubridate::today()
perf.brk.date <- NULL

N <- nrow(provLookup)
i <- 1
plot_data_list <- vector("list",N)

for (code in provLookup$Prov_Code) {
  
  tryCatch({
  p1 <- plot_performance(AE_Data, code = code, start.date = perf.start.date, end.date = perf.end.date,
                  brk.date = perf.brk.date, date.col = 'Month_Start',
                  x_title = "Month", measure = measure,
                  r1_col = r1_col, r2_col=r2_col,
                  level = level)
  plot_data_list[[i]] <- p1$data
  },
  error = function(e) NULL) 
  i <- i+1
}
