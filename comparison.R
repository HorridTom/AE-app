get_sigma_p <- function(performance_series) {
  average_n <- mean(performance_series$Total)
  total <- sum(performance_series$Total)
  total_successes <- sum(performance_series$Within_4h)
  p_bar <- total_successes/total
  sigma_p = sqrt((p_bar*(1-p_bar))/average_n)
  
  sigma_p
}

get_moving_range <- function(performance_series) {
  pv <- performance_series[['Performance']]
  mean(abs(pv[-1] - pv[1:length(pv)-1]))
}

get_sigma_z <- function(performance_series) {
  sigma_p <- get_sigma_p(performance_series)
  mr <- get_moving_range(performance_series)
  
  (2.66*mr)/(3*sigma_p)
}

get_sz_from_perf <- function(perf_data, start.date, end.date, date.col = 'Month_Start') {
  st.dt <- as.Date(start.date)
  ed.dt <- as.Date(end.date)
  
  # restrict to the period specified
  perf_data <- perf_data %>% filter(Month_Start >= st.dt, Month_Start <= ed.dt)
  if(nrow(perf_data) == 0) {return(NA)}
  
  ps <- make_perf_series(df = perf_data, prov_codes = NULL, adm_only = FALSE,
                         all_provs = TRUE, dept_types = NA, date_col = 'Month_Start')
  get_sigma_z(ps)
}

get_perf_from_perf_data <- function(perf_data, start.date, end.date, date.col = 'Month_Start') {
  st.dt <- as.Date(start.date)
  ed.dt <- as.Date(end.date)
  
  # restrict to the period specified
  perf_data <- perf_data %>% filter(Month_Start >= st.dt, Month_Start <= ed.dt)
  if(nrow(perf_data) == 0) {return(NA)}
  
  ps <- make_perf_series(df = perf_data, prov_codes = NULL, adm_only = FALSE,
                         all_provs = TRUE, dept_types = NA, date_col = 'Month_Start')
  sum(ps$Within_4h)/sum(ps$Total)
}

get_total_volume <- function(perf_data, start.date, end.date, date.col = 'Month_Start') {
  st.dt <- as.Date(start.date)
  ed.dt <- as.Date(end.date)
  
  # restrict to the period specified
  perf_data <- perf_data %>% filter(Month_Start >= st.dt, Month_Start <= ed.dt)
  if(nrow(perf_data) == 0) {return(NA)}
  
  ps <- make_perf_series(df = perf_data, prov_codes = NULL, adm_only = FALSE,
                         all_provs = TRUE, dept_types = NA, date_col = 'Month_Start')
  sum(ps$Total)
}

get_num_outside_limits_wrap <- function(perf_data, d1, d2) {
  #print(perf_data[1,'Prov_Name'])
  nol <- tryCatch({
          get_num_outside_limits(perf_data, prov_codes = NULL, date.col = 'Month_Start',
                                 cht.start.date = d1, cht.end.date = d2,
                                 cht.brk.date = NULL,
                                 period.start = d1, period.end = d2,
                                 dept_types = NA, all_provs = TRUE, lower.only = TRUE)},
          error = function(e) {NA}
        )
  nol
}

d1 <- "2017-01-01"
d2 <- "2017-12-31"


szd <- sitrep_perf %>% group_by(Prov_Code) %>% nest() %>%
  mutate(sigma_z = map(data, get_sz_from_perf, d1, d2),
         perf = map(data, get_perf_from_perf_data, d1, d2),
         num_out_lim = map(data, get_num_outside_limits_wrap, d1, d2),
         vol = map(data, get_total_volume, d1, d2)) %>%
  select(Prov_Code, sigma_z, perf, num_out_lim, vol) %>% unnest()

ggplot(szd, aes(x=sigma_z, y=perf, fill = num_out_lim, size =  vol)) +
  geom_point(pch=21, colour = "black") +
  scale_fill_brewer(palette = "blues", direction = -1, guide = guide_legend()) +
  geom_text(aes(label=if_else(vol > 300000, Prov_Code, '')), show.legend = FALSE,hjust=1, vjust=0)
