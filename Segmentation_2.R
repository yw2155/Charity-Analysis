library(RODBC)
library(rfm)
library(magrittr)
library(lubridate)
library(dplyr)
library(tidyr)
library(knitr)
library(rmarkdown)

db = odbcConnect("mysql_server_64", uid = "root", pwd = "")
sqlQuery(db, "USE ma_charity_full")

query1 = "SELECT contact_id, act_date, amount AS revenue FROM acts WHERE act_type_id = 'DO' AND act_date < 20120630"
df = sqlQuery(db, query1)

query2 = "SELECT contact_id, MIN(act_date) AS Date_of_Turning FROM acts WHERE act_type_id = 'PA' GROUP BY contact_id"
d_pa = sqlQuery(db, query2)
odbcClose(db)

segment_names = c('Sugar Daddy', 'Loyal but Poor', 'Great Potential', 'New', "Can't Lose Them", 'Still Worth', 'Impulse', 'Dead')
recency_lower = c(3,3,3,3,1,1,1,1)
recency_upper = c(5,5,5,5,3,3,3,3)
frequency_lower = c(3,3,1,1,3,3,1,1)
frequency_upper = c(5,5,3,3,5,5,3,3)
monetary_lower = c(3,1,1,3,3,1,3,1)
monetary_upper = c(5,3,3,5,5,3,5,3)

df$act_date = as.Date(df$act_date, "%Y-%m-%d")
test = split(d_pa, format(as.Date(d_pa$Date_of_Turning), "%Y"))

year = 2004
for (val in test) {
  temp = val[[1]]
  setnames(temp, old=c('contact_id','Date_of_Turning'), new=c('customer_id','Transit'))
  temp$Transit = 1
  str_date = paste(toString(year),'-12-31',sep = '')
  str_date_1 = paste(toString(year),'1231',sep = '')
  analysis_date = lubridate::as_date(str_date, tz = "UTC")
  
  query1 = paste("SELECT contact_id, act_date, amount AS revenue FROM acts WHERE act_type_id = 'DO' AND act_date < ", str_date_1, sep = '')
  df = sqlQuery(db, query1)
  
  rfm_result = rfm_table_order(df, contact_id, act_date, revenue, analysis_date)
  segments = rfm_segment(rfm_result, segment_names, recency_lower, recency_upper, frequency_lower, frequency_upper, monetary_lower,monetary_upper)
  temp_1 = data.frame(rbind(segments))
  temp_1 = temp_1[, c(1,2)]
  temp_2 = merge(x = temp_1, y = temp, by = 'customer_id', all.x = TRUE)
  temp_2$Transit[is.na(temp_2$Transit)] = 0
  
  rslt = temp_2 %>% group_by(segment) %>% count(Transit)
  
}

segments %>% count(segment) %>% arrange(desc(n)) %>% rename(Segment = segment, Count = n)
