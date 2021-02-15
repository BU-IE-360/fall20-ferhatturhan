# IE 360: Statistical Forecasting and Time Series - Group Project
# Group 12: Tufan Berk Tuğ, Ferhat Turhan, Aral Dörtoğul
# Due: 15 Febrary 2021

library(data.table)
library(httr)
library(jsonlite)
library(forecast)
library(readxl)

get_token <- function(username, password, url_site){
  
  post_body = list(username=username,password=password)
  post_url_string = paste0(url_site,'/token/')
  result = POST(post_url_string, body = post_body)
  
  # error handling (wrong credentials)
  if(result$status_code==400){
    print('Check your credentials')
    return(0)
  }
  else if (result$status_code==201){
    output = content(result)
    token = output$key
  }
  
  return(token)
}

get_data <- function(start_date='2020-03-20', token, url_site){
  
  post_body = list(start_date=start_date,username=username,password=password)
  post_url_string = paste0(url_site,'/dataset/')
  
  header = add_headers(c(Authorization=paste('Token',token,sep=' ')))
  result = GET(post_url_string, header, body = post_body)
  output = content(result)
  data = data.table::rbindlist(output)
  data[,event_date:=as.Date(event_date)]
  data = data[order(event_date)]
  return(data)
}


send_submission <- function(predictions, token, url_site, submit_now=F){
  
  format_check=check_format(predictions)
  if(!format_check){
    return(FALSE)
  }
  
  post_string="list("
  for(i in 1:nrow(predictions)){
    if(i<nrow(predictions)){
      post_string=sprintf("%s%s,",post_string,predictions$forecast[i])
    } else {
      post_string=sprintf("%s%s)",post_string,predictions$forecast[i])
    }
  }
  
  submission = eval(parse(text=post_string))
  json_body = jsonlite::toJSON(submission, auto_unbox = TRUE)
  submission=list(submission=json_body)
  
  print(submission)
  # {"31515569":2.4,"32939029":2.4,"4066298":2.4,"6676673":2.4,"7061886":2.4,"85004":2.4} 
  
  if(!submit_now){
    print("You did not submit.")
    return(FALSE)      
  }
  
  
  header = add_headers(c(Authorization=paste('Token',token,sep=' ')))
  post_url_string = paste0(url_site,'/submission/')
  result = POST(post_url_string, header, body=submission)
  
  if (result$status_code==201){
    print("Successfully submitted. Below you can see the details of your submission")
  } else {
    print("Could not submit. Please check the error message below, contact the assistant if needed.")
  }
  
  print(content(result))
  
}

check_format <- function(predictions){
  
  if(is.data.frame(predictions) | is.data.frame(predictions)){
    if('forecast' %in% names(predictions)){
      if(nrow(predictions)==24){
        if(all(is.numeric(predictions$forecast))){
          print("Format OK")
          return(TRUE)
        } else {
          print("forecast information is not numeric")
          return(FALSE)                
        }
      } else {
        print("Forecasts for 24 hours should be provided, current number of rows:")
        print(nrow(predictions))
        return(FALSE)     
      }
    } 
  } else {
    print("Wrong format. Please provide data.frame or data.table object")
    return(FALSE)
  }
  
}

# this part is main code
subm_url = 'http://46.101.124.77'
u_name = "Group12"
p_word = "cmq6gs5q2eT5690R"
submit_now = TRUE
username = u_name
password = p_word
#read and update consumptions
bulkdata <- fread('bulk_consumption_with_temp.csv')
bulkdata <- bulkdata[, Date := as.Date(Date)]
upcons <- fread('realtimecons.csv')
upcons[,Consumption:=gsub('\\,', '', `Consumption (MWh)`)]
upcons[,Consumption:=as.numeric(Consumption)]
bulkdata$Consumption <- upcons$Consumption[1:nrow(bulkdata)]
#pull new data
token = get_token(username=u_name, password=p_word, url=subm_url)
newdata = get_data(token=token,url=subm_url)
#manipulate new data
names(newdata) <- names(bulkdata)
newdata <- newdata[, Date := as.Date(Date)]
newdata <- newdata[order(Date, Hour)]
bulkdata <- rbind(bulkdata,newdata)

# Adding weighted  mean temperature column to bulkdata
bulkdata[, weightedT:= 0.0919*T_1 + 0.0721*T_2 + 0.066*T_3 + 0.1656*T_4 + 0.0321*T_5 + 0.1504*T_6 + 0.4219*T_7]

daily_data <- bulkdata[,list(mean_consumption=mean(Consumption, na.rm = T), mean_temp = mean(weightedT, na.rm = T)),by=list(Date)]

# Reading day type data from file
day_type <- read_excel("day_type.xlsx", range = "C399:C1897", col_names = FALSE)

# Adding day_type column to daily_data
daily_data <- cbind(daily_data, day_type)
names(daily_data)[4] <- "day_type"

# Calculating various HDD and CDD data and adding them to daily_data
daily_data <- daily_data[mean_temp<20,CDD20:=0]
daily_data <- daily_data[is.na(CDD20),CDD20:= mean_temp - 20]
daily_data <- daily_data[mean_temp>16,HDD16:=0]
daily_data <- daily_data[is.na(HDD16),HDD16:= 16 - mean_temp]
daily_data[, month:= as.factor(month(Date))]
daily_data[, lag_1:= NA]
daily_data$lag_1[2:1498] <- daily_data$mean_consumption[1:1497]

# Reading industrial production index data from file
prod <- read_excel("production.xlsx")

# Adding industrial production index column to daily_data
daily_data[, prod_index:= prod$production[1:1499]]

# Fit 1
fit1 <- lm(mean_consumption~-1+as.factor(day_type)+CDD20+HDD16+lag_1+as.factor(month)+prod_index, daily_data[mean_consumption!=-1])
summary(fit1)

# Adding second model's residuals to daily_data
daily_data[, residuals:= NA]
daily_data$residuals[2:1497] <- fit1$residuals

# Detecting lower and upper 5% residuals and marking them as outlier_small and outlier_great
daily_data[!is.na(residuals), quant5:=quantile(residuals,0.05)]
daily_data[!is.na(residuals), quant95:=quantile(residuals,0.95)]
daily_data[,outlier_small:=as.numeric(residuals<quant5)]
daily_data[,outlier_great:=as.numeric(residuals>quant95)]

# Fit 2
fit2 <- lm(mean_consumption~-1+as.factor(day_type)+CDD20+HDD16+lag_1+as.factor(month)+prod_index+outlier_small+outlier_great, daily_data[mean_consumption!=-1])
summary(fit2)

daily_data[2:1497,fitted:=predict(fit2)]
daily_data$outlier_small[1498:1499] <- 0
daily_data[is.na(fitted)==T,fitted:=predict(fit2,daily_data[is.na(fitted)==T])]
daily_data$lag_1[1499] <- daily_data$fitted[1498]

# Calculating the hourly distribution ratios
ratios <- bulkdata[Date=='2021-01-31', Consumption]/32258.87

# Hourly predictions
predictions=data.table(Date=rep(as.Date(Sys.time())+1,24),Hour=0:23)
predictions=predictions[order(Date,Hour)]

# Back transformation (from daily to hourly)
predictions[,forecast:=ratios*daily_data$fitted[1499]]

# Send submission to API
send_submission(predictions, token, url=subm_url, submit_now=T)
