#--------------------------------------------------------------------------------

# STAT 580 TIME SERIES ANALYSIS GROUP PROJECT 
# PRESENTATION OUTPUTS AND IMAGES 

#--------------------------------------------------------------------------------

# FUNCTIONS 

#--------------------------------------------------------------------------------

# load libraries 
load_libraries = function(){
    library(tidyverse); library(tidyquant); library(timetk); 
    library(tseries); library(astsa); library(sweep); library(forecast); 
    library(ggfortify); 
}

# data 
load_weekly_data = function(path = '/Users/hamzah/documents/csulb statistics/stat 580/580 final project/salesweekly.csv', print = T){
    read_csv(path) %>% 
        mutate(datum = mdy(datum)) %>% 
        rename(date = datum) %>% 
        mutate(total = M01AB+M01AE+N02BA+N02BE+N05B+N05C+R03+R06, 
               M01 = M01AB+M01AE, N02 = N02BA+N02BE, 
               N05 = N05B+N05C, R0 = R03+R06) %>% 
        select(date, total, M01, N02, N05, R0) -> df 
    if (print){ df %>% print() } 
    return(df) 
}

# variable descriptions 
variable_descriptions = function(){
    cat('\nPharmaceutical Drug Sales Volumes', 
        '\n---------------------------------------------------------------', 
        '\ntotal: total drug sales volume', 
        '\nM01  : anti-inflammatory and antirheumatic products', 
        '\nN02  : other analgesics and antipyretics', 
        '\nN05  : psycholeptics drugs', 
        '\nR0   : antihistamines and drugs for obstructive airway diseases', 
        '\n---------------------------------------------------------------')
}

#--------------------------------------------------------------------------------

# set up 
load_libraries() 
library(ggpubr) 

# load data 
load_weekly_data() -> df 

# plot all time series 
df %>% 
    ggplot(aes(x = date, y = M01)) + 
    geom_line(color = 'cyan') + 
    geom_smooth(method = 'loess', color = 'red', fill = 'gray90') + 
    ggtitle('Drug Class: M01', subtitle = 'Pharmaceutial Drug Sales Volume') + 
    labs(x = '', y = 'volume') + 
    scale_x_date(date_breaks = '1 year', date_labels = '%Y') + 
    theme_tq_dark() -> p_m01 
df %>% 
    ggplot(aes(x = date, y = N02)) + 
    geom_line(color = 'cyan') + 
    geom_smooth(method = 'loess', color = 'red', fill = 'gray90') + 
    ggtitle('Drug Class: N02', subtitle = 'Pharmaceutial Drug Sales Volume') + 
    labs(x = '', y = 'volume') + 
    scale_x_date(date_breaks = '1 year', date_labels = '%Y') + 
    theme_tq_dark() -> p_n02 
df %>% 
    ggplot(aes(x = date, y = N05)) + 
    geom_line(color = 'cyan') + 
    geom_smooth(method = 'loess', color = 'red', fill = 'gray90') + 
    ggtitle('Drug Class: N05', subtitle = 'Pharmaceutial Drug Sales Volume') + 
    labs(x = '', y = 'volume') + 
    scale_x_date(date_breaks = '1 year', date_labels = '%Y') + 
    theme_tq_dark() -> p_n05 
df %>% 
    ggplot(aes(x = date, y = R0)) + 
    geom_line(color = 'cyan') + 
    geom_smooth(method = 'loess', color = 'red', fill = 'gray90') + 
    ggtitle('Drug Class: R0', subtitle = 'Pharmaceutial Drug Sales Volume') + 
    labs(x = '', y = 'volume') + 
    scale_x_date(date_breaks = '1 year', date_labels = '%Y') + 
    theme_tq_dark() -> p_r0 

# plot all series in a grid 
ggarrange(p_m01, p_n02, p_n05, p_r0) 

# split data 
df %>% filter(date < '2019-01-06') -> train 
df %>% filter(date >= '2019-01-06') -> test 

#--------------------------------------------------------------------------------

# M01 EDA and Model Selection 

#--------------------------------------------------------------------------------

# create time series object 
train %>% 
    tk_ts(select = M01, start = 2014, frequency = 52, silent = T) %>% 
    tsclean() -> x 
#outlier check
train %>% 
  tk_ts(select = M01, start = 2014, frequency = 52, silent = T) -> x 

# ACF and PACF 
x %>% acf2(max.lag = 100, main = '') 

# ADF test 
x %>% adf.test(alternative = 'stationary') 

# estimating difference order 
ndiffs(x)
nsdiffs(x)

# plot time series, ACF, and PACF 
diff(x) %>% ggtsdisplay(plot.type = 'partial', smooth = T, theme = theme_tq()) 

# model 1 
x %>% Arima(order = c(2, 1, 1)) -> fit1 
fit1 %>% residuals() %>% 
    ggtsdisplay(plot.type = 'histogram', smooth = T, theme = theme_tq()) 
fit1 %>% residuals() %>% adf.test(alternative = 'stationary') 

# model 2 
x %>% Arima(order = c(3, 1, 1)) -> fit2 
fit2 %>% residuals() %>% 
    ggtsdisplay(plot.type = 'histogram', smooth = T, theme = theme_tq()) 
fit2 %>% residuals() %>% adf.test(alternative = 'stationary') 

# model 3 
x %>% Arima(order = c(4, 1, 1)) -> fit3 
fit3 %>% residuals() %>% 
    ggtsdisplay(plot.type = 'histogram', smooth = T, theme = theme_tq()) 
fit3 %>% residuals() %>% adf.test(alternative = 'stationary') 

# select best model by summary 
fit1 %>% summary() 
fit2 %>% summary() 
fit3 %>% summary() 

# select best model 
x %>% Arima(order = c(2, 1, 1), seasonal = c(0, 0, 0)) -> fit 
fit %>% summary() 

# analyze residuals 
residuals(fit) %>% adf.test(alternative = 'stationary') 
residuals(fit) %>% 
    ggtsdisplay(plot.type = 'histogram', smooth = T, theme = theme_tq()) 

# predict forecast 
fit %>% forecast(h = 41) -> fc 

# evaluate forecast 
fc %>% accuracy(test$M01) 

# plot forecast 
sw_sweep(fc) %>% 
    ggplot(aes(x = index, y = M01, color = key)) + 
    geom_ribbon(aes(ymin = lo.95, ymax = hi.95), fill = 'skyblue') + 
    geom_ribbon(aes(ymin = lo.80, ymax = hi.80), fill = 'slateblue', alpha = 0.7) + 
    geom_line(size = 1) + 
    labs(title = 'Weekly Sales Volume Forecast', x = '', y = 'Volume', 
         subtitle = 'Pharmaceutical Drugs Time Series') + 
    scale_x_yearmon(n = 6, format = '%Y') + theme_tq() 

# forecasting the rest of 2019 
df %>% 
    tk_ts(select = M01, start = 2014, frequency = 52, silent = T) %>% 
    tsclean() -> x 

# define model 
x %>% Arima(order = c(2, 1, 1), seasonal = c(0, 0, 0)) -> fit 
fit %>% summary() 

# analyze residuals 
residuals(fit) %>% adf.test(alternative = 'stationary') 
residuals(fit) %>% 
    ggtsdisplay(plot.type = 'histogram', smooth = T, theme = theme_tq()) 

# predict forecast 
fit %>% forecast(h = 11) -> fc 

# plot forecast 
sw_sweep(fc) %>% 
    ggplot(aes(x = index, y = M01, color = key)) + 
    geom_ribbon(aes(ymin = lo.95, ymax = hi.95), fill = 'skyblue') + 
    geom_ribbon(aes(ymin = lo.80, ymax = hi.80), fill = 'slateblue', alpha = 0.7) + 
    geom_line(size = 1) + 
    labs(title = 'Pharmaceutical Drug Sales Volume Forecast', x = '', y = 'Volume', 
         subtitle = 'Weekly Sales Volume for M01 Drugs') + 
    scale_x_yearmon(n = 6, format = '%Y') + theme_tq() 

#--------------------------------------------------------------------------------

# N02 EDA and Model Selection 

#--------------------------------------------------------------------------------

# create time series object 
train %>% 
    tk_ts(select = N02, start = 2014, frequency = 52, silent = T) %>% 
    tsclean() -> x 

#outlier
train %>% 
  tk_ts(select = N02, start = 2014, frequency = 52, silent = T) -> x 

# ACF and PACF 
x %>% acf2(max.lag = 100, main = '') 

# ADF test 
x %>% adf.test(alternative = 'stationary') 

# estimating difference order 
ndiffs(x)
nsdiffs(x)

# plot time series, ACF, and PACF (???) 
diff(x, lag = 52) %>% ggtsdisplay(plot.type = 'partial', smooth = T, theme = theme_tq()) 

# model 1 
x %>% Arima(order = c(2, 1, 0), seasonal = c(1, 1, 0)) -> fit1 
fit1 %>% residuals() %>% 
    ggtsdisplay(plot.type = 'histogram', smooth = T, theme = theme_tq()) 
fit1 %>% residuals() %>% adf.test(alternative = 'stationary') 

# model 2 
x %>% Arima(order = c(2, 1, 3), seasonal = c(1, 1, 0)) -> fit2 
fit2 %>% residuals() %>% 
    ggtsdisplay(plot.type = 'histogram', smooth = T, theme = theme_tq()) 
fit2 %>% residuals() %>% adf.test(alternative = 'stationary') 

# model 3 
x %>% Arima(order = c(0, 1, 1), seasonal = c(1, 1, 0)) -> fit3 
fit3 %>% residuals() %>% 
    ggtsdisplay(plot.type = 'histogram', smooth = T, theme = theme_tq()) 
fit3 %>% residuals() %>% adf.test(alternative = 'stationary') 

# select best model by summary 
fit1 %>% summary() 
fit2 %>% summary() 
fit3 %>% summary() 

#--------------------------------------------------------------------------------

# N05 EDA and Model Selection 

#--------------------------------------------------------------------------------

# create time series object 
train %>% 
    tk_ts(select = N05, start = 2014, frequency = 52, silent = T) %>% 
    tsclean() -> x 

#uncleaned series 
train %>% 
  tk_ts(select = N05, start = 2014, frequency = 52, silent = T) -> x 

# ACF and PACF 
x %>% acf2(max.lag = 100, main = '') 

# ADF test 
x %>% adf.test(alternative = 'stationary') 

# estimating difference order 
ndiffs(x)
nsdiffs(x)

# plot time series, ACF, and PACF 
diff(x) %>% ggtsdisplay(plot.type = 'partial', smooth = T, theme = theme_tq()) 

# model 1 
x %>% Arima(order = c(0, 1, 3), seasonal = c(1, 0, 0)) -> fit1 
fit1 %>% residuals() %>% 
    ggtsdisplay(plot.type = 'histogram', smooth = T, theme = theme_tq()) 
fit1 %>% residuals() %>% adf.test(alternative = 'stationary') 

# model 2 
x %>% Arima(order = c(3, 1, 2), seasonal = c(0, 0, 0)) -> fit2 
fit2 %>% residuals() %>% 
    ggtsdisplay(plot.type = 'histogram', smooth = T, theme = theme_tq()) 
fit2 %>% residuals() %>% adf.test(alternative = 'stationary') 

# model 3 
x %>% Arima(order = c(3, 1, 2), seasonal = c(1, 0, 1)) -> fit3 
fit3 %>% residuals() %>% 
    ggtsdisplay(plot.type = 'histogram', smooth = T, theme = theme_tq()) 
fit3 %>% residuals() %>% adf.test(alternative = 'stationary') 

# select best model by summary 
fit1 %>% summary() 
fit2 %>% summary() 
fit3 %>% summary() 

#--------------------------------------------------------------------------------

# R0 EDA and Model Selection 

#--------------------------------------------------------------------------------

# create time series object 
train %>% 
    tk_ts(select = R0, start = 2014, frequency = 52, silent = T) %>% 
    tsclean() -> x 
#outlier

train %>% 
  tk_ts(select = R0, start = 2014, frequency = 52, silent = T) -> x 



# ACF and PACF 
x %>% acf2(max.lag = 100, main = '') 

# ADF test 
x %>% adf.test(alternative = 'stationary') 

# estimating difference order 
ndiffs(x)
nsdiffs(x)

# plot time series, ACF, and PACF 
diff(x) %>% ggtsdisplay(plot.type = 'partial', smooth = T, theme = theme_tq()) 

# model 1 
x %>% Arima(order = c(0, 1, 2), seasonal = c(0, 0, 1)) -> fit1 
fit1 %>% residuals() %>% 
    ggtsdisplay(plot.type = 'histogram', smooth = T, theme = theme_tq()) 
fit1 %>% residuals() %>% adf.test(alternative = 'stationary') 

# model 2 
x %>% Arima(order = c(0, 1, 3), seasonal = c(0, 0, 1)) -> fit2 
fit2 %>% residuals() %>% 
    ggtsdisplay(plot.type = 'histogram', smooth = T, theme = theme_tq()) 
fit2 %>% residuals() %>% adf.test(alternative = 'stationary') 

# model 3 
x %>% Arima(order = c(3, 1, 0), seasonal = c(1, 0, 0)) -> fit3 
fit3 %>% residuals() %>% 
    ggtsdisplay(plot.type = 'histogram', smooth = T, theme = theme_tq()) 
fit3 %>% residuals() %>% adf.test(alternative = 'stationary') 

# select best model by summary 
fit1 %>% summary() 
fit2 %>% summary() 
fit3 %>% summary() 

#--------------------------------------------------------------------------------
















