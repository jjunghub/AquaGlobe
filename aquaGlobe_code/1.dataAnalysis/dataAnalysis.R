library(tidyverse)
library(stringr)
library(gridExtra)
#### Production Dataset ####
# " ... " = Data not available; unobtainable; data not separately available but included in another category 
# " - " = Nil or zero 
# " 0 " = More than zero but less than half the unit used 
# " nei " = Not elsewhere included 
# " F " = FAO estimate from available sources of information
#############################
## Data read & pre-processing
#dat <- read_csv("/Users/kimj0a/Documents/Project/GlobalDatathon/data/production_new.csv", 
dat <- read_csv("/Users/kimj0a/Documents/Project/GlobalDatathon/data/world_fishes.csv", 
                na = c("..."), locale=locale(encoding='UTF-8')) %>% as.data.frame()
dat <- dat[1:(nrow(dat)-3),]   #가장 밑에 Total summary 관련 3줄 빼줌
dat[dat=='-'] <- '0'
dat[dat=='0 0'] <- '0.05'
dat <- sapply(dat, function(dat) gsub(' F', '', dat))    #sapply : apply a function over a list or vector
dat <- as.data.frame(dat)
for(i in 6:ncol(dat)){
  dat[,i] <- as.numeric(as.character(dat[,i]))
}
dat$`Species (ASFIS species)` <- as.factor(dat$`Species (ASFIS species)`)

#### world_fishes.csv INFO. ########
# number of rows : 24169 rows
# ton 으로 measure 되는 종들만. exclude Quantity(number).
# countries : 총 247개국
# species : 총 2166 개종
# production area : 26 개 구역
# production sources : Capture production / Aquaculture (freshwater) / Aquaculture (marine) / Aquaculture (brackishwater)
# measured years : 1950 - 2015
# ------- 각 국의 근해에서 이루어진 어업만 -------
# 124169 -> 10709 lines. 2166 종
###################################


# 국가별 양식이 이루어지는 바다 영역
country_nearsea <- dat %>% group_by(`Country (Country)`) %>% 
  filter(`Production source (Detailed production source)` %in% c('Aquaculture (marine)')) %>%
  select(`Production area (FAO major fishing area)`) %>%
  unique() 

#### 나라별 위도 경도 찾기 : 필요없음. country_geoinfo.csv 참조####
country <- lon <- lat <- geo_info <- c()
country <- c("Cambodia", "Indonesia", "Korea, Republic of", "Malaysia", "Myanmar", "Philippines", "Singapore",
             "Thailand", "Viet Nam", "Bangladesh", "Brunei Darussalam", "Lao People's Dem. Rep.")

country <- as.character(notMathced$`Country (Country)`)
library(ggmap)
for(i in c(1:length(country))){
  geo <- geocode(country[i])   
  lon <- c(lon, as.numeric(geo[1]))
  lat <- c(lat, as.numeric(geo[2])) 
}
geo_info_dismatched <- data.frame(country, lon, lat)
geo_info_dismatched$lon[2] <- 22
geo_info_dismatched$lat[2] <- 41.833
geo_info_dismatched$lon[7] <- 35
geo_info_dismatched$lat[7] <- -6
geo_info_dismatched$lon[9] <- -56.3333
geo_info_dismatched$lat[9] <- 46.8333
geo_info_dismatched$lon[12] <- -158.25
geo_info_dismatched$lat[12] <- 6.9167
geo_info_dismatched$lon[13] <- 17
geo_info_dismatched$lat[13] <- 25
geo_info_dismatched$lon[17] <- 114.1667
geo_info_dismatched$lat[17] <- 22.25

write_csv(geo_info_dismatched, "/Users/kimj0a/Documents/Project/GlobalDatathon/geo_info_dismatched.csv")
#########

# 근해에서 이루어진 어업 (양식 + 근해 어획) 만  # 각 종에 대한 각 나라의 근해에서 이루어진 : 총 712 종 (ASEAN + Korea)
dat_nearsea <- dat %>% group_by(`Species (ASFIS species)`, `Country (Country)`, `Measure (Measure)`) %>% 
  filter((`Country (Country)`=="Cambodia" & `Production area (FAO major fishing area)` == "Pacific, Western Central") |
           (`Country (Country)`=="Indonesia" & `Production area (FAO major fishing area)` == "Pacific, Western Central")|
           (`Country (Country)`=="Korea, Republic of" & `Production area (FAO major fishing area)` == "Pacific, Northwest")|
           (`Country (Country)`=="Malaysia" & `Production area (FAO major fishing area)` == "Indian Ocean, Eastern")|
           (`Country (Country)`=="Malaysia" & `Production area (FAO major fishing area)` == "Pacific, Western Central")|
           (`Country (Country)`=="Myanmar" & `Production area (FAO major fishing area)` == "Indian Ocean, Eastern")|
           (`Country (Country)`=="Philippines" & `Production area (FAO major fishing area)` == "Pacific, Western Central")|
           (`Country (Country)`=="Singapore" & `Production area (FAO major fishing area)` == "Pacific, Western Central")|
           (`Country (Country)`=="Thailand" & `Production area (FAO major fishing area)` == "Indian Ocean, Eastern")|
           (`Country (Country)`=="Thailand" & `Production area (FAO major fishing area)` == "Pacific, Western Central")|
           (`Country (Country)`=="Viet Nam" & `Production area (FAO major fishing area)` == "Pacific, Western Central")|
           (`Country (Country)`=="Bangladesh" & `Production area (FAO major fishing area)` == "Indian Ocean, Eastern")|
           (`Country (Country)`=="Brunei Darussalam" & `Production area (FAO major fishing area)` == "Pacific, Western Central")|
           (`Country (Country)`=="Lao People's Dem. Rep." & `Production area (FAO major fishing area)` == "Asia - Inland waters")) %>%
  select(as.character(c(1950:2015))) %>% summarise_all(sum, na.rm = T)

# 각 국의 근해에서 이루어진 어업만. 124169 -> 10709 lines. 나라,종 별로 합치면 8765 lines
filter_dat <- data.frame()
for (i in 1:nrow(country_nearsea)){
  new <- dat %>% filter(`Country (Country)` == country_nearsea$`Country (Country)`[i] & 
                    `Production area (FAO major fishing area)` == country_nearsea$`Production area (FAO major fishing area)`[i]) 
  filter_dat <- rbind(filter_dat, new)
}
dat_nearsea <- filter_dat %>% group_by(`Species (ASFIS species)`, `Country (Country)`) %>%
  select(as.character(c(1950:2015))) %>% summarise_all(sum, na.rm = T)

dat_nearsea <- as.data.frame(dat_nearsea)
## 어종 상태 Classify
dat_nearsea$start <- dat_nearsea$max <- 
  dat_nearsea$status <- dat_nearsea$grad <- 
  dat_nearsea$pvalue <- dat_nearsea$recent <- NA
# 처음 0 or Na가 아닌 값 처음 등장하는 연도
for (i in 1:nrow(dat_nearsea)){
  for(j in c(1950:2015)){
    if(!is.na(dat_nearsea[i,as.character(j)]) & dat_nearsea[i,as.character(j)]>0){
      dat_nearsea$start[i] <- j
      break;
    }
  }
}

X <- c(1990:2015)
for (i in 1:nrow(dat_nearsea)){
  # 각 품종에 대한 Max 값 찾기
  dat_nearsea$max[i] <- max(dat_nearsea[i,as.character(c(1950:2015))], na.rm = T)
  # 회귀식 찾기 : p-value 0.05 기준으로 Coeff + 이면 : increase, - 이면 : decrease
  Y <- as.numeric(dat_nearsea[i,as.character(X)])
  if(sum(!is.na(Y)) >= 2){
    fit <- lm(Y~X, na.action = na.exclude) 
    coeff <- coef(summary(fit))[2,1]
    pvalue <- ifelse(is.na(coef(summary(fit))[2,4]), 1, coef(summary(fit))[2,4]) # 모두 값이 0일때, Nan
    if(pvalue <= 0.05) {
      ifelse(coeff > 0, dat_nearsea$status[i] <- "increase", dat_nearsea$status[i] <- "decrease")
      dat_nearsea$grad[i] <- coeff
      dat_nearsea$pvalue[i] <- pvalue
    }
  }
  # 첫 등장한 년도가 2005년도 이후면 : 새로운 출현
  if(!is.na(dat_nearsea$start[i])){
    if(dat_nearsea$start[i] > 2005) dat_nearsea$status[i] <- "new"
  }
  # 최근 5년의 평균 값이 Max 의 1/10 이하이면 : 위험
  dat_nearsea$recent[i] <- mean(as.numeric(dat_nearsea[i, as.character(c(2013,2014,2015))]), na.rm = T)
  if(!is.nan(dat_nearsea$recent[i])){
    if(dat_nearsea$max[i] / 10 > dat_nearsea$recent[i]) dat_nearsea$status[i] <- "dangerous"
  }
}

# 각 데이터의 위도경도 정보 추가
geo_info <- read_csv("/Users/kimj0a/Documents/Project/GlobalDatathon/data/country_geoinfo.csv", 
                na = c("..."), locale=locale(encoding='UTF-8')) %>% as.data.frame()
colnames(geo_info)[1] <- 'country'
colnames(geo_info)[5] <- 'lat'
colnames(geo_info)[6] <- 'lon'
dat_nearsea$lat <- dat_nearsea$lng <- NA

geo_info_dismatched <- read_csv("/Users/kimj0a/Documents/Project/GlobalDatathon/geo_info_dismatched.csv", 
                     na = c("..."), locale=locale(encoding='UTF-8')) %>% as.data.frame()

dat_nearsea$`Country (Country)` <- as.character(dat_nearsea$`Country (Country)`)

for(i in c(1:nrow(dat_nearsea))){
  dat_nearsea$lng[i] <- geo_info %>% filter(country == dat_nearsea$`Country (Country)`[i]) %>%
    select(lon) %>% as.numeric()
  dat_nearsea$lat[i] <- geo_info %>% filter(country == dat_nearsea$`Country (Country)`[i]) %>%
    select(lat) %>% as.numeric()
  if(is.na(dat_nearsea$lng[i])) dat_nearsea$lng[i]<- geo_info_dismatched %>% filter(country == dat_nearsea$`Country (Country)`[i]) %>%
    select(lon) %>% as.numeric()
  if(is.na(dat_nearsea$lat[i])) dat_nearsea$lat[i] <- geo_info_dismatched %>% filter(country == dat_nearsea$`Country (Country)`[i]) %>%
    select(lat) %>% as.numeric()
}
# Un. Sov. Soc. Rep. 이 나라는 뭔지 모르겠다.. 위도 경도 못찾은건 제외
dat_nearsea <- dat_nearsea %>% filter(!is.na(lat))
write_csv(as.data.frame(dat_nearsea), "/Users/kimj0a/Documents/Project/GlobalDatathon/worldDB_wide.csv")

# Mean. duration 추가
for(i in c(1:nrow(dat_nearsea))){
  has_value <- as.numeric(dat_nearsea[i,as.character(c(1950:2015))])
  has_value <- has_value[has_value>0]
  dat_nearsea$mean[i] <- mean(has_value)
  dat_nearsea$duration[i] <- sum(dat_nearsea[i,as.character(c(1950:2015))] > 0, na.rm = T)
}


# 각 종에 대해 관련있는 나라의 수
dat_nearsea$`Species (ASFIS species)` <- as.character(dat_nearsea$`Species (ASFIS species)`)
tb <- table(dat_nearsea$`Species (ASFIS species)`) %>% sort(decreasing = T)
write_csv(as.data.frame(tb), "/Users/kimj0a/Documents/Project/GlobalDatathon/worldRelated_countries.csv")


################################ 여기서 부터 시각화 shiny 코드에 넣기 #########
dat_nearsea <- read_csv("/Users/kimj0a/Documents/Project/GlobalDatathon/DB_wide.csv") %>%
  as.data.frame()

# 특정 종에 대한 wide format -> long format
name <- "Giant tiger prawn"    # 원하는 종 입력
test <- dat_nearsea %>% filter(`Species (ASFIS species)` == name)
test_long<- test[1:nrow(test),] %>% gather(key = "year", value = "Amount", as.character(c(1950:2015)))
test_long$year <- as.numeric(test_long$year)  

## Plot
test_long %>% ggplot(aes(x = year, y = log(Amount), colour = status)) +
  geom_line() +
  facet_wrap(~ `Country (Country)`) +
  ggtitle(name)

test_long %>% ggplot(aes(x = year, y = Amount, fill = `Country (Country)`)) +
  geom_area() +
  ggtitle(name)

## Visulalizing
library("leaflet")

# 특정 년도 데이터만 뽑아서 subdat에 넣어줌
view_year <- 2015   # 원하는 년도 입력
subdat <- test_long %>% filter(year == view_year) %>% as.data.frame()

####나라의 경계 polyline 얻기 : 용량 작게 하는 효율적인 방법?? ###
#library("ggmap")
#library("googleVis")
#library("rgdal")
# library(raster)
# library(rgeos)
#If name is 'alt' or 'GADM' you must provide a 'country=' argument. Countries are specified by their 3 letter ISO codes. Use getData('ISO3') to see these codes. In the case of GADM you must also provide the level of administrative subdivision (0=country, 1=first level subdivision). In the case of alt you can set 'mask' to FALSE.
# adm <- getData('GADM', country='Korea, Republic of', level=0)
# kor <- gSimplify(adm, tol=0.01, topologyPreserve=T)
# kor.spdf <- SpatialPolygonsDataFrame( kor, tmp.id.df );

# suggestion : wrld_simpl ?
# library(maptools)
# data(wrld_simpl)
# plot(wrld_simpl)
# test <- wrld_simpl
########

# Create continuous color
pal <- colorNumeric(
  palette = "RdYlBu",
  na.color = "#808080",
  domain = subdat$Amount)

## Bubble chart & colored country polygons 
# radius : 보통 sqrt() 취하는데 값차이가 심해서, 어떻게 잡을껀지 생각좀 해봐야할듯. 
leaflet(subdat) %>%
  #addTiles("http://server.arcgisonline.com/ArcGIS/rest/services/Ocean_Basemap/MapServer/tile/{z}/{y}/{x}") %>%
  addProviderTiles("CartoDB.Positron") %>%
  addCircleMarkers(popup = paste0("Amout : ",subdat$Amount), radius = log(subdat$Amount), 
                   weight = 0, fillOpacity = 0.5, fillColor = ~pal(subdat$Amount))  
  #addPolygons(data = adm, stroke = FALSE, smoothFactor = 0.5, fillOpacity = 0.2, color = ~pal(subdat$grad)) 


#addTiles("http://{s}.basemaps.cartocdn.com/light_nolabels/{z}/{x}/{y}.png")

## colored country polygons leaflet말고 geochart이용
G9 <- gvisGeoChart(subdat,
             locationvar="Country (Country)", colorvar="grad",
             options=list(region="142",
                          width=500, height=400,
                          colorAxis="{colors:['#ff0066', '#0099cc']}"
             ))    
plot(G9)

## Network chart가 문제인데



##############################################################################.FIN.#####

# 이외 끄적인 코드들
test_long %>% filter(`Country (Country)`=="Philippines") %>% 
  ggplot(aes(x = year, y = Amount, colour = `Species (ASFIS species)`)) +
  geom_line()


X <- c(1990:2015)
Y <- as.numeric(test[as.character(X)])

fit <- lm(Y~X) 
par(mfrow = c(2,2))
plot(fit, las = 1)

summ <- summary(fit)
fit.coeff


subdat <- dat %>% filter(start >= 2005)

## Data filtering
subdat <- dat %>% filter(`Country (Country)` == 'Korea, Republic of') %>%
  filter(`Production source (Detailed production source)` == 'Capture production') %>%
  filter(`Production area (FAO major fishing area)` == 'Pacific, Northwest') %>%
  filter(start >= 2000)

subdat <- dat %>% filter(`Country (Country)` == 'Korea, Republic of') %>%
  filter(`Production area (FAO major fishing area)` == 'Pacific, Northwest')

subdat <- dat %>% filter(`Production area (FAO major fishing area)` %in% 
                           c('Pacific, Northwest', 'Pacific, Western Central', 'Indian Ocean, Eastern'))

subdat <- dat %>% filter(`Country (Country)` == 'Indonesia') %>%
  filter(`Production source (Detailed production source)` == 'Capture production') %>%
  filter(`Production area (FAO major fishing area)` == 'Pacific, Western Central')

subdat <- dat %>% filter(`Country (Country)` == 'Singapore') %>%
  filter(`Production source (Detailed production source)` == 'Capture production') %>%
  filter(`Production area (FAO major fishing area)` == 'Pacific, Western Central')

country <- unique(dat$`Country (Country)`)

## EDA
# 1950 ~ 2015  
# 1 : 1950~1955  2: 1955~1960 ...
start <- 1950
end <- 2015
step <- 5
for (i in c(1:((end-start)/step))){
  colrange <- c((start + (i-1)*step) : (start + i*step -1))
  colname <- paste0("Range",i)
  subdat$new <- subdat %>% select(as.character(colrange)) %>%
    apply(1, mean, na.rm=T)
  colnames(subdat)[ncol(subdat)] <- as.character(i)
}
## wide format to long format
subdat_long <- subdat[1:nrow(subdat),] %>% gather(key = "year", value = "Amount", as.character(c(1950:2015)))
subdat_long$year <- as.numeric(subdat_long$year)  

subdat_long <- dat_nearsea[1:nrow(dat_nearsea),] %>% gather(key = "year", value = "Amount", as.character(c(1950:2015)))
subdat_long$year <- as.numeric(subdat_long$year)  

# 5년 단위씩 묶어서 평균
subdat_long_blur <- subdat[1:nrow(subdat),] %>% gather(key = "range", value = "Mean", as.character(c(1:((end-start)/step))))
subdat_long_blur$range <- as.numeric(subdat_long_blur$range)  

## Plot
subdat_long %>% filter(`Species (ASFIS species)` == "Alaska pollock(=Walleye poll.)") %>%
  ggplot(aes(x = year, y = Amount, colour = `Species (ASFIS species)`)) +
  geom_line()

# 특정 종 선택
summary <- subdat_long %>% group_by(`Species (ASFIS species)`, `Country (Country)`) %>% 
  select(Amount) %>% 
  summarise_all(funs(mean, median, max, min), na.rm = T) %>% 
  arrange(desc(mean))
top20 <- summary$`Species (ASFIS species)`[2:20]

# 1년단위
plot1 <- subdat_long %>% filter(`Species (ASFIS species)` %in% top20) %>%
  ggplot(aes(x = year, y = Amount, colour = `Species (ASFIS species)`)) +
  geom_line() +  
  scale_x_continuous(breaks=seq(1950, 2017, 5)) 

subdat_long %>% 
  ggplot(aes(x = year, y = Amount, colour = `Species (ASFIS species)`)) +
  geom_line() +  
  scale_x_continuous(breaks=seq(1950, 2017, 5)) 

# 5년 단위
plot2 <- subdat_long_blur %>% filter(`Species (ASFIS species)` %in% top20) %>%
  ggplot(aes(x = range, y = Mean, colour = `Species (ASFIS species)`)) +
  geom_line() +  
  scale_x_continuous(breaks=seq(1, 13, 1)) 


grid.arrange(plot1, plot2, nrow=2, ncol=1)

test <- dat %>% filter(`Species (ASFIS species)`=="Japanese Spanish mackerel") %>% View

####
test <- subdat[1,]
subdat_long <- test %>% gather(key = "year", value = "Amount", as.character(c(1950:2015)))
test <- test[1,6:ncol(test)]
test[is.na(test)] <- 0
plot(as.vector(test))

subdat_long$`Species (ASFIS species)`

subdat_long %>% filter(`Species (ASFIS species)` == "1")

ggplot(data = subdat_long, aes(x = year, y = Amount, fill = `Species (ASFIS species)`)) + 
  geom_point()

test <- subdat[!is.na(subdat[1,6:ncol(subdat)])]
plot(test)


subdat[,6:ncol(subdat)]
subdat[,6:ncol(subdat)] <- as.numeric(as.character(subdat[,6:ncol(subdat)]))


subdat[,6:ncol(subdat)] <- as.numeric(subdat$`2015`)

dat$`2000`


subdat <- subdat %>% arrange(desc(`2015`))




sapply(subdat, function(subdat) gsub('-', '0', subdat))
sapply(subdat, function(subdat) gsub('0 0', '0.5', subdat))


gsub(' F', '', subdat[,])

subdat <- subdat %>% str_replace('-','0')

subdat[subdat=='-'] <- '0'
subdat[subdat=='0 0'] <- '0.5'





gsub('F','T',a)

a <- c('f123', '1234', 'fff')
gsub('f','t',a)

grep('f',a)
a[grep('f',a)]

a<- gsub('F','T',a)

a %in% 'F'

subdat[subdat %in% 'F']


subdat[]

ifelse(subdat[,]=='-', subdat[,]<-'0', subdat)
tag_raw <- tag_raw %>% 
  str_replace('^/\\*\\*/newsCallback1\\(', '') %>% 
  str_replace('\\);$', '')


tail(dat)

setwd("/Users/kimj0a/Documents/Project/GlobalDatathon/data/")



library(tidyverse)
dat <- read_csv("12coun_fishs.csv")

plot(as.numeric(dat[,6:ncol(dat)]))


test <- as.data.frame(dat[,6:ncol(dat)])


subdat <- dat %>% filter(`Species (ASFIS species)`=="Alaska pollock(=Walleye poll.)")

ifelse(subdat[,]=="...",subdat[,]=0)

test <- as.numeric(subdat[2,6:ncol(subdat)])
plot(test, type='l')

plot(test)

