## ----setup, include=FALSE-------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## -------------------------------------------------------------------------------------------
library(tidyverse)
fires <- read.csv('~/classroom/csci385/California_Fire_Perimeters_(all).csv')
names(fires) <- tolower(names(fires))
fires <- rename(fires, year=year_)
fires <- select(fires, -objectid)
sum(is.na(fires))


## -------------------------------------------------------------------------------------------
years <- fires$year
range(years, na.rm = TRUE)
table(years)


## -------------------------------------------------------------------------------------------
states <- fires$state 
unique(states)
table(states)
select(fires, year, state) %>% filter(state != "CA" & year > 2000)
select(fires, year, state, agency) %>% filter(state != 'CA' & agency == 'CDF')
outside_state_fire <- select(fires, state, report_ac) %>% filter(state != 'CA')
summary(outside_state_fire$report_ac)


## -------------------------------------------------------------------------------------------
agencies <- fires$agency
unique(agencies)
fires %>% count(agency)
agency_year <- fires %>% group_by(agency, year) %>% count()
arrange(agency_year, desc(agency_year$n)) %>% group_by(agency) %>% slice(1:3)


## -------------------------------------------------------------------------------------------
units <- fires$unit_id 
unique(units) %>% length() 
fires %>% group_by(unit_id) %>% count() %>% arrange(desc(n)) %>% 
  filter(n > 10) %>% nrow()
unit_fires <- fires %>% group_by(unit_id, year) %>% drop_na() %>% count()
unit_fires <- rename(unit_fires, responses = n)
unit_fires %>% arrange(desc(responses)) %>% group_by(unit_id) %>% slice(1:1)%>%
  arrange(desc(responses))
fires %>% group_by(unit_id) %>% top_n(1, report_ac) %>% 
  select(year, unit_id, report_ac) %>% arrange(desc(report_ac))


## -------------------------------------------------------------------------------------------
fires %>% group_by(fire_name, report_ac) %>% arrange(desc(report_ac))
fires %>% group_by(fire_name) %>% count() %>% arrange(desc(n))
fires %>% filter(fire_name == "AMERICAN")
fire_names <- fires %>% filter(fire_name != " " | fire_name == "UNKNOWN") %>%
  group_by(fire_name) %>% drop_na()
total_duplicates <- 0 
#This takes a while to compile, answer is 172 duplicates
# for (name in unique(fire_names$fire_name)) { 
#   same_fire_name_df <- fire_names %>% filter(fire_name == name)
#   duplicates <- select(same_fire_name_df, year, agency, unit_id, fire_name) %>%
#     duplicated()
#   if(TRUE %in% duplicates) { 
#     #Need plus one because first instance of duplicate is false
#     total_duplicates <- total_duplicates + sum(duplicates == TRUE) + 1 
#   }
# }
# total_duplicates
fires %>% filter(fire_name == "CAMP" & year == 2018)


## -------------------------------------------------------------------------------------------
inc_num <- fires$inc_num
paste("Length", length(inc_num))
print(paste("NA values", sum(is.na(inc_num))))
paste("Missing values", sum(inc_num == "" | inc_num == " "))
paste("Unique values", length(unique(inc_num)))
#Note this is basically what I tried to do in above chunk. Improvement! 
fires %>% group_by(inc_num) %>% summarise(n=n()) %>% arrange(desc(n)) 
fires %>% select(inc_num, year, alarm_date) %>% filter(year == "2020") %>%
  arrange(alarm_date) %>% head()


## -------------------------------------------------------------------------------------------
library(lubridate)

fires %>% group_by(alarm_date) %>% summarise(n=n()) %>% 
  arrange(desc(n)) %>% filter(alarm_date != "") %>% head()
#View(fires %>% filter(alarm_date == "2018/11/08 00:00:00+00"))
fires %>% select(alarm_date, report_ac, fire_name) %>% 
  arrange(desc(report_ac)) %>% head()
fires %>% group_by(alarm_date) %>% summarise(n=n()) %>% 
  arrange(desc(n)) %>% filter(alarm_date != "") %>% head()

#Busiest months by number of reports 
dates <- fires$alarm_date[fires$alarm_date != ""] %>%
  parse_datetime("%Y/%m/%d %H:%M:%S %z", na = c("", "NA"))
dates <- dates[!is.na(dates)]
months <- format(dates, "%m")
table(months)

#4 methods to search by date
dates <- fires$alarm_date[fires$alarm_date != ""]
dates <- dates[!is.na(dates)]
fourth_july <- dates[substr(dates, 6, 10) == "06/04"]
mask <- fires$alarm_date %>% str_detect(regex(".06/04.", dotall=TRUE))
fourth_july <- fires$alarm_date[mask]
read <- fires %>% select(alarm_date) %>% ymd_hms(tz="UTC")
new_dates <- fires 
new_dates$alarm_date <- as.Date(new_dates$alarm_date, "%Y/%m/%d")
#new_dates %>% group_by(alarm_date, fire_name) %>% arrange(desc(alarm_date))
#print(dates[dates > "2005/06/04" & dates < "2015/06/04"])



## -------------------------------------------------------------------------------------------
#Start with one instance 
camp_fire <- fires %>% 
  filter(fire_name == "CAMP" & alarm_date == "2018/11/08 00:00:00+00")
camp_fire
format <- "%Y/%m/%d %H:%M:%S %z"
difftime(camp_fire$alarm_date, camp_fire$cont_date, format)
times <- select(fires, alarm_date, cont_date, fire_name) %>%
  filter(!is.na(alarm_date) & alarm_date != "") %>% 
  filter(!is.na(cont_date) & cont_date != "") 
fire_duration <- difftime(times$cont_date, times$alarm_date, format, units="days")
fire_duration <- fire_duration[fire_duration >= 0 ]
cat("min = ", min(fire_duration), "max = ", max(fire_duration),
    "median = ", median(fire_duration), 
    "quantile = ", quantile(fire_duration), "\n")


## -------------------------------------------------------------------------------------------
sort(table(fires$cause), decreasing=TRUE)
df <- fires %>% group_by(year, cause) %>% filter(year >= "2000")
sort(table(df$cause), decreasing=TRUE)
#fires %>% group_by(cause) %>% filter(cause == 12)
select(fires, cause, report_ac, fire_name) %>% filter(cause == 7) %>%
  arrange(desc(report_ac)) %>% head() 


## -------------------------------------------------------------------------------------------
comments <- select(fires, comments) %>% filter(comments != "" & comments != " ")
comments <- drop_na(comments)
paste("max comment", comments %>% deparse() %>% nchar() %>% table() %>% max())
mask <- deparse(comments) %>% nchar() == 260


## -------------------------------------------------------------------------------------------
fire_size <- fires$report_ac 
fire_size <- fire_size[!is.na(fire_size)]
summary(fire_size)
fires %>% filter(report_ac == 0) %>% group_by(year, report_ac) %>% 
  unique() %>% count() %>% arrange(desc(n))  %>% head()
#paste("Total acres burned ", sum(fire_size))
fires %>% filter(!is.na(report_ac)) %>% group_by(year) %>% 
  summarise(report_ac = sum(report_ac)) %>%
  arrange(desc(report_ac)) %>% head()


## -------------------------------------------------------------------------------------------
df <- select(fires, report_ac, gis_acres, year) %>% drop_na 
summary(df$report_ac)
summary(df$gis_acres)
differences <- abs(df$report_ac - df$gis_acres)
paste("Differnce of over 100 acres", length(differences[differences > 100]))
differences_year <- df %>% filter(abs(report_ac - gis_acres) > 100) %>%
  count(year) %>% arrange(desc(n)) %>% head() 
rename(differences_year, "Num large discrepencies" = "n")


## -------------------------------------------------------------------------------------------
table(fires$c_method) %>% sort(decreasing=TRUE)

get_mode <- function(vec) { 
  frequencies <- table(vec) %>% sort(decreasing=TRUE)
  strtoi(names(frequencies)[1])
} 

c_method_yr <- fires %>% filter(!is.na(c_method) & c_method != 8) %>%
  group_by(year) %>%
  summarise(c_method = get_mode(c_method))
year <- fires %>% filter(year == "2020")



## -------------------------------------------------------------------------------------------
wildfire <- fires %>% filter(objective == 1)
resource <- fires %>% filter(objective == 2) 
cat("Wildfire count ", length(wildfire$objective), 
    "WFU ", length(resource$objective))
select(resource, fire_name, report_ac, objective) %>% 
  arrange(desc(report_ac)) %>% head()


## -------------------------------------------------------------------------------------------
library(gridExtra)
library(grid)

# Lets start with data from 2019 
fire_2020 <- fires %>% filter(year == "2019") 

# Let's see fires 300 acres greater, and their causes 
large_fires <- select(fire_2020, report_ac, cause, agency) %>% 
  filter(report_ac > 300)
ggplot(large_fires, aes(x=cause, y=report_ac, color=agency)) + 
  geom_point() + 
  ggtitle("Large Fires in 2019")

# Number of fires and acres burned by cause 
causes <- fire_2020 %>% filter(!is.na(report_ac)) %>% 
  group_by(cause) %>% 
  summarise(report_ac = sum(report_ac))
num_fires <- fire_2020 %>%
  group_by(cause) %>% 
  summarise(count = n()) %>% 
  drop_na()  %>% 
  filter(cause != 16)
causes <- mutate(causes, fires = num_fires$count)
# I tried really hard to get this to display the meanings of the numbers 
num<-ggplot(data=causes,  aes(x=cause, y=fires, fill=as.factor(cause))) + 
  geom_bar(stat='identity') + 
  xlab('Cause') + ylab('Total Fires') 
# This doesn't display all causes because value of unknown so large 
size<-ggplot(data=causes, aes(x=cause, y=report_ac,fill=as.factor(cause))) +
  geom_bar(stat='identity') + 
  xlab('Cause') + ylab('Acres burned')  
grid.arrange(num, size, nrow = 1, top="Size and Number of Fires by Cause 2019")

# Number of fires by unit size 
# Try to use facet split for this 
fire_size <- fire_2020 %>% filter(!is.na(report_ac)) %>% 
  select(year, report_ac, cause)
a <- filter(fire_size, report_ac <= .25)
b <- filter(fire_size, report_ac > .25 & report_ac <= 9.99)
c <- filter(fire_size, report_ac > 9.99 & report_ac <= 99)
d <- filter(fire_size, report_ac > 99 & report_ac <= 299)
e <- filter(fire_size, report_ac > 299 & report_ac <= 999)
f <- filter(fire_size, report_ac > 999 & report_ac <= 4999)
g <- filter(fire_size, report_ac > 4999)
# Merge and Join take WAY too long
# Want three columns, year, unit, and report_ac 
get_unit <- function(vec, letter) { 
  unit <- c() 
  i <- 0
  while (i < length(vec)) { 
    unit <- append(unit, letter, after=length(unit)) 
    i <- i + 1
  }
  unit
}
df<-data.frame(year = c(a$year, b$year, c$year, d$year, e$year, f$year, g$year),
               unit = c(get_unit(a$year, "A"), get_unit(b$year, "B"),
                        get_unit(c$year, "C"), get_unit(d$year, "D"),
                        get_unit(e$year, "E"), get_unit(f$year, "F"),
                        get_unit(g$year, "G")), 
               report_ac = c(a$report_ac, b$report_ac, c$report_ac, d$report_ac,
                             e$report_ac, f$report_ac, g$report_ac), 
               cause = c(a$cause, b$cause, c$cause, d$cause, e$cause, f$cause,
                         g$cause))
ggplot(data=df, aes(x=cause, y=report_ac, color=cause)) +
  geom_point() + 
  xlab('Cause') + ylab('Fire perimeter') + 
  facet_wrap(~unit, scales="free_y") + 
  ggtitle("Fire Perimeters and Causes by Unit")


## -------------------------------------------------------------------------------------------
# Number of acres burned 2010-2020
decade_fires <- fires %>% select(year, gis_acres) %>%
  filter(year >= "2010" & !is.na(gis_acres)) 
decade_fires_sum <- decade_fires %>% group_by(year) %>% 
  summarise(gis_acres = sum(gis_acres))
decade_fires_sum$gis_acres <- as.integer(decade_fires_sum$gis_acres)
ggplot(decade_fires_sum, aes(x=year, y=gis_acres)) + 
  geom_bar(stat="identity", fill="gray70") + 
  geom_text(aes(label=gis_acres)) + 
  xlab("Years") + ylab("Acres Burned") + 
  ggtitle("Number of Acres Burned 2010-2020") 

# Number of fires 2010-2020
# Note the requirements for fires to be reported in this data set from 2010 on
num_fires <- fires %>% filter(year >= "2010") %>% 
  group_by(year) %>% 
  summarise(num=n())
num_fires$year <- as.character(num_fires$year)
ggplot(num_fires, aes(x=year, y=num)) + 
  geom_bar(stat="identity", fill="gray70") + 
  geom_text(aes(label=num)) + 
  xlab("Years") + ylab("Number of Fires") + 
  ggtitle("Number of Fires 2010-2020") 

# Fires by Cause 2020, 2019, 5 year avg 
# Cause, num fires, year 
cause2020 <- fires %>%
  select(cause, year) %>% 
  filter(year == "2020") %>% 
  group_by(cause) %>% 
  summarise(num_fires = n(), year="2020")
cause2019 <- fires %>%
  select(cause, year) %>% 
  filter(year == "2019") %>% 
  group_by(cause) %>% 
  summarise(num_fires = n(), year="2019")
cause_avg <- fires %>% 
  select(cause, year) %>% 
  filter(year >= "2015") %>% 
  group_by(cause) %>% 
  summarise(num_fires = (n()/5), year="5 Yr.Avg")
df <- bind_rows(cause2020, cause2019)
df <- bind_rows(df, cause_avg)
# I would like to add labels of what causes are but that's no fun with ggplot
ggplot(df, aes(fill=year, y=num_fires, x=cause)) + 
  geom_bar(position="dodge", stat="identity") + 
  xlab("Causes") + ylab("Number of Fires") + 
  ggtitle("Fires by Cause \n 2019, 2020, and 5 Year Average") + 
  theme(legend.position="bottom")


## -------------------------------------------------------------------------------------------
library(lubridate)
# Arson occurrence and Acres Burned last decade 
arson_num <- fires %>% 
  select(cause, year) %>% 
  filter(cause == 7 & year >= "2010") %>% 
  group_by(year) %>% 
  summarise(fires = n())
arson_size <- fires %>% 
  select(cause, year, report_ac) %>% 
  filter(cause == 7 & !is.na(report_ac) & year >= "2010") %>% 
  group_by(year) %>% 
  summarise(report_ac = sum(report_ac))
arson <- merge(arson_num, arson_size)
arson$year <- as.character(arson$year)
arson$report_ac <- as.integer(arson$report_ac)
ggplot(arson, aes(x=year, y=fires)) + 
  geom_bar(stat="identity", fill="red") + 
  geom_text(aes(label=fires)) + 
  xlab("Year") + ylab("Number of Fires") + 
  ggtitle("Arson Fire Occurrence 2010-2020") 
ggplot(arson, aes(x=year, y=report_ac)) + 
  geom_bar(stat="identity", fill="red") + 
  geom_text(aes(label=report_ac)) + 
  xlab("Year") + ylab("Acres Burned") + 
  ggtitle("Arson Fire Acres Burned 2010-2020")

# Number of fires and acres burned by month for 2020 
df <- fires %>% 
  filter(year == "2020")
df$alarm_date <- as.Date(df$alarm_date, "%Y/%m/%d")
num_fires_month <- df %>% 
  group_by(month = floor_date(alarm_date, "month")) %>%
  summarize(fires = n()) %>% 
  filter(month < "2021-01-01")
acres_month <- df %>% 
  group_by(month = floor_date(alarm_date, "month")) %>%
  filter(!is.na(report_ac)) %>% 
  summarize(report_ac = sum(report_ac)) %>% 
  filter(month < "2021-01-01")
monthly_fires <- merge(num_fires_month, acres_month, all=TRUE)
monthly_fires$month <- as.character(monthly_fires$month)
monthly_fires$report_ac <- as.integer(monthly_fires$report_ac)

ggplot(monthly_fires, aes(x=month, y=fires)) + 
  geom_bar(stat="identity", fill="red") + 
  geom_text(aes(label=fires)) + 
  xlab("Year") + ylab("Number of Fires") + 
  ggtitle("Monthly Fire Occurrence 2020") 
ggplot(monthly_fires, aes(x=month, y=report_ac)) + 
  geom_bar(stat="identity", fill="red") + 
  geom_text(aes(label=report_ac)) + 
  xlab("Year") + ylab("Acres Burned") + 
  ggtitle("Acres Burned by Month 2010")


## -------------------------------------------------------------------------------------------
# My brother has been CAL FIRE in Santa Clara County (SCU) since 2020 
cal_fire <- fires %>% 
  select(agency, unit_id) %>% 
  filter(agency == "CDF") 
cal_fire_units <- unique(cal_fire$unit_id)

fires %>% filter(year == "2020" & unit_id == "SCU")


## -------------------------------------------------------------------------------------------
#This is my first attempt here at tidying data. Only up from here. 
#Lets see if median fire is increasing every year 
fire_yr <- fires %>% filter(report_ac != "" & !is.na(report_ac)) %>% 
  group_by(year) %>% 
  summarise(median = median(report_ac), max = max(report_ac), 
            sum=sum(report_ac), fires = n())
plot <- ggplot(fire_yr, aes(x=year))
tidy <- fire_yr %>% gather("stat", "value", -year)
median <- plot + geom_point(aes(y=median)) + geom_smooth(aes(y=median)) 
max <- plot + geom_point(aes(y=max)) + geom_smooth(aes(y = max))
sum <- plot + geom_point(aes(y=sum)) + geom_smooth(aes(y=sum))
box <- ggplot(tidy, aes(x=year, y=value)) + 
  geom_boxplot(aes(color = stat)) + 
  facet_wrap(~stat, scales="free_y") + 
  theme(axis.text.x = element_text(angle = 90))
grid.arrange(median, max, sum, box, nrow=2, 
             top=textGrob("Yearly Acres Burned", gp=gpar(fontsize=15,font=1)))


## -------------------------------------------------------------------------------------------
#Collection methods 
first_yr_c_method <- fires %>% select(year, c_method) %>% 
  drop_na() %>% arrange(year) %>% head()
total_c <- table(fires$c_method)
labels <- c("GPS Ground", "GPS Air", "Infared", "Other Imagery", 
            "Photo Interpretation", "Hand Drawn", "Mixed Collection Tools",
            "Unknown")
df <- data.frame(Method = labels, count = as.vector(total_c))
df
pie <- ggplot(df, aes(x="", y=count, fill=Method)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) + theme_void()
pie + ggtitle(paste("Collection Methods Used Since", first_yr_c_method$year[1]))
#How collection methods have changed over the years 
ggplot(c_method_yr, aes(x = year, y=c_method, color=as.factor(c_method))) +
  geom_point() + 
  ggtitle("Most Popular Collection Method by Year") + 
  scale_color_manual(labels = c("GPS Ground", "Other Imagery", "Hand Drawn"),
                     values = c("red", "blue", "green")) 

