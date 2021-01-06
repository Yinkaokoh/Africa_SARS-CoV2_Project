# Africa_SARS-CoV2_Project
#Research on COVID-19 in Africa with collaborations from Nigeria Ghana and USA
#This is my R Script for SARS-CoV2 Research on 1st COVID-19 wave
#6th January 2021
#SARS-CoV2 Project
#Written on 1st January 2021 to 
#The data used are all as at 4th September 2020
#The data were downloaded from worldometer.com

###Load the packages that will be needed for the project
library(maptools)
library(RColorBrewer)
library(maps)
library(mapdata)
library(readxl)
library(ggplot2)
library(qwraps2)
library(dplyr)
library(gridExtra)
library(ggcorrplot)
library(ggpubr)
library(gridExtra)
library(vcd)
library(tidyr)
library(maps)
library(mapdata)
library(scatterpie)
library(ggmap)
library(mapproj)

### Read in the data
cases <- read_excel("C:/Users/YinkaOkoh/Desktop/Bioinformatics_Data/SARS-CoV2_Project_2021/SARS-CoV2_Project_2021/World_SARS-CoV2_Cases_040920.xlsx")
lineages <- read_excel("C:/Users/YinkaOkoh/Desktop/Bioinformatics_Data/SARS-CoV2_Project_2021/SARS-CoV2_Project_2021/World_SARS-CoV2_Lineages.xlsx")

#Load data containing African countries to get a list of African Countries
africa <- read_excel("C:/Users/YinkaOkoh/Desktop/Bioinformatics_Data/SARS-CoV2_Project_2021/SARS-CoV2_Project_2021/SARSCOV2_African_Data_Update.xlsx")


#Convert to data frames
cases <- as.data.frame(cases)
lineages <- as.data.frame(lineages)
africa <- as.data.frame(africa)

#list African countries (Western Sahara was removed because it was not in the data sent)
africa_countries <- africa %>% select(region, Population...3, Popn) %>% filter(region != "Western Sahara")

#add population to the cases and add a new column infected_per_100000
cases_with_popn <- left_join(cases, africa_countries, by = 'region')
cases_with_popn<- cases_with_popn %>% mutate(infected_per_100000 = (Confirmed/Population...3)*100000)


##This to subset African region from the world using the listed coutries
africa_map <- map_data("world", region = africa_countries$region)

#Get a Centroid for African countries
african_countries_centriods <- africa_map %>% group_by(region) %>% summarise(long = mean(range(long)), lat = mean(range(lat)))

#Select 'Confirmed Cases', 'Active' and 'Recovered' and 'Deaths' data from the world cases
card <- cases_with_popn %>% select(region, Confirmed, Deaths, Recovered, Active, Population...3, Popn, infected_per_100000)

africa_centriods <- left_join(african_countries_centriods, card, by = 'region')



#Join the other SARSCoV2 data to the long format of Map information 
africa_map_details <- left_join(africa_map, card, "region")


###### CASES IN DIFFERENT AFRICAN COUNTRIES ##########
#MAP
#Africa Map showing SARS-CoV2 Cases reported in various countries as at 4th September, 2020
ggplot(africa_map_details) +
  geom_polygon(aes(long, lat, group = group, fill = Confirmed), color = 'black') +
  coord_map("bonne", parameters = 45) +
  labs(title = "Confirmed SARS-CoV2 Cases in Africa", caption = "Source: https://www.worldometers.info/coronavirus") +
  scale_fill_continuous(name = "Cases", low = "white", high = "red", 
                        limits = c(0, 700000), breaks = c(0, 45000, 100000, 300000, 600000)) +
  theme_void()
ggsave(file = "C:/Users/YinkaOkoh/Desktop/Bioinformatics_Data/SARS-CoV2_Project_2021/SARS-CoV2_Project_2021/outputs/SARS-CoV2_Africa_Cases_Map.pdf")   


#POINT CHART
ggplot(africa_centriods, aes(y = Popn, x = Confirmed)) + 
  geom_point(size = 2, color = 'red') + 
  geom_text(label = africa_centriods$region) + theme_bw() +
  labs(title = "Confirmed cases of SARS-CoV2 in Africa",  y = "Population (x million)", x ="Number of SARS-CoV2 Cases", caption = "Source: https://www.worldometers.info/coronavirus")
ggsave(file = "C:/Users/YinkaOkoh/Desktop/Bioinformatics_Data/SARS-CoV2_Project_2021/SARS-CoV2_Project_2021/outputs/Africa_Cases_point.pdf")


#BAR CHART
ggplot(africa_centriods, aes(y = reorder(region, Confirmed), x = Confirmed)) +
  geom_bar(stat = "identity", fill = 'steelblue') +
  labs(title = "Confirmed SARS-CoV-2 cases in African Countries", y = "Country", x = "Number of cases", caption = "Source: https://www.worldometers.info/coronavirus")
ggsave(file = "C:/Users/YinkaOkoh/Desktop/Bioinformatics_Data/SARS-CoV2_Project_2021/SARS-CoV2_Project_2021/outputs/Africa_Cases_bar.pdf")


######## CASES PER 100,000 POPULATION IN AFRICAN COUNTRIES
#MAP
#Africa Map showing number of persons infected with SARS-CoV2 per 100,000 population in various African countries as at 4th September, 2020
ggplot(africa_map_details) +
  geom_polygon(aes(long, lat, group = group, fill = infected_per_100000), color = 'black') +
  coord_map("bonne", parameters = 45) +
  labs(title = "SARS-CoV2 Cases per 100,000 \n Population in African countries", caption = "Source: https://www.worldometers.info/coronavirus") +
  scale_fill_continuous(name = "Cases per 100,000", low = "white", high = "blue", 
                        limits = c(0, 1250), breaks = c(0, 250, 500, 750, 1000, 1,250)) +
  theme_void()
ggsave(file = "C:/Users/YinkaOkoh/Desktop/Bioinformatics_Data/SARS-CoV2_Project_2021/SARS-CoV2_Project_2021/outputs/SARS-CoV2_Africa_per_100k_Map.pdf")   


#POINT
ggplot(africa_map_details, aes(y = Popn, x = infected_per_100000)) + 
  geom_point(size = 2, color = 'blue') + 
  geom_text(label = africa_map_details$region) + theme_bw() +
  labs(title = "SARS-CoV2 cases per 100,000 population in Africa",  y = "Population (x million)", x ="Number of SARS-CoV2 Cases", caption = "Source: https://www.worldometers.info/coronavirus")
ggsave(file = "C:/Users/YinkaOkoh/Desktop/Bioinformatics_Data/SARS-CoV2_Project_2021/SARS-CoV2_Project_2021/outputs/Africa_Cases_per_100000.pdf")


#BAR CHART
ggplot(africa_centriods, aes(y = reorder(region, infected_per_100000), x = infected_per_100000)) +
  geom_bar(stat = "identity", fill = 'orange') +
  labs(title = "SARS-CoV-2 cases per 100,000 population in African Countries", y = "Country", x = "Number of cases", caption = "Source: https://www.worldometers.info/coronavirus")
ggsave(file = "C:/Users/YinkaOkoh/Desktop/Bioinformatics_Data/SARS-CoV2_Project_2021/SARS-CoV2_Project_2021/outputs/Africa_Cases_per_100000_bar.pdf")


####### script for lineages #########
#change continent, region and lineage to factor from character
lineages$continent <- as.factor(lineages$continent)
lineages$region <- as.factor(lineages$region)
lineages$lineage <- as.factor(lineages$lineage)

## Prevalence of each lineage
lineage_Prevalence <- lineages %>% group_by(lineage) %>% summarise(count = n()) %>% mutate(freq = round(count/sum(count), 4)) %>% mutate(percentage = freq * 100)


#group the data on continent basis 
continent_data <- lineages %>% group_by(continent, lineage) %>% summarise(N = n()) %>% mutate(rel_freq = N/sum(N)) %>% mutate(percentage = rel_freq * 100)

#subset each continent
africa_lineages <- lineages %>% filter(continent == "Africa") %>% group_by(lineage) %>% summarise(N = n()) %>% mutate(rel_freq = N/sum(N)) %>% mutate(percentage = rel_freq * 100)
asia_lineages <- lineages %>% filter(continent == "Asia") %>% group_by(lineage) %>% summarise(N = n()) %>% mutate(rel_freq = N/sum(N)) %>% mutate(percentage = rel_freq * 100)
europe_lineages <- lineages %>% filter(continent == "Europe") %>% group_by(lineage) %>% summarise(N = n()) %>% mutate(rel_freq = N/sum(N)) %>% mutate(percentage = rel_freq * 100)
north_america_lineages <- lineages %>% filter(continent == "North_America") %>% group_by(lineage) %>% summarise(N = n()) %>% mutate(rel_freq = N/sum(N)) %>% mutate(percentage = rel_freq * 100)
south_america_lineages <- lineages %>% filter(continent == "South_America") %>% group_by(lineage) %>% summarise(N = n()) %>% mutate(rel_freq = N/sum(N)) %>% mutate(percentage = rel_freq * 100)
oceania_lineages <- lineages %>% filter(continent == "Oceania") %>% group_by(lineage) %>% summarise(N = n()) %>% mutate(rel_freq = N/sum(N)) %>% mutate(percentage = rel_freq * 100)

#lineages bar chart
quantile(lineage_Prevalence$count, probs = c(0.1, 0.25, 0.50, 0.75, 0.90))
#The lineages are too many (80) for barcharts. 0.5Q was found to be 4

mean(lineage_Prevalence$count)
#The mean was found to be 56.53

#To have a good representation lineages with value above 0.5Q (4)
lineage_Prevalence75Q <- lineage_Prevalence %>% filter(count >= 4)
lineage_Prevalence_mean <- lineage_Prevalence %>% filter(count >= 56.53)

#lineage distribution - 75Q
ggplot(lineage_Prevalence75Q, aes(y = reorder(lineage, count), x = count)) +
  geom_bar(stat = "identity", fill = 'red') +
  labs(title = "SARS-CoV-2 lineages distribution (0.75 Quantile)", y = "Lineage", x = "Number of cases", caption = "Source: https://www.worldometers.info/coronavirus")
ggsave(file = "C:/Users/YinkaOkoh/Desktop/Bioinformatics_Data/SARS-CoV2_Project_2021/SARS-CoV2_Project_2021/outputs/Lineages_75Q_bar.pdf")

#Lineage distribution - 75 in percentage
ggplot(lineage_Prevalence75Q, aes(y = reorder(lineage, percentage), x = percentage)) +
  geom_bar(stat = "identity", fill = 'orange') +
  labs(title = "SARS-CoV-2 lineages distribution (0.75 Quantile) in percentage", y = "Lineage", x = "Percentage (%)", caption = "Source: https://www.worldometers.info/coronavirus")
ggsave(file = "C:/Users/YinkaOkoh/Desktop/Bioinformatics_Data/SARS-CoV2_Project_2021/SARS-CoV2_Project_2021/outputs/Lineages_75Q_percenta_bar.pdf")


#Lineage distribution - mean
ggplot(lineage_Prevalence_mean, aes(y = reorder(lineage, count), x = count)) +
  geom_bar(stat = "identity", fill = 'blue') +
  labs(title = "SARS-CoV-2 lineages distribution (mean)", y = "Lineage", x = "Number of cases", caption = "Source: https://www.worldometers.info/coronavirus")
ggsave(file = "C:/Users/YinkaOkoh/Desktop/Bioinformatics_Data/SARS-CoV2_Project_2021/SARS-CoV2_Project_2021/outputs/Lineages_mean_bar.pdf")

#Lineage (mean) Distribution in percentage
ggplot(lineage_Prevalence_mean, aes(y = reorder(lineage, percentage), x = percentage)) +
  geom_bar(stat = "identity", fill = 'orange') +
  labs(title = "SARS-CoV-2 lineages distribution (mean) in percentage", y = "Lineage", x = "Percentage (%)", caption = "Source: https://www.worldometers.info/coronavirus")
ggsave(file = "C:/Users/YinkaOkoh/Desktop/Bioinformatics_Data/SARS-CoV2_Project_2021/SARS-CoV2_Project_2021/outputs/Lineages_mean_percenta_bar.pdf")


##Lineage Distribution across various continents
# There lineages were too many to present across the continents once.
# Filtering and presenting lineages with cases above 26.14 (mean - see below) excluded Oceania
#     which highest case is 15.
# To ensure Oceania is included we filter and presented cases >= 15 and then cases below 15
mean(continent_data$N) # which is 26.14
continent_data_mean <- continent_data %>% filter (N >= 26.14)
continent_lineage_above_15 <-continent_data %>% filter (N >= 15)
continent_lineage_less_15 <-continent_data %>% filter (N < 15)

#Lineages greater than 15 across continents
ggplot(continent_lineage_above_15, aes(y = lineage, x = N)) +
  geom_bar(stat = "identity", fill = 'orange') +
  labs(title = "SARS-CoV-2 lineages (cases above 15) distribution \n across continents", y = "Lineage", x = "Number of cases", caption = "Source: https://www.worldometers.info/coronavirus") +
  facet_wrap(~ continent)
ggsave(file = "C:/Users/YinkaOkoh/Desktop/Bioinformatics_Data/SARS-CoV2_Project_2021/SARS-CoV2_Project_2021/outputs/Lineages_across_continents_above15_bar.pdf")

#Lineages greater than 15 (percentage) across continents
ggplot(continent_lineage_above_15, aes(y = lineage, x = percentage)) +
  geom_bar(stat = "identity", fill = 'steelblue') +
  labs(title = "SARS-CoV-2 lineages (cases above 15) distribution across continents", y = "Lineage", x = "Percent (%)", caption = "Source: https://www.worldometers.info/coronavirus") +
  facet_wrap(~ continent)
ggsave(file = "C:/Users/YinkaOkoh/Desktop/Bioinformatics_Data/SARS-CoV2_Project_2021/SARS-CoV2_Project_2021/outputs/Lineages_across_continents_above15_percent_bar.pdf")


#Lineages less than 15 across continents
ggplot(continent_lineage_less_15, aes(y = lineage, x = N)) +
  geom_bar(stat = "identity", fill = 'orange') +
  labs(title = "SARS-CoV-2 lineages (cases less than 15) distribution across continents", y = "Lineage", x = "Number of cases", caption = "Source: https://www.worldometers.info/coronavirus") +
  facet_wrap(~ continent)
ggsave(file = "C:/Users/YinkaOkoh/Desktop/Bioinformatics_Data/SARS-CoV2_Project_2021/SARS-CoV2_Project_2021/outputs/Lineages_across_continents_less15_bar.pdf")

#Lineages greater than 15 (percentage) across continents
ggplot(continent_lineage_less_15, aes(y = lineage, x = percentage)) +
  geom_bar(stat = "identity", fill = 'steelblue') +
  labs(title = "SARS-CoV-2 lineages (cases less than 15) distribution across continents", y = "Lineage", x = "Percent (%)", caption = "Source: https://www.worldometers.info/coronavirus") +
  facet_wrap(~ continent)
ggsave(file = "C:/Users/YinkaOkoh/Desktop/Bioinformatics_Data/SARS-CoV2_Project_2021/SARS-CoV2_Project_2021/outputs/Lineages_across_continents_less15_percent_bar.pdf")




### Lineage Distribution Continent by Continent
#Lineages in Africa
afr_line <- ggplot(africa_lineages, aes(y = reorder(lineage, N), x = N)) +
  geom_bar(stat = "identity", fill = 'green') +
  labs(title = "SARS-CoV2 Lineages in Africa", y = "Lineage", x = "Number of Cases", caption = "Source: github") 
ggsave(file = "C:/Users/YinkaOkoh/Desktop/Bioinformatics_Data/SARS-CoV2_Project_2021/SARS-CoV2_Project_2021/outputs/Lineagess_in_africa_bar.pdf")

afr_perc <- ggplot(africa_lineages, aes(y = reorder(lineage, percentage), x = percentage)) +
  geom_bar(stat = "identity", fill = 'blue') +
  labs(title = "SARS-CoV2 Lineages (percent) in Africa", y = "Lineage", x = "Percentage", caption = "Source: github") 
ggsave(file = "C:/Users/YinkaOkoh/Desktop/Bioinformatics_Data/SARS-CoV2_Project_2021/SARS-CoV2_Project_2021/outputs/Lineagess_in_africa_percent_bar.pdf")

##Lineages in Asia
asia_line <- ggplot(asia_lineages, aes(y = reorder(lineage, N), x = N)) +
  geom_bar(stat = "identity", fill = 'green') +
  labs(title = "SARS-CoV2 Lineages in Asia", y = "Lineage", x = "Number of Cases", caption = "Source: github") 
ggsave(file = "C:/Users/YinkaOkoh/Desktop/Bioinformatics_Data/SARS-CoV2_Project_2021/SARS-CoV2_Project_2021/outputs/Lineagess_in_asia_bar.pdf")

asia_perc <- ggplot(asia_lineages, aes(y = reorder(lineage, percentage), x = percentage)) +
  geom_bar(stat = "identity", fill = 'blue') +
  labs(title = "SARS-CoV2 Lineages (percent) in Asia", y = "Lineage", x = "Percentage", caption = "Source: github") 
ggsave(file = "C:/Users/YinkaOkoh/Desktop/Bioinformatics_Data/SARS-CoV2_Project_2021/SARS-CoV2_Project_2021/outputs/Lineagess_in_asia_percent_bar.pdf")

##Lineages in Europe
europe_line <- ggplot(europe_lineages, aes(y = reorder(lineage, N), x = N)) +
  geom_bar(stat = "identity", fill = 'green') +
  labs(title = "SARS-CoV2 Lineages in Europe", y = "Lineage", x = "Number of Cases", caption = "Source: github") 
ggsave(file = "C:/Users/YinkaOkoh/Desktop/Bioinformatics_Data/SARS-CoV2_Project_2021/SARS-CoV2_Project_2021/outputs/Lineagess_in_europe_bar.pdf")

europe_perc <- ggplot(europe_lineages, aes(y = reorder(lineage, percentage), x = percentage)) +
  geom_bar(stat = "identity", fill = 'blue') +
  labs(title = "SARS-CoV2 Lineages (percent) in Europe", y = "Lineage", x = "Percentage", caption = "Source: github") 
ggsave(file = "C:/Users/YinkaOkoh/Desktop/Bioinformatics_Data/SARS-CoV2_Project_2021/SARS-CoV2_Project_2021/outputs/Lineagess_in_europe_percent_bar.pdf")

##Lineages in South America
s_ame_line <- ggplot(south_america_lineages, aes(y = reorder(lineage, N), x = N)) +
  geom_bar(stat = "identity", fill = 'green') +
  labs(title = "SARS-CoV2 Lineages in South America", y = "Lineage", x = "Number of Cases", caption = "Source: github") 
ggsave(file = "C:/Users/YinkaOkoh/Desktop/Bioinformatics_Data/SARS-CoV2_Project_2021/SARS-CoV2_Project_2021/outputs/Lineagess_in_s_america_bar.pdf")

s_ame_perc <- ggplot(south_america_lineages, aes(y = reorder(lineage, percentage), x = percentage)) +
  geom_bar(stat = "identity", fill = 'blue') +
  labs(title = "SARS-CoV2 Lineages (percent) in South America", y = "Lineage", x = "Percentage", caption = "Source: github") 
ggsave(file = "C:/Users/YinkaOkoh/Desktop/Bioinformatics_Data/SARS-CoV2_Project_2021/SARS-CoV2_Project_2021/outputs/Lineagess_in_s_america_percent_bar.pdf")

##Lineages in North America
n_ame_line <- ggplot(north_america_lineages, aes(y = reorder(lineage, N), x = N)) +
  geom_bar(stat = "identity", fill = 'green') +
  labs(title = "SARS-CoV2 Lineages in North America", y = "Lineage", x = "Number of Cases", caption = "Source: github") 
ggsave(file = "C:/Users/YinkaOkoh/Desktop/Bioinformatics_Data/SARS-CoV2_Project_2021/SARS-CoV2_Project_2021/outputs/Lineagess_in_n_america_bar.pdf")

n_ame_perc <- ggplot(north_america_lineages, aes(y = reorder(lineage, percentage), x = percentage)) +
  geom_bar(stat = "identity", fill = 'blue') +
  labs(title = "SARS-CoV2 Lineages (percent) in North America", y = "Lineage", x = "Percentage", caption = "Source: github") 
ggsave(file = "C:/Users/YinkaOkoh/Desktop/Bioinformatics_Data/SARS-CoV2_Project_2021/SARS-CoV2_Project_2021/outputs/Lineagess_in_n_america_percent_bar.pdf")

##Lineages in  Oceania
oceania_line <- ggplot(oceania_lineages, aes(y = reorder(lineage, N), x = N)) +
  geom_bar(stat = "identity", fill = 'orange') +
  labs(title = "SARS-CoV2 Lineages in Oceania", y = "Lineage", x = "Number of Cases", caption = "Source: github") 
ggsave(file = "C:/Users/YinkaOkoh/Desktop/Bioinformatics_Data/SARS-CoV2_Project_2021/SARS-CoV2_Project_2021/outputs/Lineagess_in_oceania_bar.pdf")

oceania_perc <- ggplot(oceania_lineages, aes(y = reorder(lineage, percentage), x = percentage)) +
  geom_bar(stat = "identity", fill = 'steelblue') +
  labs(title = "SARS-CoV2 Lineages (percent) in Oceania", y = "Lineage", x = "Percentage", caption = "Source: github") 
ggsave(file = "C:/Users/YinkaOkoh/Desktop/Bioinformatics_Data/SARS-CoV2_Project_2021/SARS-CoV2_Project_2021/outputs/Lineagess_in_oceania_percent_bar.pdf")





#Pie chart of lineage distribution across continents
#ggplot(continent_data, aes(y = N, x = "", fill = lineage)) +
# geom_bar(width = 1, stat = "identity", color = "white") +
#coord_polar("y", start = 0) +
#geom_text(aes(label = N), color = "white") +
#theme_void() +
#facet_wrap(~ continent)

ggsave(file = "C:/Users/YinkaOkoh/Desktop/Bioinformatics_Data/SARS-CoV2_Project_2021/SARS-CoV2_Project_2021/outputs/Lineages_across_continents_percent_bar.pdf")





#ggplot(africa_lineages, aes(x = "", y = N, fill = lineage)) +
# geom_bar(stat = "identity", width = 1) +
#coord_polar("y", start = 0) +
#geom_text(aes(label = paste0(round(percentage, "%"),2), position = position_stack(vjust = 0.5))) +
#labs(x = NULL, y = NULL, fill = NULL, title = "SARS-CoV2 Lineages in Africa") +
#theme_classic()


######################################################################
##THIS IS FOR SARS-CoV2 AFRICAN DATA COLLECTED AFTER 4TH SEPTEMBER, 2020 #######
##IT CONTAINS NUMBER OF TESTS CONDUCTED AND NUMBER OF POSITIVE CASES FOR SOME COUNTRIES
##THIS BECAME NECESSARY WHILE ANALYSIZING BECAUSE WE NEEDED TO KNOW THE
#   OF POSITIVE CASES PER 100 TESTS CONDUCTED
#Read in the data
Africa_SARS_data <- read_excel("C:/Users/YinkaOkoh/Desktop/Bioinformatics_Data/SARS-CoV2_Project_2021/SARS-CoV2_Project_2021/SARSCOV2_African_Data_Update.xlsx")
#Convert it to data frame

Africa_SARS_df <- as.data.frame(Africa_SARS_data)
#This itemizes/lists the African countries
Africa_Countries <- Africa_SARS_df$region
#This to subset African region from the world using the listed coutries
Africa_Map <- map_data("world", region = Africa_Countries)
#Get a Centroid for African countries
African_Countries_Centriods <- Africa_Map %>% group_by(region) %>% summarise(long = mean(range(long)), lat = mean(range(lat)))
Africa_Centriods <- left_join(African_Countries_Centriods, Africa_SARS_df, by = 'region')
#Join the other SARSCoV2 data to the long format of Map information 
Africa_Map_details <- left_join(Africa_Map, Africa_SARS_df, "region")



########Percentage Positive SARS-CoV-2 Cases i.e
### (Cases/Total Tests)*100
##Remove countries with incomplete data
Complete_T_Tests <- Africa_Centriods %>% filter(T_Tests != "NA")

#Map for Percentage Positive Tests 
ggplot(Africa_Map_details) + 
  geom_polygon(aes(long, lat, group = group, fill = Percentage_Test_Positive), color = 'black') +
  coord_map("bonne", parameters = 45)+
  labs(title = "Percentage SARS-CoV2 tests reported positive", caption = "Source: https://www.worldometers.info/coronavirus") +
  scale_fill_continuous(name = "Percentage positive tests", low = "white", high = "red",
                        limits = c(0, 35), breaks = c(0, 5, 10, 15, 20, 25, 30)) +
  theme_void()
ggsave(file = "C:/Users/YinkaOkoh/Desktop/Bioinformatics_Data/SARS-CoV2_Project_2021/SARS-CoV2_Project_2021/outputs/Africa_Percentage_Positive_Map.pdf")

#POINT
ggplot(Africa_Map_details, aes(y = Popn, x = Percentage_Test_Positive)) + 
  geom_point(size = 2, color = 'red') + 
  geom_text(label = Africa_Map_details$region) + theme_bw() +
  labs(title = "Number of positive results out of every 100 SARS-CoV2 Tests", subtitle = "Plot of Population against number of positive results out of every 100 tests", y = "Population", x ="Number of Positive tests", caption = "Source: https://www.worldometers.info/coronavirus")
ggsave(file = "C:/Users/YinkaOkoh/Desktop/Bioinformatics_Data/SARS-CoV2_Project_2021/SARS-CoV2_Project_2021/outputs/Africa_Percentage_Positive_point.pdf")

#BAR CHART
ggplot(Complete_T_Tests, aes(y = reorder(region, Percentage_Test_Positive), x = Percentage_Test_Positive)) +
  geom_bar(stat = "identity") +
  labs(title = "Percentage reported SARS-CoV-2 positive tests", subtitle = "Number of SARS-CoV-2 positive test out of every 100 test reported", y = "Country", x = "Percent", caption = "Source: https://www.worldometers.info/coronavirus")
ggsave(file = "C:/Users/YinkaOkoh/Desktop/Bioinformatics_Data/SARS-CoV2_Project_2021/SARS-CoV2_Project_2021/outputs/Africa_Percent_Positive_bar.pdf")


################################## FOR SCATTER PIE ######################

##This is to drew the size varied pie charts for lineages in Ghana
#Lineages_in_Ghana <- c("A", "B", "B.1", "B.1.1", "B.1.3", "B.1.5", "B.2", "B.2.1")
#Ghana_Map.data <- map_data("world", region = "ghana")
#Ghanian_Map <- ggplot(Ghana_Map.data, aes(x = long, y = lat)) +
# geom_map(map = Ghana_Map.data, aes (map_id = region), fill = NA, color= "black") +
#geom_scatterpie(data = Ghana_RegionsDF, 
#               aes(Long, Lat, group = region, r = sqrt(size/35)),
#              cols = Lineages_in_Ghana,
#             alpha = 0.5) +
#labs(title = "Distribution of SARS-CoV2 lineages across the Regions in Ghana", caption = "Source: GISAID, 2020", fill = NULL) +
#theme_bw() 
#ggsave(file = "C:/Users/YinkaOkoh/Desktop/Bioinformatics_Data/PLOTS/Ghana_Regions_Lineages2.jpeg")


##Attempt to draw variable pie chart for Nigeria lineage diversity
#Ngr_lineages <- c("A", "A.1", "B.1", "B.1.1", "B.1.1.1", "B.1.2", "B.1.22", "B.1.36", "B.1.5", "B.2.1")
#Nigeria_Map.data <- map_data("world", region = "nigeria")
#Nigerian_Map <- ggplot(Nigeria_Map.data, aes(x = long, y = lat)) +
# geom_map(map = Nigeria_Map.data, aes (map_id = region), fill = NA, color= "black") +
#geom_scatterpie(data = Nigeria_StatesDF, 
#               aes(long, lat, group = Area, r = sqrt(Total/55)),
#              cols = Ngr_lineages,
#             alpha = 0.5) +
#labs(title = "Distribution of SARS-CoV2 lineages across the states in Nigeria", caption = "Source: GISAID, 2020", fill = NULL) +
#theme_bw()
#ggsave(file = "C:/Users/YinkaOkoh/Desktop/Bioinformatics_Data/Updated_Nigeria_Plots/Nigeria_States_Lineages2.pdf")








########################## END OF SCATTER PIE ############################






######################################################################################################
#SARSCoV2 reported cases in various African countries
#ggplot(Africa_Map_details) + 
# geom_polygon(aes(long, lat, group = group, fill = SARSCoV2_Cases), color = 'black') +
#coord_map("bonne", parameters = 45) +
#labs(title = "SARS-CoV2 cases in Africa", caption = "Source: https://www.worldometers.info/coronavirus") +
#scale_fill_continuous(name = "Cases", low = "white", high = "red", 
#                       limits = c(0, 800000), breaks = c(0, 45000, 100000, 300000, 600000)) +
#theme_void() 
#ggsave(file = "C:/Users/YinkaOkoh/Desktop/Bioinformatics_Data/SARS-CoV2_Africa_Update/Plots_Tables/Africa_SARSCoV2_Cases_Map.pdf")   
#scale_fill_brewer(palette = "Reds")

#Number of infected persons per million in African countries
#ggplot(Africa_Map_details) + 
# geom_polygon(aes(long, lat, group = group, fill = Cases_Per_Million), color = 'black') +
#coord_map("bonne", parameters = 45)+
#labs(title = "SARS-CoV2 per million of population in African countries", caption = "Source: https://www.worldometers.info/coronavirus") +
#scale_fill_continuous(name = "Cases_Per_Million", low = "white", high = "darkblue",
#                     limits = c(0, 13000), breaks = c(0, 1500, 2500, 5000, 7500, 10000, 15000)) +
#theme_void()
#ggsave(file = "C:/Users/YinkaOkoh/Desktop/Bioinformatics_Data/SARS-CoV2_Africa_Update/Plots_Tables/Africa_Cases_Per_Million_Map.pdf")


####Charts for Africa
#SARSCoV2 Infection per million in African Countries
#ggplot(Africa_Map_details, aes(y = Popn, x = Cases_Per_Million)) + 
# geom_point(size = 2, color = 'red') + 
#geom_text(label = Africa_Map_details$region) + theme_bw() +
#labs(title = "SARS-CoV2 cases per one million population in African countries", subtitle = "Plot of Population against number of persons infected per one million population in Africa", y = "Population", x ="Persons infected per million", caption = "Source: https://www.worldometers.info/coronavirus")
#ggsave(file = "C:/Users/YinkaOkoh/Desktop/Bioinformatics_Data/SARS-CoV2_Africa_Update/Plots_Tables/Africa_Infected_Per_Million_point.pdf")


#SARSCoV2 Infection per 100,000 in African Countries
#ggplot(Africa_Map_details, aes(y = Popn, x = Per_100000_Infected)) + 
# geom_point(size = 2, color = 'blue') + 
#geom_text(label = Africa_Map_details$region) + theme_bw() +
#labs(title = " SARS-CoV2 per hundred thousand population in African Countries", subtitle = "Plot of population against number of reported SARS-CoV-2 cases per hundred thousand population", y = "Population", x ="Persons infected per hundred thousand", caption = "Source: https://www.worldometers.info/coronavirus")
#ggsave(file = "C:/Users/YinkaOkoh/Desktop/Bioinformatics_Data/SARS-CoV2_Africa_Update/Plots_Tables/Africa_Infected_Per_100000_point.pdf")


#Bar chart of SARSCoV2 cases in African countries
#ggplot(Africa_Centriods, aes(y = reorder(region, SARSCoV2_Cases), x = SARSCoV2_Cases)) +
# geom_bar(stat = "identity", fill = 'steelblue') +
#labs(title = "Reported cases of SARS-CoV-2 in African Countries", y = "Country", x = "Number of cases", caption = "Source: https://www.worldometers.info/coronavirus")
#ggsave(file = "C:/Users/YinkaOkoh/Desktop/Bioinformatics_Data/SARS-CoV2_Africa_Update/Plots_Tables/Africa_Cases_bar.pdf")



#ggplot(Africa_SARS_df, aes(y = reorder(region, SARSCoV2_Cases), x =SARSCoV2_Cases)) +
# geom_bar(stat = "identity", fill = "steelblue") +
#labs(title = "Reported SARSCoV2 cases in African Countries", y = "Country", x = "Number of cases", caption = "Source: https://www.worldometers.info/coronavirus")
#ggsave(file = "C:/Users/YinkaOkoh/Desktop/Bioinformatics_Data/SARS-CoV2_Africa_Update/Plots_Tables/Africa_SARSCoV2_Cases_bar.pdf")



#Bar chart of SARSCoV2 infectiion per million in African countries
#ggplot(Africa_Centriods, aes(y = reorder(region, Cases_Per_Million), x = Cases_Per_Million)) +
# geom_bar(stat = "identity") +
#labs(title = "SARS-CoV-2 reported cases per one million in African Countries", y = "Country", x = "Cases per a million persons", caption = "Source: https://www.worldometers.info/coronavirus")
#ggsave(file = "C:/Users/YinkaOkoh/Desktop/Bioinformatics_Data/SARS-CoV2_Africa_Update/Plots_Tables/Africa_Infected_Per_Million_bar.pdf")


#Bar chart of SARSCoV2 infection per 100000
#ggplot(Africa_Centriods, aes(y = reorder(region, Per_100000_Infected), x = Per_100000_Infected)) +
# geom_bar(stat = "identity") +
#labs(title = "SARS-CoV-2 reported cases per hundred thousand population in Africa", y = "Country", x = "Cases per 100,000 persons", caption = "Source: https://www.worldometers.info/coronavirus")
#ggsave(file = "C:/Users/YinkaOkoh/Desktop/Bioinformatics_Data/SARS-CoV2_Africa_Update/Plots_Tables/Africa_Infected_Per_100000_bar.pdf")



##BAR CHART
#ggplot(Africa_Centriods, aes(y = reorder(region, Per_100000_Infected), x = Per_100000_Infected)) +
# geom_bar(stat = "identity") +
#labs(title = "SARS-CoV-2 reported cases per hundred thousand population in Africa", y = "Country", x = "Cases per 100,000 persons", caption = "Source: https://www.worldometers.info/coronavirus")
#ggsave(file = "C:/Users/YinkaOkoh/Desktop/Bioinformatics_Data/SARS-CoV2_Africa_Update/Plots_Tables/Africa_Infected_Per_100000_bar.pdf")

##GEOM POINT
#ggplot(Africa_Map_details, aes(y = Popn, x = Per_100000_Infected)) + 
# geom_point(size = 2, color = 'blue') + 
#geom_text(label = Africa_Map_details$region) + theme_bw() +
#labs(title = " SARS-CoV2 per hundred thousand population in African Countries", subtitle = "Plot of population against number of reported SARS-CoV-2 cases per hundred thousand population", y = "Population", x ="Persons infected per hundred thousand", caption = "Source: https://www.worldometers.info/coronavirus")
#ggsave(file = "C:/Users/YinkaOkoh/Desktop/Bioinformatics_Data/SARS-CoV2_Africa_Update/Plots_Tables/Africa_Infected_Per_100000_point.pdf")

##MAP
#ggplot(Africa_Map_details) + 
# geom_polygon(aes(long, lat, group = group, fill = Cases_Per_Million), color = 'black') +
#coord_map("bonne", parameters = 45)+
#labs(title = "SARS-CoV2 per million of population in African countries", caption = "Source: https://www.worldometers.info/coronavirus") +
#scale_fill_continuous(name = "Cases_Per_Million", low = "white", high = "darkblue",
#                     limits = c(0, 13000), breaks = c(0, 1500, 2500, 5000, 7500, 10000, 15000)) +
#theme_void()
#ggsave(file = "C:/Users/YinkaOkoh/Desktop/Bioinformatics_Data/SARS-CoV2_Africa_Update/Plots_Tables/Africa_Cases_Per_Million_Map.pdf")



#SARSCoV2 Infection per 100,000 in African Countries
#ggplot(Africa_Map_details, aes(y = Popn, x = Per_100000_Infected)) + 
# geom_point(size = 2, color = 'blue') + 
#geom_text(label = Africa_Map_details$region) + theme_bw() +
#labs(title = " SARS-CoV2 per hundred thousand population in African Countries", subtitle = "Plot of population against number of reported SARS-CoV-2 cases per hundred thousand population", y = "Population", x ="Persons infected per hundred thousand", caption = "Source: https://www.worldometers.info/coronavirus")
#ggsave(file = "C:/Users/YinkaOkoh/Desktop/Bioinformatics_Data/SARS-CoV2_Africa_Update/Plots_Tables/Africa_Infected_Per_100000_point.pdf")



#Bar chart of SARSCoV2 infection per 100000
#ggplot(Africa_Centriods, aes(y = reorder(region, Per_100000_Infected), x = Per_100000_Infected)) +
# geom_bar(stat = "identity") +
#labs(title = "SARS-CoV-2 reported cases per hundred thousand population in Africa", y = "Country", x = "Cases per 100,000 persons", caption = "Source: https://www.worldometers.info/coronavirus")
#ggsave(file = "C:/Users/YinkaOkoh/Desktop/Bioinformatics_Data/SARS-CoV2_Africa_Update/Plots_Tables/Africa_Infected_Per_100000_bar.pdf")
