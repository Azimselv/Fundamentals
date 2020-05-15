library(xml2)
install.packages("XML")
library(XML)
install.packages("RCurl")
library(RCurl)
library(rvest)
library(tidyverse)
library(magrittr)
install.packages("factoextra")
library(factoextra)
install.packages("quantmod")
library(quantmod)
library(ggplot2)
library(dplyr)
install.packages("varhandle")
library(varhandle)

DAX <- read.table("DAX.csv", header = TRUE, sep = ",",stringsAsFactors=FALSE)

DAX <- DAX[150:253,]


https://en.wikipedia.org/wiki/COVID-19_pandemic_by_country_and_territory

fcase <- read_html("https://en.wikipedia.org/wiki/COVID-19_pandemic_by_country_and_territory")

tables <- html_table(fcase, fill = TRUE)

cases <- tables[[4]]

cases.new <- cases %>% 
  mutate(`Country or territory` = strsplit(as.character(`Country or territory`), " .  ")) %>% 
  unnest(`Country or territory`)

cases.new <- cases.new %>% 
  rename(
    First_case = Date,
    Country = `Country or territory`
  )

cases.new$First_case <- paste0(cases.new$First_case, " 2020")
cases.new$First_case <- gsub("December 2020", "December 2019", cases.new$First_case)
cases.new$First_case <- gsub("December", "12", cases.new$First_case)
cases.new$First_case <- gsub("January", "1", cases.new$First_case)
cases.new$First_case <- gsub("February", "2", cases.new$First_case)
cases.new$First_case <- gsub("March", "3", cases.new$First_case)
cases.new$First_case <- gsub("April", "4", cases.new$First_case)
cases.new$First_case <- gsub("May", "5", cases.new$First_case)
cases.new$Country <- gsub("\\[.*?\\]", "", cases.new$Country)
cases.new$First_case <- as.Date(cases.new$First_case ,format="%d %m %Y")

save(cases.new,file="First.Cases.RData")

ggplot(DAX, aes(x = Date, y = Close, group = 1)) + geom_line(color = "darkblue") +
  ggtitle("DAX") +
  theme(plot.title = element_text(hjust = 0.5), text = element_text(size=7)) + 
  geom_vline(data = subset(DAX, Date == "2020-03-16"), # filter data source
             aes(xintercept = Date),
             size = 2, colour = "red")


tomatch <- c("Germany","Italy")

result <- filter(cases.new, grepl(paste(tomatch, collapse="|"), Country))

Corona <- cases.new %>% inner_join(travel, by = c("Country" = "Country"))
Corona2 <- Corona %>% left_join(lockdown, by = c("Country" = "Countries"))
Corona2 <- Corona2[c(-6:-14,-22),]

save(Corona2, file= "Core.Dataset.RData")

