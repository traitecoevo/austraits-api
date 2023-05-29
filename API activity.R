
#####################################################
#### Steps to complete before running this code #####
#####################################################

# How to download and analyse the API activity log

# Go to this tutorial and follow the steps to download and login with your email and openstack password. https://tutorials.rc.nectar.org.au/object-storage/04-object-storage-cyberduck

# Download all files

# Set the working directory to the location of the files

library(tidyverse)


x = list.files(pattern = ".txt")

out = data.frame()

for (i in 1:length(x)){
  
  test = data.frame(hit = readLines(x[i]))
  
  out = rbind(out, test)
}



out$date = str_extract(out$hit,"^([0-9]){4}-([0-9]){2}-([0-9]){2}")

out$query = str_extract(out$hit, "trait-summary|trait-count|taxa-autocomplete|trait-data|download-taxon-data|trait-autocomplete|taxa-list|trait-list")

hits = out %>% filter(!is.na(query)) %>% filter(query == "trait-summary") %>% group_by(date, query) %>% summarise(n = n())

library(lubridate)

hits$date = ymd(hits$date)

plot(hits$n)

hits = hits %>% filter(n > 500)

# Outputs
mean(hits$n)

length(which(out$query == "download-taxon-data"))
