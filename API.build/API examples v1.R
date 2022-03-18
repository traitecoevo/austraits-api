

#### AusTraits API development ####


# Load packages and austraits data

#install.packages("plumber")
library(plumber)

library(dplyr)
library(tidyr)
library(stringr)
library(readr)

#install.packages("remotes")
remotes::install_github("traitecoevo/austraits@api", upgrades="never")
library(austraits) 
austraits <- load_austraits(path="data/austraits", version="3.0.2")


################################################################################
# Test the API is running by printing the time and the "All is good" message

#* Health Check - Is the API running?
#* @get /health-check

status = function(){
  list(
    status = "All is good",
    Time = Sys.time()
  )
}


################################################################################
# Log some information about the incoming request

# The @filter function in r plumber allows some action to be completed 
# before the incoming request is passed on (using plumber::forward()) to the relevant @get function. 
# This example logs some information about the incoming request, the time, the path and the remote address 
# The @filter function is also a way to implement an authentication step before data can be accessed.

#* Log some information about the incoming request
#* @filter logger
function(req){
  cat(as.character(Sys.time()), "-",
      req$REQUEST_METHOD, req$PATH_INFO, "-",
      req$HTTP_USER_AGENT, "@", req$REMOTE_ADDR, "\n")
  
  plumber::forward()
}

################################################################################
# 1. count the number of traits per taxa

#* @apiDescription 
#* Return a count of unique traits for any given taxa from the taxon_name data field in AusTraits., e.g. Eucalyptus saligna 

#* @param taxa e.g. Angophora costata
#* @get /trait-count

function(taxa = ""){
  
  if (taxa == "") {
    
    x = austraits$traits %>%
      select(trait_name) %>% 
      distinct() %>% count() %>% unlist() %>% as.integer()
    
    print(paste0(taxa, "There are ", x, " different traits in AusTraits"))
    
  }else{
    
  x = austraits$traits %>% 
    #get the rows where the taxon_name field is equal to the entered value
    filter(taxon_name == taxa) %>% 
    #select and count the number of unique trait_name values
    select(trait_name) %>% 
    distinct() %>% count() %>% unlist() %>% as.integer()
  
  if (x != 0){
    
  print(paste0(taxa, " has data for ", x, " different traits in AusTraits"))
  
  }else{
    
    print("No data can be found for this taxon name. Check the spelling and try again")
  }
 }
}


################################################################################
# 2. Count the number of taxa per trait

#* @apiDescription Possible values for traits are found at http://traitecoevo.github.io/austraits.build/articles/austraits_database_structure.html
#* Return a count of unique species for any given trait name in the trait_name data field of AusTraits.
#* @param trait e.g. leaf_length
#* @get /taxa-count

function(trait = ""){
  
  if (trait == "") {
    
    print("No trait name has been entered")
    
  }else{

  
  # Counts of unique taxa (species) for the trait value entered
  x  = austraits$traits %>% 
    
    filter(trait_name == trait) %>% 
    
    select(taxon_name) %>% 
    
    distinct() %>% count() %>% unlist()
  
  
  if (x != 0){
    
    print(paste0("There are ", x, " species that have data for the ", trait, " trait."))
    
  }else{
    
    print("No data can be found for this trait name. Check for existing trait names at http://traitecoevo.github.io/austraits.build/articles/Trait_definitions.html")
    
   }
  }
 }



################################################################################
# 3. Present mean values for each trait associated with a taxa

#* @apiDescription 
#* Return a table of summarised trait data for a given taxa from the taxon_name data field in AusTraits

#* @param taxa e.g. Angophora costata
#* @get /trait-summary

function(taxa = ""){
  
  if (taxa == "") {
    
    print("No taxon name has been entered")
    
  }else{
  
    # subset the data to the desired taxon name and select the trait name, value and unit columns
  summary = austraits$traits %>% 
    filter(taxon_name == taxa) %>% 
    select(trait_name, value, unit) 
  
  # split the data into categorical variables 
  categorical = summary %>% filter(is.na(unit))  %>% filter(duplicated(trait_name)) %>% distinct() 
  
  # and continuous variables
  continuous = summary %>% filter(!is.na(unit)) %>% mutate(value = as.numeric(value)) %>% group_by(trait_name) %>%  summarise(value = mean(value), unit = unique(unit)) 
  
  # reduce to 3 significant figures
  continuous$value = signif(continuous$value, 3)
  
  #stack them
  x = rbind(categorical, continuous)
  
  # only print the data if data exists
  if (nrow(x) != 0){
  
  print(x)
    
}else{
  
  print("No data can be found for this taxon name. Check the spelling and try again")
  
   }
  }
}
################################################################################
# 4. Returns a table of the combined traits and methods tables for a given taxa
# Possible filters are listed.


#* @apiDescription 
#* Return a full data table for given taxa from the taxon_name data field in AusTraits.
#* @param taxa:character e.g. Angophora costata
#* @param collection:character e.g. field, glasshouse
#* @param type_of_value:character e.g. mean, min,
#* @get /trait-table

function(res, taxa = "", type_of_collection = "", type_of_value = ""){
  
  
  # filter the data to the desired taxon name
  x1 = austraits$traits %>% filter(taxon_name %in% taxa) 
  
  # get the methods table of austraits
  x2 = austraits$methods
  
  # merge the traits table with the methods table by the field names they have in common
  x3 = merge(x1, x2, by = intersect(names(x1), names(x2))) 
  
  # select the fields for the wide table format
  x3 %>% select(dataset_id, taxon_name, value, unit, value_type, date, collection_type) %>%
         filter(str_detect(collection_type, type_of_collection)) %>% 
         filter(str_detect(value_type, type_of_value))

}

################################################################################
# 5. Post a list of species names and return a full data table of multiple species as a csv file

#* @apiDescription 
#* Return a full data table for given taxa for multiple species names as a csv file
#* @serializer csv
#* @post /trait-table-download-csv

function(req, res){
  
  # make taxa object = the taxa_list inside the request body
  taxa = req$body$taxa_list

  
  #subset
  x1 = austraits$traits %>% filter(taxon_name %in% taxa) 
  
  # get the methods table of austraits
  x2 = austraits$methods
  
  # merge the eucalypts saligna traits with the methods by the field names they have in common
  x3 = merge(x1, x2, by = intersect(names(x1), names(x2))) 
  
  # select the fields for the wide table format
  x4 = x3 %>% select(dataset_id, taxon_name, value, unit, value_type, date, collection_type)
  
  #prepare the filename
  filename = str_c("AusTraits_", gsub("\\:", "-", Sys.time()), ".csv")
  filename = gsub(" ", "_", filename)
  
  #convert the subset to a csv
  as_attachment(x4, filename)
  
}


################################################################################
# Overlay the yaml file over the top for ease of testing and clarity. 
# Anything in the yaml file that conflicts with the specifications above will overwrite them. 

#* @plumber

 function(pr){
   pr %>% 
    pr_set_api_spec(yaml::read_yaml("API examples v1.yml"))
 }
