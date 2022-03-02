

#### AusTraits API development ####


# Load packages and austraits data

#install.packages("plumber")
library(plumber)


#install.packages("tidyverse")
library(tidyverse)

#install.packages("remotes")
#remotes::install_github("traitecoevo/austraits", build_vignettes = TRUE)


#install.packages("remotes")
#remotes::install_github("traitecoevo/austraits", build_vignettes = TRUE)
library(austraits) 
austraits <- load_austraits()


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

# The filter function allows some action to be done before the incoming query is passed on (plumber::forward()) to the relevant @get function

#* Log some information about the incoming request
#* @filter logger
function(req){
  cat(as.character(Sys.time()), "-",
      req$REQUEST_METHOD, req$PATH_INFO, "-",
      req$HTTP_USER_AGENT, "@", req$REMOTE_ADDR, "\n")
  plumber::forward()
}






# The @filter function in r plumber allows some action to be completed 
# before the incoming request is passed on (using plumber::forward()) to the relevant @get function. 
# This example logs some information about the incoming request. 
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

#* @param taxa
#* @get /trait_count

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
    
    print("No data can be found for this taxon name")
  }
 }
}


#######################################################################
# 2. count the number of taxa per trait

#* @apiDescription Possible values for traits are found at http://traitecoevo.github.io/austraits.build/articles/austraits_database_structure.html
#* Return a count of unique species for any given trait name in the trait_name data field of AusTraits.
#* @param trait
#* @get /taxa_count

function(trait = ""){
  
  if (trait == "") {
    
    print("No trait name has been entered")
    
  }else{
    
function(trait){
  
  
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
}


########################################################
# 3. Present mean values for each trait associated with a taxa

#* @apiDescription 
#* Return a table of summarised trait data for a given taxa from the taxon_name data field in AusTraits

#* @param taxa
#* @get /trait_summary

function(taxa = ""){
  
  if (taxa == "") {
    
    print("No taxon name has been entered")
    
  }else{
  
  summary = austraits$traits %>% 
    filter(taxon_name == taxa) %>% 
    select(trait_name, value, unit) 
  
  categorical = summary %>% filter(is.na(unit))  %>% filter(duplicated(trait_name)) %>% distinct() 
  
  continuous = summary %>% filter(!is.na(unit)) %>% mutate(value = as.numeric(value)) %>% group_by(trait_name) %>%  summarise(value = mean(value), unit = unique(unit)) 
  
  continuous$value = signif(continuous$value, 3)
  
  x = rbind(categorical, continuous)
  
  if (length(x$value) != 0){
  
  print(x)
    
}else{
  
  print("No data can be found for this taxon name. Check the spelling and try again")
  
   }
  }
}
######################################################################
# 4. Presents a table of the combined traits and methods tables for a given taxa in csv format. 
# Possible filters are listed.
# Add the %in% function 
# 


#* @apiDescription 
#* Return a full data table for given taxa from the taxon_name data field in AusTraits. Leaving the taxa field blank returns the full dataset.
#* @param 
#* @get /trait_table

function(taxa = "", collection = "", type_of_value = ""){
  
  
  #### 3. Make wide tables
  
  x1 = austraits$traits %>% filter(taxon_name %in% taxa) 
  
  # get the methods table of austraits
  x2 = austraits$methods
  
  # merge the eucalypts saligna traits with the methods by the field names they have in common
  x3 = merge(x1, x2, by = intersect(names(x1), names(x2))) 
  
  # select the fields for the wide table format
  x3 %>% select(dataset_id, taxon_name, value, unit, value_type, date, collection_type) %>%
         filter(str_detect(collection_type, collection)) %>% 
         filter(str_detect(value_type, type_of_value))
  
  

}


