
# Load packages
library(plumber)

#install.packages("remotes")
remotes::install_github("traitecoevo/austraits", build_vignettes = TRUE)

library(austraits) 
austraits <- load_austraits()



#* Health Check - Is the API running?
#* @get /health-check

status = function(){
  list(
    status = "All is good",
    Time = Sys.time()
  )
}

###################################################


#* @apiDescription Return a count of unique traits for any given canonical species name, e.g. Eucalyptus saligna 

#* @param taxa
#* @get /trait_count

function(taxa){
  
  
  x = austraits$traits %>% 
    #get the rows where the taxon_name field is Eucalyptus saligna
    filter(taxon_name == taxa) %>% 
    #select and count the number of unique trait_name values
    select(trait_name) %>% 
    unique() %>% count() %>% unlist()
  
  print(paste0(taxa, " has data for ", x, " different traits in AusTraits"))
  
  
}

#######################################################################


#* @apiDescription Return a count of unique species for any given canonical species name, e.g. leaf_length

#* @param trait
#* @get /taxa_count


function(trait){
  
  
  # Counts of unique taxa (species) existing for Leaf length
  x  = austraits$traits %>% 
    
    filter(trait_name == trait) %>% 
    
    select(taxon_name) %>% 
    
    unique() %>% count() %>% unlist()
  
  print(paste0("There are ", x, " species that have data for the ", trait, " trait."))
  
  
}

########################################################

#* @apiDescription Return a table of summarised trait data for a given taxa

#* @param taxa
#* @get /trait_summary

function(taxa){
  
  summary = austraits$traits %>% 
    filter(taxon_name == taxa) %>% 
    select(trait_name, value, unit) 
  
  categorical = summary %>% filter(is.na(unit)) %>% select(-unit) %>% filter(duplicated(trait_name)) %>% unique() %>% mutate(unit = NA)
  
  continuous = summary %>% filter(!is.na(unit)) %>% mutate(value = as.numeric(value)) %>% group_by(trait_name) %>%  summarise(value = mean(value), unit = unique(unit)) 
  
  continuous$value = signif(continuous$value, 3)
  
  rbind(categorical, continuous)
}

######################################################################

#* @apiDescription Return a full data table for a species in csv form
#* @serializer csv
#* @param 
#* @get /trait_table

function(taxa, collection){
  
  #### 3. Make wide tables
  
  x1 = austraits$traits %>% filter(taxon_name == taxa) 
  
  # get the methods table of austraits
  x2 = austraits$methods
  
  # merge the eucalypts saligna traits with the methods by the field names they have in common
  x3 = merge(x1, x2, by = intersect(names(x1), names(x2))) 
  
  # select the fields for the wide table format
  x3 %>% select(dataset_id, taxon_name, value, unit, value_type, date, collection_type) %>% filter(str_detect(collection_type, collection))
  
  
}




# #* @plumber 
# function(pr){
#   pr %>% 
#     pr_set_api_spec(yaml::read_yaml("AusTraits_API_test_1.yml"))
# }
