

#### AusTraits API development ####


# Load packages and austraits data

#install.packages("plumber")
library(plumber)

library(dplyr)
library(tidyr)
library(stringr)
library(readr)
library(purrr)
#install.packages("remotes")
remotes::install_github("traitecoevo/austraits@api")
library(austraits) 
austraits <- load_austraits(path="data/austraits", version="3.0.2")


################################################################################
as_wide_table <- function(austraits){
  
  
  ################################################################################
  # TODO: this updated with next zenodo release
  # Load the trait classification doc - classifies the tissue type and type of trait based on the trait_name data field
  # Exclude this for now -- will be added to definitions file in future release
  # trait_class = read.csv("data-raw/Trait_classifications_v3.csv")
  # trait_class[is.na(trait_class)] = ""
  # trait_class <- trait_class %>% as_tibble()
  # 
  # we only need two extra columns from the trait class table - collapsing two category and other_tags cols and renaming them for clarity
  # x2 <- 
  #   trait_class %>% dplyr::mutate(
  #   trait_category = str_c(category, "; ", other_tags) %>% gsub("; $", "", .)
  # ) %>% 
  #   dplyr::select(trait_name, tissue, trait_category)
  # 
  # Function to collapse columns in sites and contexts into single column
  process_table <- function(data) {
    
    # worker function called intop worklfow below
    # for a df, combine all column names and values
    collapse_cols <- function(data) {
      
      if(ncol(data) ==0) return(NA_character_)
      
      data %>% purrr::imap_dfr(~ sprintf("%s='%s'",.y,.x)) %>%
        tidyr::unite("text", sep="; ") %>% dplyr::pull(text)
    }
    
    data %>% 
      tidyr::pivot_wider(names_from = property, values_from = value) %>% 
      tidyr::nest(data=-any_of(c("dataset_id", "site_name", "context_name", "latitude (deg)", "longitude (deg)"))) %>%
      dplyr::mutate(site = purrr::map_chr(data, collapse_cols)) %>%
      dplyr::select(-data) 
  }
  
  ################################################################################
  # Define and adapt each table in the list of austraits to prepare for the wide table format 
  
  # the trait table needs little prep. Rename the value columns as value
  austraits$traits <- 
    austraits$traits %>% 
    dplyr::rename(c("trait_value" = "value")) 
  
  # The contexts table needs the contexts collapsed to one context name per site
  austraits$contexts <- 
    austraits$contexts %>% 
    dplyr::rename(c("property" = "context_property")) %>%
    split(austraits$contexts$dataset_id) %>%
    purrr::map_dfr(process_table)  %>% 
    dplyr::rename(c("context" = "site"))
  
  # Getting rid of the columns that will soon be deleted in the next austraits release and renaming the description column
  austraits$methods <- 
    austraits$methods %>% 
    #  -----------
  # TODO: this section can be removed for next release
  # Some studies have multiple records per traits. This breaks things when joining
  # For now select first
  dplyr::group_by(dataset_id, trait_name) %>% 
    dplyr::slice(1) %>%
    dplyr:: ungroup() %>%
    #------------
  dplyr::select(-year_collected_start, -year_collected_end) %>% 
    dplyr::rename(c("dataset_description" = "description"))  
  
  # collapse into one column
  austraits$sites <- 
    austraits$sites %>% 
    dplyr::filter(value!="unkown") %>% 
    # next line is a fix -- one dataset in 3.0.2 has value "site_name"
    dplyr::mutate(site_property = gsub("site_name", "name", site_property)) %>%
    dplyr::rename(c("property" = "site_property")) %>%
    split(., .$dataset_id) %>%
    purrr::map_dfr(process_table)
  
  # rename source data field to reflect the APC/APNI name matching process better
  austraits$taxa <- 
    austraits$taxa %>% 
    dplyr::rename(c("taxonNameValidation" = "source"))
  
  austraits_wide <- 
    austraits$traits %>%
    dplyr::left_join(by=c("dataset_id", "context_name"), austraits$contexts) %>%
    dplyr::left_join(by=c("dataset_id", "site_name"), austraits$sites) %>%
    dplyr::left_join(by=c("dataset_id", "trait_name"), austraits$methods) %>%
    dplyr::left_join(by=c("taxon_name"), austraits$taxa) %>%
    
    # reorder the names to be more intuitive
    dplyr::select(
      
      # The most useful (if you are filtering for just one taxon_name)
      dataset_id, observation_id, trait_name, taxon_name, trait_value, unit, 
      value_type, replicates, 
      # tissue, trait_category,  # Add after new zenodo release
      
      # More stuff you can filter on
      date, collection_type, sample_age_class, sampling_strategy, 
      
      #stuff relating to sites
      `latitude (deg)`, `longitude (deg)`, site_name, site,
      
      #stuff relating to contexts and methods
      context_name, context, methods, original_name,
      
      #the citations
      dataset_description, source_primary_citation, source_secondary_citation,
      
      #the taxa details
      taxonomicStatus, taxonDistribution, 
      taxonRank, genus, family, acceptedNameUsageID, 
      scientificNameAuthorship, ccAttributionIRI
    )
  
  austraits_wide
}

austraits_wide = as_wide_table(austraits)

################################################################################
# We are ready for the API

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


#* @get /trait-table

function(res, taxa = "", type_of_value = "", age_class = "", tissue_type = "", has_coordinates = "", field_only = ""){
  
  
  y = austraits_wide  %>%
    filter(taxon_name == taxa) #%>% 
    #filter(str_detect(value_type, type_of_value)) %>%
    #filter(str_detect(sample_age_class, age_class)) %>% 
    #filter(str_detect(tissue, tissue_type))
 
    if (has_coordinates == "Yes"){
      y1 = y %>% filter(is.na(`longitude (deg)`) == F)
    }else{
      y1 = y
    }
  
    if (field_only == "Yes"){
     y2 = y1 %>%  filter(str_detect(collection_type, "field"))
    }else{
      y2 = y1
    }
  
  return(y2)

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
  x1 = austraits_wide %>% filter(taxon_name %in% taxa) 
  #x1[is.na(x1)] = ""
  #prepare the filename
  filename = str_c("AusTraits_", gsub("\\:", "-", Sys.time()), ".csv")
  filename = gsub(" ", "_", filename)
  
  #convert the subset to a csv
  as_attachment(x1, filename)
  
}



################################################################################
# 6. Return the entire Austraits database
#* @apiDescription 
#* Return the entire Austraits database as a json object
#* @get /all-austraits

austraits_all = function(){
return(austraits_wide)
 
}
  


#################################################################################
# Overlay the yaml file over the top for ease of testing and clarity. 
# Anything in the yaml file that conflicts with the specifications above will overwrite them. 

#* @plumber

 function(pr){
   pr %>% 
    pr_set_api_spec(yaml::read_yaml("API examples v1.yml"))
 }

