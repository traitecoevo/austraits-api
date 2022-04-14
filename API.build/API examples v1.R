

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
remotes::install_github("traitecoevo/austraits")
library(austraits) 
austraits <- load_austraits(path="data/austraits", version = get_version_latest())

austraits_wide = as_wide_table(austraits)

################################################################################
# We are ready for the API

################################################################################
# Test the API is running by printing the time and the "All is good" message

#* Health Check - Is the API running?
#* @get /health-check
#* @head /health-check

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
  ############################ manipulate austraits to prepare for averages #################
    ### to do later ###
    
    data = austraits$traits
    #only select adult plants grown outdoors and not experimental data. Or perhaps a list of set traits that can be applied to the whole species
    
    data$value_type[which(is.na(data$value_type))] = "unknown"
    
  
  ###################### Make the categorical trait summary  ####################
    
  # subset the data to the desired taxon name and get a vector of the available categorical traits
  cat_traits = data %>% filter(taxon_name == taxa) %>% filter(is.na(unit)) %>% select(trait_name) %>% unique()
  
  #create a blank dataframe
  output = data.frame()
  
  # for each of the categorical traits...
  for (i in 1:length(cat_traits$trait_name)){
  
  # get austraits data for this taxa and the trait, remove NA values and make a table of the counts for each value
  x = data %>% filter(taxon_name == taxa & trait_name == cat_traits$trait_name[i] & is.na(value)==F) %>% select(value) %>% table() %>% as.data.frame()
  
  # make a character string made up of each trait value, followed by the count in brackets and separated by ";"
  y = paste0(str_c(x$., " (", x$Freq, ")"), collapse = "; ")
  
  # make a row of data made up of the taxon name, the trait name and the trait value character above (y). The units will be blank.
  z = data.frame(taxon_name = taxa, trait_name = cat_traits$trait_name[i], trait_values = y, units = "")
  
  #glue it to the dataframe
  output = rbind(output, z)
  }
  
  ###################### Make the numeric trait summary  ####################
  
  num_traits = austraits$traits %>% filter(taxon_name == taxa) %>% filter(!is.na(unit)) %>% select(trait_name, unit) %>% unique()
  
  #create a blank dataframe
  output1 = data.frame()
  
  # for each of the categorical traits...
  for (i in 1:length(num_traits$trait_name)){
    
  # get the data for each numeric trait, group by the value type and find the mean
  x = austraits$traits %>% filter(taxon_name == taxa & trait_name == num_traits$trait_name[i] & is.na(value)==F) %>% 
      mutate(value = as.numeric(value)) %>% 
      group_by(value_type) %>% summarise(mean = mean(value), n = n())
    
  # collapse each column to create just one summary column
  y = x %>% mutate(summary = str_c(x$value_type,": ", signif(x$mean, 2), " (", signif(x$n, 2), ")")) #%>% select(value_type, summary) %>%
    
  # divide each summary value into either min, mean or max and collapse common values together.
  y1 = data.frame(min = paste0(y$summary[grepl("min", y$summary)], collapse = ", "), 
                  mean = paste0(y$summary[grepl("mean|individual|raw|literature|unknown", y$summary)], collapse = ", "),
                  max = paste0(y$summary[grepl("max", y$summary)], collapse = ", "))
    
  # turn it into just one row of data with the species, the trait and the values
  z = data.frame(taxon_name = taxa, trait_name = num_traits$trait_name[i], y1, unit = num_traits$unit[i])
    
    output1 = rbind(output1, z)
    
  }
  
  #Present as two names tables, categorical and numeric traits
  summary = list(categorical_traits = output, numeric_traits = output1)
  
  # only print the data if data exists
  if (nrow(output)+nrow(output1) != 0){
  
  print(summary)
    
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
  
  if (length(taxa) > 2000){
    
     taxa = taxa[1:2000]
    
     
    }else{
  
  #subset
  x1 = austraits_wide %>% filter(taxon_name %in% taxa) 
    }
  #x1[is.na(x1)] = ""
  #prepare the filename
  filename = str_c("AusTraits_", gsub("\\:", "-", Sys.time()), ".csv")
  filename = gsub(" ", "_", filename)
  #convert the subset to a csv
  as_attachment(x1, filename)
 
  
}


################################################################################

  


#################################################################################
# Overlay the yaml file over the top for ease of testing and clarity. 
# Anything in the yaml file that conflicts with the specifications above will overwrite them. 

#* @plumber

 function(pr){
   pr %>% 
    pr_set_api_spec(yaml::read_yaml("API examples v1.yml"))
 }

