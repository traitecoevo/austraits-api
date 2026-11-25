#### AusTraits API development ####

######################### Load packages and austraits data ######################

library(plumber)
library(dplyr)
library(tidyr)
library(stringr)
library(readr)
library(purrr)

# load data: the file written by dataset_prep.R for this AusTraits release
data_version <- "8.0.0"
list2env(readRDS(sprintf("data/austraits_processed_API_v%s.rds", data_version)), envir = .GlobalEnv)

# The taxon a query names: the taxon given, or the taxon whose APNI identifier
# (the number ending taxon_id) is given
query_taxon = function(taxon = "", APNI_ID = ""){
  if (taxon != "") return(taxon)
  if (APNI_ID != ""){
    x = taxon_lookup$taxon_name[taxon_lookup$APNI_ID == as.character(APNI_ID)]
    if (length(x) > 0) return(x[1])
  }
  ""
}

# The species-mean rows for a taxon. Means are per species: a subspecies,
# variety or form with no rows of its own gets its species' rows, named as
# the species
summary_rows = function(taxon){
  rows = trait_summary %>% filter(taxon_name == taxon)
  if (nrow(rows) == 0 && taxon != ""){
    species = str_remove(taxon, "\\s+(subsp\\.|var\\.|f\\.)\\s.*$")
    if (species != taxon) rows = trait_summary %>% filter(taxon_name == species)
  }
  rows
}

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

# ################################################################################
# Log some information about the incoming request

# The @filter function in r plumber allows some action to be completed
# before the incoming request is passed on (using plumber::forward()) to the relevant @get or @post function.
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

# 1.1 count the number of traits per taxa by text name

#* @apiDescription
#* Return a count of unique traits for any given taxa from the taxon_name data field in AusTraits., e.g. Eucalyptus saligna

#* @param taxon e.g. Angophora costata
#* @param APNI_ID e.g. 2912252 (For Eucalyptus saligna)
#* @get /trait-count

function(taxon = "", APNI_ID = ""){

  # convert any APNI ID queries into a taxon_name string
  taxon = query_taxon(taxon, APNI_ID)

  # Traits in AusTraits for this taxon
  traits_all = unique(aus_wide$trait_name[aus_wide$taxon_name == taxon])
  x = length(traits_all)

  # Traits summarised by /trait-summary for this taxon
  traits_summary = unique(summary_rows(taxon)$trait_name)
  x1 = length(traits_summary)

  y = taxon

  # Further traits with data: those not in the summary
  z = length(setdiff(traits_all, traits_summary))

  database = str_c("doi.org/", data_doi)

  #Create the responses
  # If the summary contains traits...
  if(x1 != 0){

    a = data.frame(y, x1, z, paste0("There are ", x1, " traits available for ", y, ", with data for ", z, " further traits in the AusTraits database. These are accessible via the download CSV button or alternatively the entire database can be accessed at ", database))

    # If the summary has no traits but some exist in AusTraits
  }else if(x != 0){

    a = data.frame(y, x1, z, paste0(y, " has data for ", x, " traits in the AusTraits database. These are accessible via the download CSV button or alternatively the entire database can be accessed at ", database))

    # If there is no data for the taxon in AusTraits
  }else{

    a = data.frame(y, x1, z, paste0("There is currently no data for the taxon name you searched for in the AusTraits database. Search for another species name or access the entire database at ", database))

  }

  names(a) = c("taxon", "summary", "AusTraits", "explanation")

  a
}

################################################################################
# 1.2 Present mean values for each trait associated with a taxa

#* @apiDescription
#* Return a table of summarised trait data for a given taxa from the taxon_name data field in AusTraits

#* @param taxon e.g. Angophora costata
#* @param APNI_ID e.g. 2912252 (For Eucalyptus saligna)
#* @get /trait-summary

function(taxon = "", APNI_ID = ""){

  # The rows are precomputed by dataset_prep.R from the observed species means
  # published with the AusTraits release
  data = summary_rows(query_taxon(taxon, APNI_ID))

  ###################### The categorical trait summary  ####################

  output = data %>%
    filter(block == "categorical") %>%
    arrange(ranking) %>%
    select(taxon_name, trait_name = label, definition, trait_values)

  ###################### The numeric trait summary  ####################

  output1 = data %>%
    filter(block == "numeric") %>%
    arrange(ranking) %>%
    select(taxon_name, trait_name = label, definition, min, mean, max, unit)

  # only return the data if data exists
  if (nrow(output) == 0 & nrow(output1) == 0){
    return("No summary data can be found for this taxon.")
  }

  #Present as two names tables, categorical and numeric traits
  # (an empty table is sent as [], as before)
  if (nrow(output) == 0) output = list()
  if (nrow(output1) == 0) output1 = list()

  list(categorical_traits = output, numeric_traits = output1)
}


#################################################################################
# 1.3 Returns a table of the raw data used to calculate the species means
#* @param taxon e.g. Angophora costata
#* @param APNI_ID e.g. 2912252 (For Eucalyptus saligna)
#* @serializer csv
#* @get /download-taxon-data

function(taxon = "", APNI_ID = ""){

  data = aus_wide %>% filter(taxon_name == query_taxon(taxon, APNI_ID))

  data = data %>% mutate_all(coalesce, "")


  # plumber rejects a Content-Disposition filename that contains a quote or is
  # NA, so reduce the taxon name to characters that are safe in both an HTTP
  # header and a filename, and fall back to the query when nothing matched
  taxon_label = data$taxon_name[1]
  if (is.na(taxon_label)) taxon_label = str_c(taxon, APNI_ID)
  taxon_label = gsub("[^A-Za-z0-9._-]+", "_", taxon_label)
  taxon_label = gsub("^_|_$", "", taxon_label)
  if (taxon_label == "") taxon_label = "no_data"

  filename = str_c("AusTraits_", taxon_label, "_",  gsub("\\:", "-", Sys.time()), ".csv")
  filename = gsub(" ", "_", filename)

  #convert the subset to a csv

  as_attachment(data, filename)

}



################################################################################

# 2.1 Count the number of taxa per trait

#* @apiDescription Possible values for traits are found at http://traitecoevo.github.io/austraits.build/articles/austraits_database_structure.html
#* Return a list of unique species for any given trait name in the trait_name data field of AusTraits.
#* @post /taxa-list

function(req, res){

  # make traits object = the traits inside the request body

  traits = req$body$traits

  if (length(traits) == 0){

    traits = unique(located_data$trait_name)


  }

  #subset the data to the taxa and traits

  x = located_data %>%
    filter(trait_name %in% traits)

  if (nrow(x) > 0) {

    # Unique taxa (species) for the trait value entered
    x1 = x %>%

      select(taxon_name) %>% unique()


    x1 = x1$taxon_name

    return(x1)

  }else{

    return("Choose at least one trait as listed in the AusTraits database: https://traitecoevo.github.io/APD/index.html")

  }

}
###################################################################################################

# 2.2 Return a list of traits for a given list of taxa

#* @apiDescription Possible values for traits are found at http://traitecoevo.github.io/austraits.build/articles/austraits_database_structure.html
#* Return a list of unique species for any given trait name in the trait_name data field of AusTraits.
#* @post /trait-list

function(req, res){

  # make taxa object = the taxa inside the request body

  taxa = req$body$taxa

  if (length(taxa) == 0){


    taxa = unique(located_data$taxon_name)

  }

  #subset the data to the taxa and traits

  x = located_data %>%
    filter(taxon_name %in% taxa)

  if (nrow(x) > 0) {

    # Unique taxa (species) for the trait value entered
    x1 = x %>%

      select(trait_name) %>% unique()

    x1 = x1$trait_name

    return(x1)

  }else{

    return("Choose at least one taxon name as listed in the AusTraits database")

  }

}
# #################################################################################
# 2.3 Return taxa that begin with a given text string

#* @apiDescription Possible values for traits are found at https://traitecoevo.github.io/APD/index.html
#* Return a list of unique species for any given trait name in the trait_name data field of AusTraits.
#* @get /taxa-autocomplete

function(text_string = ""){

  #subset the data to the taxa and traits

  x = located_data %>%
    filter(str_detect(taxon_name, str_c("^", text_string))) %>%
    select(taxon_name) %>% unique()

  if (nrow(x) > 0) {

    # Unique taxa (species) for the trait value entered
    x1 = x$taxon_name

    return(x1)

  }else{

    return("Try typing the start of a taxon name e.g. Acacia")

  }

}

################################################################################
# 2.3.1 Return taxa that begin with a given text string

#* @apiDescription Possible values for traits are found at https://traitecoevo.github.io/APD/index.html
#* Return a list of unique traits for any given taxon name in the taxon_name data field of AusTraits.
#* @get /trait-autocomplete

function(text_string = ""){
  
  #subset the data to the taxa and traits
  
  x = located_data %>%
    filter(str_detect(trait_name, str_c("^", text_string))) %>%
    select(trait_name) %>% unique()
  
  if (nrow(x) > 0) {
    
    # Unique taxa (species) for the trait value entered
    x1 = x$trait_name
    
    return(x1)
    
  }else{
    
    return("Try typing the start of a trait name e.g. leaf")
    
  }
  
}

#################################################################################
# 2.4 Post a list of species names and trait names and return a full data table of the prepped data observations

#* @apiDescription
#* Return a full data table for multiple species names and traits

#* @post /trait-data

function(req, res){

  # make taxa and traits objects = the taxa and traits inside the request body

  taxa = req$body$taxa
  traits = req$body$traits

  if (length(taxa) == 0){

    taxa = unique(located_data$taxon_name)

  }

  if (length(traits) == 0){

    traits = unique(located_data$trait_name)

  }

  #subset the data to the taxa and traits

  x = located_data %>%
    filter(taxon_name %in% taxa) %>%
    filter(trait_name %in% traits)

  # Make a second table telling the user how many observations per species
  if (nrow(x) > 0) {

    # Counts of unique taxa (species) for the trait value entered
    x1 = x %>%

      select(taxon_name, trait_name) %>%

      group_by(taxon_name, trait_name) %>%

      summarise(datapoints = n())


    if(nrow(x1) > 30000){

      a = "Your selection is too large, please narrow your search"

      return(a)

    }else{

      x1 = x1 %>% mutate(definition = str_c("https://traitecoevo.github.io/APD/index.html#", trait_name)) %>%
        select(taxon_name, trait_name, definition, datapoints)


      x2 = list(x, x1)

      names(x2) = c("data", "summary")

      return(x2)

    }

  }else{

    return("Choose at least one trait and/or at least one taxon name in AusTraits")

  }

}
#
# ################################################################################
# # Make another endpoint for guessing the start of the species name
#
#
#
# #################################################################################
#
# # 2.5 Post a list of species names and return a full data table of multiple species as a csv file
#
#* @apiDescription
#* Return a full data table for given taxa for multiple species names as a csv file
#* @serializer csv
#* @post /download-trait-data

function(req, res){

  # make taxa object = the taxa_list inside the request body
  taxa = req$body$taxa
  traits = req$body$traits

  #
  if (length(taxa) == 0){

    taxa = unique(aus_wide$taxon_name)

  }

  if (length(traits) == 0){

    traits = unique(aus_wide$trait_name)

  }

  # subset
  x = aus_wide %>%
    filter(taxon_name %in% taxa) %>%
    filter(trait_name %in% traits)

  # remove NAs
  x =  x %>% mutate_all(coalesce, "")

  if(nrow(x) > 100000){

    x = data.frame(Error = c("The file size is too large. The entire AusTraits database can be downloaded from the AusTraits webpage", "doi.org/10.5281/zenodo.3568417"))

  }else{

    filename = str_c("AusTraits_", gsub("\\:", "-", Sys.time()), ".csv")
    filename = gsub(" ", "_", filename)

    #convert the subset to a csv
    as_attachment(x, filename)

  }
}


################################################################################

#################################################################################
# Overlay the yaml file over the top for ease of testing and clarity.
# Anything in the yaml file that conflicts with the specifications above will overwrite them.

#* @plumber

function(pr){
  pr %>%
    pr_set_api_spec(yaml::read_yaml("API examples v1.yml"))
  # limits requests to about 50 species names, or a 6.5 megabyte file
  options_plumber(maxRequestSize = getOption("plumber.maxRequestSize", 10000000000000))
}
#
