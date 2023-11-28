#### AusTraits API development ####
# Load packages and austraits data
#install.packages("plumber")
library(plumber)

library(dplyr)
library(tidyr)
library(stringr)
library(readr)
library(purrr)

#library(refmanager)
# 

remotes::install_github("traitecoevo/austraits")

library(austraits) 

austraits <- load_austraits(path="data/austraits", version = get_version_latest())

aus_wide = as_wide_table(austraits) %>% rename(trait_value = value)


############################# ALA trait summary prep ####################################

# Traits marked as definitely of interest
ord = data.frame(trait_name = c("plant_growth_form",  "woodiness", "life_history", "plant_height", "leaf_compoundness", "fire_response","photosynthetic_pathway",
                                "dispersal_syndrome", "dispersers", "reproductive_maturity", "reproductive_maturity_primary", "salt_tolerance", "inundation_tolerance","leaf_area","bud_bank_location","flowering_time","fruiting_time",
                                "post_fire_recruitment","life_form","root_structure","germination","seed_storage_location","wood_density","fire_and_establishing","storage_organ",
                                "serotiny","physical_defence","growth_habit","ploidy"))

# Traits marked as possibly of interest
ord1 = data.frame(trait_name = c("sex_type","root_shoot_ratio", "soil_seedbank", "flower_colour","pollination_system","fruit_type" ,"fruit_fleshiness","fruit_dehiscence","seed_shape","seed_dry_mass", "genome_size", "leaf_length","leaf_width","leaf_margin","leaf_phenology",
                                 "spinescence","parasitic","dispersal_appendage","clonal_spread_mechanism","leaf_type","leaf_shape","leaf_arrangement","leaf_lifespan","seedling_first_leaf","leaf_N_per_dry_mass","leaf_dry_mass",
                                 "leaf_dry_matter_content","leaf_P_per_dry_mass","leaf_C_per_dry_mass","leaf_K_per_dry_mass","photosynthetic_rate_per_area_saturated","leaf_work_to_punch","bark_thickness" ,"vessel_density","leaf_tannin_per_dry_mass",
                                 "chlorophyll_per_dry_mass"))

# Bind them together
ord = rbind(ord, ord1)

# Add the ranking
ord$ranking = 1:length(ord$trait_name)

# Filter the data
aus_wide_means = aus_wide %>%
  filter(trait_name %in% ord$trait_name) %>%
  filter(str_detect(life_stage, "adult")) %>%
  filter(basis_of_record %in% c("preserved_specimen", "field", "literature, field",
                                "literature","field, preserved_specimen", "field_experiment",
                                "field, field_experiment"))

# change the units for presentation
aus_wide_means$trait_value = ifelse(aus_wide_means$trait_name == "reproductive_maturity", str_c(aus_wide_means$trait_value, "_years"), aus_wide_means$trait_value)
aus_wide_means$unit = ifelse(aus_wide_means$trait_name == "reproductive_maturity", NA, aus_wide_means$unit)

aus_wide_means$unit = gsub("^mo$", "months", aus_wide_means$unit)
aus_wide_means$unit = gsub("^d$", "days", aus_wide_means$unit)

# convert all fruiting times and flowering times to months
f = aus_wide_means %>% filter(trait_name %in% c("flowering_time", "fruiting_time"))

month = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

for (i in 1:length(f$trait_value)){
  f1      <- unlist(strsplit(f$trait_value[i],""))
  f2 = ifelse(f1 == "y", month, NA)
  f3 = paste0(f2[complete.cases(f2)], collapse = "_")
  f$trait_value[i] = f3
}

aus_wide_means = rbind(aus_wide_means %>% filter(!trait_name %in% c("flowering_time", "fruiting_time")), f)

# Merge in the rankings
aus_wide_means = left_join(aus_wide_means, ord, by = c("trait_name"))

# Add in a link to the trait definition
aus_wide_means = aus_wide_means %>% mutate(definition = str_c("https://traitecoevo.github.io/APD/index.html#", str_to_lower(trait_name)))



######################### Ecocommons trait data prep ############################

located_data = aus_wide %>%
  filter(!is.na(`longitude (deg)`)) %>%
  filter(str_detect(life_stage, "adult")) %>%
  filter(basis_of_record %in% c("preserved_specimen", "field", "literature, field",
                                "literature","field, preserved_specimen", "field_experiment",
                                "field, field_experiment")) %>%

  mutate(data_type = ifelse(is.na(unit), "categorical", "numeric"))

# a reference for later
trait_type = located_data %>% dplyr::select(trait_name, unit) %>% unique() %>% mutate(data_type = ifelse(is.na(unit), "categorical", "numeric"))
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
  if (taxon != ""){

    taxon = taxon

  }else if (APNI_ID != ""){

    taxon = unique(aus_wide$taxon_name[grepl(as.character(APNI_ID), aus_wide$taxon_id)])

  }


  #Number of traits in AusTraits for this species

  x = aus_wide %>%

    #get the rows where the taxon_name field is equal to the entered value
    filter(taxon_name == taxon) %>%

    #select and count the number of unique trait_name values
    select(trait_name) %>%
    distinct() %>% count() %>% unlist() %>% as.integer()

  #Number of summarised traits for this species

  x1 = aus_wide_means %>%
    #get the rows where the taxon_name field is equal to the entered value
    filter(taxon_name == taxon) %>%
    #select and count the number of unique trait_name values
    select(trait_name) %>%
    distinct() %>% count() %>% unlist() %>% as.integer()

  y = taxon

  z = x - x1


  #Create the responses
  # If the summary contains traits...
  if(x1 != 0){

    a = data.frame(y, x1, z, paste0("There are ", x1, " traits available for ", y, ", with data for ", z, " further traits in the AusTraits database. These are accessible via the download CSV button or alternatively the entire database can be accessed at doi.org/10.5281/zenodo.10156222"))

    names(a) = c("taxon", "summary", "AusTraits", "explanation")

    return(a)

    # If the summary has no traits but some exist in AusTraits
  }else if(x1 == 0 & x != 0){

    a = data.frame(y, x1, z, print(y, " has data for ", y, " traits in the AusTraits database. These are accessible via the download CSV button or alternatively the entire database can be accessed at doi.org/10.5281/zenodo.10156222"))

    names(a) = c("taxon", "summary", "AusTraits", "explanation")

    return(a)

    # If there is no data for the taxon in AusTraits
  }else{


    a = data.frame(y, x1, z, print("There is currently no data for the taxon name you searched for in the AusTraits database. Search for another species name or access the entire database at doi.org/10.5281/zenodo.10156222"))

    names(a) = c("taxon", "summary", "AusTraits", "explanation")

    return(a)

  }
}

################################################################################
# 1.2 Present mean values for each trait associated with a taxa

#* @apiDescription
#* Return a table of summarised trait data for a given taxa from the taxon_name data field in AusTraits

#* @param taxon e.g. Angophora costata
#* @param APNI_ID e.g. 2912252 (For Eucalyptus saligna)
#* @get /trait-summary

function(taxon = "", APNI_ID = ""){

  if (taxon != ""){

    data = aus_wide_means %>% filter(taxon_name == taxon)

  }else if (APNI_ID != ""){

    data = aus_wide_means %>% filter(str_detect(taxon_id, as.character(APNI_ID)))

    taxon = data$taxon_name[1]

  }


  ###################### Make the categorical trait summary  ####################

  # subset the data to the desired taxon name and get a vector of the available categorical traits
  cat_traits = data %>% filter(is.na(unit)) %>% select(taxon_name, trait_name, definition, ranking) %>% arrange(ranking) %>% unique()


  if (nrow(cat_traits) != 0){

    #create a blank dataframe
    output = data.frame()

    # for each of the categorical traits...
    for (i in 1:length(cat_traits$trait_name)){

      # get austraits data for this taxa and the trait, remove NA values and make a table of the counts for each value
      x = data %>% filter(trait_name == cat_traits$trait_name[i] & is.na(trait_value)==F) %>% select(trait_value) %>% unique()

      # make a character string made up of each unique trait value
      y = x$trait_value %>% str_split(pattern = " ") %>% unlist() %>% sort() %>% str_c() %>% unique()
      y = paste0(y, collapse = ", ")

      # make a row of data made up of the taxon name, the trait name and the trait value character above (y). The units will be blank.
      z = data.frame(taxon_name = taxon, trait_name = cat_traits$trait_name[i], definition = cat_traits$definition[i], trait_values = y)

      #glue it to the dataframe
      output = rbind(output, z)

    }

    output$trait_values = gsub("_", " ", output$trait_values)
    output$asterisk = ifelse(str_detect(output$trait_values, ","), "*", "")

    # clean up flowering times
    if ("flowering_time" %in% output$trait_name){
      flower = output$trait_values[output$trait_name == "flowering_time"]
      flower = gsub(",", "", flower)
      flower = unlist(str_split(flower, pattern = " "))
      flower = flower %>% unique()
      flower = flower[match(month, flower)]
      flower = paste(flower[complete.cases(flower)], collapse = ", ")
      output$trait_values[output$trait_name == "flowering_time"] = flower
    }

    if ("fruiting_time" %in% output$trait_name){
      fruit = output$trait_values[output$trait_name == "fruiting_time"]
      fruit = gsub(",", "", fruit)
      fruit = unlist(str_split(fruit, pattern = " "))
      fruit = fruit %>% unique()
      fruit = fruit[match(month, fruit)]
      fruit = paste(fruit[complete.cases(fruit)], collapse = ", ")
      output$trait_values[output$trait_name == "fruiting_time"] = fruit

    }

    output$trait_name = gsub("_", " ", output$trait_name)
    output$trait_name = str_to_sentence(output$trait_name)
    output = output %>% mutate(trait_values = str_c(trait_values,"  ", asterisk)) %>% select(-asterisk) %>% mutate(trait_values = str_trim(trait_values))
    output = output %>% filter(trait_values != "")
    
  }else{

    output = list()
  }
  # end of cat traits table

  ###################### Make the numeric trait summary  ####################

  # Create a reference list of taxon-trait combinations and arrange them by ranking
  num_traits = data  %>% filter(!is.na(unit)) %>% select(taxon_name, trait_name, definition, unit, ranking) %>% arrange(ranking) %>% unique()

  if( nrow(num_traits) != 0){

    #create a blank dataframe
    output1 = data.frame()

    # for each of the numeric traits...
    for (i in 1:length(num_traits$trait_name)){

      # get the data for each numeric trait, group by the value type and find the mean
      # remove wrongly entered data and get numeric data only data only
      a = data %>% filter(taxon_name == taxon) %>%
        filter(trait_name == num_traits$trait_name[i]) %>%
        filter(!is.na(unit)) %>%
        mutate(trait_value = as.numeric(trait_value)) %>%
        filter(!is.na(trait_value))


      ##############################################################################################

      # create a dataset of unknown sites
      # calculate for mean and raw. Doesn't matter whether they have a location_d or not - the NA will count as a location and the dataset_id is unique
      data_means = a %>% filter(value_type %in% c("mean", "raw"))  %>% group_by(dataset_id) %>% summarise(mean = mean(trait_value))

      # Now create a mean via the expert mins and maxes
      expert = a %>% filter(value_type %in% c("minimum", "maximum"), basis_of_record %in% c("preserved_specimen", "literature"))

      # make an average if a range is given
      experts = expert %>% group_by(dataset_id) %>% summarise(mean = mean(trait_value), n = n())
      # make sure your mean comes from at least two observations. Should never be three because why would you have more than one estimate of a max or min per dataset?
      experts = experts %>% filter(n == 2) %>% select(-n)
      # stick them together with the other sites
      site_means = rbind(data_means, experts)

      # take the mean, min and max of sites
      overall_mean = data.frame(taxon_name = num_traits$taxon_name[i],

                                trait_name = num_traits$trait_name[i],

                                definition = num_traits$definition[i],

                                mean = mean(site_means$mean, na.rm = T),

                                unit = num_traits$unit[i]

      )


      ##################### Min and Max temporary patch ##############################
      # If theres more than 1 observation:

      if (nrow(a) > 1 & length(unique(a$value_type)) > 1){

        overall_mean$min = min(a$trait_value)
        overall_mean$max = max(a$trait_value)

      }else if ( unique(a$value_type) == "maximum"){
        overall_mean$min = NA
        overall_mean$max = max(a$trait_value)
      }else{
        overall_mean$min = NA
        overall_mean$max = NA
      }

      # stick it to the previous observation
      output1 = rbind(output1, overall_mean)


    }

    ################################## Final changes before sending ##################

    output1 = output1 %>% select(taxon_name, trait_name, definition, min, mean, max, unit)

    # I'm (incorrectly) rounding to 3 significant figures over all traits. A more sophisticated function is needed
    output1$mean = signif(as.numeric(output1$mean), 3)
    output1$min = signif(as.numeric(output1$min), 3)
    output1$max = signif(as.numeric(output1$max), 3)

    # take out the NAs
    output1 = output1 %>% mutate(across(min:max, as.character)) %>% replace_na(list(min = "", mean = "", max = ""))

    output1$trait_name = gsub("_", " ", output1$trait_name)
    output1$trait_name = str_to_sentence(output1$trait_name)
    output1$trait_name = gsub(" n ", " N ", output1$trait_name)
    output1$trait_name = gsub(" c ", " C ", output1$trait_name)
    output1$trait_name = gsub(" p ", " P ", output1$trait_name)
    output1$trait_name = gsub(" k ", " K ", output1$trait_name)
    output1$mean[output1$mean == "NaN"] = ""

  }else{

    output1 = list()
  }

  # end of numeric traits table





  # only print the data if data exists
  if (is.data.frame(output) | is.data.frame(output1)){

    #Present as two names tables, categorical and numeric traits
    summary = list(categorical_traits = output, numeric_traits = output1)

    print(summary)

  }else{

    print("No summary data can be found for this taxon.")

  }
}


#################################################################################
# 1.3 Returns a table of the raw data used to calculate the species means
#* @param taxon e.g. Angophora costata
#* @param APNI_ID e.g. 2912252 (For Eucalyptus saligna)
#* @serializer csv
#* @get /download-taxon-data

function(taxon = "", APNI_ID = ""){


  if (taxon != ""){

    data = aus_wide %>% filter(taxon_name == taxon)

  }else if (APNI_ID != ""){

    data = aus_wide %>% filter(str_detect(taxon_id, as.character(APNI_ID)))

  }

  data = data %>% mutate_all(coalesce, "")


  filename = str_c("AusTraits_", data$taxon_name[1], "_",  gsub("\\:", "-", Sys.time()), ".csv")
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
