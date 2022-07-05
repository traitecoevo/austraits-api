

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
#remotes::install_github("traitecoevo/austraits")
library(austraits) 
austraits <- load_austraits(path="data/austraits", version = get_version_latest())

austraits_wide = as_wide_table(austraits)

ord = data.frame(trait_name = c("plant_growth_form",  "woodiness", "life_history", "flowering_time", "plant_height", "leaf_compoundness", "fire_response","photosynthetic_pathway",
                                "dispersal_syndrome", "dispersers", "reproductive_maturity", "reproductive_maturity_primary", "salt_tolerance", "inundation_tolerance","leaf_area","bud_bank_location","fruiting_time",
                                "post_fire_recruitment","life_form","root_structure","germination","seed_storage_location","wood_density","fire_and_establishing","storage_organ",
                                "serotiny","physical_defence","growth_habit","ploidy"))

ord1 = data.frame(trait_name = c("sex_type","root_shoot_ratio", "soil_seedbank", "flower_colour","pollination_system","fruit_type" ,"fruit_fleshiness","fruit_dehiscence","seed_shape","seed_dry_mass", "genome_size", "leaf_length","leaf_width","leaf_margin","leaf_phenology",
                                 "spinescence","parasitic","dispersal_appendage","clonal_spread_mechanism","leaf_type","leaf_shape","leaf_arrangement","leaf_lifespan","seedling_first_leaf","leaf_N_per_dry_mass","leaf_dry_mass",
                                 "leaf_dry_matter_content","leaf_P_per_dry_mass","leaf_C_per_dry_mass","leaf_K_per_dry_mass","photosynthetic_rate_per_area_saturated","leaf_work_to_punch","bark_thickness" ,"vessel_density","leaf_tannin_per_dry_mass",
                                 "chlorophyll_per_dry_mass"))
ord = rbind(ord, ord1)

ord$ranking = 1:length(ord$trait_name)

austraits_wide_means = austraits_wide %>%
                      filter(trait_name %in% ord$trait_name) %>%
                      filter(str_detect(sample_age_class, "adult")) %>% 
                      filter(str_detect(collection_type, "field|literature|botanical_collection"))
  
austraits_wide_means = merge(austraits_wide_means, ord, by = "trait_name", all.x = T)

# Add in a link to the definition
austraits_wide_means = austraits_wide_means %>% mutate(definition = str_c("https://traitecoevo.github.io/austraits.build/articles/Trait_definitions.html#", trait_name ))
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
# 1. count the number of traits per taxa by text name

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
    
    taxon = unique(austraits_wide$taxon_name[grepl(as.character(APNI_ID), austraits_wide$acceptedNameUsageID)])
    }
    
    
   #Number of traits in AusTraits for this species
    
    x = austraits_wide %>% 
    #get the rows where the taxon_name field is equal to the entered value
    filter(taxon_name == taxon) %>% 
    #select and count the number of unique trait_name values
    select(trait_name) %>% 
    distinct() %>% count() %>% unlist() %>% as.integer()
  
    #Number of summarised traits for this species
    
    x1 = austraits_wide_means %>% 
    #get the rows where the taxon_name field is equal to the entered value
    filter(taxon_name == taxon) %>% 
    #select and count the number of unique trait_name values
    select(trait_name) %>% 
    distinct() %>% count() %>% unlist() %>% as.integer()
  
    y = taxon
    
    z = x - x1
      

    
  
  if(x1 != 0){
    a = list(y, x1, z, paste0("There are ", x1, " traits available for ", y, ", with data for ", z, " further traits in the AusTraits database"))
    names(a) = c("taxon", "summary", "AusTraits", "explanation")
    return(a)
  
  }else{
    
    return(list(0, print("No trait data can be found for this taxon in AusTraits")))
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
    
    list(x, paste0("There are ", x, " species that have data for the ", trait, " trait."))
    
  }else{
    
    list(x, "No data can be found for this trait name. Check for existing trait names at http://traitecoevo.github.io/austraits.build/articles/Trait_definitions.html")
    
   }
  }
 }



################################################################################
# 3. Present mean values for each trait associated with a taxa

#* @apiDescription 
#* Return a table of summarised trait data for a given taxa from the taxon_name data field in AusTraits

#* @param taxon e.g. Angophora costata
#* @param APNI_ID e.g. 2912252 (For Eucalyptus saligna)
#* @get /trait-summary

function(taxon = "", APNI_ID = ""){
  
  if (taxon != ""){
    
    data = austraits_wide_means %>% filter(taxon_name == taxon)
    
  }else if (APNI_ID != ""){
  ############################ manipulate austraits to prepare for averages #################
    
    data = austraits_wide_means %>% filter(str_detect(acceptedNameUsageID, as.character(APNI_ID)))
    #only select adult plants grown outdoors and not experimental data. Or perhaps a list of set traits that can be applied to the whole species
    taxon = data$taxon_name[1]
  }
    
  data$value_type[which(is.na(data$value_type))] = "unknown"
    
  
  ###################### Make the categorical trait summary  ####################
    
  # subset the data to the desired taxon name and get a vector of the available categorical traits
  cat_traits = data %>% filter(is.na(unit)) %>% select(taxon_name, trait_name, definition, ranking) %>% arrange(ranking) %>% unique()
  
  #create a blank dataframe
  output = data.frame()
  
  # for each of the categorical traits...
  for (i in 1:length(cat_traits$trait_name)){
  
  # get austraits data for this taxa and the trait, remove NA values and make a table of the counts for each value
  x = data %>% filter(trait_name == cat_traits$trait_name[i] & is.na(trait_value)==F) %>% select(trait_value) %>% unique()
  
  # make a character string made up of each unique trait value
  y = x$trait_value %>% str_split(pattern = " ") %>% unlist() %>% str_c() %>% unique()
  y = paste0(y, collapse = ", ")
  
  # make a row of data made up of the taxon name, the trait name and the trait value character above (y). The units will be blank.
  z = data.frame(taxon_name = taxon, trait_name = cat_traits$trait_name[i], definition = cat_traits$definition[i], trait_values = y)
  
  #glue it to the dataframe
  output = rbind(output, z)
  }
  
  ###################### Make the numeric trait summary  ####################
  
  num_traits = data  %>% filter(!is.na(unit)) %>% select(taxon_name, trait_name, definition, unit, ranking) %>% arrange(ranking) %>% unique()
  #num_traits = num_traits[1:min(nrow(num_traits), 20),]
  #create a blank dataframe
  output1 = data.frame()
  
  # for each of the numeric traits...
  for (i in 1:length(num_traits$trait_name)){
    
  # get the data for each numeric trait, group by the value type and find the mean
    # remove wrongly entered data and get numeric data only data only
    x2 = data %>% filter(taxon_name == taxon) %>%
      filter(trait_name == num_traits$trait_name[i]) %>% 
      filter(!is.na(value_type)) %>% 
      filter(!is.na(unit)) %>% 
      mutate(trait_value = as.numeric(trait_value)) %>% 
      filter(!is.na(trait_value))
    
    # create a dataset of unknown sites
    x1 = x2 %>% filter((value_type %in% c("unknown", "raw_value") & is.na(site_name))) 
    
    # remove raw_value values without a site name
    x2 = x2 %>% filter(!(value_type %in% c("unknown", "raw_value") & is.na(site_name)))
    
    # split the data into a list of dataframes based on the value type
    x_list <- split(x2 , f = x2$value_type)
    
    
    #########################################################
    
    # calculate for individual_mean and raw_value and unknown
    r_i_u = rbind(x_list[["raw_value"]], x_list[["individual_mean"]], x_list[["unknown"]])  %>% group_by(dataset_id, site_name) %>% summarise(mean = mean(trait_value), min = NA, max = NA)

    
    # Calculate for raw_value without sites
    raw_value1 = x1 %>% group_by(dataset_id) %>% summarise( mean = mean(trait_value), min = NA, max = NA) %>% mutate(site_name = NA) %>% select(dataset_id, site_name, mean, min, max)

    
    # Now the expert mins and maxes
    
    expert = rbind(x_list[["expert_min"]], x_list[["expert_max"]])
    
    # make an average if a range is given
    experts = expert %>% group_by(dataset_id, site_name) %>% summarise( mean = mean(trait_value), min = min(trait_value), max = max(trait_value), n = n())
    experts = experts %>% filter(n > 1) %>% select(-n)
    
    # For the remainder, simply add them to the relevant column, either min or max
    e = expert %>% filter(!(dataset_id %in% experts$dataset_id))
    min = e %>% filter(value_type == "expert_min") %>% mutate(mean = NA, min = trait_value, max = NA) %>% select(dataset_id, site_name, mean, min, max)
    max = e %>% filter(value_type == "expert_max") %>% mutate(mean = NA, min = NA, max = trait_value) %>% select(dataset_id, site_name, mean, min, max)
    
    experts = rbind(experts, min, max)
    
    # stick them together with the other sites
    site_means = rbind(x_list[["site_mean"]] %>% mutate(mean = trait_value, min = NA, max = NA) %>% select(dataset_id, site_name, mean, min, max), 
                       x_list[["multisite_mean"]] %>% mutate(mean = trait_value, min = NA, max = NA) %>% select(dataset_id, site_name, mean, min, max),
                       r_i_u, 
                       raw_value1,
                       x_list[["expert_mean"]] %>% mutate(mean = trait_value, min = NA, max = NA) %>% select(dataset_id, site_name, mean, min, max),
                       experts
    )
    
    # take the mean, min and max of sites
    overall_mean = data.frame(taxon_name = taxon, 
                              trait_name = num_traits$trait_name[i],
                              
                              definition = num_traits$definition[i],
                              
                              mean_type = rbind("min", "mean", "max"),
                              
                              # max and min include all the site means
                              mean = rbind(min = min(site_means$min, na.rm = T),
                                           mean = mean(site_means$mean, na.rm = T),
                                           max = max(site_means$max, na.rm = T)),
                              
                              unit = num_traits$unit[i]
                              

                              
    )
    
    overall_mean = unique(overall_mean)  
    
    # now is the time to compare with any individual/site max or site min
    # there are no individual mins.. perhaps in the future there might be, so this should be added.
    
    # if there are individual or site maxes..
    if (length(x_list[["individual_max"]]$trait_value) > 0| length(x_list[["site_max"]]$trait_value) > 0|length(x_list[["multisite_max"]]$trait_value)> 0){
      
      # the species max should be the max of the original + these values
      overall_mean$mean[overall_mean$mean_type == "max"] = max(x_list[["individual_max"]]$trait_value, 
                                                               x_list[["site_max"]]$trait_value,
                                                               x_list[["multisite_max"]]$trait_value,
                                                               overall_mean$mean[overall_mean$mean_type == "max"], na.rm = T)
    
      
    }
    
    # repeat for the site min values
    if (length( x_list[["site_min"]]$trait_value > 0)){
      
      overall_mean$mean[overall_mean$mean_type == "min"]= min(x_list[["site_min"]]$trait_value,
                                                              overall_mean$mean[overall_mean$mean_type == "min"], na.rm = T)

    }
    
    # Because I'm considering the means of sites in the max and min, I need to remove any observations where the mean is the same as the max or min
  
    
    output1 = rbind(output1, overall_mean)
    
    # clean it up for presentation
    row.names(output1) <- as.character(1:length(output1$mean))
    
    output1$mean = gsub("\\-Inf|Inf", "", output1$mean)
    
   
    
    # I'm (incorrectly) rounding to 3 significant figures over all traits. A more sophisticated fucntion is needed
    output1$mean = signif(as.numeric(output1$mean), 3)
    
    
  }
  

  # More cleaning for presentation
  output1 = pivot_wider(output1, names_from = mean_type, values_from = mean )
  
  output1 = output1 %>% select(taxon_name, trait_name, definition, min, mean, max, unit)

  
  # take out the NAs
  output1 = output1 %>% mutate(across(min:max, as.character)) %>% replace_na(list(min = "", mean = "", max = ""))
  
  output1$mean[output1$mean == "NaN"] = ""
  
  #Present as two names tables, categorical and numeric traits
  summary = list(categorical_traits = output, numeric_traits = output1)
  
  # only print the data if data exists
  if (nrow(output)+nrow(output1) != 0){
  
  print(summary)
    
}else{
  
  print("No data can be found for this taxon.")
  
   }
  }


################################################################################
# 4.1 Returns a table of the combined traits and methods tables for a given taxa
# Possible filters are listed.


#* @get /taxon-data

function(res, taxon = "", has_coordinates = "", field_only = ""){
  
  
  y = austraits_wide  %>%
    filter(taxon_name == taxon) %>% filter(str_detect(sample_age_class, "adult"))

 
    if (has_coordinates == "yes"){
      y1 = y %>% filter(is.na(`longitude (deg)`) == F)
    }else{
      y1 = y
    }
  
    if (field_only == "yes"){
     y2 = y1 %>%  filter(str_detect(collection_type, "field"))
    }else{
      y2 = y1
    }
  
  return(y2)

}

################################################################################
# 5. Post a list of species names and return a full data table of multiple species 

#* @apiDescription 
#* Return a full data table for given taxa for multiple species names 

#* @post /taxa-data

function(req, res, has_coordinates = "", field_only = ""){
  
  # make taxa object = the taxa_list inside the request body
  taxa = req$body$taxa_list
  
  
  #subset
  x1 = austraits_wide %>% filter(taxon_name %in% taxa) %>% filter(str_detect(sample_age_class, "adult"))
  
  if (has_coordinates == "yes"){
    y1 = x1 %>% filter(is.na(`longitude (deg)`) == F)
  }else{
    y1 = x1
  }
  
  if (field_only == "yes"){
    y2 = y1 %>%  filter(str_detect(collection_type, "field"))
  }else{
    y2 = y1
  }
  
  return(y2)
}

#################################################################################
# 4.2 Returns a table of the raw data used to calculate the species means
#* @param taxon e.g. Angophora costata
#* @param APNI_ID e.g. 2912252 (For Eucalyptus saligna)
#* @serializer csv
#* @get /download-taxon-data

function(taxon = "", APNI_ID = ""){
  
  
  if (taxon != ""){
    
    data = austraits_wide %>% filter(taxon_name == taxon)
    
  }else if (APNI_ID != ""){
    
    data = austraits_wide %>% filter(str_detect(acceptedNameUsageID, as.character(APNI_ID)))
  }
  
  data = data %>% mutate_all(coalesce, "")
  
  
  filename = str_c("AusTraits_", data$taxon_name[1], "_",  gsub("\\:", "-", Sys.time()), ".csv")
  filename = gsub(" ", "_", filename)
  #convert the subset to a csv
  as_attachment(data, filename)
  
}

#################################################################################

# 5. Post a list of species names and return a full data table of multiple species as a csv file

#* @apiDescription 
#* Return a full data table for given taxa for multiple species names as a csv file
#* @serializer csv
#* @post /download-taxa-data

function(req, res){
  
  # make taxa object = the taxa_list inside the request body
  taxa = req$body$taxa_list

  
  #subset
  x1 = austraits_wide %>% filter(taxon_name %in% taxa)

  x1 =  x1 %>% mutate_all(coalesce, "")
  
  
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

