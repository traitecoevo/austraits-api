#### AusTraits API development ####

######################### Load packages and austraits data ######################

library(plumber)
library(dplyr)
library(tidyr)
library(stringr)
library(readr)
library(purrr)

#remotes::install_github("traitecoevo/austraits", dependencies = FALSE, build_vignettes = FALSE)

library(austraits)

ver <- "7.0.0" # get_version_latest()

austraits <- load_austraits(path = "data/austraits", version = ver)

#################################################################################
aus_wide = austraits %>% as_wide_table() %>% rename(trait_value = value)


def = austraits$definitions
#def = austraits$definitions

out = data.frame()

for (i in 1:length(unique(aus_wide$trait_name))){
  
  temp = data.frame(trait_name = unique(aus_wide$trait_name)[i], 
                    label = def[[unique(aus_wide$trait_name)[i]]]$label,
                    definition = def[[unique(aus_wide$trait_name)[i]]]$entity_URI)
  
  out = rbind(out, temp)
  
}

############# create the wide table base for most of the outputs ################


############################# ALA trait summary prep ####################################

# Traits marked as definitely of interest by ALA
ord = data.frame(trait_name = c("plant_growth_form", "life_history", "plant_height", 
                                "leaf_compoundness","photosynthetic_pathway",
                                "dispersal_syndrome", "dispersers", "reproductive_maturity", 
                                 "plant_tolerance_salt", "plant_tolerance_inundation",
                                "leaf_area","bud_bank_location","flowering_time","fruiting_time",
                                "post_fire_recruitment","life_form","root_structure", "resprouting_capacity",
                                "seed_germination","serotiny","seedling_establishment_conditions",
                                "seedbank_location","storage_organ",
                                "plant_physical_defence_structures","stem_growth_habit","woodiness","wood_density","ploidy"))

# Traits marked as potentially of interest by ALA
ord1 = data.frame(trait_name = c("sex_type","root_shoot_ratio", "flower_colour","pollination_system",
                                 "fruit_type" ,"fruit_fleshiness","fruit_dehiscence","seed_shape","seed_dry_mass", 
                                 "genome_size", "leaf_length","leaf_width","leaf_margin","leaf_phenology",
                                 "plant_spinescence","parasitic","dispersal_appendage","clonal_spread_mechanism",
                                 "leaf_type","leaf_shape","leaf_arrangement","leaf_lifespan","seedling_first_node_leaf_type",
                                 "leaf_N_per_dry_mass","leaf_dry_mass", "leaf_dry_matter_content","leaf_P_per_dry_mass",
                                 "leaf_C_per_dry_mass","leaf_K_per_dry_mass","leaf_photosynthetic_rate_per_area_maximum",
                                 "leaf_work_to_punch","bark_thickness" ,"stem_vessel_density","leaf_tannin_per_dry_mass",
                                 "leaf_chlorophyll_per_dry_mass"))

# Bind them together
ord = rbind(ord, ord1)

# Filter the data to just the data that is needed for species means for ALA
aus_wide_means = aus_wide %>%
  filter(trait_name %in% ord$trait_name) %>%
  filter(str_detect(life_stage, "adult")) %>%
  filter(basis_of_record %in% c("preserved_specimen", 
                                "field", 
                                "literature field",
                                "literature",
                                "field preserved_specimen", 
                                "field_experiment",
                                "field field_experiment",
                                "field preserved_specimen",
                                "literature field field_experiment"))


######################## Edits to improve presentation #########################

aus_wide_means$trait_value = ifelse(str_detect(aus_wide_means$trait_value, "[0-9]+\\.([0-9]){2}+"), str_extract(aus_wide_means$trait_value, "[0-9]+\\.([0-9]){2}"),aus_wide_means$trait_value)

# Edit the units for presentation
aus_wide_means$trait_value = ifelse(aus_wide_means$trait_name == "reproductive_maturity"&!str_detect(aus_wide_means$trait_value, "--"), 
                                    str_c(aus_wide_means$trait_value, "_years"), 
                                    aus_wide_means$trait_value)

aus_wide_means$unit = ifelse(aus_wide_means$trait_name == "reproductive_maturity", 
                             NA, 
                             aus_wide_means$unit)

aus_wide_means$unit = gsub("^mo$", "months", aus_wide_means$unit)
aus_wide_means$unit = gsub("^d$", "days", aus_wide_means$unit)

#


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



########### Adding on trait rankings, definition urls and trait labels ################

# Add the ranking so the traits will appear in order
ord$ranking = 1:length(ord$trait_name)

# Merge in the rankings
aus_wide_means = left_join(aus_wide_means, ord, by = c("trait_name")) %>% left_join(out)



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

# Save

# Save all objects in a single RDS file
saveRDS(
  list(
    month = month,
    aus_wide = aus_wide,
    aus_wide_means = aus_wide_means,
    trait_type = trait_type,
    located_data = located_data
  ),
  sprintf("data/austraits_processed_API_v%s.rds", ver)
)

# To reload later, use:
# list2env(readRDS("data/austraits_processed.rds"), envir = .GlobalEnv)

