#### AusTraits API development ####

######################### Load packages and austraits data ######################

library(plumber)
library(dplyr)
library(tidyr)
library(stringr)
library(readr)
library(purrr)

#remotes::install_github("traitecoevo/austraits", dependencies = FALSE, build_vignettes = FALSE)

library(austraits) # >= 4.0.1, which loads the observed means published with each release

# Run from inside API.build/. update = TRUE refreshes the saved list of releases;
# the release files are downloaded only if they are not already in data/austraits/
ver <- "8.0.0"

austraits <- load_austraits(path = "data/austraits", version = ver)

# Observed species means published with the release
# (austraits-<ver>-observed-means; columns described in its -dictionary.csv)
austraits_means <- load_austraits(path = "data/austraits", version = ver, means = TRUE, update = FALSE)

# The release's own DOI, cited in the API's messages
data_doi <- get_versions(path = "data/austraits", update = FALSE) %>%
  filter(version == ver) %>% pull(doi)

#################################################################################
aus_wide = austraits %>% as_wide_table() %>% rename(trait_value = value)


def = austraits$definitions

# Trait labels and definition URIs, served by /trait-summary
out = tibble(trait_name = unique(aus_wide$trait_name)) %>%
  mutate(label = map_chr(trait_name, ~ def[[.x]]$label),
         definition = map_chr(trait_name, ~ def[[.x]]$entity_URI))

# Taxon name for each APNI identifier, for the APNI_ID queries
# (taxon_id is a URI ending in the identifier). The means add species whose
# records are all infraspecific
taxon_lookup = bind_rows(
    aus_wide %>% distinct(taxon_name, taxon_id),
    austraits_means %>% distinct(taxon_name, taxon_id)) %>%
  distinct() %>%
  mutate(APNI_ID = str_extract(taxon_id, "[0-9]+$")) %>%
  filter(!is.na(APNI_ID))

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

month = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

# Add the ranking so the traits will appear in order
ord$ranking = 1:length(ord$trait_name)

# /trait-summary serves the published observed means, formatted as the
# endpoint has always returned them. Means are per species: subspecies,
# varieties and forms are rolled up to their species.
summary_means = austraits_means %>%
  filter(trait_name %in% ord$trait_name) %>%
  inner_join(ord, by = "trait_name") %>%
  left_join(out, by = "trait_name")

# Values and counts from value_count, "value (n); value (n)"
split_value_count = function(value_count) {
  parts = str_split(value_count, "; ")
  list(values = map(parts, ~ str_remove(.x, " \\([0-9]+\\)$")),
       counts = map(parts, ~ as.integer(str_match(.x, "\\(([0-9]+)\\)$")[, 2])))
}

# Categorical traits: the values recorded, sorted and joined by ", ", with
# "  *" appended when there is more than one value, as before. Months are
# listed in calendar order (value_count already is), with "  *" when any
# month is marked by fewer than all records (before: when the records'
# month strings differed, which is the same thing).
format_categorical = function(trait_name, value_count, n_records) {
  vc = split_value_count(value_count)
  out = pmap_chr(list(trait_name, vc$values, vc$counts, n_records), function(t, v, n, total) {
    if (t %in% c("flowering_time", "fruiting_time")) {
      disagree = any(n < total)
    } else {
      v = sort(unique(v))
      disagree = length(v) > 1
    }
    v = paste(gsub("_", " ", v), collapse = ", ")
    if (disagree) paste0(v, "  *") else v
  })
  out
}

format_number = function(x) {
  x = as.character(signif(x, 3))
  replace_na(x, "")
}

summary_categorical = summary_means %>%
  filter(type == "categorical", !is.na(value_count), value_count != "") %>%
  mutate(trait_values = format_categorical(trait_name, value_count, n_records))

# reproductive_maturity has always been served as a categorical trait,
# "<years> years"; keep it there so the response keeps its shape
summary_maturity = summary_means %>%
  filter(trait_name == "reproductive_maturity", !is.na(value)) %>%
  mutate(trait_values = str_c(format_number(value), " years"))

summary_numeric = summary_means %>%
  filter(type == "numerical", trait_name != "reproductive_maturity") %>%
  mutate(min = format_number(value_min),
         mean = format_number(value),
         max = format_number(value_max),
         unit = gsub("^mo$", "months", unit),
         unit = gsub("^d$", "days", unit))

trait_summary = bind_rows(
  bind_rows(summary_categorical, summary_maturity) %>%
    mutate(block = "categorical", unit = NA_character_),
  summary_numeric %>% mutate(block = "numeric")) %>%
  select(taxon_name, ranking, block, trait_name, label, definition, trait_values, min, mean, max, unit) %>%
  arrange(taxon_name, block, ranking)


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
    data_version = ver,
    data_doi = data_doi,
    month = month,
    aus_wide = aus_wide,
    taxon_lookup = taxon_lookup,
    trait_summary = trait_summary,
    trait_type = trait_type,
    located_data = located_data
  ),
  sprintf("data/austraits_processed_API_v%s.rds", ver)
)

# To reload later, use:
# list2env(readRDS(sprintf("data/austraits_processed_API_v%s.rds", ver)), envir = .GlobalEnv)

