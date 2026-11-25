#### Compare /trait-summary and /trait-count between two running copies of the API ####
#
# Checks that a new data release keeps the response shape partners rely on
# (field names, field order, JSON types) and summarises how the values moved.
#
# Start the two APIs first, e.g. the deployed code with the previous release's
# data on port 8101 and this checkout on port 8102, then run from API.build/:
#
#   Rscript compare_api_versions.R http://127.0.0.1:8101 http://127.0.0.1:8102 200
#
# Arguments: old API URL, new API URL, number of taxa to sample (default 200).
# Taxa are sampled (seed 1) from the new release's summary table, plus fixed
# cases: infraspecific names, flowering/fruiting time, reproductive maturity.
# Writes the per-value differences to compare_api_versions.csv.

suppressPackageStartupMessages({
  library(dplyr)
  library(purrr)
  library(stringr)
  library(jsonlite)
})

args = commandArgs(trailingOnly = TRUE)
old_url = if (length(args) >= 1) args[1] else "http://127.0.0.1:8101"
new_url = if (length(args) >= 2) args[2] else "http://127.0.0.1:8102"
n_taxa = if (length(args) >= 3) as.integer(args[3]) else 200L

data_version = "8.0.0"
new_data = readRDS(sprintf("data/austraits_processed_API_v%s.rds", data_version))

set.seed(1)
species = unique(new_data$trait_summary$taxon_name)
has_trait = function(trait) unique(new_data$trait_summary$taxon_name[new_data$trait_summary$trait_name == trait])
infraspecific = unique(new_data$aus_wide$taxon_name[str_detect(new_data$aus_wide$taxon_name, " (subsp|var|f)\\. ")])

taxa = unique(c(
  "Eucalyptus saligna", "Angophora costata", "Acacia aneura",
  sample(species, n_taxa),
  sample(has_trait("flowering_time"), 10),
  sample(has_trait("fruiting_time"), 5),
  sample(has_trait("reproductive_maturity"), 10),
  sample(infraspecific, 20),
  "Not a taxon"
))

get_json = function(base, path, query) {
  url = str_c(base, path, "?", str_c(names(query), "=", map_chr(query, URLencode, reserved = TRUE), collapse = "&"))
  fromJSON(url, simplifyVector = FALSE)
}

# Shape of a JSON value: for an object its keys in order with each value's
# shape; for an array of row objects, the set of row shapes
shape = function(x) {
  if (is.null(x)) return("null")
  if (is.list(x) && !is.null(names(x))) {
    return(str_c("{", str_c(names(x), ":", map_chr(x, shape), collapse = ","), "}"))
  }
  if (is.list(x)) {
    if (length(x) == 0) return("[]")
    return(str_c("[", str_c(sort(unique(map_chr(x, shape))), collapse = "|"), "]"))
  }
  class(x)[1]
}

# An empty table and a filled one differ only in rows, so compare the shape of
# each table's rows, and the top-level layout separately
row_shapes = function(x) {
  if (!is.list(x) || is.null(names(x))) return(list(top = shape(x)))
  out = list(top = str_c(names(x), collapse = ","))
  for (nm in names(x)) {
    rows = x[[nm]]
    out[[nm]] = if (length(rows) == 0) NA_character_ else sort(unique(map_chr(rows, shape)))
  }
  out
}

shape_problems = character()
diffs = list()

for (tx in taxa) {
  for (path in c("/trait-summary", "/trait-count")) {
    old = get_json(old_url, path, list(taxon = tx))
    new = get_json(new_url, path, list(taxon = tx))
    so = row_shapes(old)
    sn = row_shapes(new)
    if (!identical(so$top, sn$top) && !(is.na(so$top) || is.na(sn$top))) {
      # A taxon with data in one release and none in the other changes layout
      # legitimately (string message vs tables): record it, it's not a shape change
      if (!(str_detect(so$top, "character") || str_detect(sn$top, "character"))) {
        shape_problems = c(shape_problems, str_c(path, " ", tx, ": top ", so$top, " vs ", sn$top))
      }
    }
    for (nm in intersect(setdiff(names(so), "top"), setdiff(names(sn), "top"))) {
      if (!anyNA(so[[nm]]) && !anyNA(sn[[nm]]) && !identical(so[[nm]], sn[[nm]])) {
        shape_problems = c(shape_problems, str_c(path, " ", tx, " ", nm, ": ", str_c(so[[nm]], collapse = "|"), " vs ", str_c(sn[[nm]], collapse = "|")))
      }
    }
    if (path == "/trait-count") {
      diffs[[length(diffs) + 1]] = tibble(taxon = tx, endpoint = path, trait = "count",
        old = as.character(old[[1]]$summary[[1]]), new = as.character(new[[1]]$summary[[1]]),
        old_taxon = NA_character_, new_taxon = NA_character_)
      next
    }
    to_rows = function(x, block, field) {
      if (!is.list(x) || is.null(x[[block]]) || length(x[[block]]) == 0) return(tibble(trait = character(), value = character(), taxon_name = character()))
      tibble(trait = map_chr(x[[block]], ~ .x$trait_name[[1]]),
             value = map_chr(x[[block]], ~ .x[[field]][[1]]),
             taxon_name = map_chr(x[[block]], ~ .x$taxon_name[[1]]))
    }
    for (b in list(c("categorical_traits", "trait_values"), c("numeric_traits", "mean"))) {
      o = to_rows(old, b[1], b[2])
      n = to_rows(new, b[1], b[2])
      d = full_join(o, n, by = "trait", suffix = c("_old", "_new"))
      if (nrow(d) > 0) {
        diffs[[length(diffs) + 1]] = tibble(taxon = tx, endpoint = str_c(path, " ", b[1]), trait = d$trait,
          old = d$value_old, new = d$value_new, old_taxon = d$taxon_name_old, new_taxon = d$taxon_name_new)
      }
    }
  }
}

diffs = bind_rows(diffs)
write.csv(diffs, "compare_api_versions.csv", row.names = FALSE)

cat("Taxa queried:", length(taxa), "\n")
cat("Shape problems:", length(shape_problems), "\n")
if (length(shape_problems) > 0) cat(head(shape_problems, 20), sep = "\n")

summary_rows = diffs %>% filter(endpoint != "/trait-count")
cat("\n/trait-summary rows: old", sum(!is.na(summary_rows$old)), " new", sum(!is.na(summary_rows$new)),
    " in both", sum(!is.na(summary_rows$old) & !is.na(summary_rows$new)),
    " old only", sum(!is.na(summary_rows$old) & is.na(summary_rows$new)),
    " new only", sum(is.na(summary_rows$old) & !is.na(summary_rows$new)), "\n")

cat_rows = summary_rows %>% filter(str_detect(endpoint, "categorical"), !is.na(old), !is.na(new))
cat("Categorical values identical:", sum(cat_rows$old == cat_rows$new), "of", nrow(cat_rows), "\n")

num_rows = summary_rows %>% filter(str_detect(endpoint, "numeric"), !is.na(old), !is.na(new)) %>%
  mutate(ratio = suppressWarnings(as.numeric(new) / as.numeric(old)))
cat("Numeric means (new / old), n =", sum(is.finite(num_rows$ratio)), "\n")
print(quantile(num_rows$ratio[is.finite(num_rows$ratio)], c(0, 0.05, 0.25, 0.5, 0.75, 0.95, 1)))
cat("Old means served as 0:", sum(num_rows$old == "0", na.rm = TRUE), "\n")

fallback = summary_rows %>% filter(!is.na(new_taxon), new_taxon != taxon) %>% distinct(taxon, new_taxon)
cat("\nQueries answered with their species' rows:", nrow(fallback), "\n")
print(head(fallback, 5))

counts = diffs %>% filter(endpoint == "/trait-count")
cat("\n/trait-count summary changed for", sum(counts$old != counts$new), "of", nrow(counts), "taxa\n")
