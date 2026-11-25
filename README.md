# austraits-api

An R [`plumber`](https://www.rplumber.io/) HTTP API that overlays a web layer over AusTraits /
`traits.build` functions, so AusTraits data can be queried over HTTP by users of any background or
level of technical expertise (for example, from our partner websites) without needing to run R.

The endpoints let users subset the data by taxon and trait, return the size of a subset under a set of
conditions (including filtering on categorical columns), and download the resulting data. Developing
the API is an iterative process driven by our partners' needs, so endpoints may evolve.

**Live API:** endpoints are served under `https://app.austraits.org/api/` (for example <https://app.austraits.org/api/health-check>). Interactive Swagger documentation, where you can try each endpoint, is at **<https://app.austraits.org/api/__docs__/>**.

## Setup

```r
# install.packages("remotes")
remotes::install_github("traitecoevo/austraits") # 4.0.1 or later
install.packages(c("plumber", "dplyr", "tidyr", "stringr", "readr", "purrr", "yaml"))
```

## Data

The API serves one AusTraits release, from `API.build/data/austraits_processed_API_v<version>.rds` (git-ignored). To build it for a new release:

1. Set `ver` in `API.build/dataset_prep.R` and `data_version` at the top of `API.build/API examples v1.R` to the release, e.g. `"8.0.0"`.
2. From inside `API.build/`, run `Rscript dataset_prep.R` (about 3 minutes). It loads the release and the observed species means published with it, using `austraits::load_austraits(version = ver)` and `load_austraits(version = ver, means = TRUE)`; files already in `API.build/data/austraits/` are not downloaded again.
3. Check the response shape against the previous release: run the previous code and data on one port and the new on another, then `Rscript compare_api_versions.R <old URL> <new URL> 200` from `API.build/`.

`/trait-summary` and the summary count in `/trait-count` serve those published means, formatted as these endpoints have always returned them: one value per species (subspecies, varieties and forms take their species' value), the geometric observed mean for numeric traits (arithmetic for traits on a signed scale), and for categorical traits every value recorded, with `*` when there is more than one (for flowering and fruiting months, when the records mark different months).

## Run the API

From the repository root, either source the wrapper:

```r
source("api_wrapper.R")
```

which runs the service on `http://0.0.0.0:8000`, or open `API.build/API examples v1.R` in RStudio and
click the **Run API** button in the top-right of the editor. A Swagger dialogue opens where you can
test inputs and see examples of the returned data.

![Run API button in RStudio](https://user-images.githubusercontent.com/50344360/156276295-eca6e5df-0bfd-4f5a-b60f-66dc3491c6f5.png)

## Endpoint overview

Endpoints are defined in `API.build/API examples v1.R`:

| Method | Path | Description |
|--------|------|-------------|
| `GET`/`HEAD` | `/health-check` | Liveness check — is the API running? |
| `GET` | `/trait-count` | Count of unique traits for a given taxon (by `taxon` name or `APNI_ID`). |
| `GET` | `/trait-summary` | Summarised trait data table for a given taxon. |
| `GET` | `/download-taxon-data` | Trait data for a taxon, served as CSV. |
| `POST` | `/taxa-list` | Unique species for a given trait name. |
| `POST` | `/trait-list` | Unique traits for a given set of inputs. |
| `GET` | `/taxa-autocomplete` | Autocomplete suggestions for taxon names. |
| `GET` | `/trait-autocomplete` | Autocomplete suggestions for trait names. |
| `POST` | `/trait-data` | Full data table for multiple species names and traits. |
| `POST` | `/download-trait-data` | Full data table for multiple taxa, served as CSV. |

A `logger` request filter records information about each incoming request before it is forwarded to
the handler.

## Deployment

The production API is served under `https://app.austraits.org/api/`, as a long-running plumber service started from `api_wrapper.R`. Deployment is manual: copy the updated code (and, for a new AusTraits release, the data file produced by `API.build/dataset_prep.R`) to the server and restart the service. After a deploy, confirm the API is up with:

```sh
curl https://app.austraits.org/api/health-check
```

The API was previously hosted on the ARDC Nectar Research Cloud from a separate `austraits-api-nectar` repository; that deployment is no longer used.

## How to cite

If you use data served by this API, please cite the AusTraits dataset:

> Falster D, Gallagher R, Wenk EH, Wright IJ, Indiarto D, Andrew SC, *et al.* (2021) **AusTraits, a
> curated plant trait database for the Australian flora.** *Scientific Data* 8:254.
> <https://doi.org/10.1038/s41597-021-01006-6>

## AusTraits family

`austraits-api` is part of the **AusTraits family** of packages maintained by the
[AusTraits](https://austraits.org) team. See **[austraits.org](https://austraits.org)** for the
project, the data, and the people behind it.

Contributing? Issues across the family are tracked on one board,
[AusTraits #9](https://github.com/orgs/traitecoevo/projects/9), and new issues are auto-added. Please
read the [issue & labelling guide](https://github.com/traitecoevo/austraits-meta/blob/master/governance/issue-guide.md)
in [`austraits-meta`](https://github.com/traitecoevo/austraits-meta) — the family's cross-package
knowledge and governance hub — before filing.

## Acknowledgements

AusTraits is made possible by contributions from our partner organisations — the
[University of New South Wales](https://www.unsw.edu.au/),
[Western Sydney University](https://www.westernsydney.edu.au/),
[Botanic Gardens of Sydney](https://www.botanicgardens.org.au/),
[the University of Melbourne](https://www.unimelb.edu.au/),
the [Atlas of Living Australia](https://www.ala.org.au/), and the Australian Government
[Department of Climate Change, Energy, the Environment and Water](https://www.dcceew.gov.au) — and
from our [advisory board, data contributors, and past partners](https://austraits.org/team/team-partners.html).

AusTraits is a co-investment partnership with the
[Australian Research Data Commons](https://ardc.edu.au/) (ARDC) through the Planet Research Data
Commons ([DOI: 10.3565/nyk4-4r91](https://doi.org/10.3565/nyk4-4r91)). The ARDC is enabled by the
Australian Government's [National Collaborative Research Infrastructure Strategy](https://www.education.gov.au/ncris)
(NCRIS).
