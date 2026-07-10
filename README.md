# austraits-api

An R [`plumber`](https://www.rplumber.io/) HTTP API that overlays a web layer over AusTraits /
`traits.build` functions, so AusTraits data can be queried over HTTP by users of any background or
level of technical expertise (for example, from our partner websites) without needing to run R.

The endpoints let users subset the data by taxon and trait, return the size of a subset under a set of
conditions (including filtering on categorical columns), and download the resulting data. Developing
the API is an iterative process driven by our partners' needs, so endpoints may evolve.

## Setup

```r
# install.packages("remotes")
remotes::install_github("traitecoevo/austraits.build")
install.packages("plumber")
```

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

This repository holds the API code. The production deployment (containerisation and hosting on the
ARDC Nectar Research Cloud) lives in
[`austraits-api-nectar`](https://github.com/traitecoevo/austraits-api-nectar).

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
read the [issue & labelling guide](https://github.com/traitecoevo/austraits-meta/blob/main/governance/issue-guide.md)
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
