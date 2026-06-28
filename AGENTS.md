# austraits-api — agent & contributor guide

`austraits-api` is the code behind the **AusTraits API**: an R [plumber](https://www.rplumber.io/)
service that overlays an HTTP layer over plain R functions so AusTraits data can be subset and
queried by taxa and trait — making the data accessible to partner websites and users of varying
technical backgrounds.

## Repo-local guidance

- **Purpose:** expose AusTraits data over HTTP via plumber endpoints. The README notes development
  is iterative, driven by partner needs.
- **Entry point:** `api_wrapper.R` boots the service — it `plumb()`s `API.build/API examples v1.R`
  and runs it with `pr_run(port = 8000, host = "0.0.0.0")`. The README also describes launching via
  the **Run API** button in RStudio for interactive testing of the endpoints.
- **Endpoint definitions:** `API.build/API examples v1.R` holds the plumber routes (annotated with
  `#*`), including `/health-check`, `/trait-count`, `/trait-summary`, `/download-taxon-data`,
  `/taxa-list`, `/trait-list`, `/taxa-autocomplete`, `/trait-autocomplete`, `/trait-data`, and
  `/download-trait-data`. `API.build/API examples v1.yml` is the accompanying OpenAPI/spec file.
- **Data prep:** `API.build/dataset_prep.R` prepares the data the endpoints serve. The served data
  lives under `API.build/data/`, which is **git-ignored** (see `.gitignore`) — it is not committed.
- **Usage analysis:** `API activity.R` is a standalone script for downloading and summarising the
  API activity log (object-storage hit logs); not part of the running service.
- **Run it:** install `plumber` (and dependencies the route file uses), then
  `Rscript api_wrapper.R` (or `source("api_wrapper.R")`), or use RStudio's Run API button. There is
  no `DESCRIPTION` / test suite — this is a plumber app, not an R package.
- **Default branch:** `master`.

> Heads-up: `API.build/data` is git-ignored, so a fresh clone has no data to serve — run
> `API.build/dataset_prep.R` (or otherwise populate `API.build/data/`) before the endpoints will
> return results.

---

## AusTraits family — cross-package context

`austraits-api` is part of the **AusTraits family** (a subset of the
[`traitecoevo`](https://github.com/traitecoevo) org) — here, the AusTraits API (programmatic data
access service). Family-wide concerns are documented centrally in
**[austraits-meta](https://github.com/traitecoevo/austraits-meta)** — don't restate them here, read
them there:

- **Start with [`AGENTS.md`](https://github.com/traitecoevo/austraits-meta/blob/main/AGENTS.md)** —
  pipeline order, who owns what, dependency direction, source-of-truth rules, cross-boundary
  artifacts, gotchas.
- **[`dependencies.yml`](https://github.com/traitecoevo/austraits-meta/blob/main/dependencies.yml)** —
  machine-readable package graph + cross-boundary artifacts.
- **[`governance/`](https://github.com/traitecoevo/austraits-meta/tree/main/governance)** —
  label taxonomy, board #9 conventions, release playbooks, triage.

**Filing issues:** the whole family is tracked on one board,
[AusTraits #9](https://github.com/orgs/traitecoevo/projects/9) (new issues auto-add to it). Follow
the [issue & labelling guide](https://github.com/traitecoevo/austraits-meta/blob/main/governance/issue-guide.md):
pick one work-type label (`bug` / `task` / `epic`); Status and Priority are set on the board, not as
labels.

> austraits-meta is hand-maintained prose — a map, not ground truth. Verify specifics against the
> actual repos.
