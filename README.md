# austraits_api_v1

This is a repository containing the code to support the AusTraits API. It uses the R-plumber package to overlay a http layer over regular R fucntions and return data based on queries from a user. The goal is to ensure AusTraits data is easily accessible to more users from different backgrounds and levels of technological expertise via our partner websites. 

The basic functions allow users to subset the data based on taxa and trait, return the size of the subset given a series of confitions which include filtering by some columns containing cateogical variables. 

Developing the API functions will be an iterative process as we consult with the needs of our partners. 

To run the code and test the API endpoints, click the Run API button in the top right hand corner of R studio (see image link below). A dialogue box should open up where you can test inputs and get examples of the returned data.

![image](https://user-images.githubusercontent.com/50344360/156276295-eca6e5df-0bfd-4f5a-b60f-66dc3491c6f5.png)

## AusTraits family

`austraits-api` is part of the **AusTraits family** of packages maintained by the
[AusTraits](https://austraits.org) team. See **[austraits.org](https://austraits.org)** for the
project, the data, and the people behind it.

Contributing? Issues across the family are tracked on one board,
[AusTraits #9](https://github.com/orgs/traitecoevo/projects/9), and new issues are auto-added. Please
read the [issue & labelling guide](https://github.com/traitecoevo/austraits-meta/blob/main/governance/issue-guide.md)
in [`austraits-meta`](https://github.com/traitecoevo/austraits-meta) — the family's cross-package
knowledge and governance hub — before filing.
