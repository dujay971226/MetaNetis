
<!-- README.md is generated from README.Rmd. Please edit that file -->

# MetaNetis

## Description

$\text{MetaNetis}$ is an package purpose-built to standardize the
critical step of biological interpretation in quantitative metabolomics
studies. It takes raw metabolite concentration data and the associated
biological context (age and biospecimen type) and benchmarks it against
a robust, internally-curated baseline derived from the
$\text{Human Metabolome Database (HMDB)}$. The core biological data
being analyzed are metabolite concentration values (e.g., or ) and their
functional associations with metabolic pathways. MetaNetis significantly
improves the current workflow in computational biology by providing
$\text{novel, quantitative functional interpretation}$.
$\text{Unlike tools such as MetaboAnalyst, which typically rely on pathway enrichment statistics}$,
MetaNetis compares results against
$\text{real-world, sample-matched metabolite reference data}$ and
directly links individual concentration deviations to pathway activity
(Hypoactive/Hyperactive). This unique approach eliminates the
interpretation bias inherent in generic comparisons and allows for the
precise, quantitative assessment of functional metabolic changes.
Development for $\text{MetaNetis}$ was conducted on version 4.5.1
(2025-06-13 ucrt) using Windows 10 x64 (build 19045).


    <!-- badges: start -->
    <!-- badges: end -->


    ## Installation

    You can install the development version of MetaNetis from [GitHub](https://github.com/dujay971226/MetaNetis) with:


    ``` r
    install.packages("devtools")
    #> Installing package into 'C:/Users/dujay/AppData/Local/Temp/RtmpQHqxXd/temp_libpath566078438da'
    #> (as 'lib' is unspecified)
    #> package 'devtools' successfully unpacked and MD5 sums checked
    #> 
    #> The downloaded binary packages are in
    #>  C:\Users\dujay\AppData\Local\Temp\RtmpeKenKy\downloaded_packages
    library("devtools")
    #> Warning: package 'devtools' was built under R version 4.5.2
    #> Loading required package: usethis
    devtools::install_github("dujay971226/MetaNetis", build_vignettes = TRUE)
    #> Using GitHub PAT from the git credential store.
    #> Downloading GitHub repo dujay971226/MetaNetis@HEAD
    #> magrittr (2.0.3 -> 2.0.4) [CRAN]
    #> purrr    (1.1.0 -> 1.2.0) [CRAN]
    #> Installing 2 packages: magrittr, purrr
    #> Installing packages into 'C:/Users/dujay/AppData/Local/Temp/RtmpQHqxXd/temp_libpath566078438da'
    #> (as 'lib' is unspecified)
    #> 
    #>   There is a binary version available but the source version is later:
    #>       binary source needs_compilation
    #> purrr  1.1.0  1.2.0              TRUE
    #> 
    #> package 'magrittr' successfully unpacked and MD5 sums checked
    #> 
    #> The downloaded binary packages are in
    #>  C:\Users\dujay\AppData\Local\Temp\RtmpeKenKy\downloaded_packages
    #> installing the source package 'purrr'
    #> ── R CMD build ─────────────────────────────────────────────────────────────────
    #>          checking for file 'C:\Users\dujay\AppData\Local\Temp\RtmpeKenKy\remotes25245d0a125b\dujay971226-MetaNetis-44f53fa/DESCRIPTION' ...  ✔  checking for file 'C:\Users\dujay\AppData\Local\Temp\RtmpeKenKy\remotes25245d0a125b\dujay971226-MetaNetis-44f53fa/DESCRIPTION'
    #>       ─  preparing 'MetaNetis':
    #>    checking DESCRIPTION meta-information ...     checking DESCRIPTION meta-information ...   ✔  checking DESCRIPTION meta-information
    #>       ─  installing the package to build vignettes
    #>          creating vignettes ...     creating vignettes ...   ✔  creating vignettes (14.4s)
    #>       ─  checking for LF line-endings in source and make files and shell scripts (619ms)
    #>       ─  checking for empty or unneeded directories
    #>       ─  building 'MetaNetis_0.1.0.tar.gz'
    #>      
    #> 
    #> Installing package into 'C:/Users/dujay/AppData/Local/Temp/RtmpQHqxXd/temp_libpath566078438da'
    #> (as 'lib' is unspecified)
    library("MetaNetis")

## 🎯 Overview

MetaNetis is an $\text{R}$ package designed to bridge the gap between
raw metabolomics data and deep biological interpretation. It provides a
standardized framework for benchmarking user-supplied metabolite
concentrations against a robust, healthy human reference population
derived from resources like the Human Metabolome Database
($\text{HMDB}$).

The package’s core value lies in its ability to generate high-confidence
classifications and translate those classifications into functional
insights about metabolic pathway activity.

Critically, MetaNetis offers robust functionality to evaluate
quantitative, clinical sampled data such as blood and urine. This
capability is a key differentiator from existing tools; for instance,
popular platforms like MetaboAnalyst are often restricted to qualitative
analysis, relying solely on pathway enrichment. MetaNetis moves beyond
simple enrichment to provide a direct, directional assessment of
metabolic activity (e.g., hyper- or hypo-regulation), making it a
powerful tool for metabolomic and clinical research.

<figure>
<img src="vignettes/package_overview.png"
alt="MetaNetis Package Overview" />
<figcaption aria-hidden="true">MetaNetis Package Overview</figcaption>
</figure>

### ✨ Features

``` r
ls("package:MetaNetis")
#> [1] "GetPathwayMap"       "GetRefRanges"        "MapToPathway"       
#> [4] "metab_to_pwys"       "MetabAnalysis"       "PlotNetwork"        
#> [7] "reference_ranges_df" "SetAltBaseline"
data(package = "MetaNetis") 
browseVignettes("MetaNetis")
#> starting httpd help server ... done
```

## Contribution

Author and Core Package ContributionsThe package MetaNetis was conceived
and primarily developed by $\text{Jay Du}$. Jay’s core contributions
include designing the novel quantitative scoring framework that moves
beyond traditional enrichment analysis, developing the robust data
processing pipeline for standardizing the Human Metabolome Database
($\text{HMDB}$) reference ranges, and implementing the functional
pathway scoring logic (MapToPathway and MetabAnalysis). Significant
effort was dedicated to creating a modular and testable package
structure, ensuring that core data retrieval functions (GetPathwayMap,
GetRefRanges) and visualization tools (PlotNetwork) are seamlessly
integrated.

Generative AI Tool ContributionsThe generative AI tool, Gemini, served
as a key collaborative partner throughout the development of MetaNetis.
Its assistance was instrumental in ensuring code stability and clarity
across the package. Specifically, Gemini was utilized to generate clear,
comprehensive commenting and documentation for all core functions within
the package. Furthermore, the AI tool provided essential debugging
support for both the and functions, helping to resolve complex issues
related to quantitative score aggregation and data matching across
disparate input structures, ensuring the reliability of the core
analytical pipeline.

## Reference

Csardi, G., & Nepusz, T. (2006). The igraph software package for complex
network research. InterJournal, Complex Systems, 1695.

Google. (2025). Gemini.

Müller, K., & Wickham, H. (2023). tibble: Simple data frames (Version
3.2.1). <https://tibble.tidyverse.org/>

Pedersen, T. L. (2022). ggraph: An implementation of grammar of graphics
for graphs (Version 2.1.0). <https://ggraph.data-imaginist.com>

R Core Team. (2024). R: A language and environment for statistical
computing. R Foundation for Statistical Computing.
<https://www.R-project.org/>

Wickham, H. (2016). ggplot2: Elegant graphics for data analysis.
Springer-Verlag.

Wickham, H., François, R., Henry, L., & Müller, K. (2023). dplyr: A
grammar of data manipulation (Version 1.1.3).
<https://dplyr.tidyverse.org/>

Wickham, H., & Hester, J. (2024). stringr: Simple, consistent string
routines (Version 1.5.1) \[R package\]. <https://stringr.tidyverse.org/>

Wickham, H., & Ruiz, M. (2023). tidyr: Tidy messy data (Version 1.3.0).
<https://tidyr.tidyverse.org/>

Wishart, D. S., et al. (2022). HMDB 5.0: The Human Metabolome Database
for 2022. Nucleic Acids Research, 50(D1), D218–D227. <https://hmdb.ca/>

## Acknowledgements

This package was developed as part of an assessment for 2025 BCB410H:
Applied Bioinformatics course at the University of Toronto, Toronto,
CANADA. MetaNetis welcomes issues, enhancement requests, and other
contributions. To submit an issue, use the GitHub issues.
