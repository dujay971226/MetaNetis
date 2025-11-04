
<!-- README.md is generated from README.Rmd. Please edit that file -->

# MetaNetis

## Description

is an package purpose-built to standardize the critical step of
biological interpretation in quantitative metabolomics studies. It takes
raw metabolite concentration data and the associated biological context
(age and biospecimen type) and benchmarks it against a robust,
internally-curated baseline derived from the . The core biological data
being analyzed are metabolite concentration values (e.g., or ) and their
functional associations with metabolic pathways. MetaNetis significantly
improves the current workflow in computational biology by providing . ,
MetaNetis compares results against and directly links individual
concentration deviations to pathway activity (Hypoactive/Hyperactive).
This unique approach eliminates the interpretation bias inherent in
generic comparisons and allows for the precise, quantitative assessment
of functional metabolic changes. Development for was conducted on
version 4.5.1 (2025-06-13 ucrt) using Windows 10 x64 (build 19045).


    <!-- badges: start -->
    <!-- badges: end -->


    ## Installation

    You can install the development version of MetaNetis from [GitHub](https://github.com/) with:


    ``` r
    install.packages("devtools")
    #> Installing package into 'C:/Users/dujay/AppData/Local/Temp/RtmpEn0ekt/temp_libpath343434bd1f7e'
    #> (as 'lib' is unspecified)
    #> package 'devtools' successfully unpacked and MD5 sums checked
    #> 
    #> The downloaded binary packages are in
    #>  C:\Users\dujay\AppData\Local\Temp\RtmpyqpTdu\downloaded_packages
    library("devtools")
    #> Warning: package 'devtools' was built under R version 4.5.2
    #> Loading required package: usethis
    devtools::install_github("dujay971226/MetaNetis", build_vignettes = TRUE)
    #> Using GitHub PAT from the git credential store.
    #> Downloading GitHub repo dujay971226/MetaNetis@HEAD
    #> magrittr (2.0.3 -> 2.0.4) [CRAN]
    #> Skipping 1 packages not available: KEGGREST
    #> Installing 1 packages: magrittr
    #> Installing package into 'C:/Users/dujay/AppData/Local/Temp/RtmpEn0ekt/temp_libpath343434bd1f7e'
    #> (as 'lib' is unspecified)
    #> package 'magrittr' successfully unpacked and MD5 sums checked
    #> 
    #> The downloaded binary packages are in
    #>  C:\Users\dujay\AppData\Local\Temp\RtmpyqpTdu\downloaded_packages
    #> ── R CMD build ─────────────────────────────────────────────────────────────────
    #>          checking for file 'C:\Users\dujay\AppData\Local\Temp\RtmpyqpTdu\remotes3f3c6472639e\dujay971226-MetaNetis-8523c35/DESCRIPTION' ...  ✔  checking for file 'C:\Users\dujay\AppData\Local\Temp\RtmpyqpTdu\remotes3f3c6472639e\dujay971226-MetaNetis-8523c35/DESCRIPTION'
    #>       ─  preparing 'MetaNetis':
    #>    checking DESCRIPTION meta-information ...     checking DESCRIPTION meta-information ...   ✔  checking DESCRIPTION meta-information
    #>       ─  checking for LF line-endings in source and make files and shell scripts
    #>       ─  checking for empty or unneeded directories
    #>       ─  building 'MetaNetis_0.1.0.tar.gz'
    #>      
    #> 
    #> Installing package into 'C:/Users/dujay/AppData/Local/Temp/RtmpEn0ekt/temp_libpath343434bd1f7e'
    #> (as 'lib' is unspecified)
    library("MetaNetis")

## 🎯 Overview

MetaNetis is an $\text{R}$ package designed to bridge the gap between
raw metabolomics data and biological interpretation. It provides a
standardized framework for benchmarking user-supplied metabolite
concentrations against a robust, healthy human reference population
derived from the Human Metabolome Database (HMDB).

The package’s core value lies in its ability to generate high-confidence
classifications and translate those classifications into functional
insights about metabolic pathway activity.

### ✨ Features

``` r
ls("package:MetaNetis")
#> [1] "GetRefRanges"        "MetabAnalysis"       "reference_ranges_df"
#> [4] "SetAltBaseline"
data(package = "MetaNetis") 
browseVignettes("MetaNetis")
#> No vignettes found by browseVignettes("MetaNetis")
```

## Acknowledgements

This package was developed as part of an assessment for 2025 BCB410H:
Applied Bioinformatics course at the University of Toronto, Toronto,
CANADA. MetaNetis welcomes issues, enhancement requests, and other
contributions. To submit an issue, use the GitHub issues.
