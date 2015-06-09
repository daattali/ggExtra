## Test environments
* local Windows 7, R 3.1.3
* ubuntu 12.04 (on travis-ci), R 3.1.3

## R CMD check results
There were no ERRORs or WARNINGs. 

There was 1 NOTE:

* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Dean Attali \<daattali@gmail.com\>'
New submission
Components with restrictions and base license permitting such:
  MIT + file LICENSE
File 'LICENSE':
  YEAR: 2015
  COPYRIGHT HOLDER: Dean Attali

## Reviewer comments
2015-03-25 Prof Brian Ripley: (I think the HTML comment is because the daattali@gmail.com email was inside `<>` tags)
> And reading the CRAN policies did not work: you are required not to send HTML!

> Your checks missed

> The Title field should be in title case, current version then in title case:

> ‘Collection of functions and layers to enhance ggplot2’

> ‘Collection of Functions and Layers to Enhance ggplot2’

2015-03-26 Uwe Ligges:
> Thanks, on CRAN now.

---

# Version 0.1.5
# Round 1

## Test environments

* local Windows 7, R 3.1.3
* ubuntu 12.04 (on travis-ci), R 3.1.3

## Submission comments:

Tested on Windows and 7 and Ubuntu 12.04.  There were no ERRORs or WARNINGs and 2 NOTEs:
1. Non-standard file/directory found at top level:
  'NEWS.md'
2. Maintainer: 'Dean Attali'

License components with restrictions and base license permitting such:
  MIT + file LICENSE
File 'LICENSE':
  YEAR: 2015
  COPYRIGHT HOLDER: Dean Attali
Found the following (possibly) invalid URLs:
  URL: http://daattali.com/shiny/ggExtra-ggMarginal-demo/
    From: man/ggExtra.Rd
          man/ggMarginal.Rd
          man/runExample.Rd
          inst/doc/overview.html
    Status: 404
    Message: Not Found

The Title field should be in title case, current version then in title case:
'Add Marginal Histograms to ggplot2, and More ggplot2 Enhancements'
'Add Marginal Histograms to Ggplot2, and More ggplot2 Enhancements'

Comments about notes:
1. I believe that NEWS.md has recently been allowed on CRAN
2. The URL is valid (I'm not sure why it keeps saying 404, I don't understand how to make R think it's ok, but the URL is fine)
3. I don't think ggplot2 should be in title case

Thanks

## Reviewer comments
