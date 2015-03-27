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
