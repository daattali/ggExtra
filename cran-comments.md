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

2015-06-08

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

2015-06-09 Kurt Hornik

Is this your own web site?  I see that   

$ curl -I -L http://daattali.com/shiny/   
HTTP/1.1 200 OK   
Server: nginx   
Date: Tue, 09 Jun 2015 14:37:35 GMT   
Content-Type: text/html; charset=UTF-8   
Content-Length: 2981   
Connection: keep-alive   
Accept-Ranges: bytes   
ETag: "2981-1431826214000"   
Cache-Control: public, max-age=0   
Last-Modified: Sun, 17 May 2015 01:30:14 GMT   
   
but   
   
$ curl -I -L http://daattali.com/shiny/ggExtra-ggMarginal-demo/   
HTTP/1.1 404 Not Found   
Server: nginx   
Date: Tue, 09 Jun 2015 14:37:12 GMT   
Content-Type: text/html; charset=UTF-8   
Content-Length: 18   
Connection: keep-alive   
x-ua-compatible: IE=edge,chrome=1   
   
   
We also see   
   
Possibly mis-spelled words in DESCRIPTION:   
  ggMarginal (9:17)   
  ggplot (2:35, 2:53, 8:60, 10:42)   
   
The Title field should be in title case, current version then in title case:   
‘Add Marginal Histograms to ggplot2, and More ggplot2 Enhancements’   
‘Add Marginal Histograms to Ggplot2, and More ggplot2 Enhancements’   
   
You need to put single quotes around ggplot2 in the title and   
description, and write ggMarginal()   
   
Best

# Version 0.1.5.2
# Round 2

## Test environments

* local Windows 7, R 3.1.3
* ubuntu 12.04 (on travis-ci), R 3.1.3

## Submission comments:

2015-06-09

Fixed from previous submission: added single quotes around 'ggplot' and added brackets after 'ggMarginal()`.  
Still getting the "Found the following (possibly) invalid URLs:" NOTE for shiny apps, I'm not sure how to fix my shiny server configuration. I will try to look into that in the future. Reply from previous submission comment: yes, it is my own site

## Reviewer comments

2015-06-10 Kurt Hornik

Ok, thanks, on CRAN now.

---

# Version 0.2.0
# Round 1

## Test environments

* local Windows 7, R 3.2.0
* ubuntu 12.04 (on travis-ci), R 3.2.0

## Submission comments:

2015-07-11

Tested on Windows and 7 and Ubuntu 12.04. There were no ERRORs or WARNINGs and 2 NOTEs.

Note 1 is about possibly invalid URLs. This is because I have a URL of a Shiny app, and Shiny Server doesn't currently support returning http headers. Note 2 said I should capitalize `ggplot2` to `Ggplot2`.

## Reviewer comments:

2015-07-11 Uwe Ligges

```
Thanks, we see:

Found the following (possibly) invalid URLs:
  URL: http://daattali.com/shiny/ggExtra-ggMarginal-demo/
    From: man/ggExtra.Rd
          man/ggMarginal.Rd
          man/runExample.Rd
          inst/doc/overview.html
    Status: 404
    Message: Not Found


* checking R code for possible problems ... NOTE
ggMarginal: no visible global function definition for 'is'
Undefined global functions or variables:
  is



Please fix.

Best,
Uwe Ligges
```

# Round 2

## Submission comments:

2015-07-11

Fixed previous comment: * checking R code for
  possible problems ... NOTE
ggMarginal: no visible
  global function definition for 'is'
Undefined global
  functions or variables:
  is

## Reviewer comments:

2015-07-12 Kurt Hornik

```
We now get

* checking dependencies in R code ... WARNING
'::' or ':::' import not declared from: ‘methods’

Pls fix.  Also, what about

Found the following (possibly) invalid URLs:
  URL: http://daattali.com/shiny/ggExtra-ggMarginal-demo/
    From: man/ggExtra.Rd
          man/ggMarginal.Rd
          man/runExample.Rd
          inst/doc/overview.html
    Status: 404
    Message: Not Found


???
```

# Round 3

## Submission comments:

2015-07-12

```
Fixed previous NOTE by adding 'methods' package to
  DESCRIPTION. (Note was:
* checking dependencies in R
  code ... WARNING
'::' or ':::' import not declared
  from: ‘methods’)

Previous submission also had
  a comment about the "possible invalid URLs", which I
  addressed in my first submission: The URLs are valid
  URLs that point to Shiny apps on a Shiny Server.
  Shiny Server doesn't currently support returning http
  headers, hence the curl command fails. But the URLs
  are fine.  Shiny Server is working on fixing that.
```

## Reviewer comments:

2015-07-12 Uwe Ligges

Thanks, on CRAN now.

---

# Version 0.3.0

# Round 1

## Test environments

* local Windows 7, R 3.2.0
* ubuntu 12.04 (on travis-ci), R 3.2.0

## Submission comments:

2015-09-05

Tested on Windows and 7 and Ubuntu 12.04. There were no ERRORs or WARNINGs and 2 NOTEs.

Note 1 informed me who the maintainer and what the license is. Note 2 said I should capitalize ggplot2 to Ggplot2.

## Reviewer comments:

2015-09-06 Uwe Ligges

```
Thanks, we see:

* checking R code for possible problems ... NOTE
ggMarginal : addMainTheme: no visible global function definition for
  'packageVersion'
Undefined global functions or variables:
  packageVersion
Consider adding
  importFrom("utils", "packageVersion")
to your NAMESPACE.

Please fix.
```

# Round 2

## Test environments

* local Windows 7, R 3.2.2
* ubuntu 12.04 (on travis-ci), R 3.2.2

## Submission comments:

2015-09-06

addressed previous comment: namespaced utils::packageVersion

## Reviewer comments:

2015-09-06 Uwe Ligges

Thanks, on CRAN now.
