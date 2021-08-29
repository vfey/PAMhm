---
title: CRAN package PAMhm
---

## Resubmission 2021-08-24
This is a resubmission. The version was increased to 0.1.2 after addressing the comments by CRAN staff member Uwe Ligges:  

* Fixed misspelled words in DESCRIPTION
* Removed underscores in DESCRIPTION
* Added reference about the PAM method to DESCRIPTION and PAM.hm() function documentation

In addition, the following changes and additions were made:  

* added a vignette and corresponding DESCRIPTION entries
* improved examples
* made small corrections and additions to PAM.hm() function documentation
* Optimised default trimming behavior
* Added argument `winsorize.mat` to control _winsorization_ and changed default behavior to apply that

## Test environments
* local OS X install: x86_64-apple-darwin17.0, R 4.0.2
* win-builder (devel and release)
* CentOS Linux release 7.9.2009 (Core) [:core-4.1-amd64:core-4.1-noarch], R 4.0.4

## R CMD check results
There were no ERRORs or WARNINGs.

There was 1 NOTE:

```
R CMD check --as-cran PAMhm_0.1.2.tar.gz
.
* checking CRAN incoming feasibility ... NOTE     
Maintainer: ‘Vidal Fey <vidal.fey@gmail.com>’
.
```
