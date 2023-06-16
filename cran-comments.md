Dear CRAN TEAM,
this is a re-submission of the package "IndexWizard". All requested changes were made.
Please Note that my affiliation changed in the meantime and thus my mail address - I will send a confirmation note from my old address as well.

Regards,
Johannes Geibel



Thanks,

Please reduce the length of the title to less than 65 characters.

--> reduced 

Please remove the redundant "Provides a function to" from the beginning of your description text.

--> changed


If there are references describing the methods in your package, please add these in the description field of your DESCRIPTION file in the form authors (year) <doi:...> authors (year) <arXiv:...> authors (year, ISBN:...) or if those are not available: <[https:...]https:...> with no space after 'doi:', 'arXiv:', 'https:' and angle brackets for auto-linking.
(If you want to add a title as well please put it in quotes: "Title")

--> The paper descibing the methology is published now, references were made where appropriate

Please add \value to .Rd files regarding exported methods and explain the functions results in the documentation. Please write about the structure of the output (class) and also what the output means. (If a function does not return a value, please document that too, e.g.
\value{No return value, called for side effects} or similar) Missing Rd-tags:
      print.SelInd.Rd: \value
      summary.SelInd.Rd: \value

--> added

Please fix and resubmit.

Best,
Victoria Wimmer


# current check results --------------------------------------------------------
## R CMD check results
❯ checking CRAN incoming feasibility ... NOTE
  Maintainer: 'Johannes Geibel <johannes.geibel@fli.de>'
  
  New submission

0 errors ✔ | 0 warnings ✔ | 1 note ✖


# previous results -------------------------------------------------------------
## Test environments
- R-hub windows-x86_64-devel (r-devel)
- R-hub ubuntu-gcc-release (r-release)
- R-hub fedora-clang-devel (r-devel)

## R CMD check results
> On windows-x86_64-devel (r-devel)
  checking CRAN incoming feasibility ... [15s] NOTE
  Maintainer: 'Johannes Geibel <johannes.geibel@uni-goettingen.de>'
  
  New submission

> On windows-x86_64-devel (r-devel)
  checking for detritus in the temp directory ... NOTE
  Found the following files/directories:
    'lastMiKTeXException'

> On ubuntu-gcc-release (r-release)
  checking CRAN incoming feasibility ... NOTE
  Maintainer: ‘Johannes Geibel <johannes.geibel@uni-goettingen.de>’
  
  New submission

> On fedora-clang-devel (r-devel)
  checking CRAN incoming feasibility ... [5s/16s] NOTE
  Maintainer: ‘Johannes Geibel <johannes.geibel@uni-goettingen.de>’
  
  New submission

> On fedora-clang-devel (r-devel)
  checking HTML version of manual ... NOTE
  Skipping checking HTML validation: no command 'tidy' found
