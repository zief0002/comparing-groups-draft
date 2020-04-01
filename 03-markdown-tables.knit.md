# Tables in Markdown (HTML)



Often it is useful to format table output to make it look good or to adhere a particular style (e.g., APA). There are several packages that help in this endeavor when working in an Rmarkdown document. Below the primary tools used are:

- The `kable()` function from the **knitr** package; and 
- Functions from the **kableExtra** package.

Other packages for formatting tables, among others, include the [gt package](https://gt.rstudio.com/), the [huxtable package](https://hughjonesd.github.io/huxtable/), and the [expss package](https://gdemin.github.io/expss/). For complete APA formatting check out the [papaja package](https://crsh.github.io/papaja_man/).


The primary input to the `kable()` function is a data frame. This data frame will include the primary contents for you table. A data frame can either be created as the ouput of a function or directly using the `data.frame()` function. Below I will create and format a couple different common tables produced for statistical reports. To do so, I will use data from *ed-schools-2018.csv* file (see the [data codebook](#ed-schools-2018) here). These data include institutional-level attributes for several graduate education schools/programs rated by *U.S. News and World Report* in 2018.

































