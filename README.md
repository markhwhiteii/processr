# NOTE
I am not actively developing this package any longer, as:
- A different package by the same name and doing the same thing is already published on CRAN: https://cran.r-project.org/web/packages/processR/index.html 
- There's been enough literature written on the issues with the Preacher and Hayes approach that I don't find it particularly useful anymore. See the review section here: https://doi.org/10.1080/00273171.2019.1656051

# processr
R implementation for the PROCESS macro; please read the introduction, which is posted on RPubs: http://rpubs.com/markhw/processr

Still under development. Install using the `devtools` package:

`devtools::install_github("markhwhiteii/processr")`

Alternative installation (instructions from [here](https://stackoverflow.com/a/60296961/4999991)):

 - open R as sudo or admin
 - install devtool if it is not already installed by `packages.install("devtools")` 
 - load the `devtools` package by `library(devtools)`
 - install processr with running `install.packages("https://github.com/markhwhiteii/processr/archive/master.tar.gz")`

for testing the instalation:

 - `library(process1)`
 - `processr::model1`

and if you want to run R in the Jupyter environment just follow [the instructions](https://github.com/IRkernel/IRkernel) to install and regsiter the kernel:

 - `install.packages('IRkernel')`
 - `IRkernel::installspec()`



