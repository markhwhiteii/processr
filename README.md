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



