# PrOCoil - Predicting the Oligomerization of Coiled Coil Proteins
We have developed an SVM-based classification method for predicting whether a
given coiled coil sequence is a trimer or dimer (assuming that it is one of both).
This method also allows for a deep analysis of the sequence which residues are
mainly responsible for the outcome. The software is available as an R package
'procoil' and as a simple-to-use [Web application](https://www.bioinf.jku.at/software/procoil/index.html).

**Important Note:** the prediction models have been updated with the release of
version 2.0.0 of the 'procoil' R package. The updated data sets and some
information on how they have been collected are available from the
[PrOCoil Data Repository (v2)](https://www.bioinf.jku.at/software/procoil/data_v2.html). If you want to use the original prediction models as published by
Mahrenholz et al. (2011), please follow the instructions in Section 5.5.3 of the
[user manual](https://bioconductor.org/packages/release/bioc/vignettes/procoil/inst/doc/procoil.pdf).
The data sets on which the original PrOCoil models were based are still available
from the [PrOCoil Data Repository (v1)](https://www.bioinf.jku.at/software/procoil/data_v1.html).


## Installation

The package can be installed from
[Bioconductor](https://bioconductor.org/). Therefore, the the simplest way to install the package is to enter
```
if (!require("BiocManager", quietly = TRUE))
    install.packages("BiocManager")

BiocManager::install("procoil")
```
into your R session. If, for what reason ever, you prefer to install the package manually, follow the instructions in the [user manual](https://bioconductor.org/packages/release/bioc/vignettes/procoil/inst/doc/procoil.pdf).

## User support

If you encounter any issues or if you have any question that might be of interest also for other users, before writing a private message to the package developers/maintainers, please create an issue in this repository and also consider posting on [Bioconductor Support](https://support.bioconductor.org/) or on [StackOverflow](https://stackoverflow.com/). For other matters regarding the package, please contact the package author(s).

## Citing this package

If you use this package for research that is published later, you are kindly asked to cite it as follows:

- C. C. Mahrenholz, I. G. Abfalter, U. Bodenhofer, R. Volkmer, and S. Hochreiter (2011). Complex networks govern coiled coil oligomerization - predicting and profiling by means of a machine learning approach. *Mol. Cell. Proteomics,* **10**(5):M110.004994, 2011. DOI: [10.1074/mcp.M110.004994](https://doi.org/10.1074/mcp.M110.004994).