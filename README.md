# RBenford

Benford's Law library in R.
Supports loading CSV files with first column as data and compare them against Benford Distributions (first digit, second digits, and both digits).

For goodness-of-fit functions, besides the common chi-Square and Mean Absolute Deviation (MAD) tests, I have also implemented the Excess Mean Absolute Deviation (ExcessMAD) test according to Silva and Gouvea (2023) which should give improved accuracy for small (i.e. < 1000) sample sizes.

Reference(s)
-------------
Silva, A. de A. and Gouvea, M.A. (2023) 'Study on the effect of sample size on type I error, in the first, second and first-two digits excessmad tests', *International Journal of Accounting Information Systems*, 48, 100599. https://doi.org/10.1016/j.accinf.2022.100599