# gaussplotR 0.2.5
* Citation information updated  
* Ready for updating CRAN

# gaussplotR 0.2.4
* Post-peer review at the Journal of Open Source Software.
* `print()` and `summary()` on objects that are output from `fit_gaussian_2D()`
now round parameter estimates to two decimal places.
* R version requirement downgraded to R >= 3.3.0 


# gaussplotR 0.2.2
* Newest functions enhance the automation of comparing among various Gaussian 
fit models.  
* The `autofit_gaussian_2D()` function can be used to find the best-fitting 
model for a given data set.  
* The `compare_gaussian_fits()` function compares models via criteria such as 
rmse or rss.  
* A `characterize_gaussian_fits()` analyzes the orientation and partial 
correlations of Gaussian data. Features include computation of partial 
correlations between response variables and independent and diagonally-tuned 
predictions, along with Z-difference scoring.  

# gaussplotR 0.2.0

* Added a `fit_gaussian_2D()` function to apply any of several available methods
to fit a 2D-Gaussian to supplied data
* `predict_gaussian_2D()` has been enhanced to allow any method to be used in
`fit_gaussian_2D()` and then fed into the predict function

# gaussplotR 0.1.6

* Added a `compute_gaussian_volume()` function to compute the volume under a 
given 2D-Gaussian

# gaussplotR 0.1.4

* Miscellaneous formatting fixes for CRAN checks. No changes to functions.

# gaussplotR 0.1.3

* Added a `NEWS.md` file to track changes to the package.
* Package now seems to be ready for submission to CRAN
