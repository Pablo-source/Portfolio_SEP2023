# renv
library(renv)

# Record packages version usin {renv}
# Create renv file
renv::init()


# To use and re-created the previous adhoc specific environmnet use this code below:
renv:restore()
