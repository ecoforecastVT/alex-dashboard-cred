# To install from CRAN
install.packages("staticryptR")
library(staticryptR)

# encrypt.r
staticryptR::staticryptr(
  files = "docs",
  directory = ".",
  password = "testpassword",
  short = TRUE,
  recursive = TRUE
)