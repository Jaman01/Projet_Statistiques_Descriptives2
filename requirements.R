pkgLoad <- function( packages = "favourites" ) {
  
  if( length( packages ) == 1L && packages == "favourites" ) {
    # A trier par ordre alphabétique.
    packages <- c(
      "Amelia",
      "aod",
      "dplyr",
      "e1071",
      "GGally",
      "ggplot2",
      "MLmetrics",
      "psych",
      "ROSE",
      "rpart",
      "rpart.plot"
    )
  }
  
  packagecheck <- match( packages, utils::installed.packages()[, 1] )
  
  packagestoinstall <- packages[ is.na( packagecheck ) ]
  
  if( length( packagestoinstall ) > 0L ) {
    utils::install.packages( packagestoinstall,
                             repos = "https://pbil.univ-lyon1.fr/CRAN/"
    )
  } else {
    print( "All requested packages already installed" )
  }
  
  for( package in packages ) {
    suppressPackageStartupMessages(
      library( package, character.only = TRUE, quietly = TRUE )
    )
  }
  
}


pkgLoad()
