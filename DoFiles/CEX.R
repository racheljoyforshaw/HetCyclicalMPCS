library(devtools)
library(lodown)
# examine all available CES microdata files
ces_cat <-
  get_catalog( "ces" ,
               output_dir = file.path( path.expand( "~" ) , "CES" ) )

# 2001 only
ces_cat <- subset( ces_cat , year == 2001 )
# download the microdata to your local computer
ces_cat <- lodown( "ces" , ces_cat )

