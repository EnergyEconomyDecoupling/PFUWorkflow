# This script creates constants and saves them in the right locations.
# If there are any changes to these constants,
# source this script before building the package.

library(magrittr)
library(IEATools)


#
# Names and constants associated with exemplar tables.
#

exemplar_names <- list(exemplar_tab_name = "exemplar_table",
                       prev_names = "Prev.names",
                       exemplars = "Exemplars",
                       exemplar_country = "Exemplar.country",
                       row_code = "ROW.code",
                       country_name = "Country.name",
                       world = "World")
usethis::use_data(exemplar_names, overwrite = TRUE)
