# install tabulizer and tabulizerjars libraries with the following line
#   remotes::install_github(c("ropensci/tabulizerjars", "ropensci/tabulizer"), INSTALL_opts = "--no-multiarch")
# Initially had problems with tabulizer, getting a java error
#   followed instructions in https://www.youtube.com/watch?v=nlsWjezvsg8 to install older version of java
#   tabulizer seems to work fine now
library(miniUI)
library(rJava)
library(tidyverse)
library(tabulizer)
library(tabulizerjars)
library(munsell)
library(reshape2)
library(reshape)

table1 <- extract_tables('./data/atomic_mass_abund.pdf',
                     method = "stream",
                     output = "data.frame")
table1b <- extract_tables('./data/atomic_mass_abund.pdf',
                         method = "decide",
                         output = "data.frame")

areas <- extract_areas('./data/atomic_mass_abund.pdf',
                       pages = c(1, 1, 2, 2, 3, 3, 4, 4, 5))
tables <- rbind(data.frame(areas[1]),
                data.frame(areas[2]),
                data.frame(areas[3]),
                data.frame(areas[4]),
                data.frame(areas[5]),
                data.frame(areas[6]),
                data.frame(areas[7]),
                data.frame(areas[8]),
                data.frame(areas[9]))



names(tables) <- c("Z", "name", "Symbol", "mass", "abundance")

# Remove extraneous lines
tables <- tables[-which(tables$Z == 'Z'),]
tables <- tables[-which(tables$mass == 'Mass of Atom'),]
tables <- tables[-which(tables$mass == '(u)'),]

# Calculate mass number of isotope
tables$A <- lag(tables$Symbol, 1)
#The following line does not work because some mass values are non-numeric, e.g., (251)
#tables$A <- round(tables$mass, 0)

tables <- tables[-which(tables$mass == ''),] # these lines include mass numbers in Symbol column

tables[which(tables$abundance == '*'), 5] <- NA
tables$mass <- as.numeric(tables$mass)
tables$A <- as.numeric(tables$A)
tables$abundance <- as.numeric(tables$abundance)
tables$Z <- as.numeric(tables$Z)
# The following loop keeps replacing empty Z values with the Z value from the line before
#  The count function generates a table of TRUE and FALSE counts wrt is.na()
#  In the last loop, where there are no empty values, the table only contains FALSE values,
#      so the loop errors out. 
#  That's ok, becuase it's already done its work.
while (count(tables, is.na(Z))[2,2] > 0) {
  tables$Z[which(is.na(tables$Z))] <- tables$Z[which(is.na(tables$Z)) - 1]
}

tables <- tables[,-2:3] # remove Symbol column

# Change certain isotopic abundances to zero.
#   Those for which other isotopes have non-zero abundances
#   First, find rows with abundance = NA
tables[which(is.na(tables$abundance)),]
# only the first two need replacement
tables[3,3] <- 0  # 3H, tritium
tables[13,3] <- 0  # 14C



#########################

write.csv(tables, "./data/abundances.csv", row.names = FALSE)
