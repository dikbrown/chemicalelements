library(rvest)
library(tidyverse)
webpage <- read_html("https://en.wikipedia.org/wiki/Chemical_element")
elements_html <- html_nodes(webpage, css = "table")
elements_html
elements_tbl <- elements_html %>%
                  html_nodes(css = "table") %>%
                  nth(7) %>%
                  html_table(fill = TRUE)
    





url = "https://en.wikipedia.org/wiki/Houston"
houston_html = read_html(url)
houston_html %>% 
  html_nodes(css = "table")
pop_table = 
  houston_html %>% 
  html_nodes(css = "table") %>% 
  nth(4) %>% 
  html_table(fill = TRUE)



url2 = "https://en.wikipedia.org/wiki/Chemical_element"
elements_html = read_html(url2)
houston_html %>% 
  html_nodes(css = "table")
element_table = 
  elements_html %>% 
  html_nodes(css = "table") %>% 
  nth(7) %>% 
  html_table(fill = TRUE)

colnames(element_table) <- c("at_num", "symbol", "name", "name_origin", 
                             "group", "period", "block", "at_mass", "density", "mp",
                             "bp", "sp_ht", "Electronegativity", "abundance_earth_crust", "origin", "phase_at_rt")
element_table <- element_table[-c(1:3),]

##############################################################################
#get numbers expressed in scientific notation - they use a multiplication symbol (not 'x')
#   and convert abundance to numeric
###################################################################################
mult_symbol <- element_table$abundance_earth_crust[36] %>%
  substr(2,2)
mult_symbol
mult_lines <- grep(mult_symbol, element_table$abundance_earth_crust)

#create list of elements with low concentration in the earth's crust
low_conc <- element_table[mult_lines, c(1, 14)]
low_conc

# Remove non-numeric characters from beginning of abundance values
### remove "less than or equal to" symbol from abundances
low_conc$abundance_earth_crust <- element_table$abundance_earth_crust[94] %>%
  substr(1,1) %>%
  gsub('',low_conc$abundance_earth_crust)
### remove "~" symbol from abundances
low_conc$abundance_earth_crust <- element_table$abundance_earth_crust[43] %>%
  substr(1,1) %>%
  gsub('',low_conc$abundance_earth_crust)
### remove "non-space space" symbol from abundances
low_conc$abundance_earth_crust <- low_conc$abundance_earth_crust[9] %>%
  substr(1,1) %>%
  gsub('',low_conc$abundance_earth_crust)

### Alternate method to split low abundance numbers into coeff + power
# element_table$abundance_earth_crust[94] %>%
#   substr(2,2) %>%
#   gsub('', low_conc$abundance_earth_crust)

#### split low abundances into two parts: coefficient and power of 10
low_conc[c('coeff', 'base_exp')] <- str_split_fixed(low_conc$abundance_earth_crust, mult_symbol, 2)
low_conc
low_conc['exp'] <- substring(low_conc$base_exp, 4) %>%
  as.numeric() %>% 
  (function(x) {x * -1})

#### Recalculate abundance
low_conc$abundance_earth_crust <- as.numeric(low_conc$coeff) * 10^low_conc$exp
low_conc
element_table$abundance_earth_crust <- as.numeric(element_table$abundance_earth_crust)
element_table[low_conc$at_num,]$abundance_earth_crust <- low_conc$abundance_earth_crust

##############################
# Fix Electronegativity column
##############################
## Just two weird values and some '-'. Fix them individually
## The hyphens get converted to NA during casting

element_table$Electronegativity[87] <- "0.79"
element_table$Electronegativity[82] <- "2.33"
element_table$Electronegativity <- as.numeric(element_table$Electronegativity)

###################################
# Fix other numeric columns
###################################

element_table$at_mass <- as.numeric(element_table$at_mass)
element_table$density <- as.numeric(element_table$density)
element_table$mp <- as.numeric(element_table$mp)
element_table$bp <- as.numeric(element_table$bp)
element_table$sp_ht <- as.numeric(element_table$sp_ht)

#####################################
# Change group = "n/a" to group = NA (i.e., lanthanides and actinides)
#####################################
element_table$group[element_table$group == "n/a"] <- NA

##############
# Save table as csv
###############

write.csv(element_table, "./data/elements.csv", row.names = FALSE)

