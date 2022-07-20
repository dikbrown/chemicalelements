library(rvest)
library(tidyverse)

isotope_url = "https://en.wikipedia.org/wiki/List_of_nuclides"
isotope_tables = read_html(isotope_url) %>%
          html_nodes(css = "table")

gettable <- function(tbl_list, n) {
    tbl_list %>% 
    nth(n) %>% 
    html_table(fill = TRUE)
}

gettable(isotope_tables, 1) # table of eight decay mechanisms
T1 <- gettable(isotope_tables, 2) # table of theoretically stable nuclides (6 var)
T2 <- gettable(isotope_tables, 3) # table that can theoretically decay, but are observationally stable (8 var)
T3 <- gettable(isotope_tables, 4) # table of observationally stable nuclides for which decay has been searched for but not found (9 var)
T4 <- gettable(isotope_tables, 5) # radioactive nuclides with t-1/2 > 100 My (10 var)
T5 <- gettable(isotope_tables, 6) # radioactive nuclides with t-1/2 of 10ky to 100 My (9 var)
T6 <- gettable(isotope_tables, 7) # radioactive nuclides with t-1/2 of 10 y to 10 ky
T7 <- gettable(isotope_tables, 8) # radioactive nuclides with t-1/2 of 1 d to 10 y
T8 <- gettable(isotope_tables, 9) # radioactive nuclides with t-1/2 of 1 h to 1 d
gettable(isotope_tables, 10) # wikipedia footer table

# add half-life column to T1 & T2
T1$"half-life (s)" <- NA
T2$"half-life (s)" <- NA
# change half-lives in T3 to NA, as they're not real numbers, just estimated lower limits for decay that has never been observed
T3$"half-life (s)" <- NA

# add mass # column to radioactive nuclide tables
T5$A <- T5$Z + T5$N
T6$A <- T6$Z + T6$N
T7$A <- T7$Z + T7$N
T8$A <- T8$Z + T8$N

#combine into a single table, dropping decay mode & decay energy columns for now, because there can be multiples - also, drop alternate t-1/2 columns
isotopes <- rbind(T1[,1:7],
                  T2[,c(1:6,9)],
                  T3[,1:7],
                  T4[,1:7],
                  T5[,c(1:6, 10)],
                  T6[,c(1:6, 10)],
                  T7[,c(1:6, 10)],
                  T8[,c(1:6, 10)])

isotopes <- isotopes[,-c(1:2)] # drop "no" column, which is simply an index number on the wikipedia page, and "nuclide" column


##############################################################################
#get numbers expressed in scientific notation - they use a multiplication symbol (not 'x')
#   and convert half-lives to numeric
###################################################################################
mult_symbol <-T7[1,6] %>%
  substr(6,6)
mult_symbol

#### split scientific notation into two parts: coefficient and power of 10
isotopes[c('coeff', 'base_exp')] <- str_split_fixed(isotopes$'half-life (s)', mult_symbol, 2)

isotopes['exp'] <- substring(isotopes$base_exp, 3) %>%
  as.numeric()

#### Recalculate half-life
isotopes$`half-life (s)` <- as.numeric(isotopes$coeff) * 10^isotopes$exp
### Remove temp columns
isotopes <- isotopes[,-(6:8)]

# Calculate nuclide mass - Defined at https://en.wikipedia.org/wiki/List_of_nuclides
### mass per nuclide = A(m - E/k)
m = 1.008664916 # mass of neutron in Da
k = 931.49410242 # (conversion between MeV and Da)

isotopes$mass <- isotopes$A * (m - isotopes$energy / k)
# round mass to correct number of significant digits
isotopes$mass <- 
  unlist(sapply(isotopes$mass,
       function (x) if (is.na(x)) {x <- NA 
                                } else if (x < 1.1) {   # this catches 1H, in which energy has 6 sig figs
                                  x <- round(x, 5)      # all the rest have 7 sig figs
                                } else if (x < 10) {    
                                  x <- round(x, 6)
                                } else if (x < 100) {
                                  x <- round(x, 5)
                                } else if (x >= 100) {
                                  x <- round(x, 4)
                                }
    )
)

##############
# Save table as csv
###############

write.csv(isotopes, "./data/isotopes.csv", row.names = FALSE)


