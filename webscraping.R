library(rvest)
url <- "https://web.archive.org/web/20181024132313/http://www.stevetheump.com/Payrolls.htm"
h <- read_html(url)

nodes <- html_nodes(h, "table")
html_text(nodes[[8]])
html_table(nodes[[8]])
nodes
h
url
nodes[1]      
sapply(nodes[2], html_table)

length(nodes)
sapply(nodes[19:21], html_table)
sapply(nodes[19], html_table)
sapply(nodes[20], html_table)
sapply(nodes[21], html_table)
tab_1z <- html_table(nodes[[10]])
tab_2z <- html_table(nodes[[19]])
tab_1z <- tab_1z[,2:4] #removing the first COLLUMNS
tab_1z <- tab_1z[2:31,]
tab_2z <- tab_2z[2:31,]
colnames(tab_2z) <- c("Team", "Payroll", "Average")
full_join(tab_1z, tab_2z, by = "Team")




tab_1z
tab_2z
head(tab_1z)
head(tab_2z)

library(rvest)
library(tidyverse)
url <- "https://en.wikipedia.org/w/index.php?title=Opinion_polling_for_the_United_Kingdom_European_Union_membership_referendum&oldid=896735054"
h2 <- read_html(url)
nodes2 <- html_nodes(h2, "table")
length(nodes2)
nrow(nodes2)

n <-5
html_table(nodes2[n], fill = TRUE)
















