library(tidyverse)
library(pdftools)
options(digits = 3)

fn <- system.file("extdata", "RD-Mortality-Report_2015-18-180531.pdf", package="dslabs")

system("cmd.exe", input = paste("start", fn))

txt <- pdf_text(fn)
txt2 <- txt[12] #page nine extraction [] is anything that matches [[ONLY1]]
 
x <- str_split(txt2, "\n")

s <- x[[1]] #question 4 returning 1st entry to x
t <- str_trim(s)
length(t) #40
# t[1]
header_index  <- str_which(t, "2015")
month <- str_split(header_index, "month",simplify = TRUE)
month
