library(tidyverse)
library(pdftools)
options(digits = 3)

fn <- system.file("extdata", "RD-Mortality-Report_2015-18-180531.pdf", package="dslabs")

system("cmd.exe", input = paste("start", fn))

txt <- pdf_text(fn)
txt2 <- txt[[9]] #page nine extraction [] is anything that matches [[ONLY1]]
 
x <- str_split(txt2, "\r\n")

s <- x[[1]] #question 4 returning 1st entry to x


class(x)
class(s)
length(s)
nrow(s)
t <- str_trim(s)
identical(s, t)

#poppy seeds