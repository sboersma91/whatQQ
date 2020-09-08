library(tidyverse)
library(pdftools)
options(digits = 3)

fn <- system.file("extdata", "RD-Mortality-Report_2015-18-180531.pdf", package="dslabs")

system("cmd.exe", input = paste("start", fn))
txt <- pdf_text(fn)
x <- str_split(txt[9], "\n")
s <- x[[1]]
# class(s)
s <- str_trim(s)
s[1]    # print string, visually inspect last character
header_index <- str_which(s, "2015")[1]
header_index

s[header_index]

tmp <- str_split(s[header_index], "\\s+", simplify = TRUE)
month <- tmp[1]
header <- tmp[-1]
month
header[3]
tmp
tmp[1]
