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

tail_index <- str_which(s, "Total")

n <- str_count(s, "\\d+")
sum( n == 1)


no_header <- gsub( '.*', '',s)
no_header
s
s[2]
tail_index
header_index

out <- c(1:header_index, which(n==1), tail_index:length(s))
s <- s[-out]
length(s)
s


s <- str_split_fixed(s, "\\s+", n = 6)[,1:5]


tab <- s %>% 
  as_data_frame() %>% 
  setNames(c("day", header)) %>%
  mutate_all(as.numeric)
mean(tab$"2015")


