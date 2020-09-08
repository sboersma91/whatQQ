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

tail_index <- str_which(s, "Total")

n <- str_count(s, "\\d+")
sum( n == 1)

no_header <- s[-1:-header_index]
no_header
no_footer <- no_header[-tail_index:-38]
no_footer 
no_n <- no_footer[-4]
no_n
no_n1 <- no_n[-6]
no_n1
remove_index <- c(header_index, tail_index, n)
zed <- s[-remove_index]
zed

out <- c(1:header_index, which(n==1), tail_index:length(s))
s <- s[-out]
length(s)
s
colnames(s) <- c("day", "2015","2016", "2017", "2018")
y <- s
as.numeric(tab[1:30,1:4])

mean(as.numeric(tab[1:30,2]))

s <- str_split_fixed(s, "\\s+", n = 6)[,1:5]


colnames(s) <- c(day, header)

