library(rvest)
library(tidyverse)
url <- "https://web.archive.org/web/20181024132313/http://www.stevetheump.com/Payrolls.htm"
h <- read_html(url)
nodes <- html_nodes(h, "table")

sapply(nodes[1:4], html_table)    # 2, 3, 4 give tables with payroll info

sapply(tail(nodes, n=3), html_table)

url2 <- "https://en.wikipedia.org/w/index.php?title=Opinion_polling_for_the_United_Kingdom_European_Union_membership_referendum&oldid=896735054"
x <- read_html(url2)
tab <- html_nodes(x, "table")
y <- length(tab)
tab[[5]] %>% html_table(fill = TRUE) %>% names()    # inspect column names
