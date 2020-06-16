library(stringr)
library(glue)
library(dplyr)
library(readr)
library(magrittr)

# CaiCurve
pix_3 <- c(1230, 1615, 2090, 2601)
I_3 <- c(367.06164, 370.78204, 375.37107, 380.37199)
mod3 <- lm(I_3 ~ poly(pix_3, 3, raw = T))
coef3 <- mod3$coefficients

# File setup
fldr <- str_extract(string = list.files("in/"), pattern = "..._.._....") 
files <- list.files(glue("in/", fldr), full.names = T)
files <- files[(str_which(files, ".csv"))]
names <- list.files(glue("in/", fldr))
names <- names[(str_which(names, ".csv"))]
names <- str_replace(names, pattern = ".csv", replacement = ".txt")

# Processing 
for (i in seq_along(files)) {
  j <- read_csv(files[i], skip = 18, col_names = F) %>% 
    dplyr::select(c(1, 3)) %>% 
    .[1:3648, ] %>% 
    as.data.frame()
  j$X1 %<>% add(1)
  j$X1 <- coef3[1] + coef3[2]*j$X1 + coef3[3]*(j$X1^2) + coef3[4]*(j$X1^3)
  mw <- which.min(j$X1 <= 361)
  j <- j[mw:3500, ]
  j$X3 <- (j$X3 - min(j$X3)) / (max(j$X3) - min(j$X3))
  write_tsv(j, path = file.path("out/", names[i]), col_names = F)
}