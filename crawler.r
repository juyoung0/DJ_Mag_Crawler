# dj mag top 100 순위 정보 크롤링

library(tidyverse)
library(rvest)

url.list <- paste0("https://djmag.com/top100dj?year=", 2004:2021)

DJmag_Scraping <- function (url) {
  url <- as.character(url)

  page <-
  read_html(url)

container <- page %>%
  html_nodes("section") %>%
  html_nodes("div.panel-pane") %>%
  html_text()

king <- container[1] %>%
  str_remove("\n\n \n \n \n ") %>%
  str_remove("\n\n\n   ") %>%
  str_replace_all(pattern = "\n\n\n", replacement = "\\/") %>%
  str_replace_all(pattern = " \n \n\n", replacement = "\\/")

Second.Group <- container[2] %>%
  str_remove_all("\n\n \n \n \n ") %>%
  str_replace_all(pattern = "\n\n\n", replacement = "\\/") %>%
  str_replace_all(pattern = " \n \n\n", replacement = "\\/") %>%
  str_replace_all(pattern = "/  ", replacement = "$NextRanking")

Third.Group <- container[4] %>%
  str_remove_all("\n\n \n \n \n ") %>%
  str_replace_all(pattern = "\n\n\n", replacement = "\\/") %>%
  str_replace_all(pattern = " \n \n\n", replacement = "\\/") %>%
  str_replace_all(pattern = "/  ", replacement = "$NextRanking")


Forth.Group <- container[6] %>%
  str_remove_all("\n\n \n \n \n ") %>%
  str_replace_all(pattern = "\n\n\n", replacement = "\\/") %>%
  str_replace_all(pattern = " \n \n\n", replacement = "\\/") %>%
  str_replace_all(pattern = "/  ", replacement = "$NextRanking")

Fifth.Group <-
  container[8] %>%
  str_remove_all("\n\n \n \n \n ") %>%
  str_replace_all(pattern = "\n\n\n", replacement = "\\/") %>%
  str_replace_all(pattern = " \n \n\n", replacement = "\\/") %>%
  str_replace_all(pattern = "/  ", replacement = "$NextRanking")

data.frame(king, Second.Group, Third.Group, Forth.Group, Fifth.Group)
}

dj.mag <- apply(data.frame(url.list), 1, DJmag_Scraping)
dj.mag.df <- do.call(rbind, dj.mag)
dj.mag.df <- dj.mag.df %>%
  mutate(Year = 2004:2021)
dj.mag.df <- dj.mag.df %>%
  select(Year, king:Fifth.Group)


dj.mag.df$king <- dj.mag.df$king %>%
  str_remove("1/1/") %>%
  str_remove_all("\n\n \n \n") %>%
  str_remove("\n\n \n\n \n ") %>%
  str_remove("Dimitri Vegas & Like Mike \n \n ")
Second.Group.Matrix<- dj.mag.df$Second.Group %>%
  str_split(pattern = "NextRanking", n = 8, simplify = TRUE)

Third.Group.Matrix <- dj.mag.df$Third.Group %>%
  str_split(pattern = "NextRanking", n = 21, simplify = TRUE)

Forth.Group.Matrix <- dj.mag.df$Forth.Group %>%
  str_split(pattern = "NextRanking", n = 32, simplify = TRUE)

dj.mag.df$Fifth.Group <- dj.mag.df$Fifth.Group %>%
  str_replace(pattern = " \n\n  ", replacement = "$NextRanking")
Fifth.Group.Matrix <- dj.mag.df$Fifth.Group %>%
  str_split(pattern = "NextRanking", n = 38, simplify = TRUE)

###### Data Cleaning

Second.Group.df <- data.frame(Second.Group.Matrix)
names(Second.Group.df) <- 2:9
Second.Group.df$Year <- 2004:2021
Second.Group.df <- Second.Group.df %>%
  gather(key = "Rank", value = "DJ", -Year)

Second.Group.df$DJ <- Second.Group.df$DJ %>%
  str_remove_all("/\n\n \n \n") %>%
  str_remove_all("\n\n \n\n \n ") %>%
  str_remove("Above & Beyond reveals new all-inclusive trance festival is in the works \n \n ") %>%
  str_remove("David Guetta Top 100 DJs \n \n ") %>%
  str_remove("\n\n \n \nHardwell \n \n ") %>%
  str_remove("NextRanking ")

Second.Group.df$DJ <- Second.Group.df$DJ %>%
  str_remove("\n\n \n \n") %>%
  str_remove("tiesto \n \n ") %>%
  str_replace(pattern = "7 ", "7") %>%
  str_remove("[:digit:]/") %>%
  str_remove("\\$")


Third.Group.Matrix <- data.frame(Third.Group.Matrix)
Third.Group.Matrix$Year <- 2004:2021

Third.Group.Matrix <- Third.Group.Matrix %>%
  gather(key = "Rank", value = "DJ", -Year)
Third.Group.Matrix$DJ <- Third.Group.Matrix$DJ %>%
  str_remove("\n\n \n \nAbove & Beyond reveals new all-inclusive trance festival is in the works \n \n ") %>%
  str_remove("\n\n \n \nblasterjaxx \n \n ") %>%
  str_remove("\n\n \n \n26 - LOST FREQUENCIES.jpg \n \n ") %>%
  str_remove("\n\n \n \n27 - ABOVE & BEYOND.jpg \n \n ") %>%
  str_remove("\n\n \n \ndyro \n \n ") %>%
  str_remove("\n\n \n \n") %>%
  str_remove("don diablo \n \n ") %>%
  str_remove("NextRanking ") %>%
  str_remove("30 - QUINTINO.jpg \n \n ") %>%
  str_remove("above and beyond \n \n ") %>%
  str_remove("  \n ") %>%
  str_remove("29 - ZEDD 2.jpg \n \n ") %>%
  str_remove("KSHMR \n \n ") %>%
  str_remove("28 - AVICII.jpg \n \n ")

Third.Group.Matrix$DJ <- Third.Group.Matrix$DJ %>%
  str_replace(pattern = " 27/", replacement = "27/") %>%
  str_replace(pattern = " 19/", replacement = "19/") %>%
  str_replace(pattern = " 11/", replacement = "11/") %>%
  str_replace(pattern = " 12/", replacement = "12/") %>%
  str_remove("nervo \n \n ") %>%
  str_remove("\\$")

Third.Group.Matrix$DJ <- Third.Group.Matrix$DJ %>%
  str_remove_all("\n") %>%
  str_replace(pattern = "24  /New Entry", replacement = "24/NA/New Entry")
Third.Group.Matrix <- Third.Group.Matrix %>%
  separate(DJ, sep = "\\/",
           into = c("Rank", "DJ"), extra = "merge")

### Forth Group

Forth.Group.df <- data.frame(Forth.Group.Matrix)
Forth.Group.df$Year <- 2004:2021
Forth.Group.df <- Forth.Group.df %>%
  gather(key = "Rank", value = "DJ", -Year)

Forth.Group.df$DJ <- Forth.Group.df$DJ %>%
  str_remove_all("\n\n \n \n") %>%
  str_remove("\n\n  \n \n \n ") %>%
  str_remove("kura \n \n ") %>%
  str_remove("steve angello \n \n ") %>%
  str_remove("31 - VINTAGE CULTURE.jpg \n \n ") %>%
  str_remove("32 - VINAI.jpg \n \n ") %>%
  str_remove("33 - HEADHUNTERZ.jpg \n \n ") %>%
  str_remove("Above & Beyond reveals new all-inclusive trance festival is in the works \n \n ") %>%
  str_remove("kygo \n \n ") %>%
  str_remove("36 - BLASTERJAXX.jpg \n \n ") %>%
  str_remove("39 - FEDDE LE GRAND.jpg \n \n ") %>%
  str_remove("  \n ") %>%
  str_remove("34 - ERIC PRYDZ.jpg \n \n ") %>%
  str_replace(pattern = " \n \n \n", replacement = "/") %>%
  str_remove("vinai \n \n ") %>%
  str_remove("fedde le grand \n \n ") %>%
  str_remove("50 - NICKY ROMERO.jpg \n \n ") %>%
  str_remove("35 - BASSJACKERS.jpg \n \n ") %>%
  str_remove("Showtek \n \n ") %>%
  str_remove("37 - ALESSO.jpg \n \n ") %>%
  str_remove("Major Lazer \n \n ") %>%
  str_remove("brennan heart \n \n ") %>%
  str_remove("38 - UMMET OZCAN.jpg \n \n ")

Forth.Group.df$DJ <- Forth.Group.df$DJ %>%
  str_remove("Angerfist \n \n ") %>%
  str_remove("49 - DEADMAU5.jpg \n \n ") %>%
  str_remove("48 - KURA.jpg \n \n ") %>%
  str_remove("40 - ANGERFIST.jpg \n \n ") %>%
  str_remove("bassjackers \n \n ") %>%
  str_remove("41 - WOLFPACK 2.jpg \n \n ") %>%
  str_remove("44 - RADICAL REDEMPTION 2.jpg \n \n ") %>%
  str_remove("Tom Swoon \n \n ") %>%
  str_remove("alok \n \n ") %>%
  str_remove("jack u \n \n ") %>%
  str_remove("TOM SWOON PIC ONLINE.jpg \n \n ") %>%
  str_remove("shogun \n \n ") %>%
  str_replace(pattern = " 42", "42") %>%
  str_replace(pattern = " 51/", "51/")

Forth.Group.df$DJ <- Forth.Group.df$DJ %>%
  str_remove("42 - NERVO.jpg \n \n") %>%
  str_remove("46 - TUJAMO.jpg \n \n ") %>%
  str_remove("45 - MAJOR LAZER.jpg \n \n ") %>%
  str_remove("Timmy Trumpet top 100 DJs \n \n ") %>%
  str_remove("\\$")
Forth.Group.df <- Forth.Group.df %>%
  separate(DJ, sep = "\\/",
           into = c("Rank", "DJ"), extra = "merge")

### Fifth Group

Fifth.Group.df <- data.frame(Fifth.Group.Matrix)
Fifth.Group.df$Year <- 2004:2021
Fifth.Group.df <- Fifth.Group.df %>%
  gather(key = "Rank", value = "DJ", -Year)

Fifth.Group.df$DJ <- Fifth.Group.df$DJ %>%
  str_remove("\n\n \n \n") %>%
  str_remove("NextRanking ") %>%
  str_remove("\n\n  \n \n \n ") %>%
  str_remove("mike candys \n \n ") %>%
  str_remove("firebeatz \n \n ") %>%
  str_remove("\n") %>%
  str_remove("Andrew Rayel Top 100 DJs \n \n ") %>%
  str_remove("Carnage Top 100 DJs \n \n ") %>%
  str_remove(" Top 100 DJs ") %>%
  str_remove("umek  \n ") %>%
  str_remove("\n \n  \n \n ")

Fifth.Group.df$DJ <- Fifth.Group.df$DJ %>%
  str_remove("borgore  \n ") %>%
  str_remove("radical redemption  \n ") %>%
  str_remove("Frontliner  \n ") %>%
  str_remove("yellow claw  \n ") %>%
  str_remove("will sparks  \n ") %>%
  str_remove("makj  \n ") %>%
  str_remove("knife party  \n ") %>%
  str_remove("flux pavillion  \n ") %>%
  str_remove("markus schulz  \n ") %>%
  str_remove("galantis  \n ") %>%
  str_remove("tujamo  \n ") %>%
  str_remove("Andrew Rayel \n ") %>%
  str_remove("carnage  \n ") %>%
  str_remove("MaRlo  \n ") %>%
  str_remove("miss k8  \n ") %>%
  str_remove("Andy C \n ") %>%
  str_remove("Krewella  \n ")

Fifth.Group.df$DJ <- Fifth.Group.df$DJ %>%
  str_replace(pattern = "81 /", replacement = "81/") %>%
  str_replace(pattern = "86   \n\n", replacement = "86/") %>%
  str_remove("sander van doorn  \n ") %>%
  str_remove("cosmic gate  \n ") %>%
  str_remove("zatox  \n ") %>%
  str_remove("swanky tunes  \n ") %>%
  str_remove("Kaskade  \n ")

Fifth.Group.df$DJ <- Fifth.Group.df$DJ %>%
  str_remove("andy c  \n ") %>%
  str_remove("\\$")

Fifth.Group.df <- Fifth.Group.df %>%
  separate(DJ, sep = "\\/",
           into = c("Rank", "DJ"), extra = "merge")

# King
King <- dj.mag.df %>%
  select(Year:king)
King$Rank <- 1
King <- King %>%
  select(Year, Rank, king)

# Merge Dataframes
Second.Group.df$Rank <- as.numeric(Second.Group.df$Rank)
Third.Group.Matrix$Rank <- as.numeric(Third.Group.Matrix$Rank)
Forth.Group.df$Rank <- as.numeric(Forth.Group.df$Rank)
Fifth.Group.df$Rank <- as.numeric(Fifth.Group.df$Rank)
names(King)[3] <- "DJ"
DJ.Mag <- bind_rows(King, Second.Group.df, Third.Group.Matrix, Forth.Group.df, Fifth.Group.df)

DJ.Mag <- DJ.Mag %>%
  separate(DJ, sep = "\\/",
           into = c("DJ", "Change"))
DJ.Mag$Rank <- as.numeric(DJ.Mag$Rank)

write.csv(DJ.Mag, file = "DJ_Mag.csv")
