getwd()
setwd("C:/Users/dejan.bulaja/Desktop/R Programming/Belgrade Housing Project/")

library(tidyverse) # Main Package - Loads dplyr, purrr
library(rvest)     # HTML Hacking & Web Scraping
library(furrr)     # Parallel Processing using purrr (iteration)
library(fs)        # Working with File System
library(xopen)     # Quickly opening URLs
library(tictoc)
library(stringr)

url <- "https://www.nekretnine.rs/stambeni-objekti/stanovi/izdavanje-prodaja/prodaja/grad/beograd/lista/po-stranici/10/"

pokupi_stanove <- function(url) {
  read_html(url) %>%
    html_nodes(".offer-title a") %>%
    html_attr("href") %>%
    enframe(name = "position", value = "stan_id") %>%
    mutate(url = str_glue("https://www.nekretnine.rs/stambeni-objekti/stanovi/{stan_id}"))
  
}

read_html(url) %>%
  html_nodes(".offer-title a") %>%
  html_attr("href") %>%
  enframe(name = "position", value = "stan_id") %>%
  mutate(url = str_glue("https://www.nekretnine.rs/stambeni-objekti/stanovi/{stan_id}"))

beograd_tbl <- tibble(strana = 2:10) # max strana
beograd_tbl <- 
  beograd_tbl %>% mutate(url = str_glue("https://www.nekretnine.rs/stambeni-objekti/stanovi/izdavanje-prodaja/prodaja/grad/beograd/lista/po-stranici/10/stranica/{strana}"))

#Pokupi stanove na svim stranama---
tic()  # oko 3 minuta
plan("multiprocess")
stanovi_svi <- beograd_tbl %>%
  mutate(stan_id = future_map(url, ~possibly(pokupi_stanove, otherwise = NA_real_)(.x)))%>%
  select(-url) %>%
  unnest(stan_id) %>% 
  select(url) %>% 
  distinct(url)
toc()

stanovi_svi




# 4.2 Prikupljanje info o svakom stanu fun ----


url <- "https://www.nekretnine.rs/stambeni-objekti/stanovi/srce-panceva-jna-ulica/NkM6UlC092R/"


pokupi_info <- function(url){
  
read_html(url) %>%
  html_nodes(".property__main-details") %>%
  html_text() %>%
  str_replace_all("[\n]","") %>% 
  str_replace_all(" ","") %>% 
  str_replace_all('(?!^)(?=[A-Z])', " ")


read_html(url) %>%
  html_nodes(".property__location") %>% 
  html_text() %>%
  str_replace_all("\n","") %>% 
  str_replace_all(" ", "") %>% 
  str_replace_all("Srbija","") %>% 
  str_replace_all("(?<=[a-z])(?=[A-Z])", " ")

read_html(url) %>%
  html_nodes(".property__amenities") %>%
  html_text() %>% str_replace_all("\n","") %>% 
  str_replace_all(" ", "") %>% 
  str_replace_all('(?!^)(?=[A-Z])', " ") %>%
  str_replace_all("Podacionekretnini","") %>% 
 # str_replace_all("²", "") %>% 
  str_replace_all("Ostalo","")
}


pokupi_info <- function(url) {
  
  html <- read_html(url)
  
  
  karak <- html %>%
    html_nodes(".item-left") %>%
    html_text()
  
  vrednosti <- html %>%
    html_nodes(".item-right") %>%
    html_text()
  
  karak_dodatno <- html %>%
    html_nodes("[class='font-weight-bold p-0 col-lg-3']") %>%
    html_text()
  
  vrednosti_dodatno <- html %>%
    html_nodes("[class='col-lg-9 additional-value']") %>%
    html_text()
  
  
  
  ulica_i_mesto <- html %>%
    html_nodes("[itemprop='streetAddress']") %>%# html_attr("content") 
    html_text()
  
  
  ulica <- ulica_i_mesto %>% .[[1]]
  mesto <- ulica_i_mesto %>% .[[2]]
  
  
  cena <- html %>% # cena
    html_nodes(".pr-2") %>%
    html_text()
  
  opstina_i_lok <- html %>%
    html_nodes("[itemprop='addressLocality']") %>%# html_attr("content") 
    html_text()%>% 
    stringi::stri_paste(collapse='')
  
  opis <- html %>%
    html_nodes("[itemprop='description']") %>%# html_attr("content") 
    html_text()
  
  
  tibble(
    karak = karak,
    vrednosti = vrednosti
  ) %>% rbind(
    tibble(
      karak = karak_dodatno,
      vrednosti = vrednosti_dodatno
    )) %>% 
    
    add_row(karak = "Ops_lok", vrednosti = opstina_i_lok, .before = 1) %>% 
    add_row(karak = "Opis", vrednosti = opis) %>%
    add_row(karak = "Mesto", vrednosti = mesto, .before = 1)%>% 
    add_row(karak = "Ulica", vrednosti = ulica, .before = 1) %>% 
    add_row(karak = "Cena", vrednosti = cena, .before = 1)
  
  
  
}

