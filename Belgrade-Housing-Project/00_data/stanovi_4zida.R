
library(tidyverse) # Main Package - Loads dplyr, purrr
library(rvest)     # HTML Hacking & Web Scraping
library(furrr)     # Parallel Processing using purrr (iteration)
library(fs)        # Working with File System
library(xopen)     # Quickly opening URLs
library(tictoc)

pokupi_stanove <- function(url) {
    read_html(url) %>%
        html_nodes(".z4-classified-title-and-price") %>%
        html_attr("href") %>%
        enframe(name = "position", value = "stan_id") %>%
        mutate(url = str_glue("https://www.4zida.rs{stan_id}"))
    
}


pokupi_stanove(url)


beograd_tbl <- tibble(strana = 1:2) # max strana
beograd_tbl <- 
    beograd_tbl %>% mutate(url = str_glue("https://www.4zida.rs/prodaja-stanova/beograd?strana={strana}"))


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


# 4.2 Prikupljanje info o svakom stanu fun ----

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

#pokupi_info(url)



tic() # oko 26 min
plan("multiprocess")
stanovi_karak_sve <- stanovi_svi %>% 
    mutate(stan_karak = future_map(url, ~possibly(pokupi_info, otherwise = NA_real_)(.x)))%>% 
    unnest() %>% 
    #select(-stan_karak) %>% 
    spread(key = karak, value = vrednosti) 
toc()


beog_cene <- stanovi_karak_sve%>% unnest() #%>% 
    #select(-stan_karak) %>% 
    spread(key = karak, value = vrednosti) 




beog_cene <- 
beog_cene %>% janitor::clean_names() %>% select(url,opstina = ops_lok,mesto,ulica,cena,
                                                povrsina,cena_po_m2,broj_soba,tip_stana,godina_izgradnje,stanje,
                                                parking,garazno_mesto,sprat,grejanje,
                                                infrastruktura,
                                                namesten=opremljenost,
                                                opremljenost=opremljenost_2,
                                                unutrasnje_prostorije,uknjizenost,
                                                opis)
                                                
                                                
                                            
a <- beog_cene %>%
    drop_na(cena)%>%
    mutate(cena = as.numeric(gsub("[.]","",cena)),
                                      id = row_number(),
                                      cena_po_m2 = as.numeric(gsub("[.]","",str_split(cena_po_m2,' ',simplify = T)[,1])),
                                      povrsina = as.numeric(str_split(povrsina,' ',simplify = T)[,1]),
                
                                      opstina = ifelse(str_split(str_sub(opstina,3),', ',simplify = T)[,2]!= 'Beograd',
                                                        str_split(str_sub(opstina,3),', ',simplify = T)[,2],
                                                       str_split(str_sub(opstina,3),', ',simplify = T)[,1]),
           
                                      opstina = ifelse(opstina=="",mesto,opstina),
                                      opstina = ifelse(opstina=="Srbija",mesto,opstina),
                                     opstina = ifelse(opstina=="Beograd",'',opstina),
                                      mesto   =  ifelse(mesto == opstina,'',mesto),
                                      parking_garaza = ifelse(is.na(parking),NA,'ima'),
                                      parking_garaza = ifelse(is.na(garazno_mesto),parking_garaza,'ima'),
                                      sprat = str_split(sprat,' ',simplify = T)[,1]) %>%
    filter(povrsina<2000,cena > 10000) %>%
    mutate_if(is.character, as.factor) %>% 
           
                                        
           
           
    select(id,everything())  
 

glimpse(a)

a %>%  group_by(opstina,mesto) %>% 
    summarise(prosek = median(cena_po_m2)) %>% 
    arrange(desc(prosek)) %>% print(n = 50)




a %>%
    map_df(~ sum(is.na(.))) %>%
    gather() %>%
    arrange(desc(value)) %>% print(n = Inf)



a %>% DataExplorer::plot_missing()


a %>%
    #select(opstina,mesto) %>% 
    map_df(~ unique(.) %>% length()) %>% 
    gather() %>% print(n=Inf)

 



