

library(tidyverse)
library(rvest)
library(progress)

# cálculo manual
url <- "https://www.corteconstitucional.gov.co/secretaria/consultac/proceso.php?proceso=1&campo=rad_codigo&date3=1992-01-01&date4=2020-25-11&radi=&palabra=D&todos=%25&registros=14036&pg="
p <- ceiling(14036 / 100) - 1

urls <- paste0(url, 0:p)

# scraper -----------------------------------------------------------------

descargar <- function(url) {
  
  obj <- httr::RETRY("GET", url)
  stopifnot(httr::status_code(obj) == 200)
  website <- httr::content(obj)
  
  info <- website %>%         ## Esta página es muy rara, esto
    html_nodes("tbody tr")    ## que sigue es muy x, parce...
  
  output <- vector("list", length(info))
  
  for (i in seq_along(info)) {
    a <- info[[i]] %>% html_nodes("td") %>% html_text()
    output[[i]] <- c(a[[1]] %>% str_extract("D.+") , a[-1])
    names(output[[i]]) <- c("radicado", "asunto", "actor", "remitido", "fecha", "sentencia")
  }
  
  bind_rows(output)
  
}

# descarga ----------------------------------------------------------------

output <- vector("list", length(urls))
pb <- progress_bar$new(format = "[:bar] :current/:total (:percent)", total = length(output))

for (i in seq_along(output)) {
  output[[i]] <- descargar(urls[[i]])
  
  pb$tick()
  Sys.sleep(rpois(1, 5))     ## be kind
}

bind_rows(output) %>% 
  mutate(fecha = lubridate::mdy(fecha, locale = "es_ES.UTF-8")) %>% 
  write_rds("meta-datos.rds", compress = "gz")



