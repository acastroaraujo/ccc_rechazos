
replace_empty_with_na <- function(x) {
  if (purrr::is_empty(x)) return(NA_character_) 
  x
}

scraper <- function(radicado) {
  
  stopifnot(length(radicado) == 1)
  url <- str_glue("https://www.corteconstitucional.gov.co/secretaria/actuacion.php?proceso=1&palabra={radicado}")
  
  obj <- httr::RETRY("GET", url)
  stopifnot(httr::status_code(obj) == 200)
  website <- httr::content(obj)
  
  Radicado <- website %>%  
    html_nodes("h1") %>% 
    html_text() %>% 
    str_squish()
  
  info <- website %>% 
    html_table()
  
  k <- 1
  
  if (length(info) == 2) {
    
    Magistrado <- info[[1]] %>% 
      str_replace("(.+) -->  (.+)", "\\2")
    
    if (str_detect(Magistrado, "-->")) {
      Magistrado <- Magistrado %>% 
        str_remove("  .+") %>% 
        str_squish()
    }
    
    k <- 2
  }
  
  data <- info[[k]][-1, ]
  names(data) <- c("Etapa", "Fecha")
  
  Fecha <- lubridate::mdy(data$Fecha, locale = "es_ES.UTF-8")
  
  if (any(is.na(Fecha))) {
    Fecha <- lubridate::mdy(data$Fecha, locale = "en_US.UTF-8")
  }
  
  Etapas <- tibble(Radicado, data) 
  Etapas$Fecha <- Fecha
  
  if (length(info) == 2) Etapas$Magistrado <- Magistrado
  
  test <- website %>% 
    html_nodes("h1 a") %>% 
    html_attr("href") %>% 
    length()
  
  if (test == 3) {
    
    Archivos <- page_two(radicado)
    if (length(info) == 2 & !is.null(Archivos)) Archivos$Magistrado <- Magistrado
    
  } else {
    Archivos <- NULL
  }
  
  list(Etapas = Etapas, Archivos = Archivos)
  
}


page_two <- function(Radicado) {
  stopifnot(length(Radicado) == 1)
  
  url <- str_glue("https://www.corteconstitucional.gov.co/secretaria/actuacion.php?proceso=1&palabra={Radicado}&mostrar=ver")
  obj <- httr::RETRY("GET", url)
  
  stopifnot(httr::status_code(obj) == 200)
  website <- httr::content(obj)
  
  info <- website %>% html_table() 
  
  if (is_empty(info)) {
    
    Archivos <- NULL
    
  } else {
    
    info <- info %>% .[[1]] 
    
    data <- info[-1, ]  
    names(data) <- info[1, ]
    
    href <- website %>% 
      html_nodes(".warning td~ td+ td") %>% 
      map_chr(
        ~ .x %>% 
          html_nodes("a") %>% 
          html_attr("href") %>% 
          replace_empty_with_na()
      )
    
    Archivos <- tibble(Radicado, data) %>% 
      mutate(Fecha = ifelse(Fecha == "", NA_character_, Fecha)) %>% 
      mutate(Fecha = as.Date(Fecha)) %>% 
      mutate(url = str_glue("https://www.corteconstitucional.gov.co/secretaria/{href}")) %>% 
      mutate(url = ifelse(Archivo == "", NA_character_, url))
    
  }
  
  return(Archivos)

}
