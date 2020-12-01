
library(tidyverse)
library(rvest)
library(progress)
source("helper-functions.R")

meta_datos <- read_rds("meta-datos.rds") 

outfolder <- "radicados/"
if (!dir.exists(outfolder)) dir.create(outfolder)

radicados_done <- str_replace(dir(outfolder), ".rds", "")
radicados_left <- setdiff(meta_datos$radicado, radicados_done)

pb <- progress_bar$new(format = "[:bar] :current/:total (:percent)", total = length(radicados_left))

while(length(radicados_left > 0)) {
  
  x <- radicados_left[[1]]
  
  out <- try(scraper(x))
  write_rds(out, str_glue("{outfolder}{x}.rds"))
  radicados_left <- radicados_left[-which(radicados_left == x)] ## int. subset
  
  pb$tick()
  Sys.sleep(runif(1, 1, 3))     ## be kind
}


# Debugger ----------------------------------------------------------------

error_index <- dir(outfolder, full.names = TRUE) %>% 
  map(read_rds) %>% 
  map_lgl(~ any(class(.x) == "try-error")) %>% 
  which()

length(error_index)



# output ------------------------------------------------------------------

output <- dir(outfolder, full.names = TRUE) %>%
  map(read_rds)

etapas <- output %>%
  pluck("Etapas") %>%
  bind_rows()

write_rds(etapas, "etapas.rds", compress = "gz")

archivos <- output %>% 
  pluck("Archivos") %>% 
  bind_rows()

write_rds(archivos, "archivos.rds", compress = "gz")

