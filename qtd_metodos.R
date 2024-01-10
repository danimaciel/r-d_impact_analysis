pacotes <- c("ggplot2", "tidyverse", "dplyr", "stringi", "readr","writexl", "readxl")
lapply(pacotes, require, character.only = TRUE)


base_metodos <- read_excel("r&d/nova_analise_metodos/base_pesq_juncao_ml_manual.xlsx")


base_metodos$count_metodologias <- sapply(strsplit(base_metodos$metodologia_ml, ","), length)

mean(base_metodos$count_metodologias)
qtd_metodos_decada <- as.data.frame(table(base_metodos$count_metodologias, base_metodos$Decada.x))


base_metodos_separada <- base_metodos %>%
  separate_rows(metodologia_ml, sep = ",")

base_metodos_separada <- base_metodos_separada %>% 
  rename(metodologia = metodologia_ml)

base_metodos_separada$metodologia <- tolower(base_metodos_separada$metodologia)
base_metodos_separada$metodologia <- trimws(base_metodos_separada$metodologia)

# Juntando dicionario com a base

dicionario_metodologias <- read_excel("r&d/nova_analise_metodos/dicionario_metodologias.xlsx", sheet = 2)
dicionario_metodologias$metodologia <- tolower(dicionario_metodologias$metodologia)

base_cruzada <- merge(base_metodos_separada, dicionario_metodologias, by = "metodologia", all.x = TRUE)

categorias <- read_excel("r&d/nova_analise_metodos/categorias.xlsx")
categorias$metodologia <- tolower(categorias$metodologia)

base_cruzada2 <- read_excel("r&d/nova_analise_metodos/base_metodos.xlsx")

base_cruzada2 <- merge(base_cruzada2, categorias, by = "metodologia", all.x = TRUE)

writexl::write_xlsx(base_cruzada2, "r&d/nova_analise_metodos/base_metodos.xlsx")

base_cruzada2$count_autor <- sapply(base_cruzada2$Autor.x, function(x) {
  autores <- unlist(strsplit(x, "[,;]"))
  return(length(autores))
})


base_cruzada2$citacao <- sapply(1:nrow(base_cruzada2), function(i) {
  autores <- unlist(strsplit(as.character(base_cruzada2$Autor.x[i]), ", "))
  autores_limpos <- gsub(" [A-Z]+\\.?", "", autores) # Remove iniciais com ou sem ponto
  ano <- base_cruzada2$Ano.x[i]
  
  if (length(autores_limpos) == 1) {
    return(paste(autores_limpos, "(", ano, ")", sep=""))
  } else if (length(autores_limpos) == 2) {
    return(paste(autores_limpos[1], "; ", autores_limpos[2], "(", ano, ")", sep=""))
  } else {
    return(paste(autores_limpos[1], " et al.", "(", ano, ")", sep=""))
  }
})


head(base_cruzada2)

writexl::write_xlsx(base_cruzada2, "r&d/nova_analise_metodos/base_citacao.xlsx")
