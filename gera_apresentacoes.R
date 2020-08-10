
library(tidyverse)

load("Completo14042020.RData")
Completo <- Completo %>% filter(Modalidade != "Educação a Distância" | is.na(Modalidade))

rais2018_g <- readRDS("rais2018_grad.RDS")
rais2018_p <- readRDS("rais2018_pos.RDS")

eg_g <- readRDS("representatividade_eg_grad.RDS")
eg_p <- readRDS("representatividade_eg_pos.RDS")
evasao_geral <- rio::import("base_evasao_curso_2011_2018.xlsx")

# gera os relatórios das unidades desse ciclo
# IP IREL CET FCI FACE IPOL FD FAC
unidades <- c("IP", "IREL", "CET", "FCI", "FACE", "IPOL", "FD", "FAC")
for (unid in unidades){
  
  grad <- Completo %>% filter(Nivel == "Graduacao", Unidade == unid)
  pos <- Completo %>% filter(Nivel != "Graduacao", Unidade == unid)
  
  rais2018_grad <- rais2018_g %>% filter(unidade == unid)
  rais2018_pos <- rais2018_p %>% filter(unidade == unid)
  
  eg_grad <- eg_g %>% filter(unidade == unid)
  eg_pos <- eg_p %>% filter(unidade == unid)
  
  evasao <- evasao_geral %>% filter(Unidade == unid)
  
  rmarkdown::render(input = "apresentacao_avalia.Rmd",
                    output_file = paste0("apresentacao_avalia_unb_",
                                         unid,
                                         ".html"),
                    # output_dir = "./apresentacoes_unidade/",
                    encoding = "UTF-8",
                    quiet = TRUE)
}