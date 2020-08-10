
library(tidyverse)
library(waffle)
library(ggtext)

evasao_ifes <- rio::import("INDICADORES_TRAJETORIA_EDUCACAO_SUPERIOR_2010_2016.xlsx",
                           skip = 8) %>% 
  # quero apenas as universidades públicas federais
  filter(TP_CATEGORIA_ADMINISTRATIVA == 1,
         TP_ORGANIZACAO_ACADEMICA == 1,
         # apenas presenciais
         TP_MODALIDADE_ENSINO  == 1,
         # sem tecnólogos
         TP_GRAU_ACADEMICO  != 3)

# gráfico de colunas ------------------------------------------------------

evasao_ifes_2016 <- evasao_ifes %>% 
  # group_by(CO_IES, CO_CURSO, CO_MUNICIPIO, TP_GRAU_ACADEMICO) %>% 
  filter(NU_ANO_REFERENCIA == max(NU_ANO_REFERENCIA)) %>% 
  group_by(NO_IES) %>% 
  select(TAP, TCA, TDA) %>% 
  summarise(across(where(is.numeric), mean)) %>% 
  arrange(desc(TCA)) %>% 
  mutate(rank_TCA = row_number())

evasao_ifes_2016_graf <- evasao_ifes_2016 %>% 
  mutate(NO_IES = factor(NO_IES, levels = rev(NO_IES))) %>% 
  pivot_longer(-c(NO_IES, rank_TCA)) %>% 
  mutate(value_graf = ifelse(name == "TCA",
                             paste0(round(value, 1), "%"),
                             NA_character_))
  

graf <- evasao_ifes_2016_graf %>% 
  filter(rank_TCA <= 20)  %>% 
  ggplot(aes(x = NO_IES, y = value, fill = name)) +
  geom_col(position = "fill") +
  coord_flip() +
  geom_text(
    # data = evasao_ifes_2016_graf %>% filter(name == "TCA", rank_TCA <= 20),
    position = position_fill(vjust = 0.5),
    family = "fira",
    size = 7,
    color = "white",
    aes(label = value_graf),
    hjust = 0.5
  ) +
  scale_fill_manual(values = c("#84898C", "#009E73", "#CD4F38FF")) +
  labs(title = "<span style='color:#CD4F38FF'>DESISTÊNCIA</span> × <span style='color:#009E73'>CONCLUSÃO</span> × <span style='color:#84898C'>PERMANÊNCIA</span>") +
  theme_classic(base_family = "fira") +
  theme(
    legend.position = "none",
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_blank(),
    axis.text = element_text(size = 14, family = "fira"),
    axis.text.x = element_blank(),
    plot.title = element_markdown(size = 24, hjust = -1)
  )
graf

ggsave(graf, filename = "./plot/graf_col_evasao_ifes.png", device = "png", dpi = 350, height = 6, width = 12)


# gráfico boxplot ---------------------------------------------------------

evasao_ifes_boxplot <- evasao_ifes %>% 
  filter(NU_ANO_REFERENCIA == max(NU_ANO_REFERENCIA)) %>% 
  select(NO_IES, TAP, TCA, TDA) %>% 
  mutate(NO_IES = ifelse(NO_IES == "UNIVERSIDADE DE BRASÍLIA",
                "UnB",
                "DEMAIS\nIFES")) %>% 
  pivot_longer(-NO_IES)

graf2 <- evasao_ifes_boxplot %>% 
  ggplot(aes(x = NO_IES, y = value, fill = name)) + 
  geom_boxplot(alpha = 0.8, color = "#84898C") +
  coord_flip() +
  scale_fill_manual(values = c("#84898C", "#009E73", "#CD4F38FF")) +
  labs(title = "<span style='color:#84898C'>PERMANÊNCIA</span> × <span style='color:#009E73'>CONCLUSÃO</span> × <span style='color:#CD4F38FF'>DESISTÊNCIA</span>") +
  theme_classic(base_family = "fira") +
  theme(
    legend.position = "none",
    axis.title = element_blank(),
    axis.text.x = element_text(size = 16, colour = "#84898C"),
    axis.ticks = element_blank(),
    axis.line.y = element_blank(),
    axis.line.x = element_line(colour = "#84898C"),
    axis.text = element_text(size = 18, family = "fira"),
    plot.title = element_markdown(size = 24, hjust = 0.5)
  )

ggsave(graf2, filename = "./plot/graf_boxplot_evasao_ifes.png", device = "png", dpi = 350, height = 6, width = 12)

# gráfico waffle ----------------------------------------------------------

evasao_ifes_waffle <- evasao_ifes %>% 
  filter(NU_ANO_REFERENCIA == max(NU_ANO_REFERENCIA)) %>% 
  select(NO_IES, TAP, TCA, TDA) %>% 
  mutate(NO_IES = ifelse(NO_IES == "UNIVERSIDADE DE BRASÍLIA",
                         "UnB",
                         "DEMAIS\nIFES")) %>% 
  group_by(NO_IES) %>% 
  summarise(across(where(is.numeric), ~round(mean(.x), 0))) %>% 
  mutate(NO_IES = factor(NO_IES, levels = rev(NO_IES))) %>% 
  pivot_longer(-NO_IES)

graf3 <- evasao_ifes_waffle %>% 
  ggplot(aes(values = value, fill = name)) +
  geom_waffle(n_rows = 5, color = "white", size = 1.25) +
  facet_wrap(~NO_IES, ncol = 1) +
  scale_fill_manual(values = c("#84898C", "#009E73", "#CD4F38FF")) +
  labs(title = "<span style='color:#84898C'>PERMANÊNCIA</span> × <span style='color:#009E73'>CONCLUSÃO</span> × <span style='color:#CD4F38FF'>DESISTÊNCIA</span>") +
  theme_classic(base_family = "fira") +
  theme(
    legend.position = "none",
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_blank(),
    plot.title = element_markdown(size = 24, hjust = 0.5),
    strip.text = element_markdown(size = 18, hjust = 0.05),
    strip.background.x = element_rect(color = "white")
  )

ggsave(graf3, filename = "./plot/graf_waffle_evasao_ifes.png", device = "png", dpi = 350, height = 6, width = 12)
