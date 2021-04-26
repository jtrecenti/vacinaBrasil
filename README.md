
<!-- README.md is generated from README.Rmd. Please edit that file -->

# vacinaBrasil

<!-- badges: start -->
<!-- badges: end -->

Dados de pessoas vacinadas no Brasil por idade. A população é estimada a
partir da PNAD contínua. Os dados de vacinação são obtidos do Open
Datasus. Ver códigos para obtenção dos resultados na pasta `data-raw/`.

**Última atualização**: 25/04/2021

## População

``` r
library(ggplot2)
library(vacinaBrasil)

fonte <- stringr::str_glue(
  "Fonte:\n",
  "População: PNAD Contínua\n",
  "Vacinação: Open DataSus"
)

dose <- datasus_idade_sexo_dose %>% 
  dplyr::select(sexo, idade, tipo = dose, n) %>% 
  dplyr::filter(tipo %in% c("1ª Dose", "2ª Dose"))

pop <- pop_idade_sexo %>% 
  dplyr::transmute(
    sexo,
    idade = as.numeric(idade),
    tipo = "População",
    n = pop 
  ) %>% 
  dplyr::filter(idade <= 100, idade >= 18)

dados <- dplyr::bind_rows(pop, dose)

dados %>% 
  dplyr::mutate(n = n * ((sexo == "Homem") * 2 -1)) %>% 
  dplyr::mutate(tipo = forcats::lvls_reorder(tipo, c(2, 1, 3))) %>% 
  ggplot(aes(x = n, y = factor(idade), fill = sexo, alpha = tipo)) +
  geom_col(width = 1, position = "identity") +
  annotate(geom = "text", x = 0, y = seq(3, 73, 10), label = seq(20, 90, 10)) +
  scale_x_continuous(
    breaks = seq(-1500, 1500, 500) * 1000,
    labels = paste0(abs(seq(-1500, 1500, 500)), "K")
  ) +
  scale_y_discrete(breaks = seq(0, 100, 10)) + 
  scale_fill_viridis_d(begin = .3, end = .8, option = "C") +
  scale_alpha_manual(values = c(1, .6, .3)) +
  theme_minimal() +
  labs(
    x = "População", 
    y = "Idade",
    fill = "",
    alpha = "",
    title = "Vacinação no Brasil",
    caption = fonte
  ) +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme(
    panel.grid.minor = element_blank(),
    axis.text.y = element_blank(),
    legend.position = "top",
    plot.title = element_text(hjust = .5)
  )
```

<img src="man/figures/README-pop-1.png" width="100%" />

## Proporcional

``` r
dados %>% 
  dplyr::mutate(n = n * ((sexo == "Homem") * 2 -1)) %>% 
  tidyr::pivot_wider(names_from = tipo, values_from = n) %>% 
  janitor::clean_names() %>% 
  dplyr::mutate(
    prop1 = x1a_dose / abs(populacao),
    prop2 = x2a_dose / abs(populacao)
  ) %>% 
  dplyr::select(sexo, idade, prop1, prop2) %>% 
  tidyr::pivot_longer(c(prop1, prop2), names_to = "tipo", values_to = "n") %>% 
  dplyr::mutate(n = pmin(abs(n), 1) * sign(n)) %>% 
  dplyr::mutate(tipo = dplyr::if_else(tipo == "prop1", "1ª Dose", "2ª Dose")) %>% 
  dplyr::mutate(tipo = forcats::lvls_reorder(tipo, c(2, 1))) %>% 
  ggplot(aes(x = n, y = factor(idade), fill = sexo, alpha = tipo)) +
  geom_col(width = 1, position = "identity") +
  annotate(geom = "text", x = 0, y = seq(3, 73, 10), label = seq(20, 90, 10)) +
  scale_x_continuous(
    breaks = seq(-1, 1, .2),
    labels = scales::percent(abs(seq(-1, 1, .2)))
  ) +
  scale_y_discrete(breaks = seq(0, 100, 10)) + 
  scale_fill_viridis_d(begin = .3, end = .8, option = "C") +
  scale_alpha_manual(values = c(1, .6, .3)) +
  theme_minimal() +
  labs(
    x = "Proporção da população", 
    y = "Idade",
    fill = "",
    alpha = "",
    title = "Vacinação no Brasil",
    caption = fonte
  ) +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme(
    panel.grid.minor = element_blank(),
    axis.text.y = element_blank(),
    legend.position = "top",
    plot.title = element_text(hjust = .5)
  )
```

<img src="man/figures/README-prop-1.png" width="100%" />

## Créditos

A ideia do primeiro gráfico foi do professor Elias Krainski, da UFPR

<https://twitter.com/eliaskrainski/status/1385056297322696709/photo/1>

O que fiz foi apenas reproduzir com todos os códigos abertos.
