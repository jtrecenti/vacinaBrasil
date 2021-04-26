## code to prepare `datasus` dataset goes here


# download ----------------------------------------------------------------

## Acessa link do arquivo
ckanr::ckanr_setup("https://opendatasus.saude.gov.br")

link <- ckanr::package_search("vacina") %>%
  purrr::pluck("results", 1, "resources") %>%
  purrr::keep(~length(.x$mimetype) > 0 && .x$mimetype == "text/csv") %>%
  purrr::pluck(1, "description") %>%
  stringr::str_extract("(?<=Dados Completos\\]\\()[^\\)]+") %>%
  stringr::str_squish()

## Acessa link do arquivo
f <- fs::file_temp("vacinas", ext = ".csv")
httr::GET(link, httr::write_disk(f, TRUE), httr::progress())



# import ------------------------------------------------------------------


n_rows <- length(vroom::vroom_lines(f)) - 1L
chunk_size <- 1e7
n_chunks <- ceiling(n_rows / chunk_size)
skips <- chunk_size * seq(0, n_chunks - 1) + 1

colunas <- c(
  "paciente_idade",
  "paciente_enumSexoBiologico",
  "vacina_descricao_dose"
)

estrutura_dados <- vroom::vroom(f, n_max = 100, col_types = "")
nm <- names(estrutura_dados)
colunas_spec <- rep("_", length(nm))
colunas_spec[nm %in% colunas] <- "c"
colunas_spec <- paste(colunas_spec, collapse = "")

sumarizar <- function(skip, f, chunk_size, colunas, colunas_spec) {
  message(skip)
  dados <- vroom::vroom(
    file = f,
    skip = skip,
    n_max = chunk_size,
    col_types = colunas_spec,
    col_names = FALSE
  )
  dados %>%
    purrr::set_names(colunas) %>%
    dplyr::count(dplyr::across(dplyr::all_of(colunas))) %>%
    dplyr::filter(
      paciente_enumSexoBiologico %in% c("M", "F"),
      !is.na(paciente_idade),
      as.numeric(paciente_idade) >= 18,
      as.numeric(paciente_idade) <= 100
    ) %>%
    dplyr::transmute(
      idade = as.numeric(paciente_idade),
      sexo = dplyr::if_else(paciente_enumSexoBiologico == "M", "Homem", "Mulher"),
      dose = stringr::str_squish(vacina_descricao_dose),
      n
    )
}


# transform ---------------------------------------------------------------

da_sumario <- purrr::map_dfr(skips, sumarizar, f, chunk_size, colunas, colunas_spec)
datasus_idade_sexo_dose <- da_sumario %>%
  dplyr::group_by(idade, sexo, dose) %>%
  dplyr::summarise(n = sum(n), .groups = "drop")

usethis::use_data(datasus_idade_sexo_dose, overwrite = TRUE)
