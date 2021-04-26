library(PNADcIBGE)

dadosPNADc <- get_pnadc(year = 2020, quarter = 4, vars=c("V2007", "V2009"))
totalsexo <- survey::svytotal(~interaction(V2007, V2009), dadosPNADc, na.rm = TRUE)

pop_idade_sexo <- tibble::tibble(
  nm = names(totalsexo),
  pop = as.numeric(totalsexo)
) %>%
  dplyr::transmute(
    sexo = stringr::str_extract(nm, "[HM][a-z]+"),
    idade = stringr::str_extract(nm, "(?<=\\.)[0-9]+$"),
    pop
  )

usethis::use_data(pop_idade_sexo, overwrite = TRUE)
