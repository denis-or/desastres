silence_pls <- function(code){
  try(suppressWarnings(
    suppressMessages(
      code
    )),
    silent = T)
}

scrape_disaster <- function(u){



  user <- 'Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/96.0.4664.110 Safari/537.36'

  r <- httr::GET(u, httr::user_agent(user))

  tabelas <- httr::content(r) |>
    xml2::xml_find_all("//table")|>
    rvest::html_table()

  tabela <- tabelas[[3]]

  tabela <- tabela[-c(1:2),]

  colnames(tabela) <- c("ordem","data_entrada","municipio","decreto_nr","decreto_data",
                        "decreto_vigencia","vencimento","tipo","desastre")


  cont_minas <- silence_pls(geobr::read_state("MG") |>
                               sf::st_transform(crs = sf::st_crs(4326)))

  coords <- silence_pls(
    geobr::read_municipal_seat() |>
      sf::st_centroid() |>
      dplyr::mutate(id_municipio = as.character(code_muni))
  )


  tab_comp <-silence_pls(tabela |>
                            munifacil::limpar_colunas(municipio, col_uf = "MG") |>
                            munifacil::incluir_codigo_ibge())

  tab_comp_c <- tab_comp |>
    dplyr::left_join(coords)|>
    dplyr::mutate(lat = sf::st_coordinates(geom)[,2],
                  lon = sf::st_coordinates(geom)[,1],
                  # icon_df = ifelse(stringr::str_detect(desastre, "SECA"), "seca", "alag"),
                  icon_df = factor(dplyr::case_when(
                    stringr::str_detect(desastre, "SECA") ~ "Seca",
                    stringr::str_detect(desastre, "ALAGAMEN") ~ "Alagamento",
                    stringr::str_detect(desastre, "ENXURRA") ~ "Alagamento",
                    stringr::str_detect(desastre, "CORRIDA") ~ "Deslizamento",
                    stringr::str_detect(desastre, "DESLIZ") ~ "Deslizamento",
                    stringr::str_detect(desastre, "ESTIAG") ~ "Seca",
                    stringr::str_detect(desastre, "INCÊND") ~ "Incendio",
                    stringr::str_detect(desastre, "INUN") ~ "Alagamento",
                    stringr::str_detect(desastre, "FRIO") ~ "Frio",
                    stringr::str_detect(desastre, "FRIA") ~ "Frio",
                    stringr::str_detect(desastre, "QUEDA") ~ "Queda de rochas",
                    stringr::str_detect(desastre, "COLAPSO") ~ "Queda de rochas",
                    stringr::str_detect(desastre, "TEMPESTADE") ~ "Tempestade",
                    stringr::str_detect(desastre, "DOENÇA") ~ "Doenca"
                  )),
                  image_df = paste0("images/",tolower(substr(icon_df, 1, 4)),".png")
    ) |>
    # dplyr::filter(substr(decreto_data, 7, 10) == '2022') |>
    dplyr::select(1:9,26:29) |>
    dplyr::filter(icon_df != "Tempestade",
                  icon_df != "Seca")


  tabela_temp <- tabelas[[1]]

  colnames(tabela_temp) <- c("ordem","municipio")

  tabela_temp_c <- tabela_temp |>
    transform(desastre = "TEMPESTADE") |>
    munifacil::limpar_colunas(municipio, col_uf = "MG") |>
    munifacil::incluir_codigo_ibge() |>
    dplyr::left_join(coords)|>
    dplyr::mutate(lat = sf::st_coordinates(geom)[,2],
                  lon = sf::st_coordinates(geom)[,1],
                  icon_df = "Tempestade",
                  image_df = paste0("images/",tolower(substr(icon_df, 1, 4)),".png"),
                  data_entrada = '06/01/2022',
                  decreto_nr = NA_character_,
                  decreto_data = NA_character_,
                  decreto_vigencia = NA_character_,
                  vencimento = NA_character_,
                  tipo = NA_character_
    ) |>
    dplyr::select(ordem, data_entrada, municipio, decreto_nr,
                  decreto_data, decreto_vigencia, vencimento,
                  tipo, desastre, lat, lon, icon_df, image_df)

  tabela_temp2 <- tabelas[[2]]

  colnames(tabela_temp2) <- c("ordem","municipio")

  tabela_temp_c2 <- tabela_temp2 |>
    transform(desastre = "SECA") |>
    munifacil::limpar_colunas(municipio, col_uf = "MG") |>
    munifacil::incluir_codigo_ibge() |>
    dplyr::left_join(coords)|>
    dplyr::mutate(lat = sf::st_coordinates(geom)[,2],
                  lon = sf::st_coordinates(geom)[,1],
                  icon_df = "Seca",
                  image_df = paste0("images/",tolower(substr(icon_df, 1, 4)),".png"),
                  data_entrada = '06/01/2022',
                  decreto_nr = NA_character_,
                  decreto_data = NA_character_,
                  decreto_vigencia = NA_character_,
                  vencimento = NA_character_,
                  tipo = NA_character_
    ) |>
    dplyr::select(ordem, data_entrada, municipio, decreto_nr,
                  decreto_data, decreto_vigencia, vencimento,
                  tipo, desastre, lat, lon, icon_df, image_df)

  tab_comp_c <- rbind(tab_comp_c, tabela_temp_c, tabela_temp_c2)

  tab_comp_c

}

get_data_disaster <- function() {

  ux <- 'http://www.defesacivil.mg.gov.br/index.php?option=com_content&view=article&id=14'

  df_disaster <- desastres::scrape_disaster(u = ux)

  df_disaster

}


