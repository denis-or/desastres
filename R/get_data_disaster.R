silence_pls <- function(code){
  try(suppressWarnings(
    suppressMessages(
      code
    )),
    silent = T)
}

scrape_disaster <- function(u){

  user <- 'Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/96.0.4664.110 Safari/537.36'

  r <- httr::GET(u, httr::user_agent(user),  httr::config(connecttimeout = 60))
  #
  if(r$status_code != 200) {
    stop("Erro de conexão com o site da Defesa Civil.")
  }

  tabelas <- httr::content(r) |>
    xml2::xml_find_all("//table") |>
    rvest::html_table()

  tabela <- tabelas[[1]]

  tabela <- tabela[-c(1:2),]

  colnames(tabela) <- c("ordem","data_entrada","municipio",
                        "decreto_nr","decreto_data", "decreto_vigencia",
                        "vencimento","tipo","desastre")

  coords <- readRDS('inst/br_pts.rds')

  tab_comp <-silence_pls(tabela |>
                           dplyr::mutate(uf = "MG",
                                         municipio = dplyr::case_when(
                                           municipio == "Presidnte Bernardes" ~ "Presidente Bernardes",
                                           municipio == "Trê Corações" ~ "Três Corações",
                                           municipio == "Piedade do Reio Grande" ~ "Piedade do Rio Grande",
                                           municipio == "Silverânia" ~ "Silveirânia",
                                           TRUE ~ municipio
                                         )) |>
                           munifacil::limpar_colunas(col_muni = municipio, col_uf = uf) |>
                           munifacil::incluir_codigo_ibge(diagnostico = F))

  tab_comp_c <- silence_pls(
    tab_comp |>
      dplyr::left_join(coords, by = "id_municipio") |>
      dplyr::mutate(
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
        image_df = paste0("images/", tolower(substr(icon_df, 1, 4)), ".png")
      ) |>
      subset(,
             c(
               ordem,
               data_entrada,
               municipio,
               decreto_nr,
               decreto_data,
               decreto_vigencia,
               vencimento,
               desastre,
               lat,
               lon,
               icon_df,
               image_df
             )
      ) |>
      dplyr::filter(#icon_df != "Tempestade",
        icon_df != "Seca")
  )

  # tabela_temp <- tabelas[[2]][-1,]
  #
  # colnames(tabela_temp) <- c("ordem","municipio")
  #
  # tabela_temp_c <- tabela_temp |>
  #   transform(uf = "MG",
  #             desastre = "TEMPESTADE",
  #             icon_df = "Tempestade") |>
  #   munifacil::limpar_colunas(municipio, col_uf = uf) |>
  #   munifacil::incluir_codigo_ibge() |>
  #   dplyr::left_join(coords, by = "id_municipio") |>
  #   transform(
  #     image_df = paste0("images/", tolower(substr(icon_df, 1, 4)), ".png"),
  #     data_entrada = '06/01/2022',
  #     decreto_nr = NA_character_,
  #     decreto_data = NA_character_,
  #     decreto_vigencia = NA_character_,
  #     vencimento = NA_character_
  #   ) |>
  #   subset(,c(
  #       ordem,
  #       data_entrada,
  #       municipio,
  #       decreto_nr,
  #       decreto_data,
  #       decreto_vigencia,
  #       vencimento,
  #       desastre,
  #       lat,
  #       lon,
  #       icon_df,
  #       image_df
  #     )
  #   )


  tabela_temp2 <- tabelas[[2]][-1,]

  colnames(tabela_temp2) <- c("ordem","municipio")

  tabela_temp_c2 <- tabela_temp2 |>
    dplyr::mutate(
      municipio = dplyr::case_when(
        municipio == "Cachoeira do Pajeú" ~ "Cachoeira de Pajeú",
        municipio == "Mamona" ~ "Mamonas",
        municipio == "Saliinas" ~ "Salinas",
        TRUE ~ municipio
      ),
      uf = "MG",
      desastre = "SECA",
      icon_df = "Seca") |>
    munifacil::limpar_colunas(municipio, col_uf = uf) |>
    munifacil::incluir_codigo_ibge(diagnostico = F) |>
    dplyr::left_join(coords, by = "id_municipio") |>
    transform(
      image_df = paste0("images/", tolower(substr(icon_df, 1, 4)), ".png"),
      data_entrada = '06/01/2022',
      decreto_nr = NA_character_,
      decreto_data = NA_character_,
      decreto_vigencia = NA_character_,
      vencimento = NA_character_
    ) |>
    subset(,
           select = c(
             ordem,
             data_entrada,
             municipio,
             decreto_nr,
             decreto_data,
             decreto_vigencia,
             vencimento,
             desastre,
             lat,
             lon,
             icon_df,
             image_df
           )
    )


  # tab_comp_c <- rbind(tab_comp_c, tabela_temp_c, tabela_temp_c2)
  tab_comp_c <- rbind(tab_comp_c, tabela_temp_c2)

  format_date_atu <- function(string){

    inicio <- stringr::str_locate(string, "em")[2]+1
    fim <- stringr::str_length(string)

    txt <- stringr::str_sub(string, inicio, fim) |>
      stringr::str_squish() |>
      stringr::str_remove_all("[:punct:]")

    txt <- format(as.Date(txt, "%d%b%Y"), format = "%d de %B %Y")

    txt
  }


  tab_comp_c$dt_atu <- httr::content(r) |>
    xml2::xml_find_all(xpath = ".//p") |>
    rvest::html_text() |>
    {\(x) grep("Atualização", x, value = TRUE)}() |>
    format_date_atu()

  tab_comp_c

}

get_data_disaster <- function() {

  ux <- 'http://www.defesacivil.mg.gov.br/index.php?option=com_content&view=article&id=14'

  df_disaster <- scrape_disaster(u = ux)

  df_disaster

}

