# Carregar pacote
pkgload::load_all()
pak::pkg_install("curso-r/munifacil")

df_disaster <- desastres::get_data_disaster()

# exportar csv
write.csv(df_disaster, "inst/df_disaster.csv")

# escrever a mensagem de commit
commit_message <-
  paste0("[GitHub Actions] Base atualizada em ", Sys.time())

# salvar a mensagem de commit
writeLines(commit_message, "mensagem-comit.txt")
