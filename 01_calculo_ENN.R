# ======================================================================
# ISOLAMENTO ESTRUTURAL DE MANCHAS DE VEGETAÇÃO NATURAL
# Métrica: Euclidean Nearest-Neighbor Distance (ENN)
#
# Classes MapBiomas incluídas:
#   3  = Formação Florestal
#   4  = Formação Savânica
#   11 = Campo Alagado e Área Pantanosa
#
# Pacotes:
#   terra
#   landscapemetrics
#   writexl
#
# Estrutura esperada:
#
# conectividade/
# ├── dados/
# │   ├── brazil_coverage_2024.tif
# │   ├── vegetacao_binaria_recorte_31983.tif
# │   ├── 31983.shp
# │   ├── 31983.dbf
# │   ├── 31983.shx
# │   ├── 31983.prj
# │   └── 31983.cpg
# ├── resultados/
# └── scripts/
#
# O shapefile deve conter os seis pontos amostrais.
# ======================================================================


# ----------------------------------------------------------------------
# 1. Pacotes
# ----------------------------------------------------------------------

pacotes <- c(
  "terra",
  "landscapemetrics",
  "writexl"
)

pacotes_ausentes <- pacotes[
  !pacotes %in% rownames(installed.packages())
]

if (length(pacotes_ausentes) > 0) {
  install.packages(pacotes_ausentes)
}

library(terra)
library(landscapemetrics)
library(writexl)


# ----------------------------------------------------------------------
# 2. Diretórios
# ----------------------------------------------------------------------

# Execute o script a partir da pasta principal do projeto:
# C:/Users/babii/OneDrive/Documents/conectividade

pasta_dados <- "dados"
pasta_resultados <- "resultados"

if (!dir.exists(pasta_resultados)) {
  dir.create(
    pasta_resultados,
    recursive = TRUE
  )
}


# ----------------------------------------------------------------------
# 3. Arquivos de entrada
# ----------------------------------------------------------------------

arquivo_mapbiomas <- file.path(
  pasta_dados,
  "brazil_coverage_2024.tif"
)

arquivo_template <- file.path(
  pasta_dados,
  "vegetacao_binaria_recorte_31983.tif"
)

arquivo_pontos <- file.path(
  pasta_dados,
  "31983.shp"
)

arquivos_entrada <- c(
  arquivo_mapbiomas,
  arquivo_template,
  arquivo_pontos
)

arquivos_ausentes <- arquivos_entrada[
  !file.exists(arquivos_entrada)
]

if (length(arquivos_ausentes) > 0) {
  stop(
    paste(
      "Arquivos não encontrados:",
      paste(arquivos_ausentes, collapse = "\n"),
      sep = "\n"
    )
  )
}


# ----------------------------------------------------------------------
# 4. Importação
# ----------------------------------------------------------------------

mapbiomas <- rast(arquivo_mapbiomas)
template <- rast(arquivo_template)
parques <- vect(arquivo_pontos)

if (nrow(parques) != 6) {
  stop(
    paste0(
      "A camada de pontos contém ",
      nrow(parques),
      " registros; eram esperados 6."
    )
  )
}


# ----------------------------------------------------------------------
# 5. Padronização do sistema de referência
# ----------------------------------------------------------------------

# Os pontos são projetados para o mesmo SRC do raster-modelo, se necessário.

if (!same.crs(parques, template)) {
  parques <- project(
    parques,
    crs(template)
  )
}

# O raster MapBiomas é reprojetado e alinhado ao raster-modelo.
# O método de vizinho mais próximo preserva as classes categóricas.

mapbiomas_local <- project(
  mapbiomas,
  template,
  method = "near"
)

names(mapbiomas_local) <- "classe_mapbiomas"


# ----------------------------------------------------------------------
# 6. Construção da paisagem binária
# ----------------------------------------------------------------------

classes_vegetacao <- c(3, 4, 11)

vegetacao_natural <- ifel(
  mapbiomas_local == classes_vegetacao[1] |
    mapbiomas_local == classes_vegetacao[2] |
    mapbiomas_local == classes_vegetacao[3],
  1,
  NA
)

names(vegetacao_natural) <- "vegetacao_natural"


# ----------------------------------------------------------------------
# 7. Validação da paisagem
# ----------------------------------------------------------------------

validacao_paisagem <- check_landscape(
  vegetacao_natural
)

print(validacao_paisagem)

if (!all(validacao_paisagem$ok)) {
  stop(
    "A paisagem não atendeu a todos os requisitos do landscapemetrics."
  )
}

# Conferência das classes originais sob os pontos amostrais.

classes_nos_parques <- terra::extract(
  mapbiomas_local,
  parques
)

# Conferência da presença dos pontos nas manchas selecionadas.

vegetacao_nos_parques <- terra::extract(
  vegetacao_natural,
  parques
)

pontos_fora <- which(
  is.na(vegetacao_nos_parques[[2]])
)

if (length(pontos_fora) > 0) {
  stop(
    paste0(
      "Os seguintes pontos não estão sobre as classes 3, 4 ou 11: ",
      paste(pontos_fora, collapse = ", "),
      "."
    )
  )
}


# ----------------------------------------------------------------------
# 8. Cálculo do ENN
# ----------------------------------------------------------------------

# directions = 8:
# células conectadas lateralmente ou diagonalmente formam a mesma mancha.

enn <- extract_lsm(
  landscape = vegetacao_natural,
  y = parques,
  extract_id = seq_len(nrow(parques)),
  what = "lsm_p_enn",
  directions = 8,
  verbose = TRUE
)

enn <- enn[
  order(enn$extract_id),
]

if (nrow(enn) != nrow(parques)) {
  stop(
    paste0(
      "Foram obtidos ",
      nrow(enn),
      " valores de ENN para ",
      nrow(parques),
      " parques."
    )
  )
}

if (anyNA(enn$value)) {
  warning(
    paste(
      "Um ou mais valores de ENN são NA.",
      "Isso pode ocorrer quando não há outra mancha da mesma classe",
      "dentro da extensão analisada."
    )
  )
}


# ----------------------------------------------------------------------
# 9. Tabela final
# ----------------------------------------------------------------------

atributos_parques <- as.data.frame(parques)

resultado_enn <- cbind(
  parque_ordem = seq_len(nrow(parques)),
  atributos_parques,
  classe_mapbiomas = classes_nos_parques[[2]],
  mancha_id = enn$id,
  ENN_m = enn$value
)

print(resultado_enn)


# ----------------------------------------------------------------------
# 10. Inclusão do resultado na camada espacial
# ----------------------------------------------------------------------

parques$parque_ordem <- seq_len(nrow(parques))
parques$classe_mb <- classes_nos_parques[[2]]
parques$mancha_id <- enn$id
parques$ENN_m <- enn$value


# ----------------------------------------------------------------------
# 11. Exportação dos resultados
# ----------------------------------------------------------------------

# Raster binário utilizado na análise

writeRaster(
  vegetacao_natural,
  file.path(
    pasta_resultados,
    "vegetacao_natural_classes_3_4_11.tif"
  ),
  overwrite = TRUE
)

# Tabela em formato CSV

write.csv(
  resultado_enn,
  file.path(
    pasta_resultados,
    "ENN_parques_vegetacao_natural.csv"
  ),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# Tabela em formato Excel

write_xlsx(
  resultado_enn,
  file.path(
    pasta_resultados,
    "ENN_parques_vegetacao_natural.xlsx"
  )
)

# Camada espacial com os valores de ENN

writeVector(
  parques,
  file.path(
    pasta_resultados,
    "parques_ENN_vegetacao_natural.gpkg"
  ),
  overwrite = TRUE
)


# ----------------------------------------------------------------------
# 12. Registro dos parâmetros metodológicos
# ----------------------------------------------------------------------

parametros <- data.frame(
  parametro = c(
    "Ano do mapa de cobertura",
    "Classes MapBiomas incluídas",
    "Regra de conectividade",
    "Métrica",
    "Unidade do ENN",
    "SRC da análise",
    "Resolução X",
    "Resolução Y"
  ),
  valor = c(
    "2024",
    "3; 4; 11",
    "8 direções",
    "Euclidean Nearest-Neighbor Distance",
    "metros",
    crs(template, proj = TRUE),
    as.character(res(vegetacao_natural)[1]),
    as.character(res(vegetacao_natural)[2])
  )
)

write.csv(
  parametros,
  file.path(
    pasta_resultados,
    "parametros_analise_ENN.csv"
  ),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)


# ----------------------------------------------------------------------
# 13. Registro da sessão computacional
# ----------------------------------------------------------------------

capture.output(
  sessionInfo(),
  file = file.path(
    pasta_resultados,
    "sessionInfo_ENN.txt"
  )
)


# ----------------------------------------------------------------------
# 14. Resumo final
# ----------------------------------------------------------------------

cat(
  "\n==========================================================\n",
  "ANÁLISE CONCLUÍDA\n",
  "==========================================================\n\n",
  "Classes consideradas como vegetação natural:\n",
  "3  - Formação Florestal\n",
  "4  - Formação Savânica\n",
  "11 - Campo Alagado e Área Pantanosa\n\n",
  "Valores de ENN:\n"
)

print(
  resultado_enn[
    ,
    c(
      "parque_ordem",
      "classe_mapbiomas",
      "mancha_id",
      "ENN_m"
    )
  ]
)

cat(
  "\nProdutos salvos na pasta:",
  normalizePath(pasta_resultados),
  "\n"
)