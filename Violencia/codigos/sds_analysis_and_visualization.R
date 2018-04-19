#====================================================#
# DIAGNOSTICO DA CONDICAO JUVENIL NO RECIFE          #              
# VIOLENCIA                                          #
# Prefeitura da Cidade do Recife                     #
# Secretaria Executiva de Juventude                  #
#----------------------------------------------------#
# Autor: Claudio A. Monteiro                         #
# Email: claudiomonteiro@gmail.com                   #
# Recife, 2018                                       #
#----------------------------------------------------#

# carregar pacotes
pacotes <- c("stringi","rgdal","ggrepel","qdap","readr","readxl", "stringr", "dplyr", "ggplot2", "viridis", "maps", "raster", "ggmap", "ggrepel", "sp", "maptools")
lapply(pacotes, library, character.only = T)

# carregar dados
bairrosCVP <- read_delim("~/PythonProjects/exploring_sds/bairrosCVP.csv", ";", escape_double = FALSE, trim_ws = TRUE)
logradourosCVP <- read_delim("~/PythonProjects/exploring_sds/logradourosCVP.csv", ";", escape_double = FALSE, trim_ws = TRUE)

# carregar shape Recife: bairros e logradouros
shp_recife <- shapefile("Dados Gerais/bases_cartograficas/Bairros.shp")
Log_recife <- readOGR("Dados Gerais/bases_cartograficas/logradouros_recife/trechoslogradouros.geojson", layer="OGRGeoJSON")

#=============================#
# verificar nomes das ruas


# extrair data de Log_recife e criar variavel teste
data_log <- Log_recife@data
data_log <- mutate(data_log, teste_shape = 1)

#---- logradouro CVP ----#

# padronizar nome dos logd
logradourosCVP$Logradouro <- toupper(logradourosCVP$Lograoduro)%>%
  stri_trans_general("Latin-ASCII")

# agregar valores por logradouro repetido
data_logd_CVP <- aggregate(logradourosCVP$CasosCVP, by = list(Logradouro = logradourosCVP$Logradouro), FUN = sum )

# percentagem de casos com logd nao informado do total
func.informe <- function(var_nao_informe_cat, var_interesse){
  n_informe = var_interesse[var_nao_informe_cat == "NAO INFORMADO"] / sum(var_interesse) * 100
  n_informe = round(n_informe, 2)
  print(paste0(n_informe,"% dos casos não foram informados" ))  
}
func.informe(data_logd_CVP$Logradouro, data_logd_CVP$x)

#---- logradouro Shape ----#

#lista_log <- c("RUA", "1TR", "2TR", "3TR", "4TR", "5TR","AV", "EST", "6TR", "REF", "2ª TR.", "1 TRERSA", "6TR", "3PR", "2PR",
#               "TRV", "COR", "SUB", "PTE", "BEC", "VIL", "PCR", "TRESSA", "2 TRERSA", "7TR", "ENT", "1PR", "PR")

# remover o que ha antes de ' '
data_log$Logradouro <- gsub("^.*? "," ", data_log$logradouro_nome)

# remover primeiro caracter
data_log$Logradouro <- substring(data_log$Logradouro, 2)

# returns string w/o leading whitespace
trim.leading <- function (x)  sub("^\\s+", "", x)
trim.leading(data_log$Logradouro)




