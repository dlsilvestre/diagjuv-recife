#====================================================#
# DIAGNOSTICO DA CONDICAO JUVENIL NO RECIFE          #              
#====================================================#
# VIOLENCIA  - CVLI                                  #
#----------------------------------------------------#
# Prefeitura da Cidade do Recife                     #
# Secretaria Executiva de Juventude                  #
#----------------------------------------------------#
# Recife 2018                                        #
#----------------------------------------------------#
# Claudio A. Monteiro                                #
# claudiomonteiro@gmail.com                          #
#----------------------------------------------------#
# Qualquer duvida contate o desenvolvedor            #
# #UseSoftwareLivre                                  #
#----------------------------------------------------#

# instalar pacotes
# install.packages(c("readxl", "stringr", "dplyr", "ggplot2", "geojsonio"))

# carregar pacotes
pacotes <- c("readr","readxl", "stringr", "dplyr", "ggplot2", "viridis", "maps", "raster", "ggmap", "ggrepel", "sp", "maptools")
lapply(pacotes, library, character.only = T)

# Tema para Graficos
tema_massa <- function (base_size = 12, base_family = "") {
  theme_minimal(base_size = base_size, base_family = base_family) %+replace% 
    theme(axis.text.x = element_text(size=12,hjust=.5,vjust=.5,face="plain"),
          axis.text.y = element_text(size=12,angle=0,hjust=1,vjust=0,face="plain"), 
          axis.title.x = element_text(colour="black",size=12,angle=0,hjust=.5,vjust=0,face="plain"),
          axis.title.y = element_text(colour="black",size=12,angle=90,hjust=0.5,vjust=0.6,face="plain"),
          title = element_text(colour="black",size=14,angle=0,hjust=.5,vjust=.5,face="bold"))
}

# importar banco CVLI 2013-2017
cvli_data <- read_excel("Violencia/dados/Rel - 1015 - CVLI - logradouros, bairro, gênero, cor da pele, idade, mês - RECIFE - Jan2013 a Nov2017.xlsx")
est_data <- read_excel("Violencia/dados/Rel - 1023 - ESTUPRO - logradouros, bairro, gênero, cor da pele, idade, mês - RECIFE - Jan2013 a Nov2017.xlsx")

# carregar shapefile 1 (completo)
shp_recife <- shapefile("Dados Gerais/bases_cartograficas/Bairros.shp")

#===============================#
# Manipular base 
#===============================#

# selecionar linhas e colunas
cvli_data <- cvli_data[-c(1:5, 2932:2942),-4] 
est_data <- est_data[-c(1:5, 2303:2311 ),]

# limpar base de dados
func.maniA <- function(data){
    # primeira linha do banco p colnames
    names <- c(data[1,])
    # retirar primeira linha 
    data <-  data[-1, ]
    # renomear colunas 
    colnames(data) <- names
    # criar var jovem
    data$IDADE <- as.numeric(data$IDADE)
    data <- data[!is.na(data$IDADE),]
    data <- mutate(data, jovem = ifelse(IDADE >= 15 & IDADE <=  29, 1, 0))
    data_jovem <- data[data$jovem == 1,]
return(list(data, data_jovem))
}

# aplicar funcao
cvli_mani <- func.maniA(cvli_data)
est_mani <- func.maniA(est_data)

# selecionar casos de jovens
cvli_jovem <- data.frame(cvli_mani[2])
est_jovem <- data.frame(est_mani[2])

# selecionar todos os casos
cvli_mani <- data.frame(cvli_mani[1])
est_mani <- data.frame(est_mani[1])

#===========================#
# ANALISE FAIXA ETARIA
#===========================#

func.faixa <- function(data, var, nome){
  #grafico
  ret_plot = ggplot(data = data)+
    geom_bar(aes(x = var), fill = "#333333")+
    geom_vline(xintercept = 15, size = 1, colour = "#FF3721", linetype = "dashed")+
    geom_vline(xintercept = 29, size = 1, colour = "#FF3721", linetype = "dashed")+
    labs(x = "Idade", y = paste("Frequência de", nome))+
    scale_x_continuous(breaks = pretty(var, n = 25)) +
    tema_massa()+
    ggsave(paste0(nome, "_por_idade.png"), path = "Violencia/resultados",width = 9, height = 6, units = "in")
  # porcentagem de jovens do total
  pct = data.frame(table(var))
  pct$var = as.numeric(as.character(pct$var))
  pct = mutate(pct, jovens = ifelse(var >=15 & var <= 29, 1, 0))
  pct = aggregate(pct$Freq, by = list(Category = pct$jovens), FUN=sum)
  pct = c(pct[2,2] /  sum(pct[1,2] + pct[2,2] ))*100
  pasteA = paste0(round(pct, 2),"% dos casos de ", nome, " são de jovens" ) 
  return(list(ret_plot, pasteA))
}

func.faixa(cvli_mani, cvli_mani$IDADE, "CVLI")
func.faixa(est_mani, est_mani$IDADE, "Estupros")

#=======================#
# ANALISE TEMPORAL 
#=======================#

#===== Ano =====#

func.ano <- function(dataJovem, nome){
  # contagem 
  jovemAno <- data.frame(table(dataJovem$ANO, dataJovem$jovem))
  # fator
  jovemAno$jovem <- factor(jovemAno$Var2, levels = c("0", "1"), labels = c("Não-jovem", "Jovem"))
  # grafico
  ggplot(data = jovemAno) +
    geom_line(aes(x = Var1, y = Freq, group = jovem, color = jovem, linetype = jovem), size = 1) + 
    geom_label(aes(x = Var1, y = Freq, label = Freq))+
    labs(x = "", y = paste("Casos", nome) )+
    scale_linetype_manual("", values = c(1, 2)) +
    scale_color_manual("", values=c("#E69F00", "#7f0000"))+
    scale_y_continuous(limits = c(0,500))+
    tema_massa()+
    ggsave(paste0(nome, "_por_ano_Linha.png"), path = "Violencia/resultados",width = 9, height = 6, units = "in")
}

func.ano(cvli_mani, "CVLI")
func.ano(est_mani, "Estupros")

#===== Mes =====#

func.mes <- function(varMes, varAno, nome){
  # ordernar meses e anos
  if (nome == "Estupros"){
    varMes <- factor(varMes, levels = c("JANEIRO", "FEVEREIRO", "MARÇO", "ABRIL", "MAIO", "JUNHO", "JULHO", "AGOSTO", "SETEMBRO", "OUTUBRO", "NOVEMBRO", "DEZEMBRO"))
  }
  else {
  varMes <- factor(varMes, levels = c("JAN", "FEV", "MAR", "ABR", "MAI", "JUN", "JUL", "AGO", "SET", "OUT", "NOV", "DEZ"))
  }
  # ordenar anos
  varAno <- factor(varAno, levels = c("2013", "2014", "2015", "2016", "2017")) 
  # juntar dados
  dataMes <- data.frame(table(varMes, varAno))
  # variavel MES/ANO
  dataMes$date <-  with(dataMes, paste0(varMes, "/", varAno))
  dataMes$date <- factor(dataMes$date, levels = dataMes$date)
  #**Retirar Dezembro**
  dataMes <- dataMes[dataMes$date != "DEZ/2017",]
  # grafico
  ggplot(data = dataMes, aes(x = date, y = Freq, group = 1)) +
    geom_line(color = "#7f0000") +
    stat_smooth(method = lm, color= "#E69F00", se = F)+
    labs(x = "", y= paste("Casos de", nome))+
    tema_massa()%+replace% 
    theme(axis.text.x = element_text(size=10,angle = 60, hjust=.5,vjust=.5,face="plain")) +
    ggsave(paste0(nome, "_jovensAno.png"), path = "Violencia/resultados", width = 8, height = 4, units = "in")
}

# executar funcao
func.mes(cvli_mani$MÊS, cvli_mani$ANO, "CVLI")
func.mes(est_mani$MÊS, est_mani$ANO, "Estupros")

#=========================================#
# SEXO / ANO 
#=========================================#

func.anoSexo <- function(dataJovem, nome){
  # contagem 
  jovemAno <- data.frame(table(dataJovem$ANO, dataJovem$SEXO))
  if (nlevels(jovemAno$Var2) > 2){
    jovemAno <- jovemAno[jovemAno$Var2 != "DESCONHECIDO",]
  }

  # grafico
    ggplot(data = jovemAno) +
    geom_line(aes(x = Var1, y = Freq, group = Var2, color = Var2, linetype = Var2), size = 1) + 
    geom_label(aes(x = Var1, y = Freq, label = Freq))+
    labs(x = "", y = paste("Casos", nome) )+
    scale_linetype_manual("", values = c(1, 2)) +
    scale_color_manual("", values=c("#E69F00", "#7f0000"))+
    tema_massa()+
    ggsave(paste0(nome, "_por_AnoSexo.png"), path = "Violencia/resultados",width = 9, height = 6, units = "in")
}

func.anoSexo(cvli_jovem, "CVLI")
func.anoSexo(est_jovem, "Estupros")

#=========================================#
# ANALISES DOS BAIRROS 
#=========================================#

#===== Funcao p/ Mapa =====#
mapa.funcao <- function(shape, data, variable, maintitle, legendtitle, pallete) { 
  library(stringi); library(ggplot2)
  # function to create merge string based on similarity
  best_match= function(string_vector,string_replacement){
    s<-string_replacement %>% 
      purrr::map_int(~{
        .x %>% 
          RecordLinkage::levenshteinSim(string_vector) %>%
          match(max(.),.)
      })
    string_vector[s] = string_replacement
    return(string_vector)
  }
  # manipulate string for merge
  data$EBAIRRNOME = data$localidade
  data$EBAIRRNOME = toupper(data$EBAIRRNOME)
  data$EBAIRRNOME = stri_trans_general(data$EBAIRRNOME , "Latin-ASCII")
  data$EBAIRRNOME = best_match(data$EBAIRRNOME, shape$EBAIRRNOME)
  data$variavel   = variable
  
  # merge data with shapefile
  shp_data <- merge(shape, data, by = "EBAIRRNOME", all = T)
  
  # definir labels no mapa (3 maiores, 3 menores)
  shp_data$variavel[is.na(shp_data$variavel)] = 0
  shp_data = shp_data[order(shp_data$variavel),]
  shp_data$bairros_detasq = 1
  # shp_data$bairros_detasq[1:4] = ""
  shp_data$bairros_detasq[c(length(shp_data)-4):c(length(shp_data))] = ""
  shp_data$bairros_detasq = with(shp_data, paste0(shp_data$bairros_detasq, shp_data$EBAIRRNOME))
  shp_data$bairros_detasq_cod = grepl(shp_data$bairros_detasq, pattern = "1")
  shp_data$bairros_detasq[shp_data$bairros_detasq_cod == TRUE ] = ""
  
  # tranformar shapefile em polygonsdataframe
  data_fortity = fortify(shp_data, region = "EBAIRRNOME")
  localidade = shp_data@data$EBAIRRNOME
  # extrair centroides dos poligonos
  centroids.df = as.data.frame(coordinates(shp_data))
  names(centroids.df) = c("Longitude", "Latitude")  #more sensible column localidades
  # base para plotagem
  variavel = shp_data@data$variavel
  nomes_centroides = shp_data$bairros_detasq
  map_dataframe = data.frame(localidade, variavel, centroids.df, nomes_centroides)
  
  plot = ggplot(data = map_dataframe, aes(map_id = localidade)) + 
    geom_map(aes(fill = shp_data$variavel),colour = grey(0.96),  map = data_fortity) +
    expand_limits(x = data_fortity$long, y = data_fortity$lat) +
    scale_fill_viridis(name = legendtitle, option= pallete, direction = -1) +
    # scale_fill_gradient(name = legendtitle, low="lightgreen", high= "darkblue")+
    geom_label_repel(aes(label = nomes_centroides, x = Longitude, y = Latitude), size = 4.5, color = "black") +
    labs(title = maintitle)+
    coord_fixed(1) +
    theme_nothing(legend = T)+
    theme(legend.position="bottom",
          legend.key.size = unit(0.7, "cm"),
          legend.text = element_text(size = 14, hjust = 3, vjust = 3),
          legend.title = element_text(size = 15, face = "plain"),
          title = element_text(size = 15, face = "bold"))
  
  return(plot)
}

#===== manipular base =====#

best_match= function(string_vector,string_replacement){
  s<-string_replacement %>% 
    purrr::map_int(~{
      .x %>% 
        RecordLinkage::levenshteinSim(string_vector) %>%
        match(max(.),.)
    })
  string_vector[s] = string_replacement
  return(string_vector)
}


#--- criar base de bairros ---#
func.maniB <- function(data){
  library(stringi)
  library(reshape2)
       # importar base pop jovem
        dataDem <- read_csv("Demografia/resultados/demo_jovem_2010.csv")
        # cvli jov e total por bairro
        data_bairro <- data.frame(table(data$BAIRRO, data$jovem))
        # from wide to long 
        data_bairro <- dcast(data_bairro, Var1 ~ Var2, value.var="Freq")
        # criar total de cvli por bairro
        data_bairro <- mutate(data_bairro, total_cvli = `0` + `1`)
        # renomear colunas
        if (length(data) == 12){
          colnames(data_bairro) <- c("BAIRRO", "cvli_nao_jovem", "cvli_jovem", "cvli_total")
        }     else{
          colnames(data_bairro) <- c("BAIRRO", "est_nao_jovem", "est_jovem", "est_total")
          }
        # mergir co cvli_bairro_jovem
        dataDem$BAIRRO <- toupper(dataDem$localidade)%>%
                          stri_trans_general("Latin-ASCII")
        data_bairro$BAIRRO <- best_match(as.character(data_bairro$BAIRRO), dataDem$BAIRRO)
        data_bairro_out <- merge(dataDem, data_bairro, by = "BAIRRO", all = T)
        # manipular missing cases
        data_bairro_out <- data_bairro_out[!is.na(data_bairro_out$localidade),]
        data_bairro_out[is.na(data_bairro_out)] <- 0
        return(data_bairro_out)
}

# executar funcoes 
cvli_bairro <- func.maniB(cvli_mani)
est_bairro <- func.maniB(est_mani)

#============================================#
#  CVLI de Jovens prop a pop de jovens por ano

func.quadroA <- function(data, varJovem, info){
  library(ggpubr)
# prop de cvli de jovens por 1000 jovens e arredondar
data <- mutate(data, prop_pop_1000 =  ((varJovem / data$pop_jovem)*1000)/5)
data$prop_pop_1000 <- round(data$prop_pop_1000, 2)
# ordenar
data$localidade <- factor(data$localidade, levels = data$localidade[order(data$prop_pop_1000)])
# grafico
jov1000bar <- ggplot(data = data, aes(localidade, y = prop_pop_1000))+
  geom_col(fill = "#7f0000")+
  geom_text(aes(label = prop_pop_1000), nudge_y = 1, size = 4)+
  labs(x = "", y = "Prop. de CVLI de Jovens por 1000 jovens")+
  coord_flip()+
  tema_massa()
#---- Mapa ----#
jov1000map <- mapa.funcao(shp_recife, data, data$prop_pop_1000, "" , legendtitle = paste("Prop. de", info ,"de Jovens \n      Por 1000 Jovens"), pallete = "A")
jov1000   <- ggarrange(jov1000bar, jov1000map, ncol = 2, nrow = 1)
ggsave(paste0(info,"_1000JovQUADRO.png"), jov1000, path = "Violencia/resultados", width = 17, height = 13, units = "in")
return(jov1000)
}

func.quadroA(cvli_bairro, cvli_bairro$cvli_jovem, "CVLI")
func.quadroA(cvli_bairro, cvli_bairro$cvli_jovem, "CVLI")


#============================================#
# Prop de CVLI de Jovens por Bairro
  
#---- Barra ----#

# prop do total
cvli_bairro <- mutate(cvli_bairro,  cvliJovProp = round(((cvli_jovem / sum(cvli_jovem))*100), 1))  
cvli_bairro$cvliJovProp2 <- paste(cvli_bairro$cvliJovProp, "%", sep="")

# ordenar
cvli_bairro$localidade <- factor(cvli_bairro$localidade, levels = cvli_bairro$localidade[order(cvli_bairro$cvliJovProp)])

# grafico
cvli_bair_jovBar <- ggplot(data = cvli_bairro, aes(localidade, y = cvliJovProp))+
    geom_col(fill = "#7f0000")+
    geom_text(aes(label = cvliJovProp2), nudge_y = 0.32)+
    labs(x = "", y = "Procent. de CVLI de Jovens por Bairro")+
    coord_flip()+
    tema_massa()
ggsave("CVLI_PropDeCVLI_Jov_barraBairro.png",cvli_bair_jov1000bar,  path = "Violencia/resultados",width = 8, height = 12, units = "in")

#---- Mapa (a partir de "funcoesgerais.R") ----#
cvli_bair_jovmap <- mapa.funcao(shp_recife, cvli_bairro, cvli_bairro$cvliJovProp, "" ,legendtitle = "Prop. de CVLI de Jovens \n           Por Bairro", pallete = "A")
ggsave("CVLI_PropDeCVLI_Jov_mapaBairro.png", cvli_bair_jovmap, path = "Violencia/resultados",width = 8, height = 11, units = "in")

#---- Combinar ----#
ggarrange(cvli_bair_jovBar, cvli_bair_jovmap, ncol = 2, nrow = 1)
ggsave("CVLI_PropDeCVLI_Jov_Bairro.png", path = "Violencia/resultados", width = 17, height = 13, units = "in")

#============================================#
#  CVLI de Jovens do Total de CVLI 

# criar prop do total de cvli
cvli_bairro <- mutate(cvli_bairro, cvliJov_TotalCvli = round(((cvli_jovem / cvli_total)*100), 2))  
cvli_bairro$cvliJov_TotalCvli[is.na(cvli_bairro$cvliJov_TotalCvli)] <- 0
cvli_bairro$cvliJov_TotalCvli[is.nan(cvli_bairro$cvliJov_TotalCvli)] <- 0
cvli_bairro$cvliJov_TotalCvli2 <- paste(cvli_bairro$cvliJov_TotalCvli, "%", sep="")

# ordenar
cvli_bairro$localidade <- factor(cvli_bairro$localidade, levels = cvli_bairro$localidade[order(cvli_bairro$cvliJov_TotalCvli)])

# grafico
cvliJov_bair_TotalBar<- ggplot(data = cvli_bairro, aes(localidade, y = cvliJov_TotalCvli))+
    geom_col(fill = "#7f0000")+
    geom_text(aes(label = cvliJov_TotalCvli2), nudge_y = 4)+
    labs(x = "", y = "Casos de CVLI de Jovens do Total")+
    coord_flip()+
    tema_massa()
ggsave("CVLI_PropDoTotaldeJovens_barraBairro.png", cvliJov_bair_TotalBar, path = "Violencia/resultados",width = 7, height = 12, units = "in")

#---- Mapa (a partir de "funcoesgerais.R") ----#
cvliJov_Totalmap <- mapa.funcao(shp_recife, cvli_bairro, cvli_bairro$cvliJov_TotalCvli, "" ,legendtitle = "Prop. de CVLI de Jovens \n      ", pallete = "A")
ggsave("CVLI_PropDeJov_TotalcvliMapa.png", cvliJov_Totalmap, path = "Violencia/resultados",width = 8, height = 11, units = "in")

#---- Combinar ----#
ggarrange(cvliJov_bair_TotalBar, cvliJov_Totalmap, ncol = 2, nrow = 1)
ggsave("CVLI_PropDeJov_Totalcvli.png", path = "Violencia/resultados", width = 17, height = 13, units = "in")

#===========================#
# MAPAS POR ANO
#===========================#

# contagem de mortes por bairro
jovem_cvli_bairro_ano <- data.frame(table(jovem_cvli$BAIRRO, jovem_cvli$ANO))

# criar variavel localidade como chr
jovem_cvli_bairro_ano$localidade <- as.character(jovem_cvli_bairro_ano$Var1)

#==== mapa (a partir de "funcoesgerais.R") ====#
# *diminuir tamanho da fonte em geom_label
#---- 2013 ----#
est_map_2013 <- mapa.funcao(shp_recife, jovem_cvli_bairro_ano[jovem_cvli_bairro_ano$Var2 == 2013,], jovem_cvli_bairro_ano$Freq[jovem_cvli_bairro_ano$Var2 == 2013], 
            "2013" , legendtitle = "CVLI Absoluta de Jovens" , pallete = "A")

#---- 2014 ----#
est_map_2014 <- mapa.funcao(shp_recife, jovem_cvli_bairro_ano[jovem_cvli_bairro_ano$Var2 == 2014,], jovem_cvli_bairro_ano$Freq[jovem_cvli_bairro_ano$Var2 == 2014], 
            "2014" , legendtitle = "CVLI Absoluta de Jovens", pallete = "A")

#---- 2015 ----#
est_map_2015 <- mapa.funcao(shp_recife, jovem_cvli_bairro_ano[jovem_cvli_bairro_ano$Var2 == 2015,], jovem_cvli_bairro_ano$Freq[jovem_cvli_bairro_ano$Var2 == 2015],
            "2015" , legendtitle = "CVLI Absoluta de Jovens", pallete = "A")

#---- 2016 ----#
est_map_2016 <- mapa.funcao(shp_recife, jovem_cvli_bairro_ano[jovem_cvli_bairro_ano$Var2 == 2016,], jovem_cvli_bairro_ano$Freq[jovem_cvli_bairro_ano$Var2 == 2016],
            "2016" , legendtitle = "CVLI Absoluta de Jovens",pallete = "A")

#---- 2017 ----#
est_map_2017 <- mapa.funcao(shp_recife, jovem_cvli_bairro_ano[jovem_cvli_bairro_ano$Var2 == 2017,], jovem_cvli_bairro_ano$Freq[jovem_cvli_bairro_ano$Var2 == 2017],
            "2017" , legendtitle = "CVLI Absoluta de Jovens", pallete = "A")

#---- combinar ----#
ggarrange(est_map_2013, est_map_2014, est_map_2015, est_map_2016, est_map_2017, ncol = 3, nrow = 2, common.legend = T, legend = "bottom")
ggsave("Violencia/resultados/CVLI_ano_mapa.png", width = 16, height = 9, units = "in")

#====================#
# Raca

# contagem
bairro_cvli_raca <- data.frame(table(jovem_cvli$BAIRRO, jovem_cvli$`COR DA PELE`))

#
aggregate(jovem_cvli, by=list(Var1=apacc_voz$sexo), FUN=sum)

#==================================================================#
# CVLI por LOGRADOURO [EXECUTAR EM MAQUINA COM BOM PROCESSAMENTO] 
#================================================================#
# checar merge dos bancos: perda de casos

#----------------------#
# manipular dados

# contagem por logradouro
jovem_cvli_logd <- data.frame(table(jovem_cvli$LOGRADOURO))
jovem_cvli_logd <- jovem_cvli_logd[order(jovem_cvli_logd$Freq, decreasing = T),]

# baixar e carregar logradouros em geoJSON
data_url <- "http://dados.recife.pe.gov.br/dataset/c1f100f0-f56f-4dd4-9dcc-1aa4da28798a/resource/18f16fda-32e2-4fe9-a5ab-0e7852258400/download/trechoslogradouros.geojson"
data_file <- "trechoslogradouros.geojson"
download.file(data_url, data_file)
data_json <- readOGR(data_file)

# tratar nomes na base de dados p mergir
jovem_cvli_logd$logradouro_nome <- jovem_cvli_logd$Var1
data_json$logradouro_nome <- gsub('RUA\\s','', data_json$logradouro_nome) 
data_json$logradouro_nome <- gsub('AV','', data_json$logradouro_nome) 
data_json$logradouro_nome <- str_trim(data_json$logradouro_nome, "left")    

# mergir dados e geo
data_json@data$id <- rownames(data_json@data)
data_json@data   <- join(data_json@data, jovem_cvli_logd, by="logradouro_nome")
#data_json@data$Freq[is.na(data_json@data$Freq)] <- 0
mapa.df <- fortify(data_json)
mapa.df <- join(mapa.df, data_json@data, by="id")

#------ CVLI por logradouro ------#
ggplot(mapa.df, aes(x=long, y=lat, group=group))+
  geom_line(aes(color= Freq), alpha=0.1)+
  geom_text(aes(label = ifelse(mapa.df$Freq>5, as.character(mapa.df$logradouro_nome),'')))+
  scale_color_viridis(name = "CVLI de Jovens", option= "A", direction = -1, na.value = (alpha=0.1)) +
  coord_fixed()+
  theme_void()
  ggsave("CVLI_jovens_logradouroA.png", path = "Violencia/resultados",width = 14, height = 17, units = "in")

# baixar o mapa de Recife
mapImage <-get_map(c(lon =  -34.91, lat =-8.045), zoom = 12)

#*********

# tranformar shapefile em polygonsdataframe
data_json_fort <- fortify(data_json, region = "logradouro_nome")
localidade = data_json@data$EBAIRRNOME

# extrair centroides dos poligonos
require(reshape2)
data_json@data$id <- rownames(data_json@data) 
data_json<- melt(data_json)
centroids.df = data.frame(coordinates(data_json))
names(centroids.df) = c("Longitude", "Latitude")  #more sensible column localidades

# base para plotagem
variavel = data_json@data$Freq
map_dataframe = data.frame(localidade, variavel, centroids.df)

plot = ggplot(data = map_dataframe, aes(map_id = localidade)) + 
  geom_map(aes(fill = data_json$Freq),colour = grey(0.96),  map = data_fortity) +
  expand_limits(x = data_json_fort$long, y = data_json_fort$lat) +
  scale_fill_viridis(name = legendtitle, option= pallete, direction = -1) +
  # scale_fill_gradient(name = legendtitle, low="lightgreen", high= "darkblue")+
  geom_label_repel(aes(label = nomes_centroides, x = Longitude, y = Latitude), size = 2.5, color = "black") +
  labs(title = maintitle)+
  coord_fixed(1) +
  theme_nothing(legend = T)+
  theme(legend.position="bottom",
        legend.key.size = unit(0.7, "cm"),
        legend.text = element_text(size = 14, hjust = 3, vjust = 3),
        legend.title = element_text(size = 15, face = "plain"),
        title = element_text(size = 15, face = "bold"))

#*********

#====== MAPA RECIFE + CVLI/LOGRADOUROS ======#
ggmap(mapImage, extent = "normal", maprange = FALSE)+ 
  geom_line(data = mapa.df, aes(long, lat, group = group, color = Freq))+
  scale_color_viridis(name= "CVLI", option= "A", direction = -1) 
  ggsave("CVLI_jovens_logradouro1.png", path = "Violencia/resultados",width = 14, height = 17, units = "in")

#==========================#
# CVLI por raca 

# contar e manipular
cvli_raca_count <- data.frame(table(jovem_cvli$`COR DA PELE`, jovem_cvli$SEXO))
cvli_raca_count$Var1 <- as.character(cvli_raca_count$Var1)
cvli_raca_count$Var1[3] <- "N?o Informada"

# proporcao
cvli_raca_count <- mutate(cvli_raca_count, prop = round((Freq / sum(Freq)),2)*100)
cvli_raca_count$prop <- paste(cvli_raca_count$prop, "%", sep="")

# ordenar
cvli_raca_count$Var1 <- factor(cvli_raca_count$Var1, 
                                      levels = cvli_raca_count$Var1[order(cvli_raca_count$Freq)])

# plotar
ggplot(cvli_raca_count, aes(x = Var1, y = Freq))+
  geom_col(fill = "#333333")+
  geom_label(aes(label = prop), size = 3.2)+
  labs(x ="", y = "Porcent. de Representantes") +
  coord_flip()
#ggsave("cvli_jovens_RACA.png", path = "Violencia/resultados",width = 7, height = 3, units = "in")

#==========================#
# CVLI por sexo 

# contar e manipular
cvli_sex_count <- data.frame(table(jovem_cvli$SEXO))
cvli_sex_count$Var1 <- as.character(cvli_sex_count$Var1)
cvli_sex_count$Var1 <- c("Mulher","Homem")

# proporcao
cvli_sex_count <- mutate(cvli_sex_count, prop = round((Freq / sum(Freq)),2)*100)
cvli_sex_count$prop <- paste(cvli_sex_count$prop, "%", sep="")

# ordenar
cvli_sex_count$Var1 <- factor(cvli_sex_count$Var1, 
                               levels = cvli_sex_count$Var1[order(cvli_sex_count$Freq)])

# plotar
ggplot(cvli_sex_count, aes(x = Var1, y = Freq))+
  geom_col(fill = "#333333")+
  geom_label(aes(label = prop), size = 3.2)+
  labs(x ="", y = "Porcent. de Representantes") +
  coord_flip()+
  tema_massa()
ggsave("cvli_jovens_sexo.png", path = "Violencia/resultados",width = 7, height = 2, units = "in")

#=============================#
# CASOS ESTUPRO               #
#=============================#

# carregar base
estupros_data <- read_excel("Violencia/dados/Rel - 1023 - ESTUPRO - logradouros, bairro, g?nero, cor da pele, idade, m?s - RECIFE - Jan2013 a Nov2017.xlsx")

#===============================#
# Manipular base

# remover linhas
estupros_data <- estupros_data[-c(1:5, 2303:2311 ),]

# primeira linha do banco p colnames
names_est <- c(estupros_data[1,])

# retirar primeira linha
estupros_data <-  estupros_data[-1,]

# renomear colunas adequadamente
colnames(estupros_data) <- str_replace(names_est, "NA", "DATA")

# jovens
estupros_data <- mutate(estupros_data, jovem = ifelse(IDADE >= 15 & IDADE <= 29, 1, 0))
estupros_data_jovem <- estupros_data_jovem[estupros_data$jovem ==1, ]

#============================#
# Estupros por faixa-etaria

# transformar em numerico
estupros_data$IDADE <- as.numeric(estupros_data$IDADE)

# grafico
ggplot(data = estupros_data)+
  geom_bar(aes(x = estupros_data$IDADE), fill = "#333333")+
  geom_vline(xintercept = 15, size = 1, colour = "#FF3721",linetype = "dashed")+
  geom_vline(xintercept = 29, size = 1, colour = "#FF3721", linetype = "dashed")+
  labs(x = "Idade", y = "Frequ?nica de Estupros")
ggsave("estupro_por_idade.png", path = "Violencia/resultados",width = 8, height = 5, units = "in")

# porcentagem de cvli de jovens do total
pct_est <-data.frame(table(estupros_data$IDADE))
pct_est$Var1 <- as.numeric(as.character(pct_est$Var1))
pct_est <- mutate(pct_est, jovens = ifelse(Var1 < 15, 1, 0))
pct_est <- aggregate(pct_est$Freq, by=list(Category=pct_est$jovens), FUN=sum)

# proporcao de 
pct_est[2, 2] / pct_est[1,2] 

#=============================#
# Estupros por bairro

#------ Barra -------#
estupros_bairro_jovem <- data.frame(table(estupros_data_jovem$BAIRRO))
estupros_bairro_jovem$Var1 <- factor(estupros_bairro_jovem$Var1, 
                              levels = estupros_bairro_jovem$Var1[order(estupros_bairro_jovem$Freq)])

estupros_bairro_jovem <- mutate(estupros_bairro_jovem, prop = round(((Freq / sum(Freq))*100), 1))  
estupros_bairro_jovem$prop2 <- paste(estupros_bairro_jovem$prop, "%", sep="")

ggplot(data = estupros_bairro_jovem, aes(Var1, y = prop))+
  geom_col(fill = "#FF872F")+
  geom_text(aes(label = prop2))+
  labs(x = "", y = "Casos de Estupros Jovens do Total")+
  coord_flip()+
  tema_massa()%+replace% 
  theme(axis.text.x = element_text(size=10,hjust=.5,vjust=.5,face="plain"),
        axis.text.y = element_text(size=9,angle=0,hjust=1,vjust=0,face="plain"), 
        axis.title.x = element_text(colour="black",size=12,angle=0,hjust=.5,vjust=0,face="plain"),
        axis.title.y = element_text(colour="black",size=12,angle=90,hjust=0.5,vjust=0.6,face="plain"),
        title = element_text(colour="black",size=14,angle=0,hjust=.5,vjust=.5,face="bold"))

ggsave("estupros_por_bairro_BARRA_ABS.png", path = "Violencia/resultados",width = 4, height = 10, units = "in")

#----- mapa jovens -----#

# criar variavel localidade como chr
estupros_bairro_jovem$localidade <- as.character(estupros_bairro_jovem$Var1)

# proporcional
estupros_bairro_jovem <- mutate(estupros_bairro_jovem, prop = (Freq / sum(Freq))*100)  

# mapa
Est_jovemMap <- mapa.funcao(shp_recife, data = estupros_bairro_jovem,
             variable = estupros_bairro_jovem$Freq, "Estupros De Jovens" ,legendtitle = "Casos de Estupro \n    (2013-2017)",
             pallete = "A")

#------ Mapa total  ------#

# contagem 
estupros_data_bairro <- data.frame(table(estupros_data$BAIRRO))

# criar variavel localidade como chr
estupros_data_bairro$localidade <- as.character(estupros_data_bairro$Var1)

# proporcional
estupros_data_bairro <- mutate(estupros_data_bairro, prop = (Freq / sum(Freq))*100)  

# mapa
EstMap <- mapa.funcao(shp_recife, data = estupros_data_bairro,
            variable = estupros_data_bairro$prop, "Estupros Totais" ,legendtitle = "Casos de Estupro \n    (2013-2017)",
            pallete = "A")

#----- combinar -----#
ggarrange(EstMap, Est_jovemMap, ncol = 2, common.legend = T, legend = "bottom")
ggsave("Violencia/resultados/MAPA_ESTUPROS.png", width = 13, height = 7, units = "in")

#====================================#
#----- mapa jovens proporcional -----#

# importar base
demo_jovem_2010 <- read_csv("Demografia/resultados/demo_jovem_2010.csv")

# manipular nome
demo_jovem_2010$localidade <- toupper(demo_jovem_2010$localidade)
demo_jovem_2010$localidade <- stri_trans_general(demo_jovem_2010$localidade , "Latin-ASCII")

# criar variavel localidade como chr
estupros_bairro_jovem$localidade <- as.character(estupros_bairro_jovem$Var1)

# mergir dados
estupros_bairro_jovem <- merge(estupros_bairro_jovem, demo_jovem_2010, by = "localidade", all = T)

# transformar NA em 0 
estupros_bairro_jovem$Freq[is.na(estupros_bairro_jovem$Freq)] <- 0

# proporcional a pop de jovens
estupros_bairro_jovem <- mutate(estupros_bairro_jovem, estupros_por_100_jovens = ((Freq / pop_jovem)*1000) )  

# mapa
Est_jovem_PropMap <- mapa.funcao(shp_recife, data = estupros_bairro_jovem,
                            variable = estupros_bairro_jovem$estupros_por_100_jovens, "" ,legendtitle = "Casos de Estupro \n    (2013-2017)",
                            pallete = "A")
Est_jovem_PropMap
ggsave("Violencia/resultados/MAPA_ESTUPROS_PROP.png", width = 8, height = 10, units = "in")

#
estupros_bairro_jovem[estupros_bairro_jovem$localidade == "JAQUEIRA",]

#====================#

# importar base
demo_jovem_2010 <- read_csv("Demografia/resultados/demo_jovem_2010.csv")

# manipular nome
demo_jovem_2010$localidade <- toupper(demo_jovem_2010$localidade)
demo_jovem_2010$localidade <- stri_trans_general(demo_jovem_2010$localidade , "Latin-ASCII")

# criar variavel localidade como chr
estupros_bairro_jovem$localidade <- as.character(estupros_bairro_jovem$Var1)

# mergir dados
estupros_bairro_jovem <- merge(estupros_bairro_jovem, demo_jovem_2010, by = "localidade", all = T)

# transformar NA em 0 
estupros_bairro_jovem$Freq[is.na(estupros_bairro_jovem$Freq)] <- 0

# proporcional a pop de jovens
estupros_bairro_jovem <- mutate(estupros_bairro_jovem, estupros_por_100_jovens = ((Freq / pop_jovem)*1000) )  

# mapa
Est_jovem_PropMap <- mapa.funcao(shp_recife, data = estupros_bairro_jovem,
                                 variable = estupros_bairro_jovem$estupros_por_100_jovens, "" ,legendtitle = "Casos de Estupro \n    (2013-2017)",
                                 pallete = "A")
Est_jovem_PropMap
ggsave("Violencia/resultados/MAPA_ESTUPROS_PROP.png", width = 8, height = 10, units = "in")

#==========================#
# Estupro por raca

# contar e manipular
est_raca_count <- data.frame(table(estupros_data$`COR DA PELE`))
est_raca_count$Var1 <- as.character(est_raca_count$Var1)

#est_raca_count <- est
est_raca_count$Var1 <- c("Amarela","Branca", "Desconhecida/N?o Informada", "N?o Informada",
                         "Negra", "Parda", "Ind?gena")

# proporcao
est_raca_count <- mutate(est_raca_count, prop = round((Freq / sum(Freq)),2)*100)
est_raca_count$prop <- paste(est_raca_count$prop, "%", sep="")

# ordenar
est_raca_count <- est_raca_count[-4,]
est_raca_count$Var1 <- factor(est_raca_count$Var1, 
                               levels = est_raca_count$Var1[order(est_raca_count$Freq)])

# plotar
ggplot(est_raca_count, aes(x = Var1, y = Freq))+
  geom_col(fill = "#333333")+
  geom_label(aes(label = prop), size = 3.5)+
  labs(x ="", y = "Porcent. do Total de Estupros") +
  coord_flip()+
  tema_massa()
ggsave("cvli_jovens_RACA.png", path = "Violencia/resultados",width = 8, height = 3, units = "in")

#========================#



