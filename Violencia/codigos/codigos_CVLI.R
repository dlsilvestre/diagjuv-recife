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
pacotes <- c("qdap","readr","readxl", "stringr", "dplyr", "ggplot2", "viridis", "maps", "raster", "ggmap", "ggrepel", "sp", "maptools")
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
# MANIPULAR BASE 
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

#======= Manipular RPA ========#

# Bairros para cada RPA
rpa1 <- c("; Recife; Santo Amaro; Boa Vista; Cabanga; Ilha do Leite; Paissandu; Santo Antônio; São José; Coelhos; Soledade; Ilha Joana Bezerra;")
rpa2 <- c("; Arruda; Campina do Barreto; Encruzilhada; Hipódromo; Peixinhos; Ponto de Parada; Rosarinho; Torreão; Água Fria; Alto Santa Terezinha; Bomba do Hemetério; Cajueiro; Fundão; Porto da Madeira; Beberibe; Dois Unidos; Linha do Tiro;")
rpa3 <- c("; Aflitos; Alto do Mandu; Alto José Bonifácio; Alto José do Pinho; Apipucos; Brejo da Guabiraba; Brejo de Beberibe; Casa Amarela; Casa Forte; Córrego do Jenipapo; Derby; Dois Irmãos; Espinheiro; Graças; Guabiraba; Jaqueira; Macaxeira; Monteiro; Nova Descoberta; Parnamirim; Passarinho; Pau-Ferro; Poço da Panela, Santana; SÍtio dos Pintos; Tamarineira; Mangabeira; Morro da Conceição; Vasco da Gama;")
rpa4 <- c("; Cordeiro; Ilha do Retiro; Iputinga; Madalena; Prado; Torre; Zumbi; Engenho do Meio; Torrões; Caxangá; Cidade Universitária; Várzea;")
rpa5 <- c("; Afogados; Areias; Barro; Bongi; Caçote; Coqueiral; Curado; Estância; Jardim São Paulo; Jiquiá; Mangueira; Mustardinha; San Martin; Sancho; Tejipió; Totó;")
rpa6 <- c("; Boa Viagem; Brasília Teimosa; Imbiribeira; Ipsep; Pina; Ibura; Jordão; Cohab;")

# combinar em lista
listaBairros <- list(rpa1, rpa2, rpa3, rpa4, rpa5, rpa6)

# funcao p manipular bairros
func.mani1 <- function(x){ 
  x = as.vector(genXtract(x, ";", ";")) # select words between ';' and transform result in vector
  x = substring(x, 2)  # remove blank space in the beggining on the words
  return(x) 
}

rpa1 <- func.mani1(rpa1)
rpa2 <- func.mani1(rpa2)
rpa3 <- func.mani1(rpa3)
rpa4 <- func.mani1(rpa4)
rpa5 <- func.mani1(rpa5)
rpa6 <- func.mani1(rpa6)

dataRPA <- data.frame(bairro = c(rpa1, rpa2, rpa3, rpa4, rpa5, rpa6), 
           RPA = c(rep("rpa1",length(rpa1)),rep("rpa2",length(rpa2)),rep("rpa3",length(rpa3)),
                   rep("rpa4",length(rpa4)),rep("rpa5",length(rpa5)),rep("rpa6",length(rpa6))
                   ))

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
  dataMes <- dataMes[dataMes$date != "DEZ/2017" & dataMes$date != "DEZEMBRO/2017",]
  # grafico
  ggplot(data = dataMes, aes(x = date, y = Freq, group = 1)) +
    geom_line(color = "#7f0000") +
    stat_smooth(method = lm, color= "#E69F00", se = F)+
    labs(x = "", y= paste("Casos de", nome))+
    tema_massa()%+replace% 
    theme(axis.text.x = element_text(size=10,angle = 60, hjust=.5,vjust=.5,face="plain")) +
    ggsave(paste0(nome, "_jovensMes.png"), path = "Violencia/resultados", width = 8, height = 4, units = "in")
}

# executar funcao
func.mes(cvli_mani$MÊS, cvli_mani$ANO, "CVLI")
func.mes(est_mani$MÊS, est_mani$ANO, "Estupros")

#=========================================#
# ANO / SEXO 
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
    tema_massa()%+replace%
    theme(legend.position="bottom")+
    ggsave(paste0(nome, "_por_AnoSexo.png"), path = "Violencia/resultados",width = 8, height = 4, units = "in")
}

func.anoSexo(cvli_jovem, "CVLI")
func.anoSexo(est_jovem, "Estupros")

#=====================#
# RACA / SEXO
#=====================#

func.racaSexo <- function(data, info){
  # contagem
  raca_sex <- data.frame(table(data$COR.DA.PELE, data$SEXO))
  # manipulacao
  raca_sex$Var1 <- as.character(raca_sex$Var1)
  raca_sex$Var1[raca_sex$Var1 == "NAO INFORMADO"] <- "Não Informada"
  raca_sex <- raca_sex[raca_sex$Var2 != "DESCONHECIDO",]
  # proporcao
  raca_sex <- mutate(raca_sex, prop = round((Freq / sum(Freq)),3)*100)
  raca_sex$prop2 <- paste(raca_sex$prop, "%", sep="")
  # ordenar
  raca_sex$Var1  = factor(raca_sex$Var1 , levels = unique(raca_sex$Var1[order(raca_sex$Freq)]), ordered=TRUE)
  # plotar
  plotRacaSexo <- ggplot(data=raca_sex, aes(x=Var1, y=prop, fill=Var2)) +
    geom_bar(stat="identity", position=position_dodge())+
    geom_text(aes(label = ifelse(raca_sex$prop>0, as.character(raca_sex$prop2),'')), vjust = -0.5,size = 3.5,
              position = position_dodge(width = 1))+
    scale_fill_manual("Sexo", values=c("#E69F00", "#7f0000"))+
    labs(x = "", y = paste("Porcentagem do Total de", info) )+
    tema_massa()
  ggsave(paste0(info, "_plotRacaSexo.png"), plotRacaSexo, path = "Violencia/resultados", width = 7, height = 3, units = "in")
  return(plotRacaSexo)
}

# executar func.racaSexo
func.racaSexo(cvli_jovem, "CVLI")
func.racaSexo(est_jovem, "Estupros")


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

#======= Manipulacao dos dados =======#

#---- funcao para combinar strings ----#
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

# executar func.maniB 
cvli_bairro <- func.maniB(cvli_mani)
est_bairro <- func.maniB(est_mani)

#===========================================================#
#======= CVLI de Jovens prop a pop de jovens por ano =======#

func.quadroA <- function(data, varJovem, info){
  library(ggpubr)
  # prop de jovens por 1000 jovens e arredondar
  data <- mutate(data, prop_pop_1000 =  ((varJovem / data$pop_jovem)*1000)/5)
  data$prop_pop_1000 <- round(data$prop_pop_1000, 2)
  # ordenar
  data$localidade <- factor(data$localidade, levels = data$localidade[order(data$prop_pop_1000)])
  # grafico
  jov1000bar <- ggplot(data = data, aes(localidade, y = prop_pop_1000))+
  geom_col(fill = "#7f0000")+
  geom_text(aes(label = prop_pop_1000), nudge_y = 1, size = 4)+
  labs(x = "", y = paste("Prop. de", info ,"de Jovens Por 1000 Jovens"))+
  coord_flip()+
  tema_massa()
  jov1000map <- mapa.funcao(shp_recife, data, data$prop_pop_1000, "" , legendtitle = paste("Prop. de", info ,"de Jovens \n      Por 1000 Jovens"), pallete = "A")
  jov1000   <- ggarrange(jov1000bar, jov1000map, ncol = 2, nrow = 1)
  ggsave(paste0(info,"_1000JovQUADRO.png"), jov1000, path = "Violencia/resultados", width = 17, height = 13, units = "in")
  return(jov1000)
}

# executar func.quadroA
func.quadroA(cvli_bairro, cvli_bairro$cvli_jovem, "CVLI")
func.quadroA(est_bairro, est_bairro$est_jovem, "Estupros")

#=================================================#
#======= Prop de CVLI de Jovens por Bairro =======#

func.quadroB <- function(data, varJovem, info){
  library(ggpubr)
  # prop de jovens por bairro
  data <- mutate(data,  JovProp = round(((varJovem / sum(varJovem))*100), 1))  
  data$JovProp <- round(data$JovProp, 2)
  # ordenar
  data$localidade <- factor(data$localidade, levels = data$localidade[order(data$JovProp)])
  # grafico
  jovPropBar <- ggplot(data = data, aes(localidade, y = JovProp))+
    geom_col(fill = "#7f0000")+
    geom_text(aes(label = JovProp), hjust = -0.2, size = 4)+
    labs(x = "", y = paste("Porcent. de", info ,"de Jovens"))+
    coord_flip()+
    tema_massa()
  # Mapa 
  jovPropMap <- mapa.funcao(shp_recife, data, data$JovProp, "" , legendtitle = paste("Porcent. de", info ,"de Jovens"), pallete = "A")
  # Combinar em quadro
  jovProp   <- ggarrange(jovPropBar, jovPropMap, ncol = 2, nrow = 1)
  ggsave(paste0(info,"_jovPropQUADRO.png"), jovProp, path = "Violencia/resultados", width = 16, height = 13, units = "in")
  return(jovProp)
}

# executar func.quadroB
func.quadroB(cvli_bairro, cvli_bairro$cvli_jovem, "CVLI")
func.quadroB(est_bairro, est_bairro$est_jovem, "Estupros")

#===============================================#
#======= CVLI de Jovens do Total de CVLI =======# 

func.quadroC <- function(data, varJovem, varTotal, info){
  library(ggpubr)
  # prop de 'info' de jovens do total de 'info'
  data <- mutate(data, propDoTotal = round(((varJovem / varTotal)*100), 2))  
  data$propDoTotal[is.nan(data$propDoTotal)] <- 0
  # ordenar
  data$localidade <- factor(data$localidade, levels = data$localidade[order(data$propDoTotal)])
  # grafico
  jovPropDoTotalBar <- ggplot(data = data, aes(localidade, y = propDoTotal))+
    geom_col(fill = "#7f0000")+
    geom_text(aes(label = propDoTotal), hjust = -0.1, size = 4)+
    labs(x = "", y = paste("Porcent. de", info ,"de Jovens do Total de", info))+
    coord_flip()+
    tema_massa()
  # Mapa 
  jovPropDoTotalMap <- mapa.funcao(shp_recife, data, data$propDoTotal, "" , legendtitle = paste("Porcent. de", info ,"de Jovens  \n      do Total de", info), pallete = "A")
  # Combinar em quadro
  jovPropDoTotal   <- ggarrange(jovPropDoTotalBar, jovPropDoTotalMap, ncol = 2, nrow = 1)
  ggsave(paste0(info,"_jovPropDoTotalQUADRO.png"), jovPropDoTotal, path = "Violencia/resultados", width = 16, height = 13, units = "in")
  return(jovPropDoTotal)
}

# executar func.quadroC
func.quadroC(cvli_bairro, cvli_bairro$cvli_jovem, cvli_bairro$cvli_total, "CVLI")
func.quadroC(est_bairro, est_bairro$est_jovem, est_bairro$est_total, "Estupros")

#=========================================#
# MAPAS POR ANO [EM CONSTRUCAO]
#=======================================#

func.mapaAno <- function(data, varAno, varJovem, info){
  ## MANIPULAR BAIRRO/ANO ##
  
  anos = c(2013:2017)
  for (i in anos){
    data = data[varAno == i,]
    varJovem = varJovem[varAno == i]
    mapa <- mapa.funcao(shp_recife, data, varJovem, paste(i) , legendtitle = paste(info, "de Jovens") , pallete = "A")
    mapas = list(mapas, mapa)
    
    ggarrange(est_map_2013, est_map_2014, est_map_2015, est_map_2016, est_map_2017, ncol = 3, nrow = 2, common.legend = T, legend = "bottom")
    ggsave("Violencia/resultados/CVLI_ano_mapa.png", width = 16, height = 9, units = "in")
  }
}

for (i in anos){
  print(i)
}

#---- 2013 ----#
est_map_2013 <- mapa.funcao(shp_recife, cvli_bairro[cvli_bairro$Ano == 2013,], cvli_bairro$cvli_jovem[cvli_bairro$Ano == 2013], 
            "2013" , legendtitle = "CVLI Absoluta de Jovens" , pallete = "A")

#---- 2014 ----#
est_map_2014 <- mapa.funcao(shp_recife, cvli_bairro[cvli_bairro$Ano == 2014,], cvli_bairro$Freq[cvli_bairro$Ano == 2014], 
            "2014" , legendtitle = "CVLI Absoluta de Jovens", pallete = "A")

#---- 2015 ----#
est_map_2015 <- mapa.funcao(shp_recife, cvli_bairro[cvli_bairro$Ano == 2015,], cvli_bairro$Freq[cvli_bairro$Ano == 2015],
            "2015" , legendtitle = "CVLI Absoluta de Jovens", pallete = "A")

#---- combinar ----#
ggarrange(est_map_2013, est_map_2014, est_map_2015, est_map_2016, est_map_2017, ncol = 3, nrow = 2, common.legend = T, legend = "bottom")
ggsave("Violencia/resultados/CVLI_ano_mapa.png", width = 16, height = 9, units = "in")


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


