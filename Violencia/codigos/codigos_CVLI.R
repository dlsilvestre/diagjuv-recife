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
library(readxl); library(stringr); library(dplyr); library(ggplot2); library(viridis)
library(maps); library(mapdata); library(raster); library(ggmap); library(ggrepel); 
library(purrr); library(OpenStreetMap); library(sp); library(maps); library(ggmap)
library(maptools)

# Tema para Graficos
tema_massa <- function (base_size = 12, base_family = "") {
  theme_minimal(base_size = base_size, base_family = base_family) %+replace% 
    theme(axis.text.x = element_text(size=12,hjust=.5,vjust=.5,face="plain"),
          axis.text.y = element_text(size=12,angle=0,hjust=1,vjust=0,face="plain"), 
          axis.title.x = element_text(colour="black",size=12,angle=0,hjust=.5,vjust=0,face="plain"),
          axis.title.y = element_text(colour="black",size=12,angle=90,hjust=0.5,vjust=0.6,face="plain"),
          title = element_text(colour="black",size=14,angle=0,hjust=.5,vjust=.5,face="bold"))
}

# carregar banco CVLI 2013-2017
cvli_data <- read_excel("Documents/git_projects/diagjuv-recife/Violencia/dados/Rel - 1015 - CVLI - logradouros, bairro, gênero, cor da pele, idade, mês - RECIFE - Jan2013 a Nov2017.xlsx")
#===============================#
# Manipular base

# remover linhas
cvli_data <- cvli_data[-c(1:5, 2932:2942 ),-4]

# primeira linha do banco p colnames
names <- c(cvli_data[1,])

# retirar primeira linha 
cvli_data <-  cvli_data[-1, ]

# renomear colunas adequadamente
colnames(cvli_data) <- str_replace(names, "NA", "DATA")

# jovem
cvli_data$IDADE <- as.numeric(cvli_data$IDADE)
cvli_data <- mutate(cvli_data, jovem = ifelse(IDADE >= 15 & IDADE <=  29, 1, 0))
cvli_jovem <- cvli_data[cvli_data$jovem == 1,]

#=========================#
# CVLI por faixa etaria 
#=========================#

# transformar em numerico
cvli_data$IDADE <- as.numeric(cvli_data$IDADE)

# grafico
ggplot(data = cvli_data)+
  geom_bar(aes(x = cvli_data$IDADE), fill = "#333333")+
  geom_vline(xintercept = 15, size = 1, colour = "#FF3721",linetype = "dashed")+
  geom_vline(xintercept = 29, size = 1, colour = "#FF3721", linetype = "dashed")+
  labs(x = "Idade", y = "Frequ?nica de CVLI")+
  tema_massa()
ggsave("mortes_por_idade.png", path = "Violencia/resultados",width = 8, height = 5, units = "in")

# porcentagem de cvli de jovens do total
pct_cvli <-data.frame(table(cvli_data$IDADE))
pct_cvli$Var1 <- as.numeric(as.character(pct_cvli$Var1))
pct_cvli <- mutate(pct_cvli, jovens = ifelse(Var1 >=15 & Var1 <= 29, 1, 0))
pct_cvli <- aggregate(pct_cvli$Freq, by=list(Category=pct_cvli$jovens), FUN=sum)

# proporcao de 
pct_cvli[2, 2] / pct_cvli[1,2]


#=======================#
# CVLI por ano 
#=======================#

#total
ano_cvli_data <- data.frame(table(cvli_data$ANO))

# selecionar jovens
cvli_data$IDADE <- as.numeric(cvli_data$IDADE)
jovem_cvli <- cvli_data[cvli_data$IDADE >= 15 & cvli_data$IDADE < 30 ,]

# contagem por ano
jv_ano_cvli_data <- data.frame(table(jovem_cvli$ANO))

# juntar bases
cvli_data1 <- data.frame(ano_cvli_data, jv_ano_cvli_data[,2])
colnames(cvli_data1) <- c("Ano", "mortesTotais", "mortesJovens")

# total -jovem
cvli_data1 <- mutate(cvli_data1, mortesTotais_jovens = mortesTotais - mortesJovens)

#---- manipular e mergir bases ----#
x1 <- data.frame(cvli_data1[,c(1:2)], grupo = "CVLI Totais")
x2 <- data.frame(cvli_data1[,c(1,3)], grupo = "CVLI de Jovens")
x3 <- data.frame(cvli_data1[,c(1,4)], grupo = "CVLI de Não-jovens")

colnames(x1)[2] <- c("CVLI")
colnames(x2)[2] <- c("CVLI")
colnames(x3)[2] <- c("CVLI")

cvli_data2 <- rbind(x2, x3)

# grafico
ggplot(data = cvli_data2) +
  geom_line(aes(x = Ano, y = CVLI, group = grupo, color = grupo), size = 1) + 
  geom_label(aes(x = Ano, y = CVLI, label = CVLI))+
  labs(x = "", y = "Casos CVLI")+
  scale_color_manual(values=c("#7f0000", "#E69F00"))+
  scale_y_continuous(limits = c(0,500))+
  tema_massa()

# salvar grafico
ggsave("mortes_total_jovens_tempo.png", path = "Violencia/resultados",
       width = 8, height = 4, units = "in")

#=====================#
# POR MES

# ordernar meses
cvli_data$M?S <- factor(cvli_data$M?S, levels = c("JAN", "FEV", "MAR", "ABR", "MAI", "JUN", "JUL",
                                                   "AGO", "SET", "OUT", "NOV", "DEZ"))
# ordenar anos
cvli_data$ANO <- factor(cvli_data$ANO, levels = c("2013", "2014", "2015", "2016", "2017")) 
mes <- data.frame(table(cvli_data$M?S,cvli_data$ANO))

# 
mes$data <-  with(mes, paste0(mes$Var1, "/",mes$Var2))
mes$datax <- factor(mes$data, levels = mes$data)
mes <- mes[-length(mes$Var1),]

# grafico
ggplot(data = mes, aes(x = datax, y = Freq, group = 1)) +
  geom_line(color = "#7f0000") +
  stat_smooth(method = lm, color= "#E69F00", se = F)+
  labs(x = "", y= "Casos de CVLI")+
  tema_massa()
  ggsave("cvli_jovens.png", path = "Violencia/resultados", width = 8, height = 4, units = "in")

#=========================================#
# CVLI por bairro 
#=========================================#

  #------ Barra -------#
  
cvli_bairro_jovem <- data.frame(table(cvli_jovem$BAIRRO))
cvli_bairro_jovem$Var1 <- factor(cvli_bairro_jovem$Var1, 
                                       levels = cvli_bairro_jovem$Var1[order(cvli_bairro_jovem$Freq)])
  
  cvli_bairro_jovem <- mutate(cvli_bairro_jovem, prop = round(((Freq / sum(Freq))*100), 1))  
  cvli_bairro_jovem$prop2 <- paste(cvli_bairro_jovem$prop, "%", sep="")
  
  ggplot(data = cvli_bairro_jovem, aes(Var1, y = prop))+
    geom_col(fill = "#FF872F")+
    geom_text(aes(label = prop2))+
    labs(x = "", y = "Casos de Estupros do Total")+
    coord_flip()+
    tema_massa()%+replace% 
    theme(axis.text.x = element_text(size=10,hjust=.5,vjust=.5,face="plain"),
          axis.text.y = element_text(size=9,angle=0,hjust=1,vjust=0,face="plain"), 
          axis.title.x = element_text(colour="black",size=12,angle=0,hjust=.5,vjust=0,face="plain"),
          axis.title.y = element_text(colour="black",size=12,angle=90,hjust=0.5,vjust=0.6,face="plain"),
          title = element_text(colour="black",size=14,angle=0,hjust=.5,vjust=.5,face="bold"))
  
  ggsave("mortes_por_bairro_BARRA_ABS.png", path = "Violencia/resultados",width = 4, height = 10, units = "in")
  
  
  

#===================#
# Absoluto

# contagem de mortes por bairro
jovem_morte_bairro <- data.frame(table(jovem_cvli$BAIRRO))

# carregar shapefile 1 (completo)
shp_recife1 <- shapefile("Dados Gerais/bases_cartograficas/Bairros.shp")

# criar variavel localidade como chr
jovem_morte_bairro$localidade <- as.character(jovem_morte_bairro$Var1)

#==== ABRIR FUNCOES GERAIS E EXECUTAR MAPA ====#

mapa.funcao(shp_recife1, data = jovem_morte_bairro,
            variable = jovem_morte_bairro$Freq, "" ,legendtitle = "CVLI de Jovens \n    (2013-2017)",
            pallete = "A")

ggsave("mortes_abs_jovens_bairro.png", path = "Violencia/resultados",width = 9, height = 11, units = "in")

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
data_json <- geojson_read(data_file, what = "sp")

# tratar nomes na base de dados p mergir
jovem_cvli_logd$logradouro_nome <- jovem_cvli_logd$Var1
data_json$logradouro_nome <- gsub('RUA\\s','', data_json$logradouro_nome) 
data_json$logradouro_nome <- gsub('AV','', data_json$logradouro_nome) 
data_json$logradouro_nome <- str_trim(data_json$logradouro_nome, "left")    

# mergir dados e geo
data_json@data$id <- rownames(data_json@data)
data_json@data   <- join(data_json@data, jovem_cvli_logd, by="logradouro_nome")
data_json@data$Freq[is.na(data_json@data$Freq)] <- 0.0001
mapa.df     <- fortify(data_json)
mapa.df     <- join(mapa.df, data_json@data, by="id")

#------ CVLI por logradouro ------#
ggplot(mapa.df, aes(x=long, y=lat, group=group))+
  geom_line(aes(color= Freq))+
  scale_color_viridis(name = "CVLI de Jovens",option= "A", direction = -1) +
  coord_fixed()+
  theme_void()
  ggsave("CVLI_jovens_logradouroA.png", path = "Violencia/resultados",width = 14, height = 17, units = "in")

# baixar o mapa de Recife
mapImage <-get_map(c(lon =  -34.91, lat =-8.045), zoom = 12)

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
Est_jovemMap <- mapa.funcao(shp_recife1, data = estupros_bairro_jovem,
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
EstMap <- mapa.funcao(shp_recife1, data = estupros_data_bairro,
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
Est_jovem_PropMap <- mapa.funcao(shp_recife1, data = estupros_bairro_jovem,
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
Est_jovem_PropMap <- mapa.funcao(shp_recife1, data = estupros_bairro_jovem,
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



