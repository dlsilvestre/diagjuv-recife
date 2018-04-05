#================================================#
# JUVENTUDE NA CIDADE DO RECIFE                  #
# Demografia 1.2 - RPAs e Bairros                #
#================================================#
# Recife - Pernambuco - Brasil                   #
#------------------------------------------------#
# Prefeitura da Cidade do Recife                 #
# Secretaria Executiva de Juventude              #
#------------------------------------------------#
# Claudio A. Monteiro                            #
# claudiomonteiro@gmail.com                      #
#------------------------------------------------#
# Qualquer duvida contate o desenvolvedor        #
# #UseSoftwareLivre                              #
#------------------------------------------------#

# instalar pacotes necessarios
# install.packages(c("readxl", "xlsx", "ggplot2", "directlabels", "ggrepel", "readr", "plyr", "rgdal", "ggmap", 
# "maps", "mapdata", "raster", "maptools", "stringi", "DT", "xtable", "gridExtra", "qdap", "ggthemes", "ggpubr", 
# "dplyr", "RecordLinkage", "viridis"), dependencies = T)

# carregar pacotes
library(ggthemes); library(qdap); library(readxl); library(xlsx); library(ggplot2); library(directlabels); library(ggrepel); 
library(readr); library(plyr); library(rgdal); library(ggmap); library(maps); library(mapdata); library(raster); library(maptools); 
library(stringi); library(DT); library(xtable); library(ggpubr); library(dplyr); library(readxl); library(qdap); library(viridis)

#---------------------------------#
#  MANIPULACAO DOS DADOS
#---------------------------------#

# carregar banco
demojovem_data <- read_excel("Demografia/dados/pop20002010_sexage_bairros.xlsx")

# Renomear colunas
colnames(demojovem_data) <- c("code_bairro","localidade", "Ano", "pop_total", "pop_homem", "pop_mulher",
                               "pop_15a19_total", "pop_15a19_homem", "pop_15a19_mulher", 
                               "pop_20a24_total", "pop_20a24_homem", "pop_20a24_mulher",
                               "pop_25a29_total", "pop_25a29_homem", "pop_25a29_mulher")

# remover linhas 1:5 e 195
demojovem_data <- demojovem_data[-c(1:6, 195),]

# dividir banco em 2000 e 2010
demojovem_2000 <- demojovem_data[demojovem_data$Ano == "2000",]
demojovem_2010 <- demojovem_data[demojovem_data$Ano == "2010",]

# inserir nomes de 2010 em 2000 e juntar bancos
demojovem_2010$code_bairro <- demojovem_2000$code_bairro
demojovem_2010$localidade <- demojovem_2000$localidade
demojovem_data <- rbind(demojovem_2000, demojovem_2010) 

# transformar colunas em numeric e inserir nomes dos bairros
demojovem_data_x <- demojovem_data[, - 2]
demojovem_data_x <- as.data.frame(sapply(demojovem_data_x, function(x) as.numeric(x) ))
demojovem_data_x$localidade <- demojovem_data$localidade
demojovem_data <-demojovem_data_x
rm(demojovem_data_x)

#--- criar variaveis juventude  ---#

# populacao jovem total
demojovem_data <- mutate(demojovem_data, pop_jovem = pop_15a19_total + pop_20a24_total + pop_25a29_total)

# populacao homem jovem
demojovem_data <- mutate(demojovem_data, pop_homem_jovem = pop_15a19_homem + pop_20a24_homem + pop_25a29_homem)

# populacao mulher jovem
demojovem_data <- mutate(demojovem_data, pop_mulher_jovem = pop_15a19_mulher + pop_20a24_mulher + pop_25a29_mulher)

# proporcao jovem do total
demojovem_data <- mutate(demojovem_data, prop_jovem_total = round((pop_jovem / pop_total), 3))

# proporcao de homens jovens do total de homens
demojovem_data <- mutate(demojovem_data, prop_homemj_homemt = round((pop_homem_jovem / pop_homem), 3))

# proporcao de mulheres jovens do total de mulheres
demojovem_data <- mutate(demojovem_data, prop_mulherj_mulhert = round((pop_mulher_jovem / pop_mulher), 3))

# proporcao de homens jovens do total de jovens
demojovem_data <- mutate(demojovem_data, prop_homemj_jovemt = round((pop_homem_jovem / pop_jovem), 3))

# proporcao de mulheres jovens do total de jovens
demojovem_data <-mutate(demojovem_data, prop_mulherj_jovemt =  round((pop_mulher_jovem / pop_jovem), 3))

#---- dividir anos ----#
demojovem_2000 <- demojovem_data[demojovem_data$Ano == 2000,]
demojovem_2010 <- demojovem_data[demojovem_data$Ano == 2010,]

write.csv(demojovem_2010, file = "Demografia/resultados/demo_jovem_2010.csv")

#----------------------------------#
# REGIOES POLITICO ADMINSTRATIVAS
#----------------------------------#

# Bairros para cada RPA
rpa1 <- c("; Recife; Santo Amaro; Boa Vista; Cabanga; Ilha do Leite; Paissandu; Santo Antônio; São José; Coelhos; Soledade; Ilha Joana Bezerra;")
rpa2 <- c("; Arruda; Campina do Barreto; Encruzilhada; Hipódromo; Peixinhos; Ponto de Parada; Rosarinho; Torreão; Água Fria; Alto Santa Terezinha; Bomba do Hemetério; Cajueiro; Fundão; Porto da Madeira; Beberibe; Dois Unidos; Linha do Tiro; Campo Grande;")
rpa3 <- c("; Aflitos; Alto do Mandu; Alto José Bonifácio; Alto José do Pinho; Apipucos; Brejo da Guabiraba; Brejo de Beberibe; Casa Amarela; Casa Forte; Córrego do Jenipapo; Derby; Dois Irmãos; Espinheiro; Graças; Guabiraba; Jaqueira; Macaxeira; Monteiro; Nova Descoberta; Parnamirim; Passarinho; Pau-Ferro; Poço da Panela; Santana; Sitio dos Pintos; Tamarineira; Mangabeira; Morro da Conceição; Vasco da Gama;")
rpa4 <- c("; Cordeiro; Ilha do Retiro; Iputinga; Madalena; Prado; Torre; Zumbi; Engenho do Meio; Torrões; Caxangá; Cidade Universitária; Várzea;")
rpa5 <- c("; Afogados; Areias; Barro; Bongi; Caçote; Coqueiral; Curado; Estância; Jardim São Paulo; Jiquiá; Mangueira; Mustardinha; San Martin; Sancho; Tejipió; Totó;")
rpa6 <- c("; Boa Viagem; Brasília Teimosa; Imbiribeira; Ipsep; Pina; Ibura; Jordão; Cohab;")

# funcao para manipular string
func.mani1 <- function(x){ 
  x = as.vector(genXtract(x, ";", ";")) # select words between ';' and transform result in vector
  x = substring(x, 2)  # remove blank space in the beggining on the words
  return(x) }

rpa1 <- func.mani1(rpa1)
rpa2 <- func.mani1(rpa2)
rpa3 <- func.mani1(rpa3)
rpa4 <- func.mani1(rpa4)
rpa5 <- func.mani1(rpa5)
rpa6 <- func.mani1(rpa6)

# combinar rpas em data.frame e salvar
dataRPA <- data.frame(bairro = c(rpa1, rpa2, rpa3, rpa4, rpa5, rpa6), 
                      RPA = c(rep("RPA1",length(rpa1)),rep("RPA2",length(rpa2)),rep("RPA3",length(rpa3)),
                              rep("RPA4",length(rpa4)),rep("RPA5",length(rpa5)),rep("RPA6",length(rpa6))
                      ))
write.csv(dataRPA, "Demografia/resultados/RPAbairroRecife.csv" ,row.names = F)

# Function to return RPA information
func.rpa <- function(data, rpa, Ano, localidade){
  rpa_data = data[ data$localidade %in% rpa, ] # select data based on neighborhoods
  
  rpa_prop = rpa_data[,19:23]                   # select prop  data
  rpa_prop = list(sapply(rpa_prop, mean)) # apply mean to each collumn
  rpa_prop = as.data.frame(do.call(rbind, rpa_prop)) # tranform in dataframe

  rpa_pop = rpa_data[,c(3:14 ,16:18)]                        # select pop data
  rpa_pop = list(sapply(rpa_pop, sum))      # sum rows
  rpa_pop = as.data.frame(do.call(rbind, rpa_pop)) # tranform in dataframe

  rpa_resul <- data.frame(localidade = localidade, Ano = Ano, rpa_pop, rpa_prop)
return(rpa_resul)
}

# execute 2000
rpa_demo_2000 <- rbind(func.rpa(demojovem_2000, rpa1, 2000, "RPA1"),
                      func.rpa(demojovem_2000, rpa2, 2000, "RPA2"),
                      func.rpa(demojovem_2000, rpa3, 2000, "RPA3"),
                      func.rpa(demojovem_2000, rpa4, 2000, "RPA4"),
                      func.rpa(demojovem_2000, rpa5, 2000, "RPA5"),
                      func.rpa(demojovem_2000, rpa6, 2000, "RPA6"))

# execute 2010
rpa_demo_2010 <- rbind(func.rpa(demojovem_2010, rpa1, 2010, "RPA1"),
                       func.rpa(demojovem_2010, rpa2, 2010, "RPA2"),
                       func.rpa(demojovem_2010, rpa3, 2010, "RPA3"),
                       func.rpa(demojovem_2010, rpa4, 2010, "RPA4"),
                       func.rpa(demojovem_2010, rpa5, 2010, "RPA5"),
                       func.rpa(demojovem_2010, rpa6, 2010, "RPA6"))

#---- juntar e salvar dados ----#
rpa_demo_data <- rbind(rpa_demo_2000, rpa_demo_2010)
rownames(rpa_demo_data) <- NULL
#write.csv(rpa_demo_data, file = "rpa_demo_data.csv")

#===============================
# VISUALIZACAO GRAFICA 
#===============================

# Tema para graficos
theme_arretado<- function (base_size = 12, base_family = "") {
  theme_minimal(base_size = base_size, base_family = base_family) %+replace% 
    theme(axis.text.x = element_text(colour= "black",size=13,hjust=.5,vjust=.5,face="plain"),
          axis.text.y = element_text(colour="black",size=13,angle=0,hjust=1,vjust=0,face="plain"), 
          axis.title.x = element_text(colour="black",size=13,angle=0,hjust=.5,vjust=0,face="plain"),
          axis.title.y = element_text(colour="black",size=13,angle=90,hjust=0.5,vjust=0.6,face="plain"),
          title = element_text(colour="black",size=10,angle=0,hjust=.5,vjust=.5,face="plain"),
          panel.grid.major = element_line(colour = grey(0.85)), 
          panel.grid.minor = element_line(colour = grey(1)),
          legend.key.size = unit(9, "mm"),
          legend.text = element_text(size = 9, hjust = 3, vjust = 3),
          legend.title = element_text(size = 9),
          axis.line = element_line(size = 1, colour = "grey70"))
}

#----------------------------------------------#
#         BAIRROS DA CIDADE DO RECIFE
#----------------------------------------------#

#==== BARRAS PROPORCAO ====#

#---- 2000 ----#
demojovem_2000 <- demojovem_2000[order(demojovem_2000$prop_jovem_total),]
demojovem_2000$localidade <- factor(demojovem_2000$localidade, levels = demojovem_2000$localidade)

bar_jovemprop00 <- ggplot(demojovem_2000, aes(x = localidade, y = prop_jovem_total))+
                          geom_bar(stat = "identity", fill = "#1c3c40") +
                          geom_label(label = demojovem_2000$prop_jovem_total, 
                                    size = 2.3, color = "black", fontface = "bold") +
                          scale_y_continuous(limits = c(0, 0.38))+
                          labs(y = "Proporção de Jovens", x = "", title = "2000") +
                          coord_flip() +
                          theme_arretado()
  
# 2010 #
demojovem_2010 <- demojovem_2010[order(demojovem_2010$prop_jovem_total),]
demojovem_2010$localidade <- factor(demojovem_2010$localidade, levels = demojovem_2010$localidade)

# Barplot 2000 
bar_jovemprop10 <- ggplot(demojovem_2010, aes(x = localidade, y = prop_jovem_total))+
                          geom_bar(stat = "identity", fill = "#1c3c40") +
                          scale_y_continuous(limits = c(0, 0.40))+
                          geom_label(label = demojovem_2010$prop_jovem_total, 
                                    size = 2.3, color = "black", fontface = "bold" ) +
                          labs(y = "Propor??o de Jovens", x = "", title = "2010") +
                          theme_arretado()+
                          coord_flip() 

#---- GRID ----#
barra_jovem_prop <- ggarrange(bar_jovemprop00, bar_jovemprop10, ncol = 2)
barra_jovem_prop

ggsave("Demografia/resultados/barra_jovem_prop.png", barra_jovem_prop, width = 9, height = 13.5, units = "in")


#=== grafico p apresentacao em slides

#--- 2000 ---#
bar_jovemprop00_x <- ggplot(demojovem_2000[c(1:5, 90:94),], aes(x = localidade, y = prop_jovem_total))+
  geom_bar(stat = "identity", fill = "#1c3c40") +
  geom_label(label = demojovem_2000[c(1:5, 90:94),]$prop_jovem_total, 
             size = 4, color = "black") +
  scale_y_continuous(limits = c(0, 0.38))+
  labs(y = "Proporção de Jovens", x = "", title = "2000") +
  coord_flip() +
  theme_arretado()
bar_jovemprop00_x

#--- 2010 ---#
bar_jovemprop10_x <- ggplot(demojovem_2010[c(1:5, 90:94),], aes(x = localidade, y = prop_jovem_total))+
  geom_bar(stat = "identity", fill = "#1c3c40") +
  scale_y_continuous(limits = c(0, 0.40))+
  geom_label(label = demojovem_2010[c(1:5, 90:94),]$prop_jovem_total, 
             size = 4, color = "black") +
  labs(y = "Proporção de Jovens", x = "", title = "2010") +
  theme_arretado()+
  coord_flip() 
bar_jovemprop10_x

# grid apresentacao
barra_jovem_propx <- ggarrange(bar_jovemprop00_x, bar_jovemprop10_x, ncol = 2)
barra_jovem_propx <- annotate_figure(barra_jovem_propx,
                                     top = text_grob("Proporção de Jovens do Total de Residentes", 
                                                      color = "black", face = "bold", size = 14))
barra_jovem_propx
ggsave("Demografia/resultados/barra_jovem_prop_APT.png", barra_jovem_propx, width = 10, height = 6, units = "in")

#===================================#
#==== BARRAS POPULACAO ABSOLUTA ====#

#---- 2000 ----#

# ordenar
demojovem_2000 <- demojovem_2000[order(demojovem_2000$pop_jovem),]
demojovem_2000$localidade <- factor(demojovem_2000$localidade, levels = demojovem_2000$localidade)

# Barplot 2000
bar_jovempop00 <- ggplot(demojovem_2000, aes(x = localidade, y = pop_jovem))+
                         geom_bar(stat = "identity", fill = "#1c3c40") +
                         labs(y = "População Abs. de Jovens", x = "", title = "2000") +
                         geom_label(label = demojovem_2000$pop_jovem,
                         size = 2.3, nudge_x = 0.25, nudge_y = 0.2, color = "black", fontface = "bold") +
                         theme_arretado()+
                         coord_flip() 

#---- 2010 ----#
demojovem_2010 <- demojovem_2010[order(demojovem_2010$pop_jovem),]
demojovem_2010$localidade <- factor(demojovem_2010$localidade, levels = demojovem_2010$localidade)

# Barplot 2010 
bar_jovempop10 <- ggplot(demojovem_2010, aes(x = localidade, y = pop_jovem))+
                         geom_bar(stat = "identity", fill = "#1c3c40") +
                         labs(y = "População Abs. de Jovens", x = "", title = "2010") +
                         geom_label(label = demojovem_2010$pop_jovem, 
                         size = 2.3, nudge_x = 0.25, nudge_y = 0.2, color = "black", fontface = "bold") +
                         theme_arretado()+
                         coord_flip()

#---- GRID ----#
barra_jovem_pop <- ggarrange(bar_jovempop00, bar_jovempop10, ncol = 2)
barra_jovem_pop
ggsave("Demografia/resultados/barra_jovem_pop.png", barra_jovem_pop, width = 9, height = 13.5, units = "in")

#--- apreesentacao pop
bar_jovemprop00_x <- ggplot(demojovem_2000[c(1:5, 90:94),], aes(x = localidade, y = pop_jovem))+
  geom_bar(stat = "identity", fill = "#1c3c40") +
  geom_label(label = demojovem_2000[c(1:5, 90:94),]$pop_jovem, 
             size = 3, color = "black") +
  labs(y = "Proporção de Jovens", x = "", title = "2000") +
  coord_flip() +
  theme_arretado()

#--- 2010 ---#
bar_jovemprop10_x <- ggplot(demojovem_2010[c(1:5, 90:94),], aes(x = localidade, y = pop_jovem))+
  geom_bar(stat = "identity", fill = "#1c3c40") +
  geom_label(label = demojovem_2010[c(1:5, 90:94),]$pop_jovem, 
             size = 3, color = "black") +
  labs(y = "Proporção de Jovens", x = "", title = "2010") +
  theme_arretado()+
  coord_flip() 

# grid apresentacao
barra_jovem_propx <- ggarrange(bar_jovemprop00_x, bar_jovemprop10_x, ncol = 2)
barra_jovem_propx <- annotate_figure(barra_jovem_propx,
                                     top = text_grob("População Absoluta de Jovens", color = "black", face = "bold", size = 14))
barra_jovem_propx
ggsave("Demografia/resultados/barra_jovem_pop_APT.png", barra_jovem_propx, width = 10, height = 6, units = "in")

#===============================#
#==== BARRAS PROPORCAO SEXO ====#

#---- 2000 ----#
data_barsex <- demojovem_2000[order(demojovem_2000$prop_mulherj_jovemt),]
data_barsex$localidade <- factor(data_barsex$localidade, levels = data_barsex$localidade)

# selecionar variaveis de interesse e manipular banco
data_barsex_fem <- data_barsex[, c("prop_mulherj_jovemt", "localidade", "Ano")]
data_barsex_masc <- data_barsex[, c("prop_homemj_jovemt", "localidade", "Ano")]

data_barsex_fem$Sexo <- "Mulheres"
data_barsex_masc$Sexo <- "Homens"

colnames(data_barsex_fem) <- c( "prop_jovem", "localidade", "Ano","Sexo")
colnames(data_barsex_masc) <- c( "prop_jovem", "localidade", "Ano","Sexo")

data_barsex <- rbind(data_barsex_fem, data_barsex_masc)

# Barplot 2000 
barra_sex_2000 <- ggplot(data_barsex, aes(x = localidade, y = prop_jovem))+
  geom_bar(stat = "identity", aes(fill = Sexo), position = "dodge") +
  labs(x = "", y = "Proporção", title = "2000") +
  scale_fill_manual("Sexo", values = c("Homens" = "#1c3c40", "Mulheres" =  "lightgreen")) +
  scale_y_continuous(limits= c(0, 0.76))+
  theme_arretado()+
  coord_flip() 

barra_sex_2000

#---- 2010 ----#
data_barsex2 <- demojovem_2010[order(demojovem_2010$prop_mulherj_jovemt),]
data_barsex2$localidade <- factor(data_barsex2$localidade, levels = data_barsex2$localidade)

# selecionar variaveis de interesse e manipular banco
data_barsex_fem2 <- data_barsex2[, c("prop_mulherj_jovemt", "localidade", "Ano")]
data_barsex_masc2 <- data_barsex2[, c("prop_homemj_jovemt", "localidade", "Ano")]

data_barsex_fem2$Sexo <- "Mulheres"
data_barsex_masc2$Sexo <- "Homens"

colnames(data_barsex_fem2) <- c( "prop_jovem", "localidade", "Ano","Sexo")
colnames(data_barsex_masc2) <- c( "prop_jovem", "localidade", "Ano","Sexo")

data_barsex2 <- rbind(data_barsex_fem2, data_barsex_masc2)

# Barplot 2010 
barra_sex_2010 <- ggplot(data_barsex2, aes(x = localidade, y = prop_jovem))+
  geom_bar(stat = "identity", aes(fill = Sexo), position = "dodge") +
  labs(x = "", y = "Proporção", title = "2010") +
  scale_fill_manual("Sexo", values = c("Homens" = "#1c3c40", "Mulheres" =  "lightgreen")) +
  scale_y_continuous(limits= c(0, 0.76))+
  theme_arretado()+
  coord_flip() 
barra_sex_2010

#---- arrange plots -----#
barra_sex <- ggarrange(barra_sex_2000, barra_sex_2010, ncol = 2, common.legend = T, legend = "right")
barra_sex
ggsave("Demografia/resultados/barra_sex.png", barra_sex, width = 9, height = 13.5, units = "in")

#===== Graficos APT =====#

# Barplot 2000 apt
barra_sex_2000_x <- ggplot(data_barsex[c(1:5, 90:99, 184:188),], aes(x = localidade, y = prop_jovem))+
  geom_bar(stat = "identity", aes(fill = Sexo), position = "dodge") +
  labs(x = "", y = "Proporção", title = "2000") +
 # geom_label(label = data_barsex[c(1:5, 90:99, 184:188),]$prop_jovem, size = 2.6, color = "black") +
  scale_fill_manual("Sexo", values = c("Homens" = "#1c3c40", "Mulheres" =  "lightgreen")) +
  theme_arretado()+
  coord_flip() 
barra_sex_2000_x

barra_sex_2010_x <- ggplot(data_barsex2[c(1:5, 90:99, 184:188),], aes(x = localidade, y = prop_jovem))+
  geom_bar(stat = "identity", aes(fill = Sexo), position = "dodge") +
  labs(x = "", y = "Proporção", title = "2010") +
  #geom_label(label = data_barsex2[c(1:5, 90:99, 184:188),]$prop_jovem, size = 2.6, color = "black") +
  scale_fill_manual("Sexo", values = c("Homens" = "#1c3c40", "Mulheres" =  "lightgreen")) +
  theme_arretado()+
  coord_flip() 
barra_sex_2010_x

#---- combinar graficos -----#
barra_sex <- ggarrange(barra_sex_2000_x, barra_sex_2010_x, ncol = 2, common.legend = T, legend = "right")
ggsave("Demografia/resultados/barra_sex_APT.png", barra_sex, width = 14, height = 6, units = "in")

#=============================================#
#                   MAPAS                     #
#=============================================#

# importar base cartografica
shape_recife <- shapefile("Dados Gerais/bases_cartograficas/Bairros.shp")

# ** Abrir arquivo 'funcoes_gerais.R" para executar mapas

#======= Pop. Absoluta de Jovens =======#

#----- 2000 -----#
map_popjovem_2000 <- mapa.funcao(shape_recife, demojovem_2000, demojovem_2000$pop_jovem, "2000" , "Pop. de Jovens")

#----- 2010 -----#
map_popjovem_2010 <- mapa.funcao(shape_recife, demojovem_2010, demojovem_2010$pop_jovem, "2010", "Pop. de Jovens")

#---- combinar graficos -----#
map_popjovem <- ggarrange(map_popjovem_2000, map_popjovem_2010, ncol = 2, common.legend = T, legend = "bottom")
map_popjovem
ggsave("Demografia/resultados/map_popjovem.png", map_popjovem, width = 14, height = 8, units = "in")

#======= Prop. de Jovens do Total de Residentes =======#

#----- 2000 -----#
map_propjovem_2000 <- mapa.funcao(shape_recife, demojovem_2000, demojovem_2000$prop_jovem_total, "Prop. de Jovens")

#----- 2010 -----#
map_propjovem_2010 <- mapa.funcao(shape_recife, demojovem_2010, demojovem_2010$prop_jovem, "Prop. de Jovens")

#---- combinar graficos -----#
map_propjovem <- ggarrange(map_propjovem_2000, map_propjovem_2010, ncol = 2, common.legend = T, legend = "bottom")
ggsave("Demografia/resultados/map_propjovem.png", map_propjovem, width = 14, height = 8, units = "in")

#======== Diferenca da pop de jovens por sexo ========#

#----- 2000 -----#
demojovem_2000$dif_jovem_sexo <-  demojovem_2000$pop_mulher_jovem - demojovem_2000$pop_homem_jovem
map_difsex_2000 <- mapa.funcao(shape_recife, demojovem_2000, demojovem_2000$dif_jovem_sexo, "Diferença")

#----- 2010 -----#
demojovem_2010$dif_jovem_sexo <-  demojovem_2010$pop_mulher_jovem - demojovem_2010$pop_homem_jovem
map_difsex_2010 <- mapa.funcao(shape_recife, demojovem_2010, demojovem_2010$dif_jovem_sexo, "Diferença")

#---- combinar graficos -----#
map_propjovem <- ggarrange(map_difsex_2000, map_difsex_2010, ncol = 2, common.legend = T, legend = "bottom")
ggsave("Demografia/resultados/map_jovem_difsexo.png", map_propjovem, width = 14, height = 8, units = "in")

#===============================================#
#                   RPAS
#==============================================#

#==============================#
# Comparacao temporal

#====== Prop. de Jovens ======#
# selecionar casos e variaveis
dataprop_rpa00 <- rpa_demo_2000[rpa_demo_2000$Ano == 2000,c("localidade", "Ano", "prop_jovem_total")]
dataprop_rpa10 <- rpa_demo_2010[rpa_demo_2010$Ano == 2010,c("localidade", "Ano", "prop_jovem_total")]

dataline_rpa00 <- data.frame(localidade = c("RPA 1", "RPA 5", "Demais RPAs"), Ano = rep(2000,3), prop_jovem_total = rep(1,3))
dataline_rpa10 <- data.frame(localidade = c("RPA 1", "RPA 5", "Demais RPAs"), Ano = rep(2010,3), prop_jovem_total = rep(1,3))

dataline_rpa00$prop_jovem_total[1] <- dataprop_rpa00$prop_jovem_total[1]
dataline_rpa10$prop_jovem_total[1] <- dataprop_rpa10$prop_jovem_total[1]
dataline_rpa00$prop_jovem_total[2] <- dataprop_rpa00$prop_jovem_total[5]
dataline_rpa10$prop_jovem_total[2] <- dataprop_rpa10$prop_jovem_total[5]
dataline_rpa00$prop_jovem_total[3] <- (sum(dataprop_rpa00$prop_jovem_total[c(2:4, 6)]) /4)
dataline_rpa10$prop_jovem_total[3] <- (sum(dataprop_rpa10$prop_jovem_total[c(2:4, 6)]) /4)

dataline_rpa <- rbind(dataline_rpa00, dataline_rpa10)
dataline_rpa$prop_jovem_total <- round(dataline_rpa$prop_jovem_total, 3)

# grafico
ggplot(data = dataline_rpa, aes(x = Ano, y = prop_jovem_total, group = localidade, color = localidade)) + 
  geom_line(aes(linetype=localidade), size=0.8) +
  scale_linetype_manual(values = c(2,1,3)) +
  scale_color_manual(values = c("#325c6c", "#400001", "darkgreen"))+
  theme_arretado()+
  scale_y_continuous(limits= c(0.2, 0.35))+
  scale_x_continuous(breaks = c(2000, 2010))+
  geom_text_repel(aes(x = Ano, y = prop_jovem_total, label = dataline_rpa$prop_jovem_total),
                  size = 3.5, colour = "black", fontface = "bold")+
  labs(x = "", y = "Proporção de Jovens")
ggsave("Demografia/resultados/RPA_propjovens_anos.png", width = 9, height = 3.5, units = "in")

#====== Pop. de Jovens ======#

# selecionar casos e vars
datapop_rpa00 <- rpa_demo_2000[rpa_demo_2000$Ano == 2000,c("localidade", "Ano", "pop_jovem")]
datapop_rpa10 <- rpa_demo_2010[rpa_demo_2010$Ano == 2010,c("localidade", "Ano", "pop_jovem")]

# criar banco
dataline_rpa00 <- data.frame(localidade = c("RPAs 1, 2 e 3", "RPAs 4 e 5", "RPA 6"), Ano = rep(2000,3), pop_jovem = rep(1,3))
dataline_rpa10 <- data.frame(localidade =c("RPAs 1, 2 e 3", "RPAs 4 e 5", "RPA 6"), Ano = rep(2010,3), pop_jovem = rep(1,3))

dataline_rpa00$pop_jovem[1] <- sum(datapop_rpa00$pop_jovem[c(1:3)]) 
dataline_rpa10$pop_jovem[1] <- sum(datapop_rpa10$pop_jovem[c(1:3)]) 
dataline_rpa00$pop_jovem[2] <- sum(datapop_rpa00$pop_jovem[c(4:5)]) 
dataline_rpa10$pop_jovem[2] <- sum(datapop_rpa10$pop_jovem[c(4:5)]) 
dataline_rpa00$pop_jovem[3] <- datapop_rpa00$pop_jovem[6]
dataline_rpa10$pop_jovem[3] <- datapop_rpa10$pop_jovem[6]

dataline_rpa <- rbind(dataline_rpa00, dataline_rpa10)

# grafico
ggplot(data = dataline_rpa, aes(x = Ano, y = pop_jovem, group = localidade, color = localidade)) + 
  geom_line(aes(linetype=localidade), size=1) +
  scale_linetype_manual(values = c(1,2,3)) +
  scale_color_manual(values = c("#325c6c", "#400001", "darkgreen"))+
  theme_arretado()+
  scale_x_continuous(breaks = c(2000, 2010))+
  geom_text(aes(x = dataline_rpa$Ano, y = dataline_rpa$pop_jovem, label = dataline_rpa$pop_jovem, vjust = 0.1, hjust = 0.45),
                  size = 4, colour = "black", fontface = "bold")+
  labs(x = "", y = "População de Jovens")
ggsave("RPA_popjovens_anos.png", width = 9, height = 3.5, units = "in")

#=====================#
# BARRAS

# select variable if interest and recode
rpa_demo_2000men <- rpa_demo_2000[, c("prop_jovemmen_jovem", "localidade", "Ano")]
rpa_demo_2000women <- rpa_demo_2000[, c("prop_jovemwom_jovem", "localidade", "Ano")]

rpa_demo_2000men$Sexo <- "Homem"
rpa_demo_2000women$Sexo <- "Mulher"

colnames(rpa_demo_2000men) <- c( "prop_jovem", "localidade", "Ano","Sexo")
colnames(rpa_demo_2000women) <- c( "prop_jovem", "localidade", "Ano","Sexo")

bar_rpasex00 <- rbind(rpa_demo_2000men, rpa_demo_2000women)

# round data
bar_rpasex00$prop_jovem <- round(bar_rpasex00$prop_jovem, 3)

# Barplot 2000 
plotsex1 <- ggplot(bar_rpasex00, aes(x = localidade, y = prop_jovem, fill = Sexo))+
  geom_bar(stat = "identity",position = "dodge") +
  scale_fill_manual("Sexo", values = c("Homem" = "#1c3c40", "Mulher" =  "lightgreen")) +
  geom_text(aes(label = bar_rpasex00$prop_jovem), 
            position=position_dodge(width=0.9), vjust=-0.20, hjust =0.02, size = 3.3, angle=70)+
  scale_y_continuous(limits= c(0 ,0.6))+
  theme_arretado()+
  labs(x = "", y = "Proporção de Jovens", title = '2000')
plotsex1
  

#-------------#
# 2010

# select variable if interest and recode
rpa_demo_2010men <- rpa_demo_2010[, c("prop_jovemmen_jovem", "localidade", "Ano")]
rpa_demo_2010women <- rpa_demo_2010[, c("prop_jovemwom_jovem", "localidade", "Ano")]

rpa_demo_2010men$Sexo <- "Homem"
rpa_demo_2010women$Sexo <- "Mulher"

colnames(rpa_demo_2010men) <- c( "prop_jovem", "localidade", "Ano","Sexo")
colnames(rpa_demo_2010women) <- c( "prop_jovem", "localidade", "Ano","Sexo")

bar_rpasex10 <- rbind(rpa_demo_2010men, rpa_demo_2010women)

# round data
bar_rpasex10$prop_jovem <- round(bar_rpasex10$prop_jovem, 3)

# Barplot 2010
plotsex2 <- ggplot(bar_rpasex10, aes(x = localidade, y = prop_jovem, fill = Sexo))+
  geom_bar(stat = "identity",position = "dodge") +
  scale_fill_manual("Sexo", values = c("Homem" = "#1c3c40", "Mulher" =  "lightgreen")) +
  geom_text(aes(label = bar_rpasex10$prop_jovem), 
            position=position_dodge(width=0.9), vjust=-0.20, hjust =0.02, size = 3.3, angle=70)+
  scale_y_continuous(limits= c(0 ,0.6))+
  theme_arretado()+
  labs(x = "", y = "Propor??o de Jovens", title = "2010")

plotsex2

# grid arrange

grid_prop_rpa <- ggarrange(plotsex1, plotsex2, ncol = 2,  
                   common.legend = T, legend = "bottom")
grid_prop_rpa
ggsave("grid_prop_rpa.png", grid_prop_rpa, width = 7.13, height = 4.51, units = "in")

#----------------------------#
# RPA POPULATION

pop_rpa00 <- rpa_demo_2000[order(rpa_demo_2000$pop_jovemmen),]
pop_rpa00$localidade <- factor(pop_rpa00$localidade, levels = pop_rpa00$localidade)

pop_rpa00$pop_jovemwomen / pop_rpa00$pop_jovemmen 

# select variable if interest and recode
pop_rpa00men <- pop_rpa00[, c("pop_jovemmen", "localidade", "Ano")]
pop_rpa00women <- pop_rpa00[, c("pop_jovemwomen", "localidade", "Ano")]

pop_rpa00men$Sexo <- "Homem"
pop_rpa00women$Sexo <- "Mulher"

colnames(pop_rpa00men) <- c( "pop_jovem", "localidade", "Ano","Sexo")
colnames(pop_rpa00women) <- c( "pop_jovem", "localidade", "Ano","Sexo")

pop_rpasex00 <- rbind(pop_rpa00men, pop_rpa00women)

# round data
pop_rpasex00$pop_jovem <- round(pop_rpasex00$pop_jovem, 0)

# Barplot 2000 
plotpopsex1 <- ggplot(pop_rpasex00, aes(x = localidade, y = pop_jovem, fill = Sexo))+
  geom_bar(stat = "identity",position = "dodge") +
  scale_fill_manual("Sexo", values = c("Homem" = "#1c3c40", "Mulher" =  "lightgreen")) +
  scale_y_continuous(limits= c(0 ,60000))+
  geom_text(aes(label = pop_rpasex00$pop_jovem), 
            position=position_dodge(width=0.9), vjust=-0.20, hjust =0.02, size = 3.3, angle=70)+
  theme_arretado()+
  labs(x = "", y = "Popula??o de Jovens", title = '2000')
plotpopsex1


#-------------#
# 2010

pop_rpa10 <- rpa_demo_2010[order(rpa_demo_2010$pop_jovem),]
pop_rpa10$localidade <- factor(pop_rpa10$localidade, levels = pop_rpa10$localidade)


# select variable if interest and recode
pop_rpa10men <- pop_rpa10[, c("pop_jovemmen", "localidade", "Ano")]
pop_rpa10women <- pop_rpa10[, c("pop_jovemwomen", "localidade", "Ano")]

pop_rpa10men$Sexo <- "Homem"
pop_rpa10women$Sexo <- "Mulher"

colnames(pop_rpa10men) <- c( "pop_jovem", "localidade", "Ano","Sexo")
colnames(pop_rpa10women) <- c( "pop_jovem", "localidade", "Ano","Sexo")

pop_rpasex10 <- rbind(pop_rpa10men, pop_rpa10women)

# Barplot 2010
plotpopsex2 <- ggplot(pop_rpasex10, aes(x = localidade, y = pop_jovem, fill = Sexo))+
  geom_bar(stat = "identity",position = "dodge") +
  scale_fill_manual("Sexo", values = c("Homem" = "#1c3c40", "Mulher" =  "lightgreen")) +
  geom_text(aes(label = pop_rpasex10$pop_jovem), 
            position=position_dodge(width=0.9), vjust=-0.20, hjust =0.02, size = 3.3, angle=70)+
  scale_y_continuous(limits= c(0 ,60000))+
  theme_arretado()+
  labs(x = "", y = "Popula??o de Jovens", title = "2010")

plotpopsex2

# grid arrange
grid_poprpa <- ggarrange(plotpopsex1, plotpopsex2, ncol = 2,  
                   common.legend = T, legend = "bottom")
