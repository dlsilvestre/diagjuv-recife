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
library(purrr); library(OpenStreetMap);
library(sp)
library(maps)
library(ggmap)
library(maptools)

# carregar banco CVLI 2013-2017
data_cvli <-  read_excel("Violência/dados/Rel - 1015 - CVLI - logradouros, bairro, gênero, cor da pele, idade, mês - RECIFE - Jan2013 a Nov2017.xlsx", 
                         col_types = c("text", "text", "text", 
                                       "text", "text", "text", "text", "date", 
                                       "text", "text", "text", "text"))

#===============================#
# Manipular base

# remover linhas
data_cvli <- data_cvli[-c(1:5, 2932:2942 ),]

# primeira linha do banco p colnames
names <- c(data_cvli[1,])

# retirar primeira linha
data_cvli <-  data_cvli[-1,]

# renomear colunas adequadamente
colnames(data_cvli) <- str_replace(names, "NA", "DATA")


#=========================#
# CVLI por faixa etaria 
#=========================#

ggplot(data = data_cvli)+
  geom_bar(aes(x = as.numeric(data_cvli$IDADE)), fill = "#333333")+
  geom_vline(xintercept = 15, size = 1, colour = "#FF3721",linetype = "dashed")+
  geom_vline(xintercept = 29, size = 1, colour = "#FF3721", linetype = "dashed")+
  labs(x = "Idade", y = "Frequênica de CVLI")
  

# salvar grafico
ggsave("mortes_por_idade.png", path = "Violência/resultados",
       width = 8, height = 5, units = "in")

#=======================#
# CVLI por ano 
#=======================#

#total
ano_data_cvli <- data.frame(table(data_cvli$ANO))

# jovem
data_cvli$IDADE <- as.numeric(data_cvli$IDADE)
jv_ano_data_cvli <- data_cvli[data_cvli$IDADE >= 15 & data_cvli$IDADE < 30 ,]
jv_ano_data_cvli <- data.frame(table(jv_ano_data_cvli$ANO))

# juntar bases
cvli_data1 <- data.frame(ano_data_cvli, jv_ano_data_cvli[,2])
colnames(cvli_data1) <- c("Ano", "mortesTotais", "mortesJovens")

# total -jovem
cvli_data1 <- mutate(cvli_data1, mortesTotais_jovens = mortesTotais - mortesJovens)

#---- manipular e mergir bases ----#
x1 <- data.frame(cvli_data1[,c(1:2)], grupo = "Mortes Totais")
x2 <- data.frame(cvli_data1[,c(1,3)], grupo = "Mortes de Jovens")
x3 <- data.frame(cvli_data1[,c(1,4)], grupo = "Mortes de Não-jovens")

colnames(x1)[2] <- c("Mortes")
colnames(x2)[2] <- c("Mortes")
colnames(x3)[2] <- c("Mortes")

cvli_data2 <- rbind(x2, x3)

# gráfico
ggplot(data = cvli_data2) +
  geom_line(aes(x = Ano, y = Mortes, group = grupo, color = grupo), size = 1) + 
  geom_label(aes(x = Ano, y = Mortes, label = Mortes))+
  scale_color_manual(values=c("#7f0000", "#E69F00"))+
  scale_y_continuous(limits = c(0,800))+
  theme(legend.position="bottom")

# salvar grafico
ggsave("mortes_total_jovens_porano.png", path = "Violência/resultados",
       width = 12, height = 8, units = "in")


# POR MES
#mes <- data.frame(table(data_cvli$ANO,data_cvli$MÊS))
#mes <- mes[order(mes$Var1),]


#=========================================#
# CVLI por bairro - total/jovem/total-jovem
#=========================================#

# contagem de mortes por bairro
jovem_morte_bairro <- data.frame(table(jv_ano_data_cvli$BAIRRO))

# carregar shapefile 1 (completo)
shp_recife1 <- shapefile("Dados Gerais/bases_cartograficas/Bairros.shp")

# criar variavel localidade como chr
jovem_morte_bairro$localidade <- as.character(jovem_morte_bairro$Var1)

#==== ABRIR FUNCOES GERAIS E EXECUTAR MAPA ====#

mapa.funcao(shape = shp_recife1, data = jovem_morte_bairro,
            variable = jovem_morte_bairro$Freq, legendtitle = "CVLI de Jovens \n   (2013-2017)",
            pallete = "A")
ggsave("mortes_jovens_bairro_A.png", path = "Violência/resultados",width = 9, height = 12, units = "in")

#==================================================================#
# CVLI por LOGRADOURO [EXECUTAR EM MAQUINA COM BOM PROCESSAMENTO] 
#================================================================#
# checar merge dos bancos: perda de casos

#----------------------#
# manipular dados

# cvli de jovens
jovem_cvli <- data_cvli[data_cvli$IDADE >= 15 & data_cvli$IDADE < 30 ,]

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
  ggsave("CVLI_jovens_logradouroA.png", path = "Violência/resultados",width = 14, height = 17, units = "in")

# baixar o mapa de Recife
mapImage <-get_map(c(lon =  -34.91, lat =-8.045), zoom = 12)

#====== MAPA RECIFE + CVLI/LOGRADOUROS ======#
ggmap(mapImage, extent = "normal", maprange = FALSE)+ 
  geom_line(data = mapa.df, aes(long, lat, group = group, color = Freq))+
  scale_color_viridis(name= "CVLI", option= "A", direction = -1) 
  ggsave("CVLI_jovens_logradouro1.png", path = "Violência/resultados",width = 14, height = 17, units = "in")











