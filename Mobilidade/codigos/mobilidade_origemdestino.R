#====================================================#
# DIAGNÓSTICO DA JUVENTUDE NA CIDADE DO RECIFE       #              
#====================================================#
# MOBILIDADE URBANA - Pesquisa Origem-Destino        #
#----------------------------------------------------#
# Secretaria Executiva de Juventude                  #
#----------------------------------------------------#
# Recife 2017                                        #
#----------------------------------------------------#
# Claudio A. Monteiro                                #
# claudiomonteiro@gmail.com                          #
#----------------------------------------------------#
# Qualquer duvida contate o desenvolvedor            #
# #UseSoftwareLivre                                  #
#----------------------------------------------------#


#============================================#
# CARREGAR INFORMACOES DE BASE
#============================================#

# instalar pacotes necessarios
# install.packages(c("readr","plyr", "rgdal", "ggplot2", "ggmap", "maps", "mapdata", "raster"), dependencies = T )

# carregar pacotes
library(readr); library(plyr); library(plyr); library(rgdal); library(ggplot2); 
library(ggmap); library(maps); library(mapdata); library(raster)

# tema ggplot2 para graficos
 tema_massa <- function (base_size = 12, base_family = "") {
   theme_minimal(base_size = base_size, base_family = base_family) %+replace% 
     theme(axis.text.x = element_text(colour= "black",size=11,hjust=.5,vjust=.5,face="plain"),
           axis.text.y = element_text(colour="black",size=11,angle=0,hjust=1,vjust=0,face="plain"), 
           axis.title.x = element_text(colour="black",size=12,angle=0,hjust=.5,vjust=0,face="plain"),
           axis.title.y = element_text(colour="black",size=12,angle=90,hjust=0.5,vjust=0.6,face="plain"),
           title = element_text(colour="black",size=14,angle=0,hjust=.5,vjust=.5,face="bold"),
           panel.grid.major = element_line(colour = grey(0.85)), 
           panel.grid.minor = element_line(colour = grey(1)),
           legend.key.size = unit(9, "mm"),
           legend.text = element_text(size = 9, hjust = 3, vjust = 3),
           legend.title = element_text(size = 9),
           axis.line = element_line(size = 1, colour = "grey70"))
 }
 
# carregar shapefile 1 (completo)
 shp_recife1 <- shapefile("Dados Gerais/bases_cartograficas/Bairros.shp")
 
 
 # carregar shapefile 2 (coord google, sem cabanga)
 shp_recife2 <- shapefile("")

# carregar base de dados
origem_dest <- read_delim("Mobilidade/dados/pesquisaodrecife2016.csv", ";", escape_double = FALSE, trim_ws = TRUE)

# baixar base cartografica do google
gg_recife <- get_map(location = c(lon = -34.946964, lat = -8.027562 ),  zoom = 11, maptype = 'roadmap')


#==========================================#
# PROCESSAMENTO E VISUALIZACAO DE DADOS
#==========================================#

# Plot ggmap
ggmap(gg_recife)+ 
  geom_polygon(data = shp_recife2, aes(x = long, y = lat, group = group), fill=NA , color = grey(0.2))+ 
  coord_fixed() +
  theme_minimal()

#----- de onde vem os recifense estudantes da UFPE
ufpe_dest <- origem_dest[origem_dest$nome_instituicao_ensino == 3,]

ufpe <- c("UNIVERSIDADE FEDERAL ADE PERNAMBUCO", "UNIVERSIDADE FEDERAL D PERNAMBUCO" , 
          "UNIVERSIDADE FEDERAL DE EPERNAMBUCO",                                                                              
          "UNIVERSIDADE FEDERAL DE FERNAMBUCO",                                                                               
          "UNIVERSIDADE FEDERAL DE PARNAMBUCO",                                                                               
          "UNIVERSIDADE FEDERAL DE PE",
          "UNIVERSIDADE FEDERAL DE PERAMBUCO",                                                                                
          "UNIVERSIDADE FEDERAL DE PERANAMBUCO",                                                                              
          "UNIVERSIDADE FEDERAL DE PERNABUCO",                                                                                
          "UNIVERSIDADE FEDERAL DE PERNAMBCO",                                                                                
          "UNIVERSIDADE FEDERAL DE PERNAMBUC",                                                                                
          "UNIVERSIDADE FEDERAL DE PERNAMBUCO", 
          "UNIVERSIDADE FEDERAL DE PERNAMBUCO -UFPE",                                                                         
          "UNIVERSIDADE FEDERAL DE PERNAMBUCO (UFPE)",
          "UNIVERSIDADE FEDERAL DE PERNAMBUCO - UFPE",
          "UNIVERSIDADE FEDERAL DE PERNAMBUCO08",                                                                             
          "UNIVERSIDADE FEDERAL DE PERNAMUBUCO",                                                                              
          "UNIVERSIDADE FEDERAL DE PERNANBUCO",                                                                               
          "UNIVERSIDADE FEDERAL DE PERNMABUCO",                                                                               
          "UNIVERSIDADE FEDERAL DE PRNAMBUCO",
          "UNIVERSIDADE FEDEREAL DE PERNAMBUCO",                                                                              
          "UNIVERSIDADE FEDRAL RURAL DE PERNAMBUCO",                                                                          
          "UNIVERSIDADE FEDRRAL DE PERNAMBUCO",                                                                               
          "UNIVERSIDADE FERDERAL DE PERNAMBUCO",                                                                              
          "UNIVERSIDADE FEREDAL DE PERNAMBUCO",                                                                               
          "UNIVERSIDADE FERERAL DE PERNAMBUCO",
          "UFPE",                                                                                                             
          "UFPE _ UNIVERSIDADE FEDERAL DE PERNAMBUCO",
          "UFPE - CAMPUS RECIFE",
          "UFPE - CENTRO DE INFORMATICA",
          "UFPE - UNIVERSIDADE FEDERAL DE PARMAMBUCO",                                                                        
          "UFPE - UNIVERSIDADE FEDERAL DE PERNAMBUCO",                                                                        
          "UFPE -CCJ",
          "UFPE / UNINASSAU",
          "UFPE E FG",                                                                                                        
          "UFPE E IFPE",                                                                                                      
          "UFPE E UNICAP",                                                                                                    
          "UFPE FDR",                                                                                                         
          "UFPE RECIFE",
          "UFPE UNIVERSIDADE FEDERAL DE PERNAMBUCO",
          "UFPE-CIN",
          "UFPE-MDU",                                                                                                         
          "UFPE-MESTRADO",                                                                                                    
          "UFPE/CAA",                                                                                                         
          "UFPE/CAV",                                                                                                         
          "UFPE/LIKA"
)

ufpe_dest<- origem_dest[origem_dest$nome_instituicao_ensino %in% ufpe, ] 
levels(as.factor(ufpe_dest$cidade_residencia))

#--------------------------------#
#==== cidades origem da ufpe ====#

ufpe_cidade <- data.frame(table(ufpe_dest$cidade_residencia))
ufpe_cidade <- ufpe_cidade[order(ufpe_cidade$Freq),]

# em proporcao
sum(ufpe_cidade$Freq)
ufpe_cidade <- mutate(ufpe_cidade, prop_cidade = Freq / sum(ufpe_cidade$Freq))
ufpe_cidade$prop_cidade <- round(ufpe_cidade$prop_cidade, 4)
ufpe_cidade$Var1 <- factor(ufpe_cidade$Var1, levels = ufpe_cidade$Var1)
ufpe_cidadeplot <- ufpe_cidade[32:46,]

# grafico barra
bar_ufpe_cid <- ggplot(ufpe_cidadeplot, aes(x = Var1, y = prop_cidade))+
  geom_bar(stat = "identity", fill = "#1c3c40") +
  geom_label(label = ufpe_cidadeplot$prop_cidade, 
             size = 4, color = "black", fontface = "bold") +
  labs(y = "Proporção do Total da Amostra", x = "", title = "Cidades Origem da UFPE") +
  coord_flip() +
  theme_massa()
bar_ufpe_cid

# salvar grafico
ggsave("bar_ufpe_cid.png", bar_ufpe_cid, width = 12, height = 6, units = "in")

#-------------------------------------------#
#==== cidades nao-recife origem da ufpe ====#

ufpe_cidfora <- data.frame(table(ufpe_dest$cidade_residencia))
ufpe_cidfora <- ufpe_cidfora[order(ufpe_cidfora$Freq),]
ufpe_cidfora <- ufpe_cidfora[ !ufpe_cidfora$Var1 == "RECIFE",]

# em proporcao
sum(ufpe_cidfora$Freq)
ufpe_cidfora <- mutate(ufpe_cidfora, prop_cidade = Freq / sum(ufpe_cidfora$Freq))
ufpe_cidfora$prop_cidade <- round(ufpe_cidfora$prop_cidade, 4)

ufpe_cidfora$Var1 <- factor(ufpe_cidfora$Var1, levels = ufpe_cidfora$Var1)
ufpe_cidfora <- ufpe_cidfora[21:45,]

# grafico barra
bar_ufpe_fora <- ggplot(ufpe_cidfora, aes(x = Var1, y = prop_cidade))+
  geom_bar(stat = "identity", fill = "#1c3c40") +
  geom_label(label = ufpe_cidfora$prop_cidade, 
             size = 4, color = "black", fontface = "bold") +
  labs(y = "Proporção da Amostra Sem Recife", x = "", title = "Cidades Origem da UFPE Fora de Recife") +
  coord_flip() +
  theme_massa()
bar_ufpe_fora

# salvar grafico
ggsave("bar_ufpe_fora.png", bar_ufpe_fora, width = 12, height = 6, units = "in")

#================================#
#==== bairros origem da UFPE ====#

library(readxl)
latlong_recife_bairros <- read_excel("~/Documents/latlong_recife_bairros.xls")

ufpe_bairros <- ufpe_dest[ufpe_dest$cidade_residencia == "RECIFE",]
ufpe_bairros <- data.frame(table(ufpe_bairros$bairro_residencia))
ufpe_bairros <- ufpe_bairros[order(ufpe_bairros$Freq),]

# em proporcao
sum(ufpe_bairros$Freq)
ufpe_bairros <- mutate(ufpe_bairros, prop_bairro = Freq / sum(ufpe_bairros$Freq))

library(stringi)
library(magrittr)
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

ufpe_bairros$Var1 <- as.character(ufpe_bairros$Var1)
latlong_recife_bairros$bairro <- toupper(latlong_recife_bairros$bairro)
latlong_recife_bairros$bairro = stri_trans_general(latlong_recife_bairros$bairro , "Latin-ASCII")
ufpe_bairros$bairro = best_match(ufpe_bairros$Var1, latlong_recife_bairros$bairro)

# merge data with lat long recife
ufpe_latlon <- merge(ufpe_bairros, latlong_recife_bairros, by = "bairro")
ufpe_latlon <- ufpe_latlon[-87,]

# merge data with shapefile@data
shp_recife3 <- merge(shp_recife2, ufpe_latlon, by = "bairro_n", all = T)

mapa.funcao(shp_recife3, ufpe_bairros2, ufpe_bairros2$Freq)

#===================================#
#==== DESTINO DOS JOVENS NA RMR ====#

#==== trabalho ====#
jovem_dest <- origem_dest[origem_dest$faixa_etaria == 3,]

# contar bairro destinos dos jovens TRABALHO
table_jovemtrab <- data.frame(table(jovem_dest$bairro_trabalho))
table_jovemtrab <- table_jovemtrab[order(table_jovemtrab$Freq),]
table_jovemtrab$Var1 <- factor(table_jovemtrab$Var1, levels = table_jovemtrab$Var1)
table_jovemtrab <- table_jovemtrab[c(143:162),]

#--- grafico ---#
plot_jovemtrab <- ggplot(table_jovemtrab, aes(x =table_jovemtrab$Var1, y = table_jovemtrab$Freq))+
  geom_bar(stat = "identity", fill = "#15041c") +
  labs(y = "Quantidade de Jovens", x = "", title = "Onde Trabalham os Jovens da RMR?") +
  annotate("text", x = 4, y = 300, label = "Fonte de Dados: Intituto Pelópidas")+
  annotate("text", x = 3, y = 300, label = "Pesquisa Origem-Destino (2016)", size = 3.3) +
  #annotate("text", x = 1, y = 300, label = "ps: VERIFICAR DADO DE RECIFE COMO UM BAIRRO", size = 2.3) +
  coord_flip() +
  tema_massa()
plot_jovemtrab

setwd("C:/Users/Monteiro-DataPC/Documents/GitProjects/diagjuv-recife/Resultados")
ggsave("bar_trabjovem_bairro.png", plot_jovemtrab, width = 10, height = 7, units = "in")

#==== estudo ====#

# contar bairro destinos dos jovens TRABALHO
table_jovemest <- data.frame(table(jovem_dest$bairro_escola))
table_jovemest <- table_jovemest[order(table_jovemest$Freq),]
table_jovemest$Var1 <- factor(table_jovemest$Var1, levels = table_jovemest$Var1)
table_jovemest <- table_jovemest[c(143:162),]

#--- grafico ---#
plot_jovemest <- ggplot(table_jovemest, aes(x =table_jovemest$Var1, y = table_jovemest$Freq))+
  geom_bar(stat = "identity", fill = "#15041c") +
  labs(y = "Quantidade de Jovens", x = "", title = "Onde Estudam os Jovens da RMR?") +
  annotate("text", x = 4, y = 70, label = "Fonte de Dados: Intituto Pelópidas")+
  annotate("text", x = 3, y = 70, label = "Pesquisa Origem-Destino (2016)", size = 3.3) +
 # annotate("text", x = 1, y = 70, label = "ps: VERIFICAR DADO DE RECIFE COMO UM BAIRRO", size = 2.3) +
  coord_flip() +
  tema_massa()
plot_jovemest

setwd("C:/Users/Monteiro-DataPC/Documents/GitProjects/diagjuv-recife/Resultados")
ggsave("bar_estjovem_bairro.png", plot_jovemest, width = 10, height = 7, units = "in")

#==== Linhas de Conexao no Mapa ====#

# carregar shapefile
shp_recife <- shapefile(file.choose())

# tranformar shape em data
recife_fort <- fortify(shp_recife)

# visualizar shape Recife
ggplot(data= recife_fort) +
  geom_polygon(aes(x = recife_fort$long, y = recife_fort$lat, group = group), 
               fill = "white",
               color = "grey60")+
  coord_fixed()+
  theme_nothing()

# origem-destino ufpe
ufpe_latlon$latcdu <- -8.052600
ufpe_latlon$longcdu <- -34.95027

# mudar nome da varivael 
ufpe_latlon$Intensidade <- ufpe_latlon$prop_bairro 

#----- origem-destino ufpe SHAPEMAP -----#
shp_ufpe_fluxo <- ggplot() + 
  geom_polygon(data= recife_fort, aes(long,lat, group=group),  fill = "white", color = "grey60") +
  geom_segment(data = ufpe_latlon, aes(x = long, y = lat, xend = longcdu, yend = latcdu, color= Intensidade)) +
  scale_color_gradient(low="lightgreen", high= "darkblue")+
  coord_equal()+
  theme_nothing()
shp_ufpe_fluxo

ggsave("shp_ufpe_fluxo.png", shp_ufpe_fluxo, width = 8, height = 8, units = "in")

#----- Origem Destino ufpe GGMAP -----#
gg_recife <- get_map(location = c(lon= -34.915573, lat =  -8.046748), zoom = 12, maptype = 'roadmap')

gg_ufpe_fluxo <- ggmap(gg_recife)+
  geom_curve(data = ufpe_latlon, aes(x = long, y = lat, xend = longcdu, yend = latcdu, color= Intensidade),
               curvature = -0.1, size = 0.75) +
  scale_color_gradient(low="lightgreen", high= "darkblue")+
  ggtitle("Fluxo de Estudantes Residentes em Recife até a UFPE") +
  coord_equal()
gg_ufpe_fluxo

setwd("C:/Users/Monteiro-DataPC/Documents/GitProjects/diagjuv-recife/Resultados")
ggsave("gg_ufpe_fluxo.png", gg_ufpe_fluxo, width = 8, height = 8, units = "in")

#---- GG SHAPE ----#

ggshape_ufpe_fluxo <- ggmap(gg_recife)+
              geom_curve(data = ufpe_latlon, aes(x = long, y = lat, xend = longcdu, 
                                                 yend = latcdu, color= Intensidade),
                                                    curvature = -0.1, size = 0.75) +
              scale_color_gradient(low="lightgreen", high= "darkblue")+
  geom_polygon(aes(x = long, y = lat, group = group), 
               data = recife_fort,
               color = "black",
               size = 0.25)+
ggshape_ufpe_fluxo

#=================================#
# como os jovens vao trabalhar?

table(jovem_dest$meio_transporte_trab)


#========================================#
# como se movimentam os jovens do Recife?
#========================================#

#------------------#
#---- TRABALHO ----#

# selecionar jovens trabalhadores do Recife
od_recife_jovem <- origem_dest[origem_dest$cidade_residencia == "RECIFE" & 
                               origem_dest$faixa_etaria == 3 &
                               origem_dest$trabalha == 1,]

# contar uso do meio de transporte p/ trabalhar
meio_transporte_trab <- c(length(grep("0", od_recife_jovem$meio_transporte_trab)),
                          length(grep("1", od_recife_jovem$meio_transporte_trab)),
                          length(grep("2", od_recife_jovem$meio_transporte_trab)),
                          length(grep("3", od_recife_jovem$meio_transporte_trab)),
                          length(grep("4", od_recife_jovem$meio_transporte_trab)),
                          length(grep("5", od_recife_jovem$meio_transporte_trab)),
                          length(grep("6", od_recife_jovem$meio_transporte_trab)),
                          length(grep("7", od_recife_jovem$meio_transporte_trab)),
                          length(grep("8", od_recife_jovem$meio_transporte_trab)),
                          length(grep("9", od_recife_jovem$meio_transporte_trab)),
                          length(grep("10", od_recife_jovem$meio_transporte_trab)),
                          length(grep("11", od_recife_jovem$meio_transporte_trab)),
                          length(grep("12", od_recife_jovem$meio_transporte_trab))
                          )

# meios de tranporte
transporte <- c("Não declarado", 
                "A pé", 
                "Bicicleta", 
                "Ônibus", 
                "Metrô", 
                "Carro (dirigindo)", 
                "Carro (como carona de familiar)", 
                "Carro (como carona de amigo/colega)", 
                "Carro (com motorista)", 
                "Motocicleta", 
                "Transporte escolar", 
                "Táxi", 
                "Fretado")

# mergir dados
data_meiotransp <- data.frame(meio_transporte_trab, transporte)

# criar prop
data_meiotransp$prop_transp <- data_meiotransp$meio_transporte_trab / sum(data_meiotransp$meio_transporte_trab)
data_meiotransp$prop_transp <- round(data_meiotransp$prop_transp, 3)*100

# ordenar para plotagem
data_meiotransp <- data_meiotransp[order(data_meiotransp$prop_transp),]
data_meiotransp$prop_transp <- factor(data_meiotransp$prop_transp, levels = data_meiotransp$prop_transp)

# 
ggplot(data_meiotransp, aes(x = transporte, y = prop_transp))+
  geom_bar(stat = "identity", fill = "#15041c") +
  labs(y = "Porcentagem do Total de Jovens Trabalhadores", x = "", title = "Meio de Tranporte dos Jovens p/ Trabalho") +
  coord_flip()

ggsave("transporte_trabalho.png", path = "Mobilidade/resultados", width = 8.5, height = 5, units = "in")

#----------------#
#---- ESTUDO ----#

# selecionar jovens estudantes do Recife
od_recife_jovem2 <- origem_dest[origem_dest$cidade_residencia == "RECIFE" & 
                                 origem_dest$faixa_etaria == 3 &
                                 origem_dest$pesquisado_estuda == 1,]

# contar uso do meio de transporte p/ trabalhar
meio_transporte_est<- c(length(grep("0", od_recife_jovem2$transporte_aula)),
                          length(grep("1", od_recife_jovem2$transporte_aula)),
                          length(grep("2", od_recife_jovem2$transporte_aula)),
                          length(grep("3", od_recife_jovem2$transporte_aula)),
                          length(grep("4", od_recife_jovem2$transporte_aula)),
                          length(grep("5", od_recife_jovem2$transporte_aula)),
                          length(grep("6", od_recife_jovem2$transporte_aula)),
                          length(grep("7", od_recife_jovem2$transporte_aula)),
                          length(grep("8", od_recife_jovem2$transporte_aula)),
                          length(grep("9", od_recife_jovem2$transporte_aula)),
                          length(grep("10", od_recife_jovem2$transporte_aula)),
                          length(grep("11", od_recife_jovem2$transporte_aula)),
                          length(grep("12", od_recife_jovem2$transporte_aula))
)

# mergir dados
data2_meiotransp <- data.frame(meio_transporte_est, transporte)

# criar prop
data2_meiotransp$prop_transp2 <- data2_meiotransp$meio_transporte_est / sum(data2_meiotransp$meio_transporte_est)
data2_meiotransp$prop_transp2 <- round(data2_meiotransp$prop_transp2, 3)*100

# ordenar para plotagem
data2_meiotransp <- data2_meiotransp[order(data2_meiotransp$prop_transp2),]
data2_meiotransp$prop_transp2 <- factor(data2_meiotransp$prop_transp2, levels = data2_meiotransp$prop_transp2)

# 
ggplot(data2_meiotransp, aes(x = transporte, y = prop_transp2))+
  geom_bar(stat = "identity", fill = "#15041c") +
  labs(y = "Porcentagem do Total de Jovens Estudantes", x = "", title = "Deslocamento de Jovens Estudantes") +
  coord_flip()

ggsave("transporte_estudo.png", path = "Mobilidade/resultados", width = 8.5, height = 5, units = "in")












