#----------------------------------------------------#
# JUVENTUDE NA CIDADE DO RECIFE                      #
# Demografia 1.1 - Recife em Perspectiva Comparada   #            
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

# instalar pacotes necessários
#install.packages(c("readxl", "xlsx", "ggplot2", "directlabels", "ggrepel", "readr", "plyr", "rgdal", "ggmap", 
# "maps", "mapdata", "raster", "maptools", "stringi", "DT", "xtable", "gridExtra", "qdap", "ggthemes", "ggpubr", 
# "dplyr"), dependencies = T)

# carregar pacotes
library(ggthemes)
library(qdap)
library(readxl)
library(xlsx)
library(ggplot2)
library(directlabels)
library(ggrepel)
library(readr)
library(plyr)
library(rgdal)
library(ggmap)
library(maps)
library(mapdata)
library(raster)
library(maptools)
library(stringi)
library(DT)
library(xtable)
library(ggpubr)
library(dplyr)

#----------------------------------------#
# Comparando Recife, Pernambuco e Brasil
#----------------------------------------#

# carregar banco
pop_jovem_comp <- read_excel("~/Research/Juventude OIT PCR/Original Data/Demografia/comp_sexage2.xlsx")

# renomear colunas
colnames(pop_jovem_comp) <- c("Localidade", "Ano", "pop_total", "pop_homem", "pop_mulher",
                               "pop_15a19_total", "pop_15a19_homem", "pop_15a19_mulher", 
                               "pop_20a24_total", "pop_20a24_homem", "pop_20a24_mulher",
                               "pop_25a29_total", "pop_25a29_homem", "pop_25a29_mulher")

# remover linhas 1:5 e 21
pop_jovem_comp <- pop_jovem_comp[-c(1:5, 21),]

# definir nome de linhas
pop_jovem_comp$Localidade[2:5] <- "Brasil"
pop_jovem_comp$Localidade[7:10] <- "Pernambuco"
pop_jovem_comp$Localidade[11:15] <- "Recife"

# transformar em numeric e renomear 
pop_jovem_comp_x <- pop_jovem_comp[, - 1]
pop_jovem_comp_x <- as.data.frame(sapply(pop_jovem_comp_x, function(x) as.numeric(x) ))
pop_jovem_comp_x$Localidade <- pop_jovem_comp$Localidade
pop_jovem_comp <-pop_jovem_comp_x

#--- criar variaveis juventude  ---#

# populacao jovem total
pop_jovem_comp <- mutate(pop_jovem_comp, pop_jovem = pop_15a19_total + pop_20a24_total + pop_25a29_total)

# populacao homem jovem
pop_jovem_comp <- mutate(pop_jovem_comp, pop_homem_jovem = pop_15a19_homem + pop_20a24_homem + pop_25a29_homem)

# populacao mulher jovem
pop_jovem_comp <- mutate(pop_jovem_comp, pop_mulher_jovem = pop_15a19_mulher + pop_20a24_mulher + pop_25a29_mulher)

# proporcao jovem do total
pop_jovem_comp <- mutate(pop_jovem_comp, prop_jovem_total = round((pop_jovem / pop_total), 3))

# proporcao de homens jovens do total de homens
pop_jovem_comp <- mutate(pop_jovem_comp, prop_homemj_homemt = round((pop_homem_jovem / pop_homem), 3))

# proporcao de mulheres jovens do total de mulheres
pop_jovem_comp <- mutate(pop_jovem_comp, prop_mulherj_mulhert = round((pop_mulher_jovem / pop_mulher), 3))

# proporcao de homens jovens do total de jovens
pop_jovem_comp <- mutate(pop_jovem_comp, prop_homemj_jovemt = round((pop_homem_jovem / pop_jovem), 3))

# proporcao de mulheres jovens do total de jovens
pop_jovem_comp <-mutate(pop_jovem_comp, prop_mulherj_jovemt =  round((pop_mulher_jovem / pop_jovem), 3))

#---- GRAFICOS DE LINHA ----#

# definir diretorio
setwd("C:/Users/Monteiro-DataPC/Documents/Research/Juventude OIT PCR/Outputs")

#--- arretado theme ---#
theme_arretado<- function (base_size = 12, base_family = "") {
  theme_minimal(base_size = base_size, base_family = base_family) %+replace% 
    theme(axis.text.x = element_text(colour= "black",size=11,hjust=.5,vjust=.5,face="plain"),
          axis.text.y = element_text(colour="black",size=11,angle=0,hjust=1,vjust=0,face="plain"), 
          axis.title.x = element_text(colour="black",size=12,angle=0,hjust=.5,vjust=0,face="plain"),
          axis.title.y = element_text(colour="black",size=12,angle=90,hjust=0.5,vjust=0.6,face="plain"),
          title = element_text(colour="black",size=10,angle=0,hjust=.5,vjust=.5,face="plain"),
          panel.grid.major = element_line(colour = grey(0.85)), 
          panel.grid.minor = element_line(colour = grey(1)),
          legend.key.size = unit(9, "mm"),
          legend.text = element_text(size = 9, hjust = 3, vjust = 3),
          legend.title = element_text(size = 9),
          axis.line = element_line(size = 1, colour = "grey70"))
}

#----# Evolucao da proporcao de jovens por localidade ----#
ggplot(data = pop_jovem_comp, aes(x = Ano, y = prop_jovem_total, group = Localidade, color = Localidade)) + 
  geom_line(aes(linetype=Localidade), size=1) +
  scale_linetype_manual(values = c(1, 2, 3)) +
  scale_y_continuous(limits= c(0.1, 0.45))+
  scale_color_manual(values = c("#325c6c", "#400001", "darkgreen"))+
  geom_text_repel(aes(x = Ano, y = prop_jovem_total, 
                      label = pop_jovem_comp$prop_jovem_total), size = 2.5)+
  labs(x = "", y = "Proporção")+
  theme_arretado()
 ggsave("line_comp_prop.png", width = 6, height = 3, units = "in")


#---- Evolucao da populacao de jovens por localidade ----#
pop_jovem_comp$pop_jovem_log <- log(pop_jovem_comp$pop_jovem)

ggplot(data = pop_jovem_comp, aes(x = Ano, y = pop_jovem_log, group = Localidade, color = Localidade)) + 
  geom_line(aes(linetype=Localidade), size=1) +
  scale_linetype_manual(values = c(1, 2, 3)) +
  scale_color_manual(values = c("#325c6c", "#400001", "darkgreen"))+
  geom_text(aes(x = Ano, y = pop_jovem_log, label = pop_jovem_comp$pop_jovem, 
                angle = -10 , vjust = -0.5, hjust = 0.4), size = 3,colour="black", fontface = "bold")+
  labs(x = "", y = "População de Jovens")+
  theme_arretado()
ggsave("line_comp_pop.png", width = 9, height = 4, units = "in")

#---- GRAFICOS SEXO ----#
 
# manipular
comp_jovem_homem <- pop_jovem_comp[, c("prop_homemj_jovemt", "Localidade", "Ano")]
comp_jovem_mulher <- pop_jovem_comp[, c("prop_mulherj_jovemt", "Localidade", "Ano")]

comp_jovem_homem$Sexo <- "Homem"
comp_jovem_mulher$Sexo <- "Mulher"

colnames(comp_jovem_homem) <- c( "prop_jovem", "Localidade", "Ano","Sexo")
colnames(comp_jovem_mulher) <- c( "prop_jovem", "Localidade", "Ano","Sexo")

barra_comp_sexprop <- rbind(comp_jovem_homem, comp_jovem_mulher)

# proporcao de homens jovens aos longo do tempo
linha_homemprop <- ggplot(comp_jovem_homem, aes(x=Ano, y=prop_jovem, group= Localidade, color = Localidade)) +
        geom_line(aes(linetype=Localidade), size=1) +
        scale_linetype_manual(values = c(1, 2, 3)) +
        scale_y_continuous(limits= c(0.40, 0.60))+
        scale_color_manual(values = c("#325c6c", "#400001", "darkgreen"))+
        labs(x = "", y = "Proporção", title = "Homens")  +
        geom_text_repel(aes(x = Ano, y=prop_jovem, label = comp_jovem_homem$prop_jovem), size = 2.3)+
        theme_arretado()

# line plot mulher
linha_mulherprop <- ggplot(comp_jovem_mulher, aes(x=Ano, y=prop_jovem, group= Localidade, color = Localidade)) +
  geom_line(aes(linetype=Localidade), size=1) +
  scale_linetype_manual(values = c(1, 2, 3)) +
  scale_y_continuous(limits= c(0.40, 0.60))+
  scale_color_manual(values = c("#325c6c", "#400001", "darkgreen"))+
  labs(x = "Proporção", y = "Proporção", title = "Mulheres")  +
  geom_text_repel(aes(x = Ano, y=prop_jovem, label = comp_jovem_mulher$prop_jovem), size = 2.3)+
  theme_arretado()

lineplot_sex <- ggarrange(linha_homemprop, linha_mulherprop, ncol = 2, nrow = 1,
                          common.legend = TRUE,
                          legend = "bottom")
lineplot_sex
ggsave("lineplot_sex.png", width = 7, height = 3, units = "in")

#------------------------#
# Age Pyramid
#------------------------#

#----------------
# manipulate data
library(readxl)
Tabela_200 <- read_excel("~/Research/Juventude OIT PCR/Original Data/Demografia/Tabela 200.xlsx")
pyram_data <- Tabela_200[-c(1:4),-1]

# FUNCTION MANI
mani.pyram <- function(x){
  x <- x[ -c(1:2),]
  x$Idade <- factor(x$Idade, levels = c("0 a 4 anos", "5 a 9 anos", "10 a 14 anos", "15 a 19 anos", "20 a 24 anos", 
                                        "25 a 29 anos", "30 a 34 anos", "35 a 39 anos","40 a 44 anos", "45 a 49 anos", "50 a 54 anos",    
                                        "55 a 59 anos", "60 a 64 anos", "65 a 69 anos", "70 a 74 anos", "75 a 79 anos", "80 a 84 anos",
                                        "85 a 89 anos", "90 a 94 anos", "95 a 99 anos", "100 anos ou mais" ), 
                    labels = c("0 a 4", "5 a 9", "10 a 14", "15 a 19", "20 a 24", 
                               "25 a 29", "30 a 34", "35 a 39","40 a 44", "45 a 49", "50 a 54",    
                               "55 a 59", "60 a 64", "65 a 69", "70 a 74", "75 a 79", "80 a 84",
                               "85 a 89", "90 a 94", "95 a 99", "100 ou mais"))
  x_m <- x[x$Sexo == "Homens",]
  x_w <- x[x$Sexo == "Mulheres",]
  x_w$Idade <- x_m$Idade
  x <- rbind(x_m, x_w)
  x <- as.data.frame(x)
}

#--- BRASIL
pyram_databrasil00 <- pyram_data[1:44,-4]
pyram_databrasil10 <- pyram_data[1:44,-3]

colnames(pyram_databrasil00) <- c("Idade", "Sexo", "População")
colnames(pyram_databrasil10) <- c("Idade", "Sexo", "População")

pyram_databrasil00$População <- as.numeric(pyram_databrasil00$População)
pyram_databrasil10$População <- as.numeric(pyram_databrasil10$População)

# 2000
pyram_databrasil00 <- mani.pyram(pyram_databrasil00)

brasil_py00 <- ggplot(data = pyram_databrasil00, 
                      mapping = aes(x = Idade, fill = Sexo, 
                                    y = ifelse(test = Sexo == "Homens", yes = -População, no = População), fill = Sexo)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = abs, limits = max(pyram_databrasil00$População) * c(-1,1)) +
  labs(y = "População", x = "Brasil") +
  theme(axis.text.x = element_text(colour= "black",size=9,hjust=.5,vjust=.5,face="plain"),
        axis.text.y = element_text(colour="black",size=9,angle=0,hjust=1,vjust=0,face="plain"),  
        axis.title.x = element_text(colour="black",size=9,angle=0,hjust=.5,vjust=0,face="plain"),
        axis.title.y = element_text(colour="black",size=15,angle=90,hjust=.5,vjust=0.5,face="bold"),
        panel.background = element_rect(fill = NA),
        panel.grid.major = element_line(colour = grey(0.82)),
        legend.key.size = unit(9, "mm"),
        legend.text = element_text(size = 14, hjust = 3, vjust = 3),
        title = element_text(size = 15, hjust = 3, vjust = 3, face = "bold"),
        legend.title = element_text(size = 14),
        axis.line = element_line(size = 1, colour = "grey82"))+
  scale_fill_manual(values = c("#1c3c40","lightgreen"))+
  labs(x = "Brasil", y = "População", title = "2000")+
  coord_flip()

brasil_py00
# 2010
pyram_databrasil10 <- mani.pyram(pyram_databrasil10)

brasil_py10 <- ggplot(data = pyram_databrasil10, 
                      mapping = aes(x = Idade, fill = Sexo, 
                                    y = ifelse(test = Sexo == "Homens", yes = -População, no = População))) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = abs, limits = max(pyram_databrasil10$População) * c(-1,1)) +
  labs(y = "População", x = "") +
  theme(axis.text.x = element_text(colour= "black",size=9,hjust=.5,vjust=.5,face="plain"),
        axis.text.y = element_text(colour="black",size=9,angle=0,hjust=1,vjust=0,face="plain"),  
        axis.title.x = element_text(colour="black",size=9,angle=0,hjust=.5,vjust=0,face="plain"),
        panel.background = element_rect(fill = NA),
        panel.grid.major = element_line(colour = grey(0.82)),
        legend.key.size = unit(9, "mm"),
        legend.text = element_text(size = 14, hjust = 3, vjust = 3),
        title = element_text(size = 15, hjust = 3, vjust = 3, face = "bold"),
        legend.title = element_text(size = 14),
        axis.line = element_line(size = 1, colour = "grey82"))+
  scale_fill_manual(values = c("#1c3c40","lightgreen"))+
  labs(x = "", y = "População", title = "2010")+
  coord_flip()

brasil_py10

#--- PERNAMBUCO
pyram_dataPE00 <- pyram_data[47:88,-4]
pyram_dataPE10 <- pyram_data[47:88,-3]

colnames(pyram_dataPE00) <- c("Idade", "Sexo", "População")
colnames(pyram_dataPE10) <- c("Idade", "Sexo", "População")

pyram_dataPE00$População <- as.numeric(pyram_dataPE00$População)
pyram_dataPE10$População <- as.numeric(pyram_dataPE10$População)

# 2000
pyram_dataPE00 <- mani.pyram(pyram_dataPE00)

PE_py00 <- ggplot(data = pyram_dataPE00, 
                  mapping = aes(x = Idade, fill = Sexo, 
                                y = ifelse(test = Sexo == "Homens", yes = -População, no = População))) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = abs, limits = max(pyram_dataPE00$População) * c(-1,1)) +
  labs(y = "População", x = "Pernambuco") +
  theme(axis.text.x = element_text(colour= "black",size=9,hjust=.5,vjust=.5,face="plain"),
        axis.text.y = element_text(colour="black",size=9,angle=0,hjust=1,vjust=0,face="plain"),  
        axis.title.x = element_text(colour="black",size=9,angle=0,hjust=.5,vjust=.5,face="plain"),
        axis.title.y = element_text(colour="black",size=15,angle=90,hjust=.5,vjust=0.5,face="bold"),
        panel.background = element_rect(fill = NA),
        panel.grid.major = element_line(colour = grey(0.82)),
        legend.key.size = unit(9, "mm"),
        legend.text = element_text(size = 14, hjust = 3, vjust = 3),
        legend.title = element_text(size = 14),
        axis.line = element_line(size = 1, colour = "grey82"))+
  scale_fill_manual(values = c("#1c3c40","lightgreen"))+
  labs(x = "Pernambuco", y = "População")+
  coord_flip()

PE_py00
# 2010
pyram_dataPE10 <- mani.pyram(pyram_dataPE10)

PE_py10 <- ggplot(data = pyram_dataPE10, 
                  mapping = aes(x = Idade, fill = Sexo, 
                                y = ifelse(test = Sexo == "Homens", yes = -População, no = População))) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = abs, limits = max(pyram_dataPE10$População) * c(-1,1)) +
  labs(y = "População", x = "") +
  theme(axis.text.x = element_text(colour= "black",size=9,hjust=.5,vjust=.5,face="plain"),
        axis.text.y = element_text(colour="black",size=9,angle=0,hjust=1,vjust=0,face="plain"),  
        axis.title.x = element_text(colour="black",size=9,angle=0,hjust=.5,vjust=0,face="plain"),
        panel.background = element_rect(fill = NA),
        panel.grid.major = element_line(colour = grey(0.82)),
        legend.key.size = unit(9, "mm"),
        legend.text = element_text(size = 14, hjust = 3, vjust = 3),
        legend.title = element_text(size = 14),
        axis.line = element_line(size = 1, colour = "grey82"))+
  scale_fill_manual(values = c("#1c3c40","lightgreen"))+
  labs(x = "", y = "População")+
  coord_flip()

PE_py10

#--- RECIFE
pyram_dataRec00 <- pyram_data[91:132,-4]
pyram_dataRec10 <- pyram_data[91:132,-3]

colnames(pyram_dataRec00) <- c("Idade", "Sexo", "População")
colnames(pyram_dataRec10) <- c("Idade", "Sexo", "População")

pyram_dataRec00$População <- as.numeric(pyram_dataRec00$População)
pyram_dataRec10$População <- as.numeric(pyram_dataRec10$População)

# 2000
pyram_dataRec00 <- mani.pyram(pyram_dataRec00)

Rec_py00 <- ggplot(data = pyram_dataRec00, 
                   mapping = aes(x = Idade, fill = Sexo, 
                                 y = ifelse(test = Sexo == "Homens", yes = -População, no = População))) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = abs, limits = max(pyram_dataRec00$População) * c(-1,1)) +
  labs(y = "População", x = "Recife") +
  theme(axis.text.x = element_text(colour= "black",size=9,hjust=.5,vjust=.5,face="plain"),
        axis.text.y = element_text(colour="black",size=9,angle=0,hjust=1,vjust=0,face="plain"),  
        axis.title.x = element_text(colour="black",size=9,angle=0,hjust=.5,vjust=0,face="plain"),
        axis.title.y = element_text(colour="black",size=15,angle=90,hjust=.5,vjust=0.5,face="bold"),
        panel.background = element_rect(fill = NA),
        panel.grid.major = element_line(colour = grey(0.82)),
        legend.key.size = unit(9, "mm"),
        legend.text = element_text(size = 14, hjust = 3, vjust = 3),
        legend.title = element_text(size = 14),
        axis.line = element_line(size = 1, colour = "grey82"))+
  scale_fill_manual(values = c("#1c3c40","lightgreen"))+
  labs(x = "Recife", y = "População")+
  coord_flip()
Rec_py00

# 2010
pyram_dataRec10 <- mani.pyram(pyram_dataRec10)


Rec_py10 <- ggplot(data = pyram_dataRec10, 
                   mapping = aes(x = Idade, fill = Sexo, 
                                 y = ifelse(test = Sexo == "Homens", yes = -População, no = População))) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = abs, limits = max(pyram_dataRec10$População) * c(-1,1)) +
  labs(y = "População", x = "") +
  theme(axis.text.x = element_text(colour= "black",size=9,hjust=.5,vjust=.5,face="plain"),
        axis.text.y = element_text(colour="black",size=9,angle=0,hjust=1,vjust=0,face="plain"),  
        axis.title.y = element_text(colour="black",size=9,angle=90,hjust=.5,vjust=0.5,face="plain"),
        axis.title.x = element_text(colour="black",size=9,angle=0,hjust=.5,vjust=0.5,face="plain"),
        panel.background = element_rect(fill = NA),
        panel.grid.major = element_line(colour = grey(0.82)),
        legend.key.size = unit(9, "mm"),
        legend.text = element_text(size = 14, hjust = 3, vjust = 3),
        legend.title = element_text(size = 14),
        axis.line = element_line(size = 1, colour = "grey82"))+
  scale_fill_manual(values = c("#1c3c40","lightgreen"))+
  labs(x = "", y = "População")+
  coord_flip()
Rec_py10

#------ GRID PLOTS

pyramid <- ggarrange(brasil_py00, brasil_py10, PE_py00, PE_py10, Rec_py00, Rec_py10,
                     ncol = 2, nrow = 3, common.legend = TRUE)
pyramid

#ggsave("pyramid.png", pyramid, width = 8.5, height =11.5, units = "in")
















