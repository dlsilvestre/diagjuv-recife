#================================================#
# JUVENTUDE NA CIDADE DO RECIFE                  #
# Demografia 1.3 - Raça e Cor                    #
#================================================#
# Recife - Pernambuco - Brasil                   #
# Prefeitura da Cidade do Recife                 #
#------------------------------------------------#
# Secretaria Executiva de Juventude              #
#------------------------------------------------#
# Claudio A. Monteiro                            #
# claudiomonteiro@gmail.com                      #
#------------------------------------------------#
# Qualquer duvida contate o desenvolvedor        #
# #UseSoftwareLivre                              #
#------------------------------------------------#

# instalar pacotes necessarios
# install.packages(c("readxl", "dplyr", "reshape"))

# carregar pacotes
library(readxl); library(dplyr); library(reshape)

#----carregar banco de dados----#
pop_idaderaca_2010 <- read_excel("Dados Originais/Demografia/pop_idaderaca_2010.xlsx")


#============================#
# Manipulação dos dados      #
#============================#

# remover linhas desnecessarias
pop_idaderaca_2010 <- pop_idaderaca_2010[-c(1:6,686),]

# renomear colunas
colnames(pop_idaderaca_2010) <- c("localidade", "raca_cor", "pop_total", "pop_15a19", "pop_20a24", "pop_25a29")

# substituir NA por 0
pop_idaderaca_2010 <- data.frame(pop_idaderaca_2010[1:2], sapply(pop_idaderaca_2010[3:6], function(x) gsub("-", "0", x)), stringsAsFactors = F)

# transformar em numeric
pop_idaderaca_2010 <- data.frame(pop_idaderaca_2010[1:2], sapply(pop_idaderaca_2010[3:6], function(x) as.numeric(x)), stringsAsFactors = F)

# criar variavel 'pop_jovem'
pop_idaderaca_2010 <- mutate(pop_idaderaca_2010, pop_jovem = pop_15a19 + pop_20a24 + pop_25a29)


#----Substituir NAs por Nomes----#

# transformar 'raca_cor' em factor
pop_idaderaca_2010$raca_cor <- as.factor(pop_idaderaca_2010$raca_cor)

# dividir dados com base na raça
pop_idaderaca_amarela <- pop_idaderaca_2010[pop_idaderaca_2010$raca_cor == "Amarela",]
pop_idaderaca_branca <- pop_idaderaca_2010[pop_idaderaca_2010$raca_cor == "Branca",]
pop_idaderaca_indigena <- pop_idaderaca_2010[pop_idaderaca_2010$raca_cor == "Indígena",]
pop_idaderaca_parda <- pop_idaderaca_2010[pop_idaderaca_2010$raca_cor == "Parda",]
pop_idaderaca_preta <- pop_idaderaca_2010[pop_idaderaca_2010$raca_cor == "Preta",]
pop_idaderaca_semdecla <- pop_idaderaca_2010[pop_idaderaca_2010$raca_cor == "Sem declaração",]
pop_idaderaca_total <- pop_idaderaca_2010[pop_idaderaca_2010$raca_cor == "Total",]

# nomear 
pop_idaderaca_amarela$localidade <- pop_idaderaca_branca$localidade <- pop_idaderaca_indigena$localidade <- 
pop_idaderaca_parda$localidade <- pop_idaderaca_preta$localidade <- pop_idaderaca_semdecla$localidade <- 
pop_idaderaca_total$localidade 

# combinar
pop_idaderaca_2010 <- rbind(pop_idaderaca_amarela, 
                            pop_idaderaca_branca,
                            pop_idaderaca_indigena,  
                            pop_idaderaca_parda,
                            pop_idaderaca_preta, 
                            pop_idaderaca_semdecla, 
                            pop_idaderaca_total)
# remover 
pop_idaderaca_2010 <- pop_idaderaca_2010[,-c(4:6)]

# transformar de long em wide
pop_idaderaca_2010 <- reshape(pop_idaderaca_2010, idvar = "localidade", 
              timevar = "raca_cor", direction = "wide")

#===============================
# VISUALIZACAO GRAFICA 
#===============================

#--- arretado theme ---#
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

reshape(mydf, direction = "wide", 
        idvar = c("hash", "age", "sex"), 
        timevar = "color")














