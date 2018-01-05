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
# install.packages(c("readxl", "dplyr"))

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

# transformar em numeric
pop_idaderaca_2010 <- data.frame(pop_idaderaca_2010[1:2], sapply(pop_idaderaca_2010[3:6], function(x) as.numeric(x)))
  
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
pop_idaderaca_20102 <- rbind(pop_idaderaca_amarela, 
                            pop_idaderaca_branca,
                            pop_idaderaca_indigena,  
                            pop_idaderaca_parda,
                            pop_idaderaca_preta, 
                            pop_idaderaca_semdecla, 
                            pop_idaderaca_total)

















