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
install.packages(c("readxl", "stringr", "dplyr", "ggplot2"))

# carregar pacotes
library(readxl); library(stringr); library(dplyr); library(ggplot2)

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


#=========================================#
# CVLI por ano - total/jovem/total-jovem
#=========================================#

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
x3 <- data.frame(cvli_data1[,c(1,4)], grupo = "Mortes Totais - Jovens")

colnames(x1)[2] <- c("Mortes")
colnames(x2)[2] <- c("Mortes")
colnames(x3)[2] <- c("Mortes")

cvli_data2 <- rbind(x1, x2)

# gráfico
ggplot(data = cvli_data2) +
  geom_line(aes(x = Ano, y = Mortes, group = grupo, color = grupo), size = 1) + 
  geom_label(aes(x = Ano, y = Mortes, label = Mortes))+
  scale_color_manual(values=c("#7f0000", "#E69F00"))+
  scale_y_continuous(limits = c(0,800))+
  theme(legend.position="bottom")

# salvar grafico
ggsave("mortes_total_jovens_porano.png", path = "Violência/resultados",
       width = 10, height = 5, units = "in")


# POR MES
#mes <- data.frame(table(data_cvli$ANO,data_cvli$MÊS))
#mes <- mes[order(mes$Var1),]


#=========================================#
# CVLI por bairro - total/jovem/total-jovem
#=========================================#

















