#====================================================#
# DIAGNÓSTICO DA JUVENTUDE NA CIDADE DO RECIFE       #              
#====================================================#
# SAÚDE E MEIO AMBIENTE                              #
#----------------------------------------------------#
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

# instalar pacotes necessarios
# install.packages(c("devtools", "readxl"))

library(readxl); library(devtools)

#=================================#
#           MORTALIDADE    
#=================================# 

# instalar pacote 'datasus'
devtools::install_github("danicat/datasus")
library(datasus)

# selecionar pasta de trabalho do pacote
datasus.init('Saúde e Meio Ambiente/dados')

# baixar dados de mortalidade por causas externas para PE
df <- sim.load("DOEXT", c(2010:2014), "PE")

# PROBLEMA COM O ACESSO AOS ARQUIVOS - ENVIAR MENSAGEM A DANICAT

#=================================#
#            SANEAMENTO
#=================================#

# carregar bases
esgoto2000 <- read_excel("Saúde e Meio Ambiente/dados/esgotamento-CENSO2000.xls", col_names = FALSE)
esgoto2010 <- read_excel("Saúde e Meio Ambiente/dados/esgotamento-CENSO2010.xls", col_names = FALSE)

# selecionar dados por bairros
esgoto2000 <- esgoto2000[712:805,]
esgoto2010 <- esgoto2010[1018:1111,]

# recodificar variaveis
















