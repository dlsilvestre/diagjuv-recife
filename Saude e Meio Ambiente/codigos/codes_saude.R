#====================================================#
# DIAGN?STICO DA JUVENTUDE NA CIDADE DO RECIFE       #              
#====================================================#
# SAUDE E MEIO AMBIENTE                              #
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

#=================================#
#           MORTALIDADE    
#=================================# 

# instalar pacote 'datasus'
#devtools::install_github("danicat/datasus")
library(datasus)

# selecionar pasta de trabalho do pacote
datasus.init('Saude e Meio Ambiente/dados')

# baixar dados de mortalidade por causas externas para PE
df <- sim.load("DOEXT", 2014, "PE")

# PROBLEMA COM O ACESSO AOS ARQUIVOS - ENVIAR MENSAGEM A DANICAT
