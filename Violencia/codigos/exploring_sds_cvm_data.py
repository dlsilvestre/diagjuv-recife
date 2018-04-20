# coding=latin1
"""
EXPLORANDO DADOS DA SDS-RECIFE EM PYTHON

Autor(a): Claudio Luis Alves Monteiro
Email: claudiomonteirol.a@gmail.com
Data: 2018-04-06

Copyright(c) 2018 Claudio Luis Alves Monteiro
"""

'''
Ps: tested on python3 linux terminal
sudo pip3 install pandas
sudo pip3 install xlrd
'''

#===== SDS_CVM =====#

# importar modules
import pandas as pd

# importar arquivo xls
workbookCVM = pd.ExcelFile('/home/pacha/Desktop/bases_sds_prefeitura/1.1.5 - Crimes Violentos Contra a Mulher (CVM)/SEC. MULHER - 2015.xlsx')
dataCVM = workbookCVM.parse(0)

# selecionar primeira linha do bancoN
NomesColunas = dataCVM.loc[[0]].values.flatten().tolist()

# onde ' ' substituir por '_'
#listaNew = []
#for nome in NomesColunas:
#    for letra in nome:
#        if letra == ' ':
#            nome.remove(letra)
# print(listaNew)

def transEspacoUnderline(listaTxt):
	listaFinal = []
	for nome in listaTxt:
		for letra in texto:
			textoTrans = ""
			if letra == ' ':
				textoTrans += '_'
			else:
				textoTrans += letra
			listaFinal.append(textoTrans)
	return listaFinal

#NomesColunas2 = [NomesColunas.replace(' ', '_') for nome in NomesColunas]

# renomear colunas
dataCVM.columns = NomesColunas

# remover primeira linha do banco
dataCVM = dataCVM.drop(dataCVM.index[[0]])

# contagem de casos por bairro e LOGRADOURO
#bairrosCVM = dataCVM.bairro do fato.value_counts()
#logradourosCVM = dataCVM.LOGRADOURO.value_counts()

'''
Criar uma funcao:

listaCont = [] # lista para inserir contagem de cada coluna
for coluna in base:
    colunaCont = coluna.value.count()  # contagem
    colunaCont = colunaCont.reset_index()       # reset index
    bairrosCVP.columns = ['Bairro', 'CasosCVP'] # renomear colunas
    salvar coluna em formato .csv      # ou criar uma base com essas infos e salvar

'''


'''
#  definir index como coluna
bairrosCVP = bairrosCVP.reset_index()
logradourosCVP = logradourosCVP.reset_index()

# renomear colunas
bairrosCVP.columns = ['Bairro', 'CasosCVP']
logradourosCVP.columns = ['Lograoduro', 'CasosCVP']

# exportar base de dados
bairrosCVP.to_csv("bairrosCVP.csv", sep=';', encoding='utf-8')
logradourosCVP.to_csv("logradourosCVP.csv", sep=';', encoding='utf-8')

'''