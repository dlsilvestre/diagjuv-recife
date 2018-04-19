# coding=latin1
"""
EXPLORANDO DADOS DA SDS-RECIFE EM PYTHON

Autor(a): Claudio Luis Alves Monteiro
Email: claudiomonteirol.a@gmail.com
Data: 2018-04-06

Copyright(c) 2018 Claudio Luis Alves Monteiro
"""

# importar modules
import pandas as pd

# importar arquivo xls
data1 = pd.ExcelFile('CVP/CVP_2012_2017_07.08.2017.xlsx')

# selecionar planilha
dataCVP = data1.parse('BASE 2012 - 2017')

# contagem de casos por bairro e LOGRADOURO
bairrosCVP = dataCVP.BAIRRO.value_counts()
logradourosCVP = dataCVP.LOGRADOURO.value_counts()

#  definir index como coluna
bairrosCVP = bairrosCVP.reset_index()
logradourosCVP = logradourosCVP.reset_index()

# renomear colunas
bairrosCVP.columns = ['Bairro', 'CasosCVP']
logradourosCVP.columns = ['Lograoduro', 'CasosCVP']

# exportar base de dados
bairrosCVP.to_csv("bairrosCVP.csv", sep=';', encoding='utf-8')
logradourosCVP.to_csv("logradourosCVP.csv", sep=';', encoding='utf-8')

# importar arquivo xls
workbookCVM = pd.ExcelFile('/home/pacha/Desktop/bases_sds_prefeitura/1.1.5 - Crimes Violentos Contra a Mulher (CVM)/SEC. MULHER - 2015.xlsx')
dataCVM = workbookCVM.parse(0)

# selecionar primeira linha do banco
NomesColunas = dataCVM.loc[[0]].values.flatten().tolist()

# onde ' ' substituir por '_'
#listaNew = []
#for nome in NomesColunas:
#    for letra in nome:
#        if letra == ' ':
#            nome.remove(letra)
# print(listaNew)

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
[EM DESENVOLVIMENTO]

# ====== grafico de barra MATPLOTLIB ======#

# importar module
import matplotlib.pyplot as plt
import matplotlib

# grafico
plt.bar(bairrosCVP.CasosCVP, bairrosCVP.Bairro)
plt.xticks(y_pos, objects)
plt.title('Casos de Crime Violento Contra o Patrimonio no Recife')
plt.xlabel('Casos de CVP')

plt.show()
plt.bar(x, y, width, color="blue")
# ====== grafico de barra PLOTLY ======#

# importar plotly e definir credenciais
import plotly

plotly.tools.set_credentials_file(username='claudiomonteiro', api_key='cC6UDJzPLeVrcS4LX6M6')

# importar plotly e objetos graficos
import plotly.plotly as py
import plotly.graph_objs as go

# transformar em barra
bairrosCVPbarra = go.Bar(bairrosCVP)

# plotar bairros X numero de CVP
py.offline.plot(dataCVP, filename="")

'''






