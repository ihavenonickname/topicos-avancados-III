source('leitura-dados.r')
source('classificacao.r')

df = carregar_dataframe('./phones.csv')
cm_knn = obter_melhor_knn(df, kinicial=1, kpasso=2)

print(cm_knn)
