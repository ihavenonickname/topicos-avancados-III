source("dependencias.r")
source("leitura-dados.r")
source("classificacao.r")

print("Lendo dados...")
df = carregar_dataframe("./phones.csv")

print("Gerando matriz de correlação")
df_num = Filter(is.numeric, df)
corrplot::corrplot(cor(df_num), method="color")

print("Aplicando knn...")
cm_knn = obter_melhor_knn(df, kinicial=2, kpasso=5)
print(cm_knn)
print(cm_knn[["k"]])
