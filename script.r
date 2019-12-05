source("dependencias.r")
source("leitura-dados.r")
source("classificacao.r")
source("regressao.r")

print("Lendo dados...")
df = carregar_dataframe("./phones.csv")

print("Gerando matriz de correlação")
df_num = Filter(is.numeric, df)
corrplot::corrplot(cor(df_num), method="color")

# A rotina abaixo demora vários segundos para rodar!
print("Aplicando knn...")
cm_knn = obter_melhor_knn(df, kinicial=2, kpasso=5)
print(cm_knn)
print(cm_knn[["k"]])

print("Aplicando regressões...")
linear = gerar_modelo_linear(df)
naolinear = gerar_modelo_naolinear(df)
print("Média de erro com regressão linear")
print(linear[["erro"]])
print("Média de erro com regressão não-linear")
print(naolinear[["erro"]])
plot(sort(df[["preco"]]), col="black")
points(sort(linear[["valores"]]), col="blue")
points(sort(naolinear[["valores"]]), col="orange")
