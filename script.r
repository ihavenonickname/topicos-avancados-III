source("dependencias.r")
source("leitura-dados.r")
source("classificacao.r")

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

print("Aplicando regressão linear...")
linear_model = lm(preco ~ ram + hd, data=df)
linear_prediction = predict(linear_model, df)
print("Média de erro com regressão linear")
print(mean(abs(df[["preco"]] - linear_prediction)))

print("Aplicando regressão não-linear...")
nonlinear_model = nls(preco ~ a1*ram^2 + a2*hd^2 + b1*ram + b2*hd + c, data=df, start=list(a1=1, b1=1, a2=1, b2=1, c=0))
nonlinear_prediction = predict(nonlinear_model, df)
print("Média de erro com regressão não-linear")
print(mean(abs(df[["preco"]] - nonlinear_prediction)))

print("Plotando regressões...")
plot(sort(df[["preco"]]), col="green")
points(sort(linear_prediction), col="blue")
points(sort(nonlinear_prediction), col="red")
