gerar_modelo_linear = function (df) {
    modelo = lm(preco ~ ram + hd, data=df)
    valores = predict(modelo, df)
    erro = mean(abs(df[["preco"]] - valores))

    return (list(modelo=modelo, valores=valores, erro=erro))
}

gerar_modelo_naolinear = function (df) {
    modelo = nls(preco ~ a1*ram^2 + a2*hd^2 + b1*ram + b2*hd + c, data=df, start=list(a1=1, b1=1, a2=1, b2=1, c=0))
    valores = predict(modelo, df)
    erro = mean(abs(df[["preco"]] - valores))

    return (list(modelo=modelo, valores=valores, erro=erro))
}
