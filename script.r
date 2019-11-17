obter_medida_numerica = function (coluna, sufixo) {
    padrao = paste('(\\d+(?:\\.\\d+)?)\\s', sufixo, sep='')

    coluna = regmatches(coluna, regexec(padrao, coluna))

    coluna = sapply(coluna, function (x) {
        return (if (identical(x, character(0))) NA else x[[2]])
    })

    coluna = as.double(coluna)
}

converter_de_memoria = function (coluna) {
    coluna = trimws(coluna)
    coluna = tolower(coluna)

    col_mb = obter_medida_numerica(coluna, 'mb')
    col_gb = obter_medida_numerica(coluna, 'gb')

    col_gb = as.double(col_gb) * 1024

    coluna = rep(NA, length(coluna))
    coluna[which(!is.na(col_mb))] = col_mb[which(!is.na(col_mb))]
    coluna[which(!is.na(col_gb))] = col_gb[which(!is.na(col_gb))]

    return (coluna)
}

converter_de_camera = function (coluna, na_vira_0) {
    coluna = tolower(coluna)
    coluna = obter_medida_numerica(coluna, 'mp')

    if (na_vira_0) {
        coluna[is.na(coluna)] = 0
    }

    return (coluna)
}

converter_de_os = function (coluna) {
    coluna = tolower(coluna)

    nova_coluna = rep("other", length(coluna))
    nova_coluna[grepl("ios", coluna)] = "ios"
    nova_coluna[grepl("windows", coluna)] = "windows"
    nova_coluna[grepl("android", coluna)] = "android"
    nova_coluna[grepl("blackberry", coluna)] = "blackberry"
    nova_coluna[grepl("symbian", coluna)] = "symbian"
    nova_coluna = as.factor(nova_coluna)

    return (nova_coluna)
}

converter_de_tela = function (coluna) {
    coluna = tolower(coluna)
    coluna = obter_medida_numerica(coluna, 'inches')

    return (coluna)
}

converter_de_preco = function (coluna) {
    coluna = tolower(coluna)
    coluna = regmatches(coluna, gregexpr('\\d+(?:\\.\\d+)?', coluna))
    coluna = as.double(coluna)
    coluna[coluna > 5000] = NA

    return (coluna)
}

carregar_dataframe = function () {
    df = read.csv(file='./phones.csv', header=TRUE, sep=',')

    df[["storage"]] = converter_de_memoria(df[["internal_memory"]])
    df[["ram"]] = converter_de_memoria(df[["RAM"]])
    df[["pcamera"]] = converter_de_camera(df[["primary_camera"]], FALSE)
    df[["scamera"]] = converter_de_camera(df[["secondary_camera"]], TRUE)
    df[["display"]] = converter_de_tela(df[["display_resolution"]])
    df[["os"]] = converter_de_os(df[["OS"]])
    df[["price"]] = converter_de_preco(df[["approx_price_EUR"]])

    df = df[c("storage", "ram", "pcamera", "scamera", "display", "os", "price")]

    df[['ram']][df[['ram']] > 50000] = NA

    df = na.omit(df)

    df[["category"]] = as.factor(cut(
        df[["price"]],
        breaks=c(0, 80, 120, 180, 250, 350, 500, 1400),
        labels=c("D", "C", "CB", "B", "BA", "A", "A+")
    ))

    return (df)
}

df = carregar_dataframe()

print("Total de celulares:")
print(nrow(df))

print("Correlação entre câmera primária e câmera secundária:")
print(cor(df[["scamera"]], df[["pcamera"]]))

print("Correlação entre disco e memória RAM:")
print(cor(df[["storage"]], df[["ram"]]))

print("Correlação entre tela e câmera primária:")
print(cor(df[["display"]], df[["pcamera"]]))

print("Correlação entre tela e preço:")
print(cor(df[["display"]], df[["price"]]))

print("Correlação entre disco e preço:")
print(cor(df[["storage"]], df[["price"]]))

print("Correlação entre câmera primária e preço:")
print(cor(df[["pcamera"]], df[["price"]]))

print("Correlação entre memória RAM e preço:")
print(cor(df[["ram"]], df[["price"]]))

print("Categorias de preço:")
print(summary(df[["category"]]))

print("Sistemas operacionais:")
print(summary(df[["os"]]))
