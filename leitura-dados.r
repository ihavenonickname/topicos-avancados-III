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

carregar_dataframe = function (caminho) {
    df = read.csv(file=caminho, header=TRUE, sep=',')

    df[["hd"]] = converter_de_memoria(df[["internal_memory"]])
    df[["ram"]] = converter_de_memoria(df[["RAM"]])
    df[["pcamera"]] = converter_de_camera(df[["primary_camera"]], FALSE)
    df[["scamera"]] = converter_de_camera(df[["secondary_camera"]], TRUE)
    df[["tela"]] = converter_de_tela(df[["display_resolution"]])
    df[["os"]] = converter_de_os(df[["OS"]])
    df[["preco"]] = converter_de_preco(df[["approx_price_EUR"]])

    df = df[c("hd", "ram", "pcamera", "scamera", "tela", "os", "preco")]

    df[['ram']][df[['ram']] > 50000] = NA

    df = na.omit(df)

    df[["categoria"]] = as.factor(cut(
        df[["preco"]],
        breaks=c(0, 100, 250, 500, 1400),
        labels=c("E", "C", "B", "A")
    ))

    return (df)
}
