converter_de_memoria = function (coluna) {
    coluna = sapply(coluna, function (x) {
        x = tolower(x)

        r = regmatches(x, gregexpr('\\d+', x))

        if (is.na(r)) {
            return (NA)
        }

        r = strtoi(r)

        if (grepl("gb", x)) {
            return (r * 1024)
        }

        if (grepl("mb", x)) {
            return (r)
        }

        return (NA)
    })

    return (coluna)
}

converter_de_camera = function (coluna, ignorar_sem) {
    coluna = sapply(coluna, function (x) {
        x = tolower(x)

        r = regmatches(x, regexec('(\\d+(?:\\.\\d+)?)\\smp', x))

        r = as.double(r[[1]][2])

        if (is.na(r) && ignorar_sem) {
            r = 0
        }

        return (r)
    })

    return (coluna)
}

converter_de_os = function (coluna) {
    coluna = sapply(coluna, function (x) {
        x = tolower(x)

        if (grepl("windows", x)) {
            return ("windows")
        }

        if (grepl("android", x)) {
            return ("android")
        }

        if (grepl("ios", x)) {
            return ("ios")
        }

        if (grepl("blackberry", x)) {
            return ("blackberry")
        }

        if (grepl("symbian", x)) {
            return ("symbian")
        }

        if (grepl("firefox", x)) {
            return ("firefox")
        }

        if (grepl("linux", x)) {
            return ("linux")
        }

        if (grepl("tizen", x)) {
            return ("tizen")
        }

        if (x != "") {
            return ("other")
        }

        return (NA)
    })

    coluna = as.factor(coluna)

    return (coluna)
}

converter_de_tela = function (coluna) {
    coluna = sapply(coluna, function (x) {
        x = tolower(x)

        r = regmatches(x, regexec('(\\d+(?:\\.\\d+)?)\\sinches', x))

        r = as.double(r[[1]][2])

        return (r)
    })

    return (coluna)
}

converter_de_preco = function (coluna) {
    coluna = sapply(coluna, function (x) {
        x = tolower(x)

        r = regmatches(x, gregexpr('\\d+(?:\\.\\d+)?', x))

        r = as.double(r)

        if (is.na(r) || r > 5000) {
            return (NA)
        }

        return (r)
    })

    return (coluna)
}

carregar_dataframe = function () {
    dataset = read.csv(file='./phones.csv', header=TRUE, sep=',')

    df = data.frame(
        "storage"=converter_de_memoria(dataset$internal_memory),
        "memory"=converter_de_memoria(dataset$RAM),
        "pcamera"=converter_de_camera(dataset$primary_camera, FALSE),
        "scamera"=converter_de_camera(dataset$secondary_camera, TRUE),
        "display"=converter_de_tela(dataset$display_resolution),
        "os"=converter_de_os(dataset$OS),
        "price"=converter_de_preco(dataset$approx_price_EUR)
    )

    df = na.omit(df)

    df[["category"]] = as.factor(cut(
        df[["price"]],
        breaks=c(0, 80, 120, 180, 250, 350, 500, 1400),
        labels=c("D", "C", "CB", "B", "BA", "A", "A+")
    ))

    return (df)
}

df = carregar_dataframe()

gc()

print("Total de celulares:")
print(nrow(df))

print("Correlação entre câmera primária e câmera secundária:")
print(cor(df[["scamera"]], df[["pcamera"]]))

print("Correlação entre disco e memória RAM:")
print(cor(df[["storage"]], df[["memory"]]))

print("Correlação entre tela e câmera primária:")
print(cor(df[["display"]], df[["pcamera"]]))

print("Correlação entre tela e preço:")
print(cor(df[["display"]], df[["price"]]))

print("Correlação entre disco e preço:")
print(cor(df[["storage"]], df[["price"]]))

print("Correlação entre câmera primária e preço:")
print(cor(df[["pcamera"]], df[["price"]]))

print("Correlação entre memória RAM e preço:")
print(cor(df[["memory"]], df[["price"]]))

print("Categorias de preço:")
print(table(df[["category"]]))
