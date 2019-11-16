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

converter_de_camera = function (coluna) {
    coluna = sapply(coluna, function (x) {
        x = tolower(x)

        r = regmatches(x, regexec('(\\d+(?:\\.\\d+)?)\\smp', x))

        r = as.double(r[[1]][2])

        r = if (is.na(r)) 0 else r

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

carregar_dataframe = function () {
    dataset = read.csv(file='./phones.csv', header=TRUE, sep=',')

    df = data.frame(
        "storage"=converter_de_memoria(dataset$internal_memory),
        "memory"=converter_de_memoria(dataset$RAM),
        "pcamera"=converter_de_camera(dataset$primary_camera),
        "scamera"=converter_de_camera(dataset$secondary_camera),
        "display"=converter_de_tela(dataset$display_resolution),
        "os"=converter_de_os(dataset$OS)
    )

    df = na.omit(df)

    return (df)
}

df = carregar_dataframe()

gc()

plot(df$scamera~df$pcamera, ylim=c(0, 50), xlim=c(0, 50))
