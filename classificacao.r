obter_melhor_knn = function (df, kinicial, kpasso) {
    df = subset(df, select=-c(os, preco))
    tamanho = floor(0.9 * nrow(df))
    indices_treino = sample(seq_len(nrow(df)), size=tamanho)
    colunas_manter = -which(names(df) %in% c('categoria'))

    df_treino = df[indices_treino, colunas_manter]
    df_teste = df[-indices_treino, colunas_manter]
    rotulos_treino = df[indices_treino, 'categoria']
    rotulos_teste = df[-indices_treino, 'categoria']

    k = kinicial
    best_k = NA
    best_cm = NA
    best_acc = 0

    warning_antigo <- getOption('warn')
    options(warn=-1)

    while (TRUE) {
        predicao = tryCatch(
            { class::knn(df_treino, df_teste, rotulos_treino, k) },
            error=function (err) { return (NA) }
        )

        if (is.na(predicao)) {
            break
        }

        cm = caret::confusionMatrix(predicao, rotulos_teste)
        acc = as.double(cm[['overall']][1])

        if (acc > best_acc) {
            best_acc = acc
            best_cm = cm
            best_k = k
        }

        k = k + kpasso
    }

    options(warn=warning_antigo)

    best_cm[['k']] = best_k

    return (best_cm)
}
