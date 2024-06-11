#' @title
#' Calculer des parts de marché
#'
#' @description
#' Fonction qui permet de calculer les parts de marché des exportateurs pour
#' chaque produit / couple produit-importateur. Cette utilisation est celle qui
#' a été pensé à la création de la fonction. Mais elle peut être utilisée pour
#' calculer les parts de marché des importateurs pour chaque produit par exemple.
#' La définition de la part de marché (de qui, pour qui...) dépend des noms de
#' colonnes entrées en argument de la fonction.
#'
#' @details
#' La fonction va agréger les flux pour chaque couple année-produit-`summarize_v`
#' puis à partir de cette agrégation calculer les parts de marché. De base, ces
#' parts sont calculées pour chaque produit-année. Mais avec le paramètre `by`,
#' il est possible de changer la définition de la part de marché. Par exemple,
#' si `by = "importer"`, les parts de marché seront calculées pour chaque
#' année-produit-importateur. Elle sera alors interprétée comme étant la part
#' de marché de l'exportateur sur le marché du produit pour cet importateur.
#'
#' Les parts de marchés sont de base pensées et calculées avec les exportateurs
#' étant les pays définis par les variables `i` ou `exporteur`. Mais si la
#' variable d'intérêt est une variable de groupement régional par exemple,
#' il suffit de rentrer son nom pour que les parts de marché soient calculées.
#'
#' Cependant, les parts de marché ne peuvent être calculées que sur un 'niveau'
#' après le produit : des parts de marché calculées au niveau
#' produit-région-importateur ne sont pas possibles. De même, les parts de
#' marché prennent toujours en compte la dimmension temps et produit.
#'
#' Cette fonction utilise les fonctionnalité du package
#' [arrow](https://arrow.apache.org/docs/r/) pour performer des calculs sans
#' avoir à charger BACI en mémoire. Cependant le calcul de la médiane
#' pondérée nécessite le passage de la base (uniquement la partie nécessaire)
#' en mémoire. Si la base est trop importante, les calculs peuvent prendre un
#' certain temps, voir entraîner un problème de mémoire de l'ordinateur. Si
#' cela arrive, il est conseillé de réduire le nombre d'années sur lesquelles
#' la fonction doit calculer les gammes et d'exécuter plusieurs fois la fonction
#' jusqu'à avoir toutes les années voulues.
#'
#' @param baci Peut être un  chemin d'accès vers le dossier contenant
#' les données de BACI au format parquet. Peut également être un dataframe ou
#' bien des données au format arrow (requête ou non) permettant ainsi de chaîner
#' les opérations entre elles. ce paramètre est obligatoire.
#' @param summarize_k Nom de la variable "produit" à partir de
#' laquelle sera calculée la part de marché. Par défaut c'est la variable `k`
#' (le code produit HS6) qui sera utilisé. On peut l'utiliser également pour une
#' variable "chapitre" ou "secteur" par exemple.
#' @param summarize_v Nom de la variable ( en chaîne de caractère) sur laquelle
#' l'agrégation des flux va s'effectuer. Par défaut, c'est la variable
#' `exporteur` qui est prise en compte.
#' @param by Nom de la variable (en chaîne de caractère) sur laquelle les parts
#' de marché peuvent être calculées à un niveau plus fin. Par défaut, les parts
#' de marché sont calculées au niveau année-produit. Ce paramètre permet de
#' rajouter une dimmension, qui sera classiquement `importeur`.
#' @param seuil Valeur numérique entre 0 et 100 qui permet de filtrer les
#' données dans le dataframe en fonction de la part de marché. Seules les
#' observations ayant une part de marché supérieure ou égale à ce seuil seront
#' gardées. Par défaut, le seuil est de 0 : toutes les observations sont
#' gardées.
#' @param years Vecteur de numériques qui permet de filtrer les données en
#' fonction des années. Par défaut, toutes les années sont gardées.
#' @param codes Vecteur de chaînes de caractères qui permet de filtrer les
#' données en fonction des codes produits. Par défaut, tous les codes sont
#' gardés.
#' @param path_output Chemin d'accès vers le fichier où les données seront
#' sauvegardées. Par défaut, les données ne sont pas sauvegardées. Les formats
#' possibles sont .csv, .xlsx et .parquet.
#' @param return_output Booléen qui permet de retourner les données. Par défaut, les
#' données ne sont pas retournées.
#' @param return_pq Booléen pour indiquer si les données doivent être retournées
#' en format arrow si TRUE. Par défaut : FALSE.
#'
#' @return Un dataframe avec les parts de marché calculées.
#' @export
#'
#' @examples # Pas d'exemple.
market_share <- function(baci, summarize_k = "k",
                         summarize_v = "exporter", by = NULL,
                         seuil = 0, years = NULL, codes = NULL,
                         path_output = NULL, return_output = FALSE,
                         return_pq = FALSE){

  # Messages d'erreur -------------------------------------------------------
  # Erreur si seuil est inférieur à 0
  if(seuil < 0 | seuil > 100){
    stop("seuil doit \uEAtre sup\uE9rieur ou \uE9gal \uE0 0 et inf\uE9rieur ou \uE9gal \uE0 100.")
  }

  # Erreur si seuil n'est pas un numérique
  if(!is.numeric(seuil)){
    stop("seuil doit \uEAtre un num\uE9rique.")
  }

  # Erreur si years n'est pas NULL ou une liste de numériques
  if(!is.null(years) & !is.numeric(years)){
    stop("years doit \uEAtre NULL ou une liste de num\uE9riques.")
  }

  # Erreur si codes n'est pas NULL ou une liste de chaînes de caractères
  if(!is.null(codes) & !is.character(codes)){
    stop("codes doit \uEAtre NULL ou une liste de cha\uEEnes de caract\uE8res.")
  }

  # Erreur si path_output n'est pas NULL ou une chaîne de caractères
  if(!is.null(path_output) & !is.character(path_output)){
    stop("path_output doit \uEAtre NULL ou une cha\uEEne de caract\uE8res.")
  }

  # Erreur si path_output n'est pas un fichier .csv ou .xlsx ou parquet
  if(!is.null(path_output)){
    if(!tools::file_ext(path_output) %in% c("csv", "xlsx", "parquet")){
      stop("path_output doit \uEAtre un fichier '.csv', '.xlsx' ou '.parquet'.")
    }
  }

  # Erreur si return_output n'est pas un booléen
  if(!is.logical(return_output)){
    stop("return_output doit \uEAtre un bool\uE9en.")
  }

  # Message d'erreur si return_pq n'est pas un booléen
  if (!is.logical(return_pq)) {
    stop("return_pq doit \uEAtre un bool\uE9en")
  }

  # Message d'avertissement si return_output = FALSE et return_pq = TRUE
  if (return_output == FALSE & return_pq == TRUE){
    message("Les donn\uE9es ne seront pas retourn\uE8es car return_output = FALSE")
  }


  # Calcul de parts de marché -----------------------------------------------
  # Ouvrir les données de BACI
  if (is.character(baci) == TRUE){
    # Ouvrir les données depuis un dossier parquet
    df_baci <-
      baci |>
      arrow::open_dataset()
  }
  else if (is.data.frame(baci) == TRUE){
    # Ouvrir les données depuis un dataframe : passage en format arrow
    df_baci <-
      baci |>
      arrow::arrow_table()
  }
  else{
    # Ouvrir les données depuis format arrow : rien à faire
    df_baci <- baci
  }

  # Garder les années voulues si years != NULL
  if(!is.null(years)){
    df_baci <-
      df_baci |>
      dplyr::filter(t %in% years)
  }

  # Garder les codes voulus si codes != NULL
  if(!is.null(codes)){
    df_baci <-
      df_baci |>
      dplyr::filter(k %in% codes)
  }

  # Calculer les parts de marché
  if (is.null(by)){
    df_baci <-
      df_baci |>
      # Somme les valeurs et quantités de chaque exportateur pour chaque produit
      dplyr::summarize(
        .by = c(t, {{summarize_k}}, {{summarize_v}}),
        v = sum(v, na.rm = TRUE),
        q = sum(q, na.rm = TRUE)
      ) |>
      dplyr::collect() |>
      dplyr::mutate(
        .by = c(t, {{summarize_k}}),
        market_share = v / sum(v, na.rm = TRUE) * 100
      ) |>
      #arrow::arrow_table() |>
      dplyr::arrange(t, !!dplyr::sym(summarize_k), !!dplyr::sym(summarize_v)) |>
      dplyr::filter(market_share >= seuil)
  }
  else {
    df_baci <-
      df_baci |>
      # Somme les valeurs et quantités de chaque exportateur pour chaque produit
      dplyr::summarize(
        .by = c(t, {{summarize_k}}, {{summarize_v}}, {{by}}),
        v = sum(v, na.rm = TRUE),
        q = sum(q, na.rm = TRUE)
      ) |>
      dplyr::collect() |>
      dplyr::mutate(
        .by = c(t, {{summarize_k}}, {{summarize_v}}),
        market_share = v / sum(v, na.rm = TRUE) * 100
      ) |>
      arrow::arrow_table() |>
      dplyr::arrange(t, !!dplyr::sym(summarize_k), !!dplyr::sym(summarize_v), !!dplyr::sym(by)) |>
      dplyr::filter(market_share >= seuil)
  }

  # Sauvegarder les données si path_output != NULL
  # Sauvegarder en .xslx si path_output est un chemin .xlsx
  if(!is.null(path_output)){
    if(tools::file_ext(path_output) == "xlsx"){
      df_baci <-
        df_baci |>
        dplyr::collect()

      openxlsx::write.xlsx(df_baci, path_output, row.names = FALSE)
    }
    else if(tools::file_ext(path_output) == "csv"){
      readr::write_csv(df_baci, path_output)
    }
    else if(tools::file_ext(path_output) == "parquet"){
      arrow::write_dataset(df_baci, path_output, format = "parquet")
    }
  }

  # Retourner les données si return = TRUE
  if (return_output == TRUE){
    if (return_pq == TRUE){
      return(df_baci)
    }
    else{
      df_baci <-
        df_baci |>
        dplyr::collect()

      return(df_baci)
    }
  }
}
