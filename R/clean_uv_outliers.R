# Fonction ----------------------------------------------------------------
#' @title
#' Déterminer et filtrer les outliers des valeurs unitaires dans BACI
#'
#' @description
#' Détermine les outliers dans la distribution des valeurs unitaires de BACI
#' et peut les retirer du jeu de données afin d'essayer d'éviter des biais
#' dans les mesures.
#'
#' @details
#' Les valeurs unitaires calculées à partir de BACI peuvent contenir des
#' outliers assez importants. En effet, les valeurs unitaire sont calculées
#' en divisant la valeur des exportations par la quantité exportée. Ces
#' dernières peuvent être sujettes à un certain nombre d'erreurs et leur
#' fiabilité n'est pas toujours garantie.
#'
#' Pour éviter que ces valeurs ne biaisent les analyses, il peut être recommandé
#' de supprimer les outliers de la distribution des valeurs unitaires. Cette
#' fonction permet de déterminer les outliers selon quatre méthodes différentes:
#'
#' - La méthode 'classic' détermine les outliers en fonction des quantiles de
#' la distribution des valuers unitaires par année-produit. Si la valeur
#' unitaire est supérieure au quantile déterminé par `seuil_H` ou inférieure
#' au quantile déterminé par `seuil_L`, alors elle est considérée comme un
#' outlier.
#'
#' - La méthode 'fh13' qui provient de l'article de
#' [Fontagné & Hatte (2013)](https://pse.hal.science/hal-00959394/) détermine
#' les outliers en fonction de la différence entre la valeur unitaire et la
#' moyenne des valeurs unitaires par produit (de toute la période). Si la
#' différence est supérieure au quantile déterminé par `seuil_H` ou inférieure
#' au quantile déterminé par `seuil_L`, alors elle est considérée comme un
#' outlier. La distribution des différences peut être celle de toutes les
#' valeurs unitaires ou celle des valeurs unitaires par produit selon le
#' paramètre `whole`.
#'
#' - La méthode 'h06' qui provient de l'article de
#' [Hallak (2006)](https://www.sciencedirect.com/science/article/abs/pii/S0022199605000516)
#' détermine les outliers en fonction de la moyenne des valeurs unitaires par
#' produit, exportateur et année. Si la valeur unitaire est supérieure à la
#' moyenne multipliée par `seuil_H` ou inférieure à la moyenne divisée par
#' `seuil_L`, alors elle est considérée comme un outlier.
#'
#' - la méthode 'sd' détermine les outliers comme étant les flux dont la
#' différence entre la valeur unitaire et la moyenne produit-année est
#' supérieure à un certain nombre de fois l'écart-type de cette différence, ou
#' inférieure un nombre de fois à l'opposé de l'écart-type de cette différence.
#'
#' La fonction permet de garder ou non les outliers dans les données.
#'
#'
#'
#' @param baci Peut être un  chemin d'accès vers le dossier contenant
#' les données de BACI au format parquet. Peut également être un dataframe ou
#' bien des données au format arrow (requête ou non) permettant ainsi de chaîner
#' les opérations entre elles. ce paramètre est obligatoire.
#' @param years Années à garder dans les données. Si NULL, toutes les années
#' sont gardées.
#' @param codes Codes à garder dans les données. Si NULL, tous les codes sont
#' gardés.
#' @param method Méthode de détermination des outliers. Peut être 'classic',
#' 'fh13', 'h06', ou 'sd'.
#' @param seuil_H Seuil supérieur pour la détermination des outliers. Doit
#' être compris entre 0 et 1 pour les méthodes 'classic' et 'fh13'.
#' @param seuil_L Seuil inférieur pour la détermination des outliers. Doit
#' être compris entre 0 et 1 pour les méthodes 'classic' et 'fh13'.
#' @param visualisation Booléen pour déterminer si les données exportées
#' doivent contenir les outliers ainsi que les variables pour les déterminer
#' ou non.
#' @param path_output Chemin vers le dossier où exporter les données en format
#' parquet. Si NULL, les données ne sont pas exportées.
#' @param return_output Booléen pour déterminer si les données doivent être
#' retournées.
#' @param return_pq Booléen pour indiquer si les données doivent être retournées
#' en format arrow si TRUE. Par défaut : FALSE.
#'
#'
#' @return Un dataframe arrow contenant les données nettoyées des outliers.
#' @export
#'
#' @examples # Pas d'exemples.
#' @source [Lionel Fontagné, Sophie Hatte. European High-End Products in International Competition. 2013.](https://pse.hal.science/hal-00959394/)
#' @source [Hallak, J. C. (2006). Product quality and the direction of trade. Journal of international Economics, 68(1), 238-265.](https://www.sciencedirect.com/science/article/abs/pii/S0022199605000516)
clean_uv_outliers <- function(baci, years = NULL, codes = NULL,
                              method = "classic", seuil_H, seuil_L,
                              visualisation = FALSE, path_output = NULL,
                              return_output = FALSE, return_pq = FALSE) {

  # Messages d'erreurs si mauvais paramètres --------------------------------
  # Message d'erreur si years n'est pas NULL ou un vecteur de numériques
  if (!is.null(years) & !is.numeric(years)) {
    stop("years doit \uEAtre NULL ou un vecteur de num\uE9riques")
  }

  # Message d'erreur si codes n'est pas NULL ou un vecteur de chaînes de caractères
  if (!is.null(codes) & !is.character(codes)) {
    stop("codes doit \uEAtre NULL ou un vecteur de cha\uEEnes de caract\uE8res")
  }

  # Message d'erreur si method n'est pas un des trois choix possibles
  if (!method %in% c("classic", "fh13", "h06", "sd")) {
    stop("'method' doit \uEAtre 'classic', 'fh13', 'h06', ou 'sd'")
  }

  # Message d'erreur si seuil_H n'est pas un numérique
  if (!is.numeric(seuil_H)) {
    stop("seuil_H doit \uEAtre un num\uE9rique")
  }

  # Message d'erreur si seuil_L n'est pas un numérique
  if (!is.numeric(seuil_L)) {
    stop("seuil_L doit \uEAtre un num\uE9rique")
  }

  # Message d'erreur si seuil_H est inférieur à seuil_L pour tout sauf 'sd'
  if (method != "sd" & seuil_H < seuil_L) {
    stop("seuil_H doit \uEAtre sup\uE9rieur \uE0 seuil_L")
  }

  # Message d'erreur si seuil_H est négatif
  if (seuil_H < 0) {
    stop("seuil_H doit \uEAtre positif")
  }

  # Message d'erreur si seuil_L est négatif
  if (seuil_L < 0) {
    stop("seuil_L doit \uEAtre positif")
  }

  # Message d'erreur si Seuil_H n'est pas compris entre 0 et 1 si method != "h06"
  if (!method %in% c("h06", "sd") & (seuil_H < 0 | seuil_H > 1)) {
    stop("seuil_H doit \uEAtre compris entre 0 et 1 pour l'utilisation des m\uE9thodes 'classic' et 'fh13'")
  }

  # Message d'erreur si Seuil_L n'est pas compris entre 0 et 1 si method != "h06"
  if (!method %in% c("h06", "sd") & (seuil_L < 0 | seuil_L > 1)) {
    stop("seuil_L doit \uEAtre compris entre 0 et 1 pour l'utilisation des m\uE9thodes 'classic' et 'fh13'")
  }

  # Message d'erreur si visualisation n'est pas un booléen
  if (!is.logical(visualisation)) {
    stop("visualisation doit \uEAtre un bool\uE9en")
  }

  # Message d'erreur si path_output n'est pas une chaîne de caractère
  if (!is.null(path_output) & !is.character(path_output)) {
    stop("path_output doit \uEAtre NULL ou une cha\uEEne de caract\uE8re")
  }

  # Message d'erreur si return_output n'est pas un booléen
  if (!is.logical(return_output)) {
    stop("return_output doit \uEAtre un bool\uE9en")
  }

  # Message d'erreur si return_pq n'est pas un booléen
  if (!is.logical(return_pq)) {
    stop("return_pq doit \uEAtre un bool\uE9en")
  }

  # Message d'avertissement si return_output = FALSE et return_pq = TRUE
  if (return_output == FALSE & return_pq == TRUE){
    message("Les donn\uE9es ne seront pas retourn\uE8es car return_output = FALSE")
  }


  # Préparation des données -------------------------------------------------
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

  # Ne garder que les années sélectionnées (s'il y a une sélection)
  if (!is.null(years)) {
    df_baci <-
      df_baci |>
      dplyr::filter(t %in% years)
  }

  # Ne garder que les codes sélectionnés (s'il y a une sélection)
  if (!is.null(codes)) {
    df_baci <-
      df_baci |>
      dplyr::filter(k %in% codes)
  }

  # Caluler les valeurs unitaires pour chaque flux
  df_baci <-
    df_baci |>
    dplyr::mutate(
      uv = v / q
    )


  # Détermination des outliers ----------------------------------------------
  # Une vu est un outlier si elle est supérieure ou inférieure aux quantiles
  # déterminés par 'seuil_H' et 'seuil_L' selon la distribution des vu
  # par années-produit.
  if (method == "classic"){
    df_baci <-
      df_baci |>
      # Passer les données en format R -> calcul des quantiles
      dplyr::collect() |>
      # Définir les outliers par année-produit
      dplyr::mutate(
        .by = c(t, k),
        extreme =
          dplyr::case_when(
            uv >= quantile(uv, seuil_H, na.rm = TRUE) ~ 1,
            uv <= quantile(uv, seuil_L, na.rm = TRUE) ~ -1
          )
      )
  }

  # Une valeur unitaire est un outlier si sa différence avec la moyenne des
  # valeurs unitaires (par produit) se trouve en dehors des quantiles déterminés
  # par 'seuil_H' et 'seuil_L'. La distribution choisie dépend du paramètre
  # 'whole'. S'il est TRUE alors la distribution est celle de toutes les valeurs
  # unitaires, sinon elle est celle des valeurs unitaires par produit.
  if (method == "fh13"){
    df_baci <-
      df_baci |>
      # Passer les données en format R -> calcul des quantiles / moyenne
      dplyr::collect() |>
      # Calculer la différence entre la valeur unitaire et la moyenne par produit
      dplyr::mutate(
        .by = k,
        mean_diff = uv - mean(uv, na.rm = TRUE)
      ) |>
        dplyr::mutate(
          extreme =
            dplyr::case_when(
              mean_diff >= quantile(mean_diff, seuil_H, na.rm = T) ~ 1,
              mean_diff <= quantile(mean_diff, seuil_L, na.rm = T) ~ -1
            )
        )

    # Enlever les variables calculées si visualisation est FALSE
    if (visualisation == FALSE){
      df_baci <-
        df_baci |>
        dplyr::select(!c(mean_diff))
    }
  }

  # Une valeur unitaire est définie comme étant un outlier si elle est
  # supérieure ou inférieure à la moyenne des vu (produit, exportateur, année)
  # multipliée ou divisée par 'seuil_H' ou 'seuil_L' respectivement.
  if (method == "h06"){
    df_baci <-
      df_baci |>
      # Passer les données en format R -> calcul des moyennes
      dplyr::collect() |>
      # Définir les outliers par produit, exportateur et année
      dplyr::mutate(
        .by = c(k, i, t),
        extreme =
          dplyr::case_when(
            uv > mean(uv, na.rm = TRUE) * seuil_H ~ 1,
            uv < mean(uv, na.rm = TRUE) / seuil_L ~ -1
          )
      )
  }

  if (method == "sd"){
    df_baci <-
      df_baci |>
      dplyr::collect() |>
      dplyr::mutate(
        .by = c(k, t),
        mean_diff = uv - mean(uv, na.rm = TRUE),
        chapter = substr(k, 1, 2)
      ) |>
      dplyr::mutate(
        .by = c(t,chapter),
        extreme =
          dplyr::case_when(
            mean_diff > seuil_H * stats::sd(mean_diff, na.rm = TRUE) ~ 1,
            mean_diff < - seuil_L * stats::sd(mean_diff, na.rm = TRUE) ~ -1
          ),
        ecart_type = stats::sd(mean_diff, na.rm = TRUE)
      )

    if (visualisation == FALSE){
      df_baci <-
        df_baci |>
        dplyr::select(!c(mean_diff))
    }
  }

  # Exportation des données -------------------------------------------------
  # Retourner les données en format arrow
  df_baci <-
    df_baci |>
    arrow::arrow_table()

  # Enlever les variables calculées si visualisation est FALSE
  # Enlever les outlier si visualisation est FALSE
  if (visualisation == FALSE){
    df_baci <-
      df_baci |>
      dplyr::filter(is.na(extreme)) |>
      dplyr::select(!c(uv, extreme))
  }

  # Exporter les données en parquet
  if (!is.null(path_output)){
    df_baci |>
      dplyr::group_by(t) |>
      arrow::write_dataset(path_output)
  }

  # Retourner les données en format data.frame
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
