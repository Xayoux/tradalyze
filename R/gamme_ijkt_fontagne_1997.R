#' @title
#' Calcul de gamme de valeurs unitaires selon la méthode de Fontagné, Freudenberg
#' et Péridy (1997)
#'
#' @description
#' Détermine la gamme à laquelle chaque flux appartient grâce à sa valeur
#' unitaire. Les trois gammes possibles sont Low (L), Medium (M) et High (H).
#' La méthode est basée sur la comparaison des valeurs unitaires avec la médiane
#' pondérée par la valeur commerciale des valeurs unitaires pour chaque marché
#' (produit-pays).
#'
#' @details
#' Les gammes sont déterminées de la façon suivante : la valeur unitaire de
#' chaque flux élémentaire est comparée à la médiane pondérée par la valeur
#' commerciale des valeurs unitaires pour chaque marché (produit-pays).
#' Si la valeur unitaire est supérieure à (1 + `alpha_H`) fois la médiane
#' pondérée, le flux est considéré comme High (H).
#' Si la valeur unitaire est inférieure à (1 / `alpha_L`) fois la médiane
#' pondérée, le flux est considéré comme Low (L). Sinon, le flux est
#' considéré comme Medium (M).
#'
#' Selon cette méthodologie, les seuils basiques sont `alpha_H` = 1.15
#' et `alpha_L` = 1.15 (Un écart de 15% à la médianne pondérée).
#'
#' Cette fonction permet de performer le calcul de gammes pour différents
#' seuils de manière simultanée. Selon le paramètre `pivot`, le format de sortie
#' sera différent. En cas de format 'longer', le dataframe de sortie contiendra
#' une variable pour le seuil H, une pour le seuil L et une pour la gamme. Les
#' observations seront égales au nombre d'observation sélectionnées multipliées
#' par le nombre de seuils. Il faut donc faire attention à ne pas calculer
#' directement sur toutes les observations mais à bien trier selon les seuils
#' voulus. En cas de format 'wider', le dataframe de sortie contiendra une
#' variable gamme par seuil (avec les seuils indiqués dans le nom des variables)
#' et une observation unique par flux.
#'
#' Cette dynamique des noms permet d'utiliser les données parquet créées pour
#' effectuer d'autres calculs de gammes (à l'exception des gammes de
#' fontagné et al 2007).
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
#' @param alpha_H Seuil pour déterminer les gammes hautes. Par défaut, 1.15
#' (uv > 1.15 * medf_ref). Peut être un vecteur de numériques.
#' @param alpha_L Seuil pour déterminer les gammes basses. Par défaut, 1.15
#' (uv < 1/0.85 * medf_ref). Peut être un vecteur de numériques.
#' Doit avoir le même nombre de valeurs que alpha_H.
#' @param years Les années à considérer (un vecteur de numériques). Par défaut,
#' toutes les années sont prises en compte.
#' @param codes Les codes des produits à considérer (un vecteur de chaînes de
#' caractères). Par défaut, tous les produits sont pris en compte.
#' @param pivot Un caractère qui indique si le format de sortie doit être
#' 'longer' ou 'wider'. Par défaut, 'longer'. Un format longer indique que le
#' dataframe de sortie contient un variable pour le seuil H, une pour le seuil
#' L et une pour la gamme. Les observations sont égales au nombre d'observation
#' sélectionnées multipliées par le nombre de seuils. Un format wider indique
#' que le dataframe de sortie contient une variable par seuil et une observation
#' unique par flux.
#' @param return_output Un booléen qui permet de retourner le résultat de la
#' fonction. Par défaut, la fonction ne retourne rien.
#' @param path_output Chemin vers le dossier où le résultat de la fonction doit
#' être stocké en format parquet par année. Par défaut, le résultat n'est
#' pas stocké.
#' @param remove Un booléen qui permet de supprimer tous les fichiers commençant
#' par t= dans le path_output s'il est non nul. Par défaut, FALSE.
#' Evite les confusions si plusieurs utilisations dans le même dossier.
#' @param return_pq Booléen pour indiquer si les données doivent être retournées
#' en format arrow si TRUE. Par défaut : FALSE.
#'
#' @return Un dataframe / dossier parquet contenant les données de la base BACI
#' avec les gammes calculées.
#' \describe{
#'   \item{i}{Code iso numérique de l'importateur}
#'   \item{j}{Code iso numérique de l'exportateur}
#'   \item{k}{Code HS6 du produit (en chaîne de caractère)}
#'   \item{t}{Année}
#'   \item{v}{Valeur totale du flux en milliers de dollars courants}
#'   \item{q}{Quantité du flux en tonnes métriques}
#'   \item{exporter}{Code iso3 de l'exportateur}
#'   \item{importer}{Code iso3 de l'importateur}
#'   \item{uv}{Valeur unitaire du flux en milliers de dollars courants/tonne
#'   métrique}
#'   \item{med_ref_t_k}{médiane pondérée par le commerce des valeurs unitaires
#'   des flux pour chaque groupe (t, k)}
#'   \item{gamme_fontagne_1997_...}{Gamme de la valeur unitaire du flux
#'   (H, M, L)}
#'   \item{seuil_H_}{Seuil pour déterminer les gammes hautes}
#'   \item{seuil_L_}{Seuil pour déterminer les gammes basses}
#' }
#' @export
#'
#' @examples # Pas d'exemples.
#' @source [Fontagné, L., Freudenberg, M., & Péridy, N. (1997). Trade patterns inside the single market (No. 97-07). Paris: CEPII.](http://cepii.fr/PDF_PUB/wp/1997/wp1997-07.pdf)
gamme_ijkt_fontagne_1997 <- function(baci, alpha_H = 1.15,
                                     alpha_L = alpha_H,
                                     years = NULL, codes = NULL,
                                     pivot = "longer", return_pq = FALSE,
                                     return_output = FALSE, path_output = NULL,
                                     remove = FALSE){


  # Définition des messages d'erreur ----------------------------------------
  # Message d'erreur si alpha_H ou alpha_L ne sont pas des numériques
  if(!is.numeric(alpha_H) | !is.numeric(alpha_L)){
    stop("alpha_H et alpha_L doivent \uEAtre des num\uE9riques.")
  }

  # Message d'erreur si alpha_L n'a pas le même nombre de valeurs que alpha_H
  if(length(alpha_L) != length(alpha_H)){
    stop("alpha_L doit avoir le m\uEAme nombre de valeurs que alpha_H.")
  }

  # Message d'erreur si years n'est pas NULL et n'est pas un vecteur de numériques
  if(!is.null(years) & !is.numeric(years)){
    stop("years doit \uEAtre NULL ou un vecteur de num\uE9riques.")
  }

  # Message d'erreur si codes n'est pas NULL et n'est pas un vecteur de chaînes de caractères
  if(!is.null(codes) & !is.character(codes)){
    stop("codes doit \uEAtre NULL ou un vecteur de cha\uEEnes de caract\uE8res.")
  }

  # Message d'erreur si return_output n'est pas un booléen
  if(!is.logical(return_output)){
    stop("return_output doit \uEAtre un bool\uE9en.")
  }

  # Message d'erreur si path_output n'est pas NULL et n'est pas une chaine de caractère
  if(!is.null(path_output) & !is.character(path_output)){
    stop("path_output doit \uEAtre NULL ou un chemin d'acc\uE8s sous forme de cha\uEEne de caract\uE8res.")
  }

  # Message d'erreur si remove n'est pas un booléen
  if(!is.logical(remove)){
    stop("remove doit \uEAtre un bool\uE9en.")
  }

  # Message d'erreur si return_pq n'est pas un booléen
  if (!is.logical(return_pq)) {
    stop("return_pq doit \uEAtre un bool\uE9en")
  }

  # Message d'avertissement si return_output = FALSE et return_pq = TRUE
  if (return_output == FALSE & return_pq == TRUE){
    message("Les donn\uE9es ne seront pas retourn\uE8es car return_output = FALSE")
  }


  # Si remove == TRUE alors supprimer tous les fichiers commençant par t= dans le path_output s'il est non nul
  if(remove == TRUE & !is.null(path_output)){
    # supprimer les dossier commençant par t= : les dossier parquet par année
    list.files(path_output, pattern = "t=", full.names = TRUE) |>
      unlink(recursive = TRUE)
  }




  # Filtrage des données ----------------------------------------------------
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


  # Définition des gammes ---------------------------------------------------

  # Sous-fonction pour calculer les gammes sur un seuil
  calc_gamme_func <- function(df_baci, alpha_H, alpha_L){
    df_baci <-
      df_baci |>
      # Calcul des valeurs unitaires + ajout des seuils
      dplyr::mutate(
        uv = v / q,
        alpha_H = alpha_H,
        alpha_L = alpha_L
      ) |>
      # Passage en mémoire pour le calcul de la médiane pondérée
      dplyr::collect() |>
      # Calcule de la médiane pondérée pour chaque couple (t,k)
      dplyr::mutate(
        .by = c(t, k),
        med_ref_t_k = matrixStats::weightedMedian(uv, w = v, na.rm = TRUE)
      ) |>
      # Retour en format arrow
      arrow::arrow_table() |>
      # Calcul des gammes en fonction du seuil choisit
      dplyr::mutate(
        gamme_fontagne_1997 =
          dplyr::case_when(
            uv > alpha_H * med_ref_t_k ~ "H",
            uv < (1 / (alpha_L)) * med_ref_t_k ~ "L",
            uv > (1 / (alpha_L)) * med_ref_t_k &  uv < alpha_H * med_ref_t_k ~ "M"
          )
      )

    # La fonction retourne le dataframe
    return(df_baci)
  }

  # Calculer les gammes pour chaque seuil (un df par seuil) : stockés dans une liste
  liste_df_baci <-
    # Pour chaque élément des vecteurs alpha, calculer les gammes
    purrr::map2(
      alpha_H,
      alpha_L,
      \(alpha_H, alpha_L) calc_gamme_func(df_baci, alpha_H, alpha_L)
    ) # Voir ça comme une boucle for

  # Concaténer les df de la liste (en format arrow donc pas list_rbind)
  while(length(liste_df_baci) > 1){
    # Tant qu'il y a plus d'un df dans la liste, joindre le 1er et le 2eme
    liste_df_baci[[1]] <-
      liste_df_baci[[1]] |>
      dplyr::full_join(liste_df_baci[[2]])

    # Supprimer le deuxième dataframe de la liste
    liste_df_baci[[2]] <- NULL
  }

  # Extraire le df final de la liste
  df_baci <-
    liste_df_baci[[1]]

  # Garder le format 'longer' et grouper par t, alpha_H, alpha_L pour l'exportation en parquet
  if (pivot == "longer"){
    df_baci <-
      df_baci |>
      dplyr::group_by(t, alpha_H, alpha_L)
  }

  # Pivoter en 'wider' pour avoir une colonne par gamme et grouper par t
  if (pivot == "wider"){
    df_baci <-
      df_baci |>
      # Passage en mémoire car pivot_wider ne fonctionne pas avec arrow
      dplyr::collect() |>
      tidyr::pivot_wider(
        names_from = c(alpha_L, alpha_H),
        values_from = c(gamme_fontagne_1997),
        names_prefix = "gamme_fontagne_1997_"
      ) |>
      arrow::arrow_table() |>
      dplyr::group_by(t)
  }



  # Enregistrer la nouvelle base en format parquet par année si path_output != NULL
  if(!is.null(path_output)){
    df_baci |>
      arrow::write_dataset(path_output, format = "parquet")
  }

  # Retourner le résultat si return_output == TRUE
  if (return_output == TRUE){
    if (return_pq == TRUE){
      df_baci <-
        df_baci |>
        ungroup()
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
