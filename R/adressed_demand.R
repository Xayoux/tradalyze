
# Documentation -------------------------------------------------------------
#' @title
#' Calculer la demande adressée à chaque pays
#'
#' @description
#' Cacule la demande adressée à chaque pays pour chaque année. La
#' demande adressée peut être calculée par pays ou par groupement de produits.
#' Le résultat retourné peut être la valeur de la demande adressée, la demande
#' adressée en base 100 ou bien le ratio de la base 100 avec la base 100 d'un
#' pays de référence.
#'
#' @details
#' La demande adressée sert à observer l'évolution de la demande potentielle
#' adressée à un pays en prenant en compte le positionnement du pays sur
#' l'ensemble des marchés étudiés sur une année de référence. Elle est calculée
#' en sommant l'ensemble des importations de tous les pays sur un produit donné,
#' chaque valeur d'importation étant pondérée par la part que le pays représente
#' dans les exportations totale du pays étudié sur le produit donné.
#' La formule est la suivante :
#'
#' \eqn{DA_{it} = \sum_{jk}M_{jkt} \times \frac{X_{ijkt=0}}{X_{it=0}}}
#'
#' @param baci Chemin d'accès, dataframe ou format parquet des données de baci
#' à utiliser.
#' @param years Années à garder dans les données.
#' @param codes Codes à garder dans les données.
#' @param year_ref Année de référence pour le calcul de la demande adressée et
#' de la base 100.
#' @param var_exporter Variable contenant les exportateurs.
#' @param var_k Variable à utiliser pour le groupement des produits.
#' @param var_importer Variable à utiliser pour le groupement exportateurs.
#' Il est fortement recommandé de laisser "importer". 
#' @param exporter_ref Exportateur de référence pour le calcul du ratio de la
#' base 100.
#' @param base_100 Booléen indiquant si la demande adressée doit être calculée
#' en base 100 par rapport à l'année de référence.
#' @param compare Booléen indiquant si le ratio de la demande adressée doit être
#' calculé par rapport à un pays de référence.
#' @param return_output Booléen indiquant si le résultat doit être retourné.
#' @param return_pq Booléen indiquant si le résultat doit être retourné en format
#' parquet.
#' @param path_output Chemin d'accès pour enregistrer le résultat. Peut être
#' en format csv ou parquet.
#'
#' @return Un dataframe contenant la demande adressée.
#' @export
#'
#' @examples # Pas d'exemples.
# Fonction adressed_demand -------------------------------------------------
## Définition de la fonction -----------------------------------------------
adressed_demand <- function(baci, years = NULL, codes = NULL, year_ref, var_exporter,
                            var_k, var_importer = "importer",
                            exporter_ref = NULL, base_100 = TRUE,
                            compare = FALSE,
                            return_output = TRUE, return_pq = FALSE,
                            path_output = NULL){

## Importation des données------------------------------------------------
  # Ouvrir les données de BACI
  if (is.character(baci) == TRUE){
    # Ouvrir les données depuis un dossier parquet (si chemin c'est parquet)
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

## Calcul de la demande adressée -----------------------------------------
  # Agréger le commerce
  baci <-
    baci  |>
    dplyr::summarize(
      .by = c(t, {{var_exporter}}, {{var_k}}, {{var_importer}}),
      v = sum(v, na.rm = TRUE)
    )

  
  # Calcul des poids de chaque marché pour tous les exportateurs
  # les poids sont déterminés sur une année uniquement
  # Permet la comparaison dans le temps à partir d'une situation de départ
  df_poids <-
    baci |>
    # Les poids ne sont calculé que par rapport à l'année de référence
    dplyr::filter(
      t == year_ref
    ) |>
    dplyr::collect() |>
    # Calculer ce que le marché (pays-produit) représente dans les exportation du pays
    # par catégories de var_k
    dplyr::mutate(
      .by = c({{var_exporter}}, {{var_k}}),
      poids = v / sum(v, na.rm = TRUE)
    ) |>
    dplyr::select({{var_k}}, poids, {{var_exporter}}, {{var_importer}})


  # Calculer la demande adressée pour chaque pays, chaque année
  df_da <-
    baci |>
    dplyr::collect() |>
    # Calcul du total des imports de chaque importateur pour chaque produit
    dplyr::summarize(
      .by = c({{var_k}}, t, {{var_importer}}),
      total_import_jk = sum(v, na.rm = TRUE)
    ) |>
    # Joindre les poids calculés précédemment un poids par importer-k
    dplyr::left_join(
      df_poids,
      dplyr::join_by({{var_k}}, {{var_importer}}),
      relationship = "many-to-many"
    ) |>
    # Si pas de poids = pas d'exports vers le pays = 0
    dplyr::mutate(poids = tidyr::replace_na(poids, 0)) |>
    # pour chaque exporter-t-produit : somme des poids * total_import_k
    dplyr::summarize(
      .by = c({{var_exporter}}, t, {{var_k}}),
      DA = sum(poids * total_import_jk, na.rm = TRUE)
    ) |>
    dplyr::filter(!is.na({{var_exporter}}))

  ## Base 100 ---------------------------------------------------------------
  if (base_100 == TRUE){
    # Isoler la demande adressée de l'année de référence
    # Sert à calculer la base 100
    df_da_2010 <-
      df_da |>
      dplyr::filter(t == year_ref) |>
      # Supprimer lan variable t, pour que le join aille sur toutes les lignes
      dplyr::select(-t) |>
      dplyr::rename(DA_2010 = DA)


    # Caculer la demande adressée en base 100 par rapport à l'année de référence
    df_da <-
      df_da |>
      dplyr::left_join(
        df_da_2010,
        dplyr::join_by({{var_exporter}}, {{var_k}})
      )  |>
      dplyr::mutate(
        DA_100 = DA / DA_2010 * 100
      )

    ## Comparaison avec un pays de référence ---------------------------------
    if (compare == TRUE){
      # Isoler la demande adressée de l'exportateur de référence
      df_da_100_exporter_ref <-
        df_da|>
        dplyr::filter(!!dplyr::sym(var_exporter) == exporter_ref) |>
        dplyr::select(t, {{var_k}}, DA_100) |>
        dplyr::rename(DA_100_exporter_ref = DA_100)

      # Calculer le ratio entre les et les DA du pays de ref
      df_da <-
        df_da |>
        dplyr::filter(!!dplyr::sym(var_exporter) != exporter_ref) |>
        dplyr::left_join(
          df_da_100_exporter_ref,
          dplyr::join_by({{var_k}}, t)
        ) |>
        dplyr::mutate(
          DA_diff = DA_100 / DA_100_exporter_ref
        ) |>
        dplyr::filter(!is.na(DA_diff))
    }
  }


  ## Exportation des résultats -----------------------------------------------
  # Retourner le résultat
 if (return_output == TRUE){
   if (return_pq == TRUE){
     df_da <-
       df_da |>
       arrow::arrow_table()
   }

   return(df_da |>  dplyr::collect())
 }

  # Enregistrer le résultat
  if (!is.null(path_output)){
    if (tools::file_ext(path_output) == "csv"){
      df_da |>
        readr::write_csv(path_output)
    }
    else if (tools::file_ext(path_output) == "pq"){
      df_da |>
        arrow::write_parquet(path_output)
    }
  }


}
