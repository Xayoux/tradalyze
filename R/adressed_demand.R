
# Documentation -------------------------------------------------------------
#' @title
#' Compute the Adressed Demand
#'
#' @description
#' Compute the demand adress at each exporter, each year by product. The
#' result can be in level or in base 100.
#'
#' @details
#' The adress demand allow to observe the evolution of a potential demand
#' adressed to a country, taking into account the initial positionnement
#' of the country.
#'
#' It is calculate by making the sum of the import for an importer and
#' weight it by the share this importer represent for the exporter. Then the
#' result for all importers is summed for each exporter.
#'
#' \eqn{DA_{it} = \sum_{jk}M_{jkt} \times \frac{X_{ijkt=0}}{X_{it=0}}}
#'
#' For this variable it is mandatory to have variables t and k in your dataframe
#' for the time and products.
#'
#' @param year_ref Numeric indicating the year of reference to compute the
#' adressed demand and the base 100.
#' @param var_exporter Character indicating the name of the variable containing
#' the exporters.
#' @param var_k Character indicating the name of the variable containing
#' the products/group-products.
#' @param var_importer Character indicating the name of variable containing
#' the importers. it is recommended to let "importer".
#' @param base_100 Logical indicating whether the adressed demand must be
#' transformed in base 100 (TRUE) or not (FALSE : the default). If TRUE two new
#' variables will be added :
#' \describe{
#'   \item{DA_year_ref}{Numeric : Value of the adressed demand in the
#' reference year defined by `year_ref` parameter.}
#'   \item{DA_100}{Numeric : Value of the adressed demand transformed
#' in base 100.}
#' }
#' @param compare Logical indicating if the base 100 should be compared (TRUE)
#' by a ratio, to the base 100 of a reference exporter, defined by the parameter
#' `exporter_ref`, or not (FALSE the default). If TRUE, two nex variables are
#' added
#' \describe{
#'   \item{DA_100_exporter_ref}{Numeric : Value of the adressed demand for the
#' reference exporter defined by `exporter_ref` parameter.}
#'   \item{DA_100_diff'}{Numeric : Value of the ratio between the base 100 and
#' the base 100 of the reference exporter. A value greater than 1 indicate that
#' the the variable for the exporter has increased more than the variable for
#' the reference exporter.}
#' }
#' @inheritParams .filter_baci
#' @inheritParams .export_data
#' @inheritParams add_chelem_classification
#' @inheritParams aggregate_compare
#'
#' @return BACI data with the following variables : variables contained in
#' `year_ref`, `var_exporter`, `var_k`, `var_importer`. There is also "DA" as new
#' variable. Depending on the value taken by `base_100` and
#' `compare` 2 or 4 more variables can be present. See the explications of these
#' parameters.
#'
#' @examples
#' ## Calculate the DA
#' ## adressed_demand(
#' ##   baci = "baci-parquet-folder",
#' ##   year_ref = 2010,
#' ##   var_exporter = "exporter",
#' ##   var_k = "k",
#' ##   base_100 = FALSE
#' ## )
#'
#' ## Calculate the DA and take the base 100 and comapre it to France
#' ## adressed_demand(
#' ##   baci = "baci-parquet-folder",
#' ##   year_ref = 2015,
#' ##   var_exporter = "exporter",
#' ##   var_k = "category",
#' ##   base_100 = TRUE,
#' ##   compare = TRUE,
#' ##   exporter_ref = "FRA"
#' ## )
#'
#' @seealso
#' [.load_data()] For more informations concerning the loading.
#' [.filter_baci()] For more informations concerning the filtering of data inside the function.
#' [.export_data()] For more informations concerning the export of the data inside the function.
#'
#' @export
adressed_demand <- function(baci, years = NULL, codes = NULL,
                            export_countries = NULL, import_countries = NULL,
                            year_ref, var_exporter,
                            var_k, var_importer = "importer",
                            base_100 = FALSE,
                            compare = FALSE, exporter_ref = NULL,
                            return_output = TRUE, return_arrow = TRUE,
                            path_output = NULL){

  # Check if year ref is a numeric and length 1
  tradalyze::.check_numeric(year_ref, "year_ref")
  tradalyze::.check_length_1(year_ref, "year_ref")

  # Check if var_exporter is character and length 1
  tradalyze::.check_character(var_exporter, "var_exporter")
  tradalyze::.check_length_1(var_exporter, "var_exporter")

  # Check if var_k is character and length 1
  tradalyze::.check_character(var_k, "var_k")
  tradalyze::.check_length_1(var_k, "var_k")

  # Check if var_importer is character and length 1
  tradalyze::.check_character(var_importer, "var_importer")
  tradalyze::.check_length_1(var_importer, "var_importer")

  # Check if base_100 is logical and length 1
  tradalyze::.check_logical(base_100 ,"base_100")
  tradalyze::.check_length_1(base_100, "base_100")

  # Check if compare is logical and length 1
  tradalyze::.check_logical(compare, "compare")
  tradalyze::.check_length_1(compare, "compare")

  if (compare == TRUE){
    # Check if exporter_ref is character and length 1
    tradalyze::.check_character(exporter_ref, "exporter_ref")
    tradalyze::.check_length_1(exporter_ref, "exporter_ref")
  }

  # Check validity of export parameters
  tradalyze::.export_data(
    data = NULL,
    return_output = return_output,
    return_arrow = return_arrow,
    path_output = path_output,
    eval = FALSE,
    collect = NULL
  )
  
  baci <-
    tradalyze::.load_data(baci) |>
    tradalyze::.filter_baci(
      years = years,
      codes = codes,
      export_countries = export_countries,
      import_countries = import_countries
    )

  # Check if t, k, v and other var are present in baci
  tradalyze::.check_var_exist(baci, "baci", c("t", "k", "v", var_exporter, var_k, var_importer))


  # Agréger le commerce par "Régions exportatrices", importer et produits HS6.
  # Permet une aggégration des exportateurs mais reste sur le niveau
  # le plus fin pour le reste.
  baci <-
    baci  |>
    dplyr::summarize(
      .by = c(t, k, {{var_exporter}}, {{var_importer}}, {{var_k}}),
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
    # Calculer ce que le marché (pays-produit) représente dans les exportation 
    # du pays/région par catégories de var_k : permet d'aggréger l'information
    dplyr::mutate(
      .by = c({{var_exporter}}, {{var_k}}),
      poids = v / sum(v, na.rm = TRUE)
    ) |>
    dplyr::select({{var_k}}, poids, {{var_exporter}}, {{var_importer}}, v, k)


  # Calculer la demande adressée pour chaque pays, chaque année
  df_da <-
    baci |>
    dplyr::collect() |>
    # Calcul du total des imports de chaque importateur pour chaque produit
    # rester au niveau le plus fin possible. 
    dplyr::summarize(
      .by = c(k, t, {{var_importer}}),
      total_import_jk = sum(v, na.rm = TRUE)
    ) |>
    # Joindre les poids calculés précédemment par importer-k. Niveau le + fin
    dplyr::left_join(
      df_poids,
      dplyr::join_by(k, {{var_importer}}),
      relationship = "many-to-many"
    ) |>
    # Si pas de poids = pas d'exports vers le pays = 0
    dplyr::mutate(poids = tidyr::replace_na(poids, 0)) |>
    # pour chaque exporter-t-produit : somme des poids * total_import_k
    # Obtenir l'information au niveau agrégé que l'on souhaite
    dplyr::summarize(
      .by = c({{var_exporter}}, t, {{var_k}}),
      DA = sum(poids * total_import_jk, na.rm = TRUE)
    ) |>
    dplyr::filter(!is.na({{var_exporter}}))

  if (base_100 == TRUE){
    # Isoler la demande adressée de l'année de référence
    # Sert à calculer la base 100
    df_da_year_ref <-
      df_da |>
      dplyr::filter(t == year_ref) |>
      # Supprimer lan variable t, pour que le join aille sur toutes les lignes
      dplyr::select(-t) |>
      dplyr::rename(DA_year_ref = DA)


    # Caculer la demande adressée en base 100 par rapport à l'année de référence
    df_da <-
      df_da |>
      dplyr::left_join(
        df_da_year_ref,
        dplyr::join_by({{var_exporter}}, {{var_k}})
      )  |>
      dplyr::mutate(
        DA_100 = DA / DA_year_ref * 100
      )

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
          DA_100_diff = DA_100 / DA_100_exporter_ref
        ) |>
        dplyr::filter(!is.na(DA_diff))
    }
  }

  tradalyze::.export_data(
    data = df_da,
    return_output = return_output,
    return_arrow = return_arrow,
    path_output = path_output,
    eval = TRUE,
    collect = TRUE
  )
}

utils::globalVariables(c("t", "k", "v", "poids", "total_import_jk", "DA",
                         "DA_year_ref", "DA_100", "DA_100_exporter_ref",
                         "DA_100_diff"))
