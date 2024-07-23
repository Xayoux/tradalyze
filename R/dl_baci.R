# ATENTION : si nouvelle version de correction(202401b par exemple), le b n'est pas
# compris dna sl'élément em donc on continue de télécharger la version non corrigée.
# Documentation -------------------------------------------------------------
#' @title
#' Télécharger BACI et la transformer en parquet
#'
#' @description
#' Fonction qui permet de télécharger BACI dans la nomenclature voulue
#' (uniquement dernière version disponible).
#' Les données sont ensuite transformées en fichiers parquet (1 par année).
#'
#' Si la fonction est lancée dans une session interactive, alors il sera
#' demandé à l'utilisateur de confirmer s'il souhaite télécharger les données.
#' Sinon le téélchargement s'effectuera automatiquement. 
#'
#' @param revision Une chaîne de caractères qui indique la nomenclature voulue
#' (HS92 par défaut). Valeurs possibles : HS92, HS96, HS02, HS07, HS12, HS17,
#' HS22.
#' @param dl_folder Un chemin d'accès vers le dossier où les données seront
#' téléchargées.
#' @param rm_csv Un booléen qui indique si les fichiers csv de BACI doivent
#' être supprimés après la transformation en parquet (TRUE par défaut).
#' @param dl_zip Un booléen qui indique si le fichier zip de BACI doit être
#' téléchargé (FALSE par défaut). Si TRUE, alors le zip sera forcément
#' téléchargé, si FALSE il ne sera téléchargé que s'il n'existe pas dans le
#' répertoire de téléchargement.
#'
#' @return Un dossier parquet par année. Les variables de BACI sont les
#' suivantes :
#' \describe{
#'  \item{i}{Code iso numérique de l'importateur}
#'  \item{j}{Code iso numérique de l'exportateur}
#'  \item{k}{Code HS6 du produit (en chaîne de caractère)}
#'  \item{t}{Année}
#'  \item{v}{Valeur totale du flux en milliers de dollars courants}
#'  \item{q}{Quantité du flux en tonne métrique}
#'  \item{exporter}{Code iso3 de l'exportateur}
#'  \item{importer}{Code iso3 de l'importateur}
#'  }
#' @export
#'
#' @examples # Pas d'exemple.
#'
#' @source Base [BACI](http://www.cepii.fr/CEPII/en/bdd_modele/bdd_modele_item.asp?id=37)
#' du [CEPII](http://www.cepii.fr/CEPII/en/welcome.asp).
# Fonction dl_baci ----------------------------------------------------------
## Définition de la fonction -----------------------------------------------
dl_baci <- function(revision = "HS92", dl_folder, rm_csv = TRUE,
                    dl_zip = FALSE){

  ## Messages d'erreurs + avertissements -------------------------------------
  # Créer le dossier s'il n'existe pas + message d'avertissement
  if (!dir.exists(dl_folder)) {
    dir.create(dl_folder, recursive = TRUE)
    message("Le dossier ", dl_folder, " a \uE9t\uE9 cr\uE9\uE9.")
  }

  # Erreur si revision n'est pas une des valeurs possibles
  if (!revision %in% c("HS92", "HS96", "HS02", "HS07", "HS12", "HS17", "HS22")) {
    stop("La valeur de 'revision' doit \uEAtre une des suivantes : HS92, HS96, HS02, HS07, HS12, HS17, HS22.")
  }

  # Erreur si dl_zip n'est pas un booléen
  if (!is.logical(dl_zip)) {
    stop("dl_zip doit \uEAtre un bool\uE9en.")
  }

  # Erreur si rm_csv n'est pas un booléen
  if (!is.logical(rm_csv)) {
    stop("rm_csv doit \uEAtre un bool\uE9en.")
  }

  ## Information sur BACI ----------------------------------------------------
  # Lien vers la page BACI du cepii
  html_baci <- rvest::read_html("http://www.cepii.fr/CEPII/en/bdd_modele/bdd_modele_item.asp?id=37")

  # Récupérer l'information sur la dernière version de BACI
  version <-
    html_baci |>
    rvest::html_nodes(xpath = '//*[@id="telechargement"]') |>
    rvest::html_elements("em") |>
    rvest::html_text()

  # Demander si l'utilisateur veut télécharger cette version de baci
  question_dl <- stringr::str_glue("Voulez-vous t\u00E9l\u00E9charger BACI {version} ?")
  reponse_dl <- svDialogs::dlg_message(question_dl, "yesnocancel")$res

  if (reponse_dl == "yes"){
    # Créer le lien pour télécharger la dernière version de BACI
    dl_link <- stringr::str_glue("http://www.cepii.fr/DATA_DOWNLOAD/baci/data/BACI_{revision}_V{version}.zip")

    # # Tester si le fichier zip de BACI existe déjà

    # Si dl_zip == TRUE, télécharger le fichier zip
    # Télécharge le zip même s'il existe
    if (dl_zip == TRUE) {
      curl::multi_download(
        dl_link,
        here::here(dl_folder, stringr::str_glue("BACI_{revision}_V{version}.zip"))
      )
    }
    # Si dl_zip == FALSE, vérifier si le fichier zip existe.
    # S'il existe alors, on ne télécharge pas. Sinon, on télécharge.
    else {
      if (!file.exists(here::here(dl_folder, stringr::str_glue("BACI_{revision}_V{version}.zip")))) {
        # Si le fichier zip n'existe pas, télécharger BACI
        curl::multi_download(
          dl_link,
          here::here(dl_folder, stringr::str_glue("BACI_{revision}_V{version}.zip"))
        )
      }
    }

    # Décompresser le fichier zip au même endroit
    print("Extraction des fichier csv")
    here::here(dl_folder, stringr::str_glue("BACI_{revision}_V{version}.zip")) |>
      utils::unzip(exdir = dl_folder)

    # Créer les formats parquet pour BACI
    print("Cr\uE9ation des fichiers parquet")
    analyse.competitivite::transfo_baci_pq(
      csv_folder = dl_folder,
      path_output = dl_folder,
      version = version
    )

    # Supprimer les fichiers csv de BACI pour gain de place si rm_csv == TRUE
    if (rm_csv == TRUE) {
      dl_folder |>
        list.files(full.names = TRUE, pattern = "^BACI.*csv") |>
        purrr::walk(file.remove)
    }
    print("Donn\u00E9es de BACI t\u00E9l\uE900charg\u00E9es !")
  } else {
    print(stringr::str_glue("Refus de t\u00E9l\u00E9charger BACI {version}"))
  }
}


# Fonction pour transformer un dossier baci en format parquet
#' Transforme des fichiers csv de BACI en format parquet
#'
#' @param csv_folder Chemin d'accès au dossier contenant les fichiers csv de BACI
#' @param path_output Chemin d'accès au dossier où seront stockés les fichiers parquet de BACI (par défaut, le même que csv_folder).
#' Ils seront stockés dans un dossier nommé "BACI-parquet".
#' @param version Version des fichiers BACI. Trouvable sur la page [BACI](http://www.cepii.fr/CEPII/en/bdd_modele/bdd_modele_item.asp?id=37)
#'
#' @return Les fichiers parquet de BACI.
#' @export
#'
#' @examples # Pas d'exemple.
transfo_baci_pq <- function(csv_folder, path_output = csv_folder, version){

  # Définition des messages d'erreur ----------------------------------------
  # Message d'erreur si path_baci_parquet n'est pas une chaine de caractère
  if(!is.character(csv_folder)){
    stop("csv_folder doit \uEAtre un chemin d'acc\uE8s sous forme de cha\uEEne de caract\uE8res.")
  }

  # Message d'erreur si path_output n'est pas une chaine de caractère
  if(!is.character(path_output)){
    stop("path_output doit \uEAtre un chemin d'acc\uE8s sous forme de cha\uEEne de caract\uE8res.")
  }

  baci_path_vector <-
    csv_folder |>
    list.files(full.names = TRUE, pattern = "^BACI.*csv$")

  # D2finir un schéma d'importation pour les données BACI
  schema_baci <-
    arrow::schema(
      arrow::Field$create("t", type = arrow::string()),
      arrow::Field$create("i", type = arrow::string()),
      arrow::Field$create("j", type = arrow::string()),
      arrow::Field$create("k", type = arrow::string()), # Evite la disparition de 0
      arrow::Field$create("v", type = arrow::string()), # être sur qu'il n'y a pas de pb
      arrow::Field$create("q", type = arrow::string()) # être sur qu'il n'y a pas de pb
    )

  df_country_codes <-
    here::here(csv_folder, stringr::str_glue("country_codes_V{version}.csv")) |>
    readr::read_csv() |>
    dplyr::select(country_code, country_iso3) |>
    dplyr::mutate(country_code = as.character(country_code))

  # Créer le dossier BACI-parquet s'il n'existe pas
  if (!dir.exists(here::here(path_output, "BACI-parquet"))) {
    dir.create(here::here(path_output, "BACI-parquet"), recursive = TRUE)
  }

  # Ecrire la base BACI en parquet : un par année : gain de place + efficacité
  baci_path_vector |>
    # Ouvrir BACI : pas en mémoire
    arrow::open_dataset(format = "csv", schema = schema_baci) |>
    dplyr::filter(t != "t") |>
    dplyr::left_join(
      df_country_codes,
      by = c("i" = "country_code")
    ) |>
    dplyr::rename(exporter = country_iso3) |>
    dplyr::left_join(
      df_country_codes,
      by = c("j" = "country_code")
    ) |>
    dplyr::rename(importer = country_iso3) |>
    dplyr::mutate(
      t = as.numeric(dplyr::if_else(!stringr::str_detect(stringr::str_trim(t), "^[0-9.]+$"), NA, stringr::str_trim(t))),
      i = as.numeric(dplyr::if_else(!stringr::str_detect(stringr::str_trim(i), "^[0-9.]+$"), NA, stringr::str_trim(i))),
      j = as.numeric(dplyr::if_else(!stringr::str_detect(stringr::str_trim(j), "^[0-9.]+$"), NA, stringr::str_trim(j))),
      q = as.numeric(dplyr::if_else(!stringr::str_detect(stringr::str_trim(q), "^[0-9.]+$"), NA, stringr::str_trim(q))),
      v = as.numeric(dplyr::if_else(!stringr::str_detect(stringr::str_trim(v), "^[0-9.]+$"), NA, stringr::str_trim(v)))
    ) |>
    # Grouper par année (un fichier parquet par année)
    dplyr::group_by(t) |>
    # Ecrire cette base en parquet
    arrow::write_dataset(
      path = here::here(path_output, "BACI-parquet"),
      format = "parquet"
    )

}
