#' @title
#' Télécharger BACI et la transformer en parquet
#'
#' @description
#' Fonction qui permet de télécharger BACI dans la nomenclature voulue
#' (uniquement dernière version disponible).
#' Les données sont ensuite transformées en fichiers parquet (1 par année).
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
dl_baci <- function(revision = "HS92", dl_folder, rm_csv = TRUE,
                    dl_zip = FALSE){

  # Messages d'erreurs + avertissements -------------------------------------
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

  # Information sur BACI ----------------------------------------------------
  # Lien vers la page BACI du cepii
  html_baci <- rvest::read_html("http://www.cepii.fr/CEPII/en/bdd_modele/bdd_modele_item.asp?id=37")

  # Récupérer l'information sur la dernière version de BACI
  version <-
    html_baci |>
    rvest::html_nodes(xpath = '//*[@id="telechargement"]') |>
    rvest::html_elements("em") |>
    rvest::html_text()

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
  here::here(dl_folder, stringr::str_glue("BACI_{revision}_V{version}.zip")) |>
    utils::unzip(exdir = dl_folder)

  # Créer les formats parquet pour BACI
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
}
