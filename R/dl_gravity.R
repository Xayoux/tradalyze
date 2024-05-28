#' @title
#' Télécharge les données de la base de données Gravity
#'
#' @description
#' Télécharge la base de données Gravity depuis le site du CEPII dans sa
#' dernière version disponible puis la transforme en format parquet avec un
#' fichier par année.
#'
#' @param dl_folder Chemin d'accès au dossier où seront stockés les fichiers
#' csv de Gravity. Ils seront stockés dans un dossier nommé
#' "Gravity_csv_V(version)". Ainsi si la version change un nouveau sous-dossier
#' sera créé.
#' @param dl_zip Si TRUE, télécharge le fichier zip de Gravity dans tous les cas.
#' Si FALSE, vérifie si le fichier zip existe déjà. Si oui, ne télécharge pas et
#' effectue uniquement le dezipage.
#'
#' @return Les fichiers parquet et csv de Gravity.
#' @export
#'
#' @examples # Pas d'exemple
#' @source Base [Gravity](http://www.cepii.fr/CEPII/fr/bdd_modele/bdd_modele_item.asp?id=8)
#' du [CEPII](http://www.cepii.fr/CEPII/fr/welcome.asp)

dl_gravity <- function(dl_folder, dl_zip = FALSE){

  # Lien vers la page de la base de données Gravity sur le site du CEPII
  html_gravity <- rvest::read_html("http://www.cepii.fr/CEPII/fr/bdd_modele/bdd_modele_item.asp?id=8")

  # Récupérer la version de Gravity pour télécharger la dernière version disponible
  version <-
    html_gravity |>
    rvest::html_nodes(xpath = '//*[@id="telechargement"]') |>
    rvest::html_nodes("em") |>
    rvest::html_text() |>
    stringr::str_extract("\\d{6}")


  # Récupérer le lien de téléchargement du fichier zip de Gravity
  dl_link <-
    html_gravity |>
    rvest::html_nodes("a") |>
    rvest::html_attr("href") |>
    (\(links) grep("csv", links, value = TRUE, ignore.case = TRUE))() |>
    (\(link) grep(version, link, value = TRUE))()


  # Chemin d'accès au dossier contenant la dernière version de Gravity
  gravity_folder <-
    here::here(
      dl_folder,
      stringr::str_glue("Gravity_csv_V{version}")
    )

  # Chemin d'accès au fichier zip de Gravity dans sa dernière version
  gravity_zip <-
    here::here(
      gravity_folder,
      stringr::str_glue("Gravity_csv_V{version}.zip")
    )


  # Créer le dossier où sera mis le zip téléchargé, s'il n'existe pas déjà
  if (!dir.exists(here::here(dl_folder))) {
    dir.create(gravity_folder, recursive = TRUE)
  }


  # Télécharger le fichier zip de Gravity
  if (dl_zip == TRUE) { # Si TRUE télécharger dans tous les cas
    curl::multi_download(
      dl_link,
      gravity_zip
    )
  }
  else { # Si Faux télécharger le zip que s'il n'existe pas
    if (!file.exists(gravity_zip)) {
      # Si le fichier zip n'existe pas, télécharger BACI
      curl::multi_download(
        dl_link,
        gravity_zip
      )
    }
  }

  # Décompresser le fichier zip au même endroit
  gravity_zip |>
    utils::unzip(exdir = gravity_folder)

  # Créer les formats parquet de Gravity
  analyse.competitivite::transfo_gravity_pq(
    csv_folder = gravity_folder,
    path_output = gravity_folder,
    version = version
  )
}



# Fonction pour transformer un dossier Gravity en format parquet
#' Transforme La base de données Gravity en format parquet.
#'
#' @param csv_folder Chemin d'accès au dossier contenant les fichiers csv de
#' gravity
#' @param path_output Chemin d'accès au dossier où seront stockés les fichiers
#' parquet de Gravity (par défaut, le même que csv_folder).
#' Ils seront stockés dans un dossier nommé "Gravity-parquet".
#' @param version Version des fichiers Gravity. Trouvable sur la page
#' [Gravity](http://www.cepii.fr/CEPII/fr/bdd_modele/bdd_modele_item.asp?id=8)
#'
#' @return Les fichiers parquet de Gravity.
#' @export
#'
#' @examples # Pas d'exemple.
transfo_gravity_pq <- function(csv_folder, path_output = csv_folder, version){

  gravity_path <-
    here::here(csv_folder, stringr::str_glue("Gravity_V{version}.csv"))


  # Créer le dossier Gravity-parquet s'il n'existe pas
  if (!dir.exists(here::here(path_output, "Gravity-parquet"))) {
    dir.create(here::here(path_output, "Gravity-parquet"), recursive = TRUE)
  }

  # Ecrire la base gravity en parquet : un par année : gain de place + efficacité
  gravity_path |>
    # Ouvrir Gravity : pas en mémoire
    arrow::read_csv_arrow(as_data_frame = FALSE) |>
    # Grouper par année (un fichier parquet par année)
    dplyr::group_by(year) |>
    # Ecrire cette base en parquet
    arrow::write_dataset(
      path = here::here(path_output, "Gravity-parquet"),
      format = "parquet"
    )
}
