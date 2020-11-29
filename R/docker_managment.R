
#' @export
tk_init_docker <- function(){


  # Check if Docker exists
  version <- try({system("docker -v", intern = T)})
  if(inherits(version, "try-error")){
    cli::cli_alert_danger("Unable to find the installation of docker, make sure to install docker before using `tiktokt`")
  } else {
    cli::cli_alert_success(version)
  }


  # Check image tiktoksignature
  if(any(stringr::str_detect(system("docker images", intern = T), "tiktoksignature"))){
    cli::cli_alert_success("Found image `tiktoksignature`")
  } else {
    cli::cli_alert_warning("Unable to found image `tiktoksignature` ; pull it from `github.com/benjaminguinaudeau/tiktok_signature`")
    cat("This will take some time (at least 5 minutes)")
    folder <- tempfile()
    git2r::clone("https://github.com/benjaminguinaudeau/tiktok_signature.git", local_path = folder)
    system(glue::glue("docker build {folder} -t tiktoksignature"))
    if(any(stringr::str_detect(system("docker images", intern = T), "tiktoksignature"))){
      cli::cli_alert_success("Successfully installed image `tiktoksignature`")
    } else {
      cli::cli_alert_danger("Unable to install image `tiktoksignature")
    }
  }

  # Check if container is running ; if not create it
  if(!any(stringr::str_detect(system("docker ps -a", intern = T), "tiktoksignature$"))){
    message("Creating container `tiktoksignature`... This might take a couple of minutes.")
    system("docker run -dt -p 32768:6543 --name tiktoksignature tiktoksignature:latest", intern = T)
    if(!any(stringr::str_detect(system("docker ps", intern = T), "tiktoksignature$"))){
      cli::cli_alert_danger("Unable to create container `tiktoksignature`")
    } else {
      cli::cli_alert_success("Successfully created container")
    }
    Sys.sleep(4)

  } else {
    if(any(stringr::str_detect(system("docker ps", intern = T), "tiktoksignature$"))){
      cli::cli_alert_warning("Starting container `tiktoksignature`")
      system("docker start tiktoksignature")
      Sys.sleep(4)
    } else {
    cli::cli_alert_success("Found running container `tiktoksignature`")
    }
  }


  # Test if signature can be obtained
  if(stringr::str_length(get_docker_signature("")) > 16){
    cli::cli_alert_success("Successfully signed a test url. Signature is working as expected")
  } else {
    cli::cli_alert_danger("Error when signing a url")
  }

}
