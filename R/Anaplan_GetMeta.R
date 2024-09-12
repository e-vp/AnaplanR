

anaplan_list_workspaces <- function( token = anaplan_auth() ){

  body <- httr2::request("https://api.anaplan.com/2/0") |>
    httr2::req_url_path_append("/workspaces") |>
    httr2::req_headers( Authorization = paste("AnaplanAuthToken", httr2::resp_body_json(token)$tokenInfo$tokenValue )) |>
    httr2::req_headers( Accept = "application/json") |>
    httr2::req_perform() |>
    httr2::resp_body_json()

  tibble::as_tibble_col( body[['workspaces']],  column_name = "value" ) |>
    tidyr::unnest_wider( value )

}

anaplan_list_models <- function( token = anaplan_auth(), workspaceid, activeonly = TRUE, details = TRUE ){

  body <- httr2::request("https://api.anaplan.com/2/0") |>
    httr2::req_url_path_append("/workspaces", workspaceid, "models") |>
    httr2::req_url_query( modelDetails = if (details == TRUE) "true" else "false"   ) |>
    httr2::req_headers( Authorization = paste("AnaplanAuthToken", httr2::resp_body_json(token)$tokenInfo$tokenValue )) |>
    httr2::req_headers( Accept = "application/json") |>
    httr2::req_perform() |>
    httr2::resp_body_json()

  tibble::as_tibble_col( body[['models']],  column_name = "value" ) |>
    tidyr::unnest_wider( value ) |>
    dplyr::filter( activeState != "ARCHIVED" | !(activeonly) )

}


anaplan_list_lineitems <- function( token = anaplan_auth(), modelid ){

  body <- httr2::request("https://api.anaplan.com/2/0") |>
    httr2::req_url_path_append("/models", modelid, "lineItems") |>
    httr2::req_url_query( includeAll = "true" ) |>
    httr2::req_headers( Authorization = paste("AnaplanAuthToken", httr2::resp_body_json(token)$tokenInfo$tokenValue )) |>
    httr2::req_headers( Accept = "application/json") |>
    httr2::req_perform() |>
    httr2::resp_body_json()

  tibble::as_tibble_col( body[['items']],  column_name = "value" ) |>
    tidyr::unnest_wider( value )

}


anaplan_export_viewdata <- function( token = anaplan_auth(), modelid, moduleid ){

  body <- httr2::request("https://api.anaplan.com/2/0") |>
    httr2::req_url_path_append("/models", modelid, "views", moduleid, "data" ) |>
    httr2::req_url_query( format = "v1" ) |>
    httr2::req_url_query( exportType = "TABULAR_SINGLE_COLUMN" ) |>
    httr2::req_url_query( moduleId = moduleid ) |>
    httr2::req_headers( Authorization = paste("AnaplanAuthToken", httr2::resp_body_json(token)$tokenInfo$tokenValue )) |>
    httr2::req_headers( Accept = "text/csv") |>
    httr2::req_perform() |>
    httr2::resp_body_string()

    read.table(text = body, sep =",", header = TRUE, stringsAsFactors = FALSE)
}


anaplan_bulkdata_initReq <- function( token = anaplan_auth(), workspaceid, modelid, viewid ){

  body <- httr2::request("https://api.anaplan.com/2/0") |>
    httr2::req_url_path_append("/workspaces", workspaceid, "models", modelid, "views", viewid, "readRequests" ) |>
    httr2::req_headers( Authorization = paste("AnaplanAuthToken", httr2::resp_body_json(token)$tokenInfo$tokenValue )) |>
    httr2::req_headers( Accept = "application/json") |>
    httr2::req_headers( 'Content-Type' = "application/json") |>
    httr2::req_body_json( list( exportType="TABULAR_SINGLE_COLUMN")) |>
    httr2::req_perform() |>
    httr2::resp_body_json()
}

anaplan_bulkdata_getStatus <- function( token = anaplan_auth(), workspaceid, modelid, viewid, requestid ){

  body <- httr2::request("https://api.anaplan.com/2/0") |>
    httr2::req_url_path_append("/workspaces", workspaceid, "models", modelid, "views", viewid, "readRequests", requestid ) |>
    httr2::req_headers( Authorization = paste("AnaplanAuthToken", httr2::resp_body_json(token)$tokenInfo$tokenValue )) |>
    httr2::req_headers( Accept = "application/json") |>
    httr2::req_throttle( rate = 10 / 60) |>
    httr2::req_perform() |>
    httr2::resp_body_json()
}


anaplan_bulkdata_getPages <- function( token = anaplan_auth(), workspaceid, modelid, viewid, requestid, pageno ){

  body <- httr2::request("https://api.anaplan.com/2/0") |>
    httr2::req_url_path_append("/workspaces", workspaceid, "models", modelid, "views", viewid, "readRequests", requestid, "pages", pageno  ) |>
    httr2::req_headers( Authorization = paste("AnaplanAuthToken", httr2::resp_body_json(token)$tokenInfo$tokenValue )) |>
    httr2::req_headers( Accept = "text/csv") |>
    httr2::req_perform() |>
    httr2::resp_body_string()
}

anaplan_bulkdata_deleteReq <- function( token = anaplan_auth(), workspaceid, modelid, viewid, requestid){

  body <- httr2::request("https://api.anaplan.com/2/0") |>
    httr2::req_url_path_append("/workspaces", workspaceid, "models", modelid, "views", viewid, "readRequests", requestid ) |>
    httr2::req_headers( Authorization = paste("AnaplanAuthToken", httr2::resp_body_json(token)$tokenInfo$tokenValue )) |>
    httr2::req_headers( Accept = "application/json") |>
    httr2::req_method( "DELETE") |>
    httr2::req_perform() |>
    httr2::resp_body_string()

}


anaplan_bulkdata_read <- function( token = anaplan_auth(), workspaceid, modelid, viewid ){

  request <- anaplan_bulkdata_initReq( token,  workspaceid, modelid, viewid )

  if ( request$status$code == 200 ) {

    reqStatus <- anaplan_bulkdata_getStatus(
      token,
      workspaceid,
      modelid,
      viewid,
      request$viewReadRequest$requestId
    )
    while ( reqStatus$viewReadRequest$requestState != "COMPLETE" ){
      reqStatus <- anaplan_bulkdata_getStatus(
        token,
        workspaceid,
        modelid,
        viewid,
        request$viewReadRequest$requestId
      )
    }

    cli::cli_progress_bar( "% of total ", total = reqStatus$viewReadRequest$availablePages  )
    result_str <- ""

    for (pg in seq( from = 0, to = reqStatus$viewReadRequest$availablePages-1, by = 1)) {

      cli::cli_progress_update()

      result_str <- result_str |>
        stringr::str_c(
          anaplan_bulkdata_getPages(
            token,
            workspaceid,
            modelid,
            viewid,
            request$viewReadRequest$requestId,
            pg)
        )
    }

    cli::cli_progress_done()
    anaplan_bulkdata_deleteReq( token, workspaceid, modelid, viewid, request$viewReadRequest$requestId )

  }

  return(
    list(
      str = result_str,
      pages = reqStatus$viewReadRequest$availablePages)
    )

}

