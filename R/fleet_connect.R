#' Connect to Pyfleets
#'
#' @param api_token API token for configuration.  Default pulls from
#' environment variables
#' @param coordinator_address coordinator address for configuration.
#' Default pulls from environment variables
#'
#' @return A fleet session object
#' @export
#'
#' @examples
#' res = fleet_connect()
fleet_connect = function(api_token =  Sys.getenv("DATAFLEETS_API_KEY"),
                         coordinator_address = Sys.getenv("DATAFLEETS_COORDINATOR")) {
  pf = reticulate::import("pyfleets", convert = FALSE)
  pf_nb = reticulate::import("pyfleets.notebook", convert = FALSE)

  FleetSession = pf$FleetSession

  config = list(
    'api-token' =  api_token,
    'coordinator.address' = coordinator_address,
  )

  fleet = FleetSession$builder$config(config)$get_or_create()
  # show_progress(fleet)
  return(fleet)
}


#' Run SQL Query for Pyfleets
#'
#' @param fleet An output of \code{\link{fleet_connect}}
#' @param query the SQL query to send to pyfleets
#' @param meta meta data information, passed to `QueryParameters`
#' @param epsilon epsilon for differential privacy, passed to `QueryParameters`
#' @param max_contrib maximum contribution, passed to `QueryParameters`
#' @param schema SQL schema to pass to `QueryParameters`
#' @param use_cached_result logical passed to `QueryParameters`
#' @param ... pass through arguments
#'
#' @return A `data.frame` of data
#' @export
fleet_sql = function(fleet, query, ...) {
  tfile = csv_query(fleet, query, ...)
  readr::read_csv(tfile)
}


#' @export
#' @rdname csv_query
csv_query = function(fleet,
                     query,
                     meta = NULL,
                     epsilon=NULL,
                     max_contrib=NULL,
                     schema=NULL,
                     use_cached_result=TRUE) {


  params = pf$QueryParameters(sql=query,
                              epsilon=epsilon,
                              max_contrib=max_contrib,
                              meta=meta,
                              use_cached_result=use_cached_result)
  confirmation = fleet$comp$query_sql(params)
  result_id = confirmation$artifact_result
  result_id = as.character(result_id)

  request_id = as.character(confirmation$request_id)
  # result_id = self._wait_for_result(confirmation.request_id, "query").artifact_result

  status = "not done"
  while (status != "completed") {
    out = fleet$comp$poll_request_state(request_id)
    status = as.character(out$status)
    print(status)
    Sys.sleep(2)
  }

  csv = fleet$comp$get_query_result(result_id)
  csv = as.character(csv)
  csv
}

#' @export
#' @rdname csv_query
csv_query_file = function(fleet, query, ...) {
  csv = csv_query(fleet, query, ...)
  tfile = tempfile(fileext = ".csv")
  writeLines(csv, con = tfile)
  return(tfile)
}


