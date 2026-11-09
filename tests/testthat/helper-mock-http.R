# Build a real httr response object around a fixed body, so the package's own
# parsing code (httr::content(), xml2, jsonlite) runs exactly as it would
# against the vendor. No network access is involved.
fake_response <- function(body = "",
                          status = 200L,
                          type = "text/html; charset=utf-8",
                          method = "GET",
                          url = "https://example.test/") {
  raw_body <- if (is.raw(body)) body else charToRaw(enc2utf8(body))
  structure(
    list(
      url = url,
      status_code = as.integer(status),
      headers = httr::insensitive(list(`content-type` = type)),
      all_headers = list(),
      cookies = data.frame(),
      content = raw_body,
      date = Sys.time(),
      times = numeric(0),
      request = structure(list(method = method, url = url), class = "request"),
      handle = NULL
    ),
    class = "response"
  )
}

fake_json <- function(x, status = 200L, method = "GET") {
  fake_response(
    jsonlite::toJSON(x, auto_unbox = TRUE, null = "null"),
    status = status,
    type = "application/json; charset=utf-8",
    method = method
  )
}

# Route mocked httr verbs by request path. `routes` is a named list mapping a
# path such as "API/user/login" to a function(args) returning a response.
# Every call is recorded in `calls` for assertions on what was sent.
http_router <- function(routes) {
  state <- new.env(parent = emptyenv())
  state$calls <- list()
  handle <- function(verb, args) {
    path <- paste(unlist(args$path), collapse = "/")
    call <- c(list(verb = verb, path = path), args)
    state$calls[[length(state$calls) + 1]] <- call
    route <- routes[[paste(verb, path)]] %||% routes[[path]]
    if (is.null(route)) {
      stop("No mocked route for ", verb, " ", path, call. = FALSE)
    }
    route(args)
  }
  list(
    retry = function(verb, ...) handle(verb, list(...)),
    get = function(...) handle("GET", list(...)),
    post = function(...) handle("POST", list(...)),
    calls = function() state$calls
  )
}

`%||%` <- function(x, y) if (is.null(x)) y else x
