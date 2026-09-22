library(RODBC)

# Opens an ODBC connection to SQL Server.
# The server address and credentials are read from environment variables
# (set them in a local .Renviron file -- see .Renviron.example). Nothing
# sensitive is stored in this script.
getSQLConnection <- function(UserName = Sys.getenv("SQL_USER"),
                             Password = Sys.getenv("SQL_PWD"),
                             Database = Sys.getenv("SQL_DB"),
                             Server   = Sys.getenv("SQL_SERVER")) {

  if (!nzchar(Server)) {
    stop("SQL_SERVER is not set. Add it to your .Renviron file.")
  }

  hostOS <- Sys.info()[["sysname"]]

  if (hostOS == "Windows") {
    sqlDriver <- "{SQL Server}"
    dsnName   <- Server
  } else {
    sqlDriver <- "ODBC Driver 17 for SQL Server"
    port      <- Sys.getenv("SQL_PORT", "1433")
    dsnName   <- paste0(Server, ",", port)
  }

  connString <- paste0(
    "driver=", sqlDriver,
    ";server=", dsnName,
    ";database=", Database,
    ";uid=", UserName,
    ";pwd=", Password
  )

  conn <- tryCatch(
    odbcDriverConnect(connString),
    error = function(e) stop(paste("Connection failed:", e$message))
  )

  if (inherits(conn, "RODBC")) {
    return(conn)
  } else {
    # Do not print the connection string: it contains the password
    stop("Connection failed. Check driver, server address, and credentials in .Renviron.")
  }
}
