#' @name connect_db
#'
#' @title Connect to PostgreSQL database
#'
#' @description
#' This function opens a prompt to insert connection details to a PostgreSQL
#' database.
#'
#' @param dbname,host,port Character values passed to [DBI::dbConnect()].
#' @param user,password Character values. They are also passed to
#'     [DBI::dbConnect()] but can be alternatively inserted or modified in the
#'     prompt.
#' @param ... Further arguments passed to [DBI::dbConnect()].
#'
#' @return
#' A connection as [PostgreSQLConnection-class].
#'
#' @references
#' http://r.789695.n4.nabble.com/tkentry-that-exits-after-RETURN-tt854721.html#none
#'
#' https://gist.github.com/mages/2aed2a053e355e3bfe7c#file-getlogindetails-r
#'
#' Dalgaard (2001).
#'
#' @author Miguel Alvarez \email{kamapu@@posteo.com}
#'
#' @export
connect_db <- function(dbname = "", host = "localhost", port = "5432",
                       user = "", password = "", ...) {
  # Top level
  tt <- tktoplevel()
  tkwm.title(tt, "Connect Database")
  # Preset values
  User <- tclVar(user)
  Password <- tclVar(password)
  # Labels
  label_User <- tklabel(tt, text = "User:")
  label_Password <- tklabel(tt, text = "Password:")
  # Boxes
  entry_User <- tkentry(tt, width = "20", textvariable = User)
  entry_Password <- tkentry(tt, width = "20", show = "*", textvariable = Password)
  # The grid
  # tkgrid(tklabel(tt, text="Enter your login details"))
  tkgrid(label_User, entry_User)
  tkgrid(label_Password, entry_Password)
  # Nicier arrangements
  tkgrid.configure(entry_User, entry_Password, sticky = "w")
  tkgrid.configure(label_User, label_Password, sticky = "e")
  # Actions
  OnOK <- function() {
    tkdestroy(tt)
  }
  OK_but <- tkbutton(tt, text = " OK ", command = OnOK)
  tkbind(entry_Password, "<Return>", OnOK)
  tkgrid(OK_but)
  tkfocus(tt)
  tkwait.window(tt)
  # Connection
  RPostgreSQL::dbConnect("PostgreSQL",
    dbname = dbname,
    host = host,
    port = port,
    user = tclvalue(User),
    password = tclvalue(Password),
    ...
  )
}
