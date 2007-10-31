.onLoad <- function(library, section) {
   require("methods")
   require("TSdbi")
   require("RMySQL")
   }

setClass("TSMySQLConnection", contains=c("MySQLConnection","TSdbOptions")) 
setMethod("print", "TSMySQLConnection", function(x, ...) {
    cat("database: ", x@dbname) 
    if (x@vintage) cat( " Has vintages." )
    if (x@panel) cat( " Has panels." )
    cat("\n")
    print(class(x)) 
    invisible(x)
    })

setMethod("TSconnect",   signature(drv="MySQLDriver", dbname="character"),
   definition=function(drv, dbname) {
        con <- dbConnect(drv, dbname=dbname)
	if(0 == length(dbListTables(con))){
	  dbDisconnect(con)
          stop("Database ",dbname," has no tables.")
	  }
	if(!dbExistsTable(con, "Meta")){
	  dbDisconnect(con)
          stop("Database ",dbname," does not appear to be a TS database.")
	  }
	panel   <- dbExistsTable(con, "Panels")
	vintage <- dbExistsTable(con, "Vintages")
	new("TSMySQLConnection" , con, dbname=dbname, 
	                      vintage=vintage, panel=panel) 
	})

setMethod("TSput",   signature(x="ANY", serIDs="character", con="MySQLConnection"),
   definition= function(x, serIDs, con=options()$TSconnection, ...)
      TSdbi:::TSput.SQL(x, serIDs, con,...) )

setMethod("TSget",   signature(serIDs="character", con="MySQLConnection"),
   definition= function(serIDs, con=options()$TSconnection, ...)
       TSdbi:::TSget.SQL(serIDs, con, ...) )

setMethod("TSdates",    signature(serIDs="character", con="MySQLConnection"),
   definition= function(serIDs, con=options()$TSconnection, ...)
      TSdbi:::TSdates.SQL(serIDs, con, ...) )


setMethod("TSdoc",   signature(x="character", con="MySQLConnection"),
   definition= function(x, con=options()$TSconnection, ...)
        TSdbi:::TSdoc.SQL(x=x, con=con, ...) )

setMethod("TSdescription",   signature(x="character", con="MySQLConnection"),
   definition= function(x, con=options()$TSconnection, ...)
        TSdbi:::TSdescription.SQL(x=x, con=con, ...) )

setMethod("TSdelete", signature(serIDs="character", con="MySQLConnection"),
   definition= function(serIDs, con=options()$TSconnection, ...)
       TSdbi:::TSdelete.SQL(serIDs=serIDs, con=con, ...) )
