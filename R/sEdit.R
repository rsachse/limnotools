## package: simecol
## sEdit = simple Edit
## function for editing named vectors and simple lists
## Thomas.Petzoldt@Mailbox.TU-Dresden.de
## License: GPL 2.0 or above
## 16.02.2004

## ToDo
##  handle long vectors/lists (with several pages)
##  handle character vectors correctly
##  error handling if wrong data are entered

sEdit <- function(name, title="Please enter values:") {
    ## conversion functions
    chrToNum <- function(x) {
      row.names <- names(x)
      x <- type.convert(x, as.is=TRUE)
      names(x) <- row.names
      x
    }
    listToNum <- function(x) {
      if (is.character(x)) {
        type.convert(unlist(strsplit(x, ",")), as.is=TRUE)
      } else {
        unlist(x)
      }
    }
    listToStr <- function(x) {
      paste(x, collapse=", ")
    }
    ## create and handle dialog box
    editVec <- function(slot) {
      ## dialog box helper functions
      build <- function(slot) {
        ret <- character(length(slot))
        for (i in 1:length(slot)) {
            ret[i] <- tclvalue(row.names[i])
        }
        ret
      }
      reset <- function() {
        for (i in 1:length(slot)) {
            tclvalue(row.names[i]) <- slot[i]
        }
      }
      ## create dialog box
      tt <- tktoplevel()
      tkwm.title(tt,title)
      entries <- as.list(slot)
      row.names <- names(slot)
      if (is.null(row.names)) {
        row.names <- paste("var",1:length(slot),sep="")
      }
      for (i in 1:length(slot)) {
        entries[[i]] <- tkentry(tt, textvariable=row.names[i])       
        tkgrid(tklabel(tt,text=row.names[i]), entries[[i]])
      }
      reset.but  <- tkbutton(tt, text="Reset", command=reset)
      submit.but <- tkbutton(tt, text="OK",
                             command=function()tclvalue(done) <- 1)
      tkgrid(reset.but, submit.but)
      done <- tclVar(0)

      ## capture destroy event
      tkbind(tt, "<Destroy>", function()tclvalue(done) <- 2)
    
      ## initialize with oiginal slot values
      reset()
      tkwm.deiconify(tt) # raises the tk window

      tkwait.variable(done)
      if(tclvalue(done)=="2") stop("dialog cancelled")
      tkdestroy(tt)
      ret <- build(slot)
      names(ret) <- names(slot) # restore original names
      ret
    }
    ## -------------- main ----------------
    tcltk <- require("tcltk", quietly=TRUE)
    if (is.vector(name) & !is.list(name) & (tcltk)) {
      ## slot is a vector
      ret  <- editVec(name)
      ret  <- chrToNum(ret)
    } else if (is.list(name) & (sum(sapply(name, is.list)) < 1) & (tcltk)) {
      ## slot is a list of vectors
      name <- sapply(name, listToStr)
      ret  <- editVec(name)
      ret  <- lapply(ret, listToNum)
    } else {
      ## default editor, e.g. data.frame or if tcltk is missing
      ret  <- edit(name) 
    }
    return(ret)
}

